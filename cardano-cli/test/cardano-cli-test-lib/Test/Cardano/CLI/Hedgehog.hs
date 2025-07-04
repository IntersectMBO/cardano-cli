{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.CLI.Hedgehog
  ( module Hedgehog.Extras.Test
  , moduleWorkspace
  , runWithWatchdog_
  )
where

import Control.Concurrent qualified as IO
import Control.Concurrent.STM.TChan (TChan, newTChanIO, tryReadTChan, writeTChan)
import Control.Exception (IOException)
import Control.Monad
import Control.Monad.Base
import Control.Monad.Catch (Handler (..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource (MonadResource, register)
import Control.Retry qualified as R
import Data.Maybe (listToMaybe)
import Data.Time
  ( UTCTime
  , diffUTCTime
  , getCurrentTime
  )
import GHC.Conc.Sync
import GHC.Stack (CallStack, HasCallStack)
import GHC.Stack qualified as GHC
import System.Directory qualified as IO
import System.Environment qualified as IO
import System.FilePath ((</>))
import System.IO qualified as IO
import System.IO.Temp qualified as IO
import System.Info qualified as IO

import Hedgehog (MonadTest)
import Hedgehog qualified as H
import Hedgehog.Extras.Stock.CallStack
import Hedgehog.Extras.Test hiding
  ( Watchdog
  , kickWatchdog
  , makeWatchdog
  , moduleWorkspace
  , runWatchdog
  , runWithWatchdog
  , runWithWatchdog_
  , workspace
  )
import Hedgehog.Extras.Test.Concurrent qualified as H

-- | Watchdog command
data WatchdogCommand
  = -- | Add another delay in seconds
    Kick !Int
  | -- | Stop the watchdog
    PoisonPill

-- | A watchdog instance. See the module header for more detailed description.
data Watchdog = Watchdog
  { watchdogConfig :: !WatchdogConfig
  , watchedThreadId :: !ThreadId
  -- ^ monitored thread id
  , startTime :: !UTCTime
  -- ^ watchdog creation time
  , kickChan :: TChan WatchdogCommand
  -- ^ a queue of watchdog commands
  }

-- | Execute a test case with a watchdog.
runWithWatchdog_
  :: MonadBaseControl IO m
  => CallStack
  -> WatchdogConfig
  -- ^ configuration
  -> (HasCallStack => m a)
  -- ^ a test case to be wrapped in watchdog
  -> m a
runWithWatchdog_ cs config testCase =
  runWithWatchdog cs config (const testCase)

-- | Execute a test case with a watchdog.
runWithWatchdog
  :: MonadBaseControl IO m
  => CallStack
  -> WatchdogConfig
  -- ^ configuration
  -> (HasCallStack => Watchdog -> m a)
  -- ^ a test case to be wrapped in watchdog
  -> m a
runWithWatchdog cs config testCase = do
  watchedThreadId <- liftBase IO.myThreadId
  watchdog <- liftBase $ makeWatchdog config watchedThreadId
  H.withAsync (runWatchdog cs watchdog) $
    \_ -> testCase watchdog

-- | Create manually a new watchdog, providing the target thread ID. After all watchdog timeouts expire,
-- the target thread will get 'WatchdogException' thrown to it asynchronously (using 'throwTo').
makeWatchdog
  :: MonadBase IO m
  => WatchdogConfig
  -> ThreadId
  -- ^ thread id which will get killed after all kicks expire
  -> m Watchdog
makeWatchdog config watchedThreadId' = liftBase $ do
  watchdog <- Watchdog config watchedThreadId' <$> getCurrentTime <*> newTChanIO
  kickWatchdog watchdog
  pure watchdog

-- | Enqueue a kick for the watchdog. It will extend the timeout by another one defined in the watchdog
-- configuration.
kickWatchdog :: MonadIO m => Watchdog -> m ()
kickWatchdog Watchdog{watchdogConfig = WatchdogConfig{watchdogTimeout}, kickChan} =
  liftIO $
    atomically $
      writeTChan kickChan (Kick watchdogTimeout)

getCallerLocations :: CallStack -> String
getCallerLocations cs =
  case GHC.getCallStack cs of
    a : b : _ -> uncurry getCallerLocation a <> ", " <> uncurry getCallerLocation b
    a : _ -> uncurry getCallerLocation a
    _ -> "<no call stack>"

getCallerLocation :: String -> GHC.SrcLoc -> String
getCallerLocation callerName callerLoc =
  GHC.srcLocFile callerLoc <> ":" <> show (GHC.srcLocStartLine callerLoc) <> ": " <> callerName

-- | Run watchdog in a loop in the current thread. Usually this function should be used with 'H.withAsync'
-- to run it in the background.
runWatchdog
  :: MonadBase IO m
  => CallStack
  -> Watchdog
  -> m ()
runWatchdog cs w@Watchdog{watchedThreadId, startTime, kickChan} =
  GHC.withFrozenCallStack $ liftBase $ do
    atomically (tryReadTChan kickChan) >>= \case
      Just PoisonPill ->
        -- deactivate watchdog
        pure ()
      Just (Kick kickTimeout) -> do
        -- got a kick, wait for another period
        IO.threadDelay $ kickTimeout * 1_000_000
        runWatchdog cs w
      Nothing -> do
        -- we are out of scheduled timeouts, kill the monitored thread
        currentTime <- getCurrentTime
        liftIO $ IO.hPutStrLn IO.stderr $ "===> kill: " <> getCallerLocations cs
        liftIO $ IO.hFlush IO.stderr
        IO.throwTo watchedThreadId . WatchdogException $ diffUTCTime currentTime startTime

-- | Create a workspace directory which will exist for at least the duration of
-- the supplied block.
--
-- The directory will have the prefix as "$prefixPath/$moduleName" but contain a generated random
-- suffix to prevent interference between tests
--
-- The directory will be deleted if the block succeeds, but left behind if
-- the block fails.
--
-- The 'prefix' argument should not contain directory delimeters.
moduleWorkspace
  :: HasCallStack
  => MonadResource m
  => MonadTest m
  => String
  -> (FilePath -> m ())
  -> m ()
moduleWorkspace prefix f = GHC.withFrozenCallStack $ do
  let srcModule = maybe "UnknownModule" (GHC.srcLocModule . snd) (listToMaybe (GHC.getCallStack GHC.callStack))
  workspace (prefix <> "-" <> srcModule) f

-- | Create a workspace directory which will exist for at least the duration of
-- the supplied block.
--
-- The directory will have the supplied prefix but contain a generated random
-- suffix to prevent interference between tests
--
-- The directory will be deleted if the block succeeds, but left behind if
-- the block fails.
workspace
  :: MonadTest m
  => HasCallStack
  => MonadResource m
  => FilePath
  -> (FilePath -> m ())
  -> m ()
workspace prefixPath f = GHC.withFrozenCallStack $ do
  systemTemp <- H.evalIO IO.getCanonicalTemporaryDirectory
  maybeKeepWorkspace <- H.evalIO $ IO.lookupEnv "KEEP_WORKSPACE"
  ws <- H.evalIO $ IO.createTempDirectory systemTemp $ prefixPath <> "-test"
  H.annotate $ "Workspace: " <> ws
  H.evalIO $ IO.writeFile (ws </> "module") callerModuleName
  f ws
  when (IO.os /= "mingw32" && maybeKeepWorkspace /= Just "1") $ do
    -- try to delete the directory 20 times, 100ms apart
    let retryPolicy = R.constantDelay 100_000 <> R.limitRetries 20
        -- retry only on IOExceptions
        ioExH _ = Handler $ \(_ :: IOException) -> pure True
    -- For some reason, the temporary directory removal sometimes fails.
    -- Lets wrap this in MonadResource to try multiple times, during the cleanup, before we fail.
    void
      . register
      . R.recovering retryPolicy [ioExH]
      . const
      $ IO.removePathForcibly ws
