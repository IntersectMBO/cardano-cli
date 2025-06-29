{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}

module Test.Cardano.CLI.Hedgehog
  ( module Hedgehog.Extras.Test
  , runWithWatchdog_
  )
where

import Control.Concurrent qualified as IO
import Control.Concurrent.STM.TChan (TChan, newTChanIO, tryReadTChan, writeTChan)
import Control.Monad.Base
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Control
import Data.Time
  ( UTCTime
  , diffUTCTime
  , getCurrentTime
  )
import GHC.Conc.Sync
import GHC.Stack (HasCallStack)

import Hedgehog.Extras.Test hiding
  ( Watchdog
  , kickWatchdog
  , makeWatchdog
  , runWatchdog
  , runWithWatchdog
  , runWithWatchdog_
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
  :: HasCallStack
  => MonadBaseControl IO m
  => WatchdogConfig
  -- ^ configuration
  -> (HasCallStack => m a)
  -- ^ a test case to be wrapped in watchdog
  -> m a
runWithWatchdog_ config testCase = runWithWatchdog config (const testCase)

-- | Execute a test case with a watchdog.
runWithWatchdog
  :: HasCallStack
  => MonadBaseControl IO m
  => WatchdogConfig
  -- ^ configuration
  -> (HasCallStack => Watchdog -> m a)
  -- ^ a test case to be wrapped in watchdog
  -> m a
runWithWatchdog config testCase = do
  watchedThreadId <- liftBase IO.myThreadId
  watchdog <- liftBase $ makeWatchdog config watchedThreadId
  H.withAsync (runWatchdog watchdog) $
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

-- | Run watchdog in a loop in the current thread. Usually this function should be used with 'H.withAsync'
-- to run it in the background.
runWatchdog
  :: MonadBase IO m
  => Watchdog
  -> m ()
runWatchdog w@Watchdog{watchedThreadId, startTime, kickChan} = liftBase $ do
  atomically (tryReadTChan kickChan) >>= \case
    Just PoisonPill ->
      -- deactivate watchdog
      pure ()
    Just (Kick kickTimeout) -> do
      -- got a kick, wait for another period
      IO.threadDelay $ kickTimeout * 1_000_000
      runWatchdog w
    Nothing -> do
      -- we are out of scheduled timeouts, kill the monitored thread
      currentTime <- getCurrentTime
      IO.throwTo watchedThreadId . WatchdogException $ diffUTCTime currentTime startTime
