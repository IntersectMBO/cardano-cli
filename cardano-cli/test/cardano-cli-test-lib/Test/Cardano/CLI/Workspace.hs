{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.CLI.Workspace
  ( moduleWorkspace2
  )
where

import Prelude (Num (..), Ord (..))

import Control.Applicative (Applicative (..))
import Control.Concurrent qualified as IO
import Control.Exception (IOException)
import Control.Exception.Lifted (try)
import Control.Monad (Monad (return), when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Resource (MonadResource)
import Data.Bool ((&&))
import Data.Either (Either (..))
import Data.Eq (Eq ((/=)))
import Data.Function (($), (.))
import Data.Int (Int)
import Data.Maybe (Maybe (..), listToMaybe, maybe)
import Data.Semigroup (Semigroup (..))
import Data.String (String)
import Data.Tuple (snd)
import GHC.Stack
import GHC.Stack qualified as GHC
import System.Directory qualified as IO
import System.Environment qualified as IO
import System.FilePath ((</>))
import System.IO (FilePath, IO)
import System.IO qualified as IO
import System.IO.Temp qualified as IO
import System.Info qualified as IO
import System.Timeout qualified as IO

import Hedgehog (MonadTest)
import Hedgehog qualified as H
import Hedgehog.Extras.Stock.CallStack (callerModuleName)
import Hedgehog.Extras.Test.Base hiding (moduleWorkspace, workspace)

-- | Create a workspace directory which will exist for at least the duration of
-- the supplied block.
--
-- The directory will have the supplied prefix but contain a generated random
-- suffix to prevent interference between tests
--
-- The directory will be deleted if the block succeeds, but left behind if
-- the block fails.
workspace
  :: HasCallStack
  => MonadBaseControl IO m
  => MonadResource m
  => MonadTest m
  => FilePath
  -> (FilePath -> m ())
  -> m ()
workspace prefixPath f =
  withFrozenCallStack $
    bracket init fini $ \ws -> do
      H.annotate $ "Workspace: " <> ws
      H.evalIO $ IO.writeFile (ws </> "module") callerModuleName
      f ws
 where
  init = do
    systemTemp <- H.evalIO IO.getCanonicalTemporaryDirectory
    H.evalIO $ IO.createTempDirectory systemTemp $ prefixPath <> "-test"
  fini ws = do
    maybeKeepWorkspace <- H.evalIO $ IO.lookupEnv "KEEP_WORKSPACE"
    when (IO.os /= "mingw32" && maybeKeepWorkspace /= Just "1") $
      removeWorkspaceRetries ws 20
  removeWorkspaceRetries
    :: MonadBaseControl IO m
    => MonadResource m
    => MonadTest m
    => FilePath
    -> Int
    -> m ()
  removeWorkspaceRetries ws retries =
    GHC.withFrozenCallStack $ do
      result <- try (liftIO (IO.timeout (5 * 1000) (IO.removePathForcibly ws)))
      case result of
        Right (Just ()) -> return ()
        Right Nothing -> pure ()
        Left (_ :: IOException) -> do
          if retries > 0
            then do
              liftIO (IO.threadDelay 100000) -- wait 100ms before retrying
              removeWorkspaceRetries ws (retries - 1)
            else do
              failMessage GHC.callStack "Failed to remove workspace directory after multiple attempts"

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
moduleWorkspace2
  :: HasCallStack
  => MonadBaseControl IO m
  => MonadResource m
  => MonadTest m
  => String
  -> (FilePath -> m ())
  -> m ()
moduleWorkspace2 prefix f = GHC.withFrozenCallStack $ do
  let srcModule = maybe "UnknownModule" (GHC.srcLocModule . snd) (listToMaybe (GHC.getCallStack GHC.callStack))
  workspace (prefix <> "-" <> srcModule) f
