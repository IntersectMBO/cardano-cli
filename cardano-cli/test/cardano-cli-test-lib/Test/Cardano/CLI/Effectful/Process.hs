{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.CLI.Effectful.Process
  ( execCardanoCliOk
  , execCardanoCliWithEnvVarsOk
  , execDetailCardanoCliOk
  , execDetailConfigCardanoCliOk
  )
where

import Data.Monoid (Last (..))
import GHC.Stack qualified as GHC
import System.Exit (ExitCode (..))

import Effectful (Eff, IOE)
import Effectful.Zoo.Concurrent
import Effectful.Zoo.Core
import Effectful.Zoo.Environment
import Effectful.Zoo.Error.Static
import Effectful.Zoo.FileSystem
import Effectful.Zoo.Hedgehog
import Effectful.Zoo.Hedgehog.Api.Process
import Effectful.Zoo.Hedgehog.Effect.Hedgehog
import Effectful.Zoo.Log.Static
import HaskellWorks.Error.Types
import HaskellWorks.Prelude

-- | Execute cardano-cli via the command line.
--
-- Waits for the process to finish and returns the stdout.
execCardanoCliOk
  :: HasCallStack
  => r <: Concurrent
  => r <: Environment
  => r <: Error Failure
  => r <: FileSystem
  => r <: Hedgehog
  => r <: IOE
  => r <: Log Text
  => [String]
  -- ^ Arguments to the CLI command
  -> Eff r String
  -- ^ Captured stdout
execCardanoCliOk args =
  GHC.withFrozenCallStack
    . trapFail @IOException
    . trapFail @GenericError
    $ execFlexOk "cardano-cli" "CARDANO_CLI" args

-- | Execute cardano-cli via the command line but set
-- environment variables. Fails if the process returns a non-zero exit code.
--
-- Waits for the process to finish and returns the stdout.
execCardanoCliWithEnvVarsOk
  :: HasCallStack
  => r <: Concurrent
  => r <: Environment
  => r <: Error Failure
  => r <: FileSystem
  => r <: Hedgehog
  => r <: IOE
  => r <: Log Text
  => [(String, String)]
  -- ^ Environment variables to set
  -> [String]
  -- ^ Arguments to the CLI command
  -> Eff r String
execCardanoCliWithEnvVarsOk envVars args = GHC.withFrozenCallStack $ do
  env <- getEnvironment
  result <-
    execDetailConfigCardanoCliOk
      defaultExecConfig
        { execConfigEnv = Last $ Just (envVars ++ env)
        }
      args
  case result of
    (ExitFailure _, _, stderr) -> do
      jotString_ stderr
      failure
    (ExitSuccess, stdout, _) -> return stdout

-- | Execute cardano-cli via the command line, expecting it to fail.
--
-- Waits for the process to finish and returns the exit code, stdout and stderr.
execDetailCardanoCliOk
  :: HasCallStack
  => r <: Concurrent
  => r <: Environment
  => r <: Error Failure
  => r <: FileSystem
  => r <: Hedgehog
  => r <: IOE
  => r <: Log Text
  => [String]
  -- ^ Arguments to the CLI command
  -> Eff r (ExitCode, String, String)
  -- ^ Captured stdout
execDetailCardanoCliOk params =
  GHC.withFrozenCallStack
    . trapFail @IOException
    $ execDetailConfigCardanoCliOk defaultExecConfig params

-- | Execute cardano-cli via the command line, expecting it to fail, and accepting custom config.
--
-- Waits for the process to finish and returns the exit code, stdout and stderr.
execDetailConfigCardanoCliOk
  :: HasCallStack
  => r <: Concurrent
  => r <: Environment
  => r <: Error Failure
  => r <: FileSystem
  => r <: Hedgehog
  => r <: IOE
  => r <: Log Text
  => ExecConfig
  -- ^ Configuration for the execution
  -> [String]
  -- ^ Arguments to the CLI command
  -> Eff r (ExitCode, String, String)
  -- ^ Captured stdout
execDetailConfigCardanoCliOk cfg =
  GHC.withFrozenCallStack $ do
    execDetailFlex cfg "cardano-cli" "CARDANO_CLI"
