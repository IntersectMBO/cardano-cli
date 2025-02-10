{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.Compatible.Run
  ( CompatibleCmdError
  , renderCompatibleCmdError
  , runAnyCompatibleCommand
  , runCompatibleCommand
  )
where

import Cardano.Api

import Cardano.CLI.Compatible.Commands
import Cardano.CLI.Compatible.Governance
import Cardano.CLI.Compatible.Transaction
import Cardano.CLI.Render
import Cardano.CLI.Types.Errors.CmdError

import RIO

import Data.Text (Text)

data CompatibleCmdError
  = CompatibleTransactionError CompatibleTransactionError
  | CompatibleGovernanceError CmdError

renderCompatibleCmdError :: Text -> CompatibleCmdError -> Doc ann
renderCompatibleCmdError cmdText = \case
  CompatibleTransactionError e -> renderAnyCmdError cmdText prettyError e
  CompatibleGovernanceError e -> renderCmdError cmdText e

runAnyCompatibleCommand :: AnyCompatibleCommand -> ExceptT CompatibleCmdError IO ()
runAnyCompatibleCommand (AnyCompatibleCommand cmd) = runCompatibleCommand cmd

runCompatibleCommand :: CompatibleCommand era -> ExceptT CompatibleCmdError IO ()
runCompatibleCommand (CompatibleTransactionCmd txCmd) =
  liftIO $
    executeRio
      (runCompatibleTransactionCmd txCmd)
runCompatibleCommand (CompatibleGovernanceCmds govCmd) =
  firstExceptT CompatibleGovernanceError $ runCompatibleGovernanceCmds govCmd

-- | This is a temporary function until all commands are migrated to RIO
-- Once this happens we can remove this function and rely on `toplevelExceptionHandler`
executeRio :: RIO () () -> IO ()
executeRio r = do
  runRIO () r
    `catch` (\(e :: SomeException) -> putStrLn $ displayException e)
