{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.Run
  ( runAnyEraCommand
  , runCmds
  , runGovernanceCmds
  )
where

import Cardano.Api
import Cardano.Api.Experimental (IsEra, obtainCommonConstraints)

import Cardano.CLI.EraBased.Command
import Cardano.CLI.EraBased.Genesis.Run
import Cardano.CLI.EraBased.Governance.Run
import Cardano.CLI.EraBased.Query.Run
import Cardano.CLI.EraBased.StakeAddress.Run
import Cardano.CLI.EraBased.StakePool.Run
import Cardano.CLI.EraBased.TextView.Run
import Cardano.CLI.EraBased.Transaction.Run
import Cardano.CLI.EraIndependent.Address.Run
import Cardano.CLI.EraIndependent.Key.Run
import Cardano.CLI.EraIndependent.Node.Run
import Cardano.CLI.Helper (printEraDeprecationWarning)
import Cardano.CLI.Type.Error.CmdError

import RIO

runAnyEraCommand
  :: ()
  => AnyEraCommand
  -> ExceptT CmdError IO ()
runAnyEraCommand = \case
  AnyEraCommandOf era cmd -> do
    printEraDeprecationWarning era
    obtainCommonConstraints era $ runCmds cmd

runCmds
  :: IsEra era
  => Cmds era
  -> ExceptT CmdError IO ()
runCmds = \case
  AddressCmds cmd ->
    newExceptT $
      runRIO () $
        (Right <$> runAddressCmds cmd)
          `catch` (pure . Left . CmdBackwardCompatibleError)
  KeyCmds cmd ->
    newExceptT $
      runRIO () $
        (Right <$> runKeyCmds cmd)
          `catch` (pure . Left . CmdBackwardCompatibleError)
  GovernanceCmds cmd ->
    newExceptT $
      runRIO () $
        (Right <$> runGovernanceCmds cmd)
          `catch` (pure . Left . CmdBackwardCompatibleError)
  GenesisCmds cmd ->
    newExceptT $
      runRIO () $
        (Right <$> runGenesisCmds cmd)
          `catch` (pure . Left . CmdBackwardCompatibleError)
  NodeCmds cmd ->
    newExceptT $
      runRIO () $
        (Right <$> runNodeCmds cmd)
          `catch` (pure . Left . CmdBackwardCompatibleError)
  QueryCmds cmd ->
    newExceptT $
      runRIO () $
        (Right <$> runQueryCmds cmd)
          `catch` (pure . Left . CmdBackwardCompatibleError)
  StakeAddressCmds cmd ->
    newExceptT $
      runRIO () $
        (Right <$> runStakeAddressCmds cmd)
          `catch` (pure . Left . CmdBackwardCompatibleError)
  StakePoolCmds cmd ->
    newExceptT $
      runRIO () $
        (Right <$> runStakePoolCmds cmd)
          `catch` (pure . Left . CmdBackwardCompatibleError)
  TextViewCmds cmd ->
    runTextViewCmds cmd
      & firstExceptT CmdTextViewError
  TransactionCmds cmd ->
    runTransactionCmds cmd
      & firstExceptT CmdTransactionError
