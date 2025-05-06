{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.Run
  ( runAnyEraCommand
  , runCmds
  , runGovernanceCmds
  )
where

import Cardano.Api
import Cardano.Api.Experimental (obtainCommonConstraints)

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

import Data.Function ((&))
import Data.Typeable (Typeable)

runAnyEraCommand
  :: ()
  => AnyEraCommand
  -> ExceptT CmdError IO ()
runAnyEraCommand = \case
  AnyEraCommandOf era cmd -> do
    printEraDeprecationWarning era
    obtainCommonConstraints era $ runCmds cmd

runCmds
  :: Typeable era
  => Cmds era
  -> ExceptT CmdError IO ()
runCmds = \case
  AddressCmds cmd ->
    runAddressCmds cmd & firstExceptT CmdAddressError
  KeyCmds cmd ->
    runKeyCmds cmd
      & firstExceptT CmdKeyError
  GovernanceCmds cmd ->
    runGovernanceCmds cmd
  GenesisCmds cmd ->
    runGenesisCmds cmd
      & firstExceptT CmdGenesisError
  NodeCmds cmd ->
    runNodeCmds cmd
      & firstExceptT CmdNodeError
  QueryCmds cmd ->
    runQueryCmds cmd
      & firstExceptT CmdQueryError
  StakeAddressCmds cmd ->
    runStakeAddressCmds cmd
      & firstExceptT CmdStakeAddressError
  StakePoolCmds cmd ->
    runStakePoolCmds cmd
      & firstExceptT CmdStakePoolError
  TextViewCmds cmd ->
    runTextViewCmds cmd
      & firstExceptT CmdTextViewError
  TransactionCmds cmd ->
    runTransactionCmds cmd
      & firstExceptT CmdTransactionError
