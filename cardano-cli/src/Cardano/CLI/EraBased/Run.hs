{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.CLI.EraBased.Run
  ( runAnyEraCommand
  , runCmds
  , runGovernanceCmds
  )
where

import Cardano.Api.Experimental (IsEra, obtainCommonConstraints)

import Cardano.CLI.Compatible.Exception
import Cardano.CLI.EraBased.Command
import Cardano.CLI.EraBased.Genesis.Run
import Cardano.CLI.EraBased.Governance.Run
import Cardano.CLI.EraBased.Query.Run
import Cardano.CLI.EraBased.StakeAddress.Run
import Cardano.CLI.EraBased.StakePool.Run
import Cardano.CLI.EraBased.TextView.Run
import Cardano.CLI.EraBased.Transaction.Run (runTransactionCmds)
import Cardano.CLI.EraIndependent.Address.Run
import Cardano.CLI.EraIndependent.Key.Run
import Cardano.CLI.EraIndependent.Node.Run
import Cardano.CLI.Helper (printEraDeprecationWarning)

runAnyEraCommand
  :: ()
  => AnyEraCommand
  -> CIO e ()
runAnyEraCommand = \case
  AnyEraCommandOf era cmd -> do
    printEraDeprecationWarning era
    obtainCommonConstraints era $ runCmds cmd

runCmds
  :: IsEra era
  => Cmds era
  -> CIO e ()
runCmds = \case
  AddressCmds cmd ->
    runAddressCmds cmd
  KeyCmds cmd ->
    runKeyCmds cmd
  GovernanceCmds cmd ->
    runGovernanceCmds cmd
  GenesisCmds cmd ->
    runGenesisCmds cmd
  NodeCmds cmd ->
    runNodeCmds cmd
  QueryCmds cmd ->
    runQueryCmds cmd
  StakeAddressCmds cmd ->
    runStakeAddressCmds cmd
  StakePoolCmds cmd ->
    runStakePoolCmds cmd
  TextViewCmds cmd ->
    runTextViewCmds cmd
  TransactionCmds cmd ->
    runTransactionCmds cmd
