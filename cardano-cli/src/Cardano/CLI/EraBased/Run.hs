{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.Run
  ( runAnyEraCommand
  , runCmds
  , runGovernanceCmds
  ) where

import Cardano.Api

import Cardano.CLI.EraBased.Commands
import Cardano.CLI.EraBased.Options.Governance
import Cardano.CLI.EraBased.Run.Address
import Cardano.CLI.EraBased.Run.Genesis
import Cardano.CLI.EraBased.Run.Governance
import Cardano.CLI.EraBased.Run.Governance.Actions
import Cardano.CLI.EraBased.Run.Governance.Committee
import Cardano.CLI.EraBased.Run.Governance.DRep
import Cardano.CLI.EraBased.Run.Governance.Query
import Cardano.CLI.EraBased.Run.Governance.Vote
import Cardano.CLI.EraBased.Run.Key
import Cardano.CLI.EraBased.Run.Node
import Cardano.CLI.EraBased.Run.Query
import Cardano.CLI.EraBased.Run.StakeAddress
import Cardano.CLI.EraBased.Run.StakePool
import Cardano.CLI.EraBased.Run.TextView
import Cardano.CLI.EraBased.Run.Transaction
import Cardano.CLI.Types.Errors.CmdError

import Control.Monad.Trans.Except
import Control.Monad.Trans.Except.Extra (firstExceptT)
import Data.Function ((&))

runAnyEraCommand
  :: ()
  => AnyEraCommand
  -> ExceptT CmdError IO ()
runAnyEraCommand = \case
  AnyEraCommandOf sbe cmd ->
    shelleyBasedEraConstraints sbe $ runCmds cmd

runCmds
  :: ()
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

runGovernanceCmds
  :: ()
  => GovernanceCmds era
  -> ExceptT CmdError IO ()
runGovernanceCmds = \case
  GovernanceMIRPayStakeAddressesCertificate w mirpot vKeys rewards out ->
    runGovernanceMIRCertificatePayStakeAddrs w mirpot vKeys rewards out
      & firstExceptT CmdGovernanceCmdError
  GovernanceMIRTransfer w ll oFp direction ->
    runGovernanceMIRCertificateTransfer w ll oFp direction
      & firstExceptT CmdGovernanceCmdError
  GovernanceCommitteeCmds cmds ->
    runGovernanceCommitteeCmds cmds
      & firstExceptT CmdGovernanceCommitteeError
  GovernanceActionCmds cmds ->
    runGovernanceActionCmds cmds
      & firstExceptT CmdGovernanceActionError
  GovernanceDRepCmds cmds ->
    runGovernanceDRepCmds cmds
  GovernanceVoteCmds cmds ->
    runGovernanceVoteCmds cmds
  GovernanceQueryCmds cmds ->
    runGovernanceQueryCmds cmds
