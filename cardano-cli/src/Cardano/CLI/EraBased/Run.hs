{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.Run
  ( runAnyEraCommand
  , runEraBasedCommand
  , runEraBasedGovernanceCmds
  ) where

import           Cardano.Api

import           Cardano.CLI.EraBased.Commands
import           Cardano.CLI.EraBased.Options.Governance
import           Cardano.CLI.EraBased.Run.Governance
import           Cardano.CLI.EraBased.Run.Governance.Actions
import           Cardano.CLI.EraBased.Run.Governance.Committee
import           Cardano.CLI.EraBased.Run.Governance.DRep
import           Cardano.CLI.EraBased.Run.Governance.Vote
import           Cardano.CLI.Types.Errors.CmdError

import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Except.Extra (firstExceptT)
import           Data.Function ((&))

runAnyEraCommand :: ()
  => AnyEraCommand
  -> ExceptT CmdError IO ()
runAnyEraCommand = \case
  AnyEraCommandOf sbe cmd ->
    shelleyBasedEraConstraints sbe $ runEraBasedCommand cmd

runEraBasedCommand :: ()
  => EraBasedCommand era -> ExceptT CmdError IO ()
runEraBasedCommand = \case
  EraBasedGovernanceCmds cmd -> runEraBasedGovernanceCmds cmd

runEraBasedGovernanceCmds :: ()
  => EraBasedGovernanceCmds era
  -> ExceptT CmdError IO ()
runEraBasedGovernanceCmds = \case
  EraBasedGovernanceMIRPayStakeAddressesCertificate w mirpot vKeys rewards out ->
    runGovernanceMIRCertificatePayStakeAddrs w mirpot vKeys rewards out
      & firstExceptT CmdGovernanceCmdError

  EraBasedGovernanceMIRTransfer w ll oFp direction ->
    runGovernanceMIRCertificateTransfer w ll oFp direction
      & firstExceptT CmdGovernanceCmdError

  EraBasedGovernanceCommitteeCmds cmds ->
    runGovernanceCommitteeCmds cmds
      & firstExceptT CmdGovernanceCommitteeError

  EraBasedGovernanceActionCmds cmds ->
    runGovernanceActionCmds cmds
      & firstExceptT CmdGovernanceActionError

  EraBasedGovernanceDRepCmds cmds ->
    runGovernanceDRepCmds cmds

  EraBasedGovernanceVoteCmds cmds ->
    runGovernanceVoteCmds cmds
