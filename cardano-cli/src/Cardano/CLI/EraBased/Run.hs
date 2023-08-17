{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.Run
  ( AnyEraCmdError(..)
  , runAnyEraCommand
  , runEraBasedCommand
  , runEraBasedGovernanceCmds
  ) where

import           Cardano.Api

import           Cardano.CLI.Commands.Governance
import           Cardano.CLI.EraBased.Commands
import           Cardano.CLI.EraBased.Options.Governance
import           Cardano.CLI.EraBased.Run.Certificate
import           Cardano.CLI.EraBased.Run.Governance
import           Cardano.CLI.EraBased.Run.Governance.Actions
import           Cardano.CLI.EraBased.Run.Governance.Committee
import           Cardano.CLI.EraBased.Vote

import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Except.Extra (firstExceptT)
import           Data.Function ((&))

data AnyEraCmdError
  = AnyEraCmdGovernanceCmdError        !GovernanceCmdError
  | AnyEraCmdEraDelegationError        !EraBasedDelegationError
  | AnyEraCmdEraBasedRegistrationError !EraBasedRegistrationError
  | AnyEraCmdEraBasedVoteError         !EraBasedVoteError
  | AnyEraCmdGovernanceCommitteeError  !GovernanceCommitteeError
  | AnyEraCmdGovernanceActionError     !GovernanceActionsError
  deriving Show

instance Error AnyEraCmdError where
  displayError = \case
    AnyEraCmdGovernanceCmdError e -> displayError e
    AnyEraCmdEraDelegationError e -> displayError e
    AnyEraCmdEraBasedRegistrationError e -> displayError e
    AnyEraCmdEraBasedVoteError e -> displayError e
    AnyEraCmdGovernanceCommitteeError e -> displayError e
    AnyEraCmdGovernanceActionError e -> displayError e

runAnyEraCommand :: ()
  => AnyEraCommand
  -> ExceptT AnyEraCmdError IO ()
runAnyEraCommand = \case
  AnyEraCommandOf sbe cmd ->
    shelleyBasedEraConstraints sbe $ runEraBasedCommand cmd

runEraBasedCommand :: ()
  => EraBasedCommand era -> ExceptT AnyEraCmdError IO ()
runEraBasedCommand = \case
  EraBasedGovernanceCmds cmd -> runEraBasedGovernanceCmds cmd

runEraBasedGovernanceCmds :: ()
  => EraBasedGovernanceCmds era
  -> ExceptT AnyEraCmdError IO ()
runEraBasedGovernanceCmds = \case
  EraBasedGovernancePreConwayCmd w ->
    runEraBasedGovernancePreConwayCmd w
  EraBasedGovernancePostConwayCmd w ->
    runEraBasedGovernancePostConwayCmd w
  EraBasedGovernanceMIRPayStakeAddressesCertificate w mirpot vKeys rewards out ->
    runGovernanceMIRCertificatePayStakeAddrs w mirpot vKeys rewards out
      & firstExceptT AnyEraCmdGovernanceCmdError

  EraBasedGovernanceMIRTransfer w ll oFp direction ->
    runGovernanceMIRCertificateTransfer w ll oFp direction
      & firstExceptT AnyEraCmdGovernanceCmdError

  EraBasedGovernanceDelegationCertificateCmd stakeIdentifier delegationTarget outFp ->
    runGovernanceDelegationCertificate stakeIdentifier delegationTarget outFp
      & firstExceptT AnyEraCmdEraDelegationError

  EraBasedGovernanceRegistrationCertificateCmd regTarget outFp ->
    runGovernanceRegistrationCertificate regTarget outFp
      & firstExceptT AnyEraCmdEraBasedRegistrationError

  EraBasedGovernanceVoteCmd anyVote outFp ->
    runGovernanceVote anyVote outFp
      & firstExceptT AnyEraCmdEraBasedVoteError

  EraBasedGovernanceCommitteeCmds cmds ->
    runGovernanceCommitteeCmds cmds
      & firstExceptT AnyEraCmdGovernanceCommitteeError

  EraBasedGovernanceActionCmds cmds ->
    runGovernanceActionCmds cmds
      & firstExceptT AnyEraCmdGovernanceActionError

  EraBasedGovernanceDRepGenerateKey w vrf sgn ->
    runGovernanceDRepKeyGen w vrf sgn
      & firstExceptT AnyEraCmdGovernanceCmdError


runEraBasedGovernancePreConwayCmd
  :: ShelleyToBabbageEra era
  -> ExceptT e IO ()
runEraBasedGovernancePreConwayCmd _w = pure ()

runEraBasedGovernancePostConwayCmd
  :: ConwayEraOnwards era
  -> ExceptT e IO ()
runEraBasedGovernancePostConwayCmd _w = pure ()
