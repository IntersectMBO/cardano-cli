{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.Run
  ( AnyEraCmdError(..)
  , runAnyEraCommand
  , runEraBasedCommand
  , runEraBasedGovernanceCmds

  , renderAnyEraCmdError
  ) where

import           Cardano.Api

import           Cardano.CLI.EraBased.Commands
import           Cardano.CLI.EraBased.Options.Governance
import           Cardano.CLI.EraBased.Run.Certificate
import           Cardano.CLI.EraBased.Run.Governance
import           Cardano.CLI.EraBased.Run.Governance.Actions
import           Cardano.CLI.EraBased.Run.Governance.Committee
import           Cardano.CLI.EraBased.Vote

import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Except.Extra (firstExceptT)
import           Data.Text (Text)

newtype AnyEraCmdError
  = AnyEraCmdGenericError ()

renderAnyEraCmdError :: AnyEraCmdError -> Text
renderAnyEraCmdError = \case
  AnyEraCmdGenericError () -> "Generic any era command error"

runAnyEraCommand :: ()
  => AnyEraCommand
  -> ExceptT AnyEraCmdError IO ()
runAnyEraCommand = \case
  AnyEraCommandOf sbe cmd ->
    firstExceptT AnyEraCmdGenericError $ shelleyBasedEraConstraints sbe $ runEraBasedCommand cmd

runEraBasedCommand :: ()
  => EraBasedCommand era -> ExceptT () IO ()
runEraBasedCommand = \case
  EraBasedGovernanceCmds cmd -> runEraBasedGovernanceCmds cmd

runEraBasedGovernanceCmds :: ()
  => EraBasedGovernanceCmds era
  -> ExceptT () IO ()
runEraBasedGovernanceCmds = \case
  EraBasedGovernancePreConwayCmd w ->
    runEraBasedGovernancePreConwayCmd w
  EraBasedGovernancePostConwayCmd w ->
    runEraBasedGovernancePostConwayCmd w
  EraBasedGovernanceMIRPayStakeAddressesCertificate w mirpot vKeys rewards out ->
    firstExceptT (const ()) -- TODO: Conway era - fix error handling
      $ runGovernanceMIRCertificatePayStakeAddrs w mirpot vKeys rewards out

  EraBasedGovernanceMIRTransfer w ll oFp direction ->
    firstExceptT (const ()) -- TODO: Conway era - fix error handling
      $ runGovernanceMIRCertificateTransfer w ll oFp direction

  EraBasedGovernanceDelegationCertificateCmd stakeIdentifier delegationTarget outFp ->
    firstExceptT (const ()) -- TODO fix error handling
      $ runGovernanceDelegationCertificate stakeIdentifier delegationTarget outFp

  EraBasedGovernanceRegistrationCertificateCmd regTarget outFp ->
    firstExceptT (const ()) -- TODO: Conway era - fix error handling
      $ runGovernanceRegistrationCertificate regTarget outFp
  EraBasedGovernanceVoteCmd anyVote outFp ->
    firstExceptT (const ()) -- TODO: Conway era - fix error handling
      $ runGovernanceVote anyVote outFp

  EraBasedGovernanceCommitteeCmds cmds ->
    firstExceptT (const ()) -- TODO: Conway era - fix error handling
      $ runGovernanceCommitteeCmds cmds

  EraBasedGovernanceActionCmds cmds ->
    firstExceptT (const ()) -- TODO: Conway era - fix error handling
      $ runGovernanceActionCmds cmds
runEraBasedGovernancePreConwayCmd
  :: ShelleyToBabbageEra era
  -> ExceptT () IO ()
runEraBasedGovernancePreConwayCmd _w = pure ()

runEraBasedGovernancePostConwayCmd
  :: ConwayEraOnwards era
  -> ExceptT () IO ()
runEraBasedGovernancePostConwayCmd _w = pure ()
