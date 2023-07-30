{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Run.EraBased
  ( AnyEraCmdError(..)
  , runAnyEraCommand
  , runEraBasedCommand
  , runEraBasedGovernanceCmd

  , renderAnyEraCmdError
  ) where

import           Cardano.Api

import           Cardano.CLI.Commands.EraBased
import           Cardano.CLI.Commands.Governance
import           Cardano.CLI.EraBased.Certificate
import           Cardano.CLI.EraBased.Options.Governance
import           Cardano.CLI.EraBased.Run.Governance
import           Cardano.CLI.EraBased.Run.Governance.Vote

import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Except.Extra (firstExceptT)
import           Data.Function ((&))
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
  EraBasedGovernanceCmd cmd ->
    firstExceptT (const ()) -- TODO fix error handling
      $ runEraBasedGovernanceCmd cmd

runEraBasedGovernanceCmd :: ()
  => EraBasedGovernanceCmd era
  -> ExceptT GovernanceCmdError IO ()
runEraBasedGovernanceCmd = \case
  EraBasedGovernancePreConwayCmd w ->
    runEraBasedGovernancePreConwayCmd w
  EraBasedGovernancePostConwayCmd w ->
    runEraBasedGovernancePostConwayCmd w
  EraBasedGovernanceMIRPayStakeAddressesCertificate w mirpot vKeys rewards out ->
    runGovernanceMIRCertificatePayStakeAddrs w mirpot vKeys rewards out
  EraBasedGovernanceMIRTransfer w ll oFp direction ->
    runGovernanceMIRCertificateTransfer w ll oFp direction
  EraBasedGovernanceDelegationCertificateCmd stakeIdentifier delegationTarget outFp ->
    runGovernanceDelegrationCertificate stakeIdentifier delegationTarget outFp
      & firstExceptT GovernanceDelegationError
  EraBasedGovernanceVoteCmds cmds ->
    runGovernanceVoteCmds cmds

  EraBasedGovernanceRegistrationCertificateCmd regTarget outFp ->
    firstExceptT (const GovernanceUnknownError) -- TODO: Conway era - fix error handling
      $ runGovernanceRegistrationCertificate regTarget outFp

runEraBasedGovernancePreConwayCmd
  :: ShelleyToBabbageEra era
  -> ExceptT GovernanceCmdError IO ()
runEraBasedGovernancePreConwayCmd _w = pure ()

runEraBasedGovernancePostConwayCmd
  :: ConwayEraOnwards era
  -> ExceptT GovernanceCmdError IO ()
runEraBasedGovernancePostConwayCmd _w = pure ()
