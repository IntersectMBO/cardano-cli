{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.Run
  ( AnyEraCmdError(..)
  , runAnyEraCommand
  , runEraBasedCommand
  , runEraBasedGovernanceCmd

  , renderAnyEraCmdError
  ) where

import           Cardano.Api
import qualified Cardano.Api.Ledger as Ledger
import           Cardano.Api.Shelley

import           Cardano.CLI.Commands.EraBased
import           Cardano.CLI.EraBased.Certificate
import           Cardano.CLI.EraBased.Options.Governance

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
    firstExceptT AnyEraCmdGenericError $ obtainEraCryptoConstraints sbe $ runEraBasedCommand cmd

runEraBasedCommand :: ()
  => Ledger.EraCrypto (ShelleyLedgerEra era) ~ Ledger.StandardCrypto
  => EraBasedCommand era -> ExceptT () IO ()
runEraBasedCommand = \case
  EraBasedGovernanceCmd cmd -> runEraBasedGovernanceCmd cmd

runEraBasedGovernanceCmd :: ()
  => Ledger.EraCrypto (ShelleyLedgerEra era) ~ Ledger.StandardCrypto
  => EraBasedGovernanceCmd era
  -> ExceptT () IO ()
runEraBasedGovernanceCmd = \case
  EraBasedGovernancePreConwayCmd w ->
    runEraBasedGovernancePreConwayCmd w
  EraBasedGovernancePostConwayCmd w ->
    runEraBasedGovernancePostConwayCmd w
  EraBasedGovernanceMIRPayStakeAddressesCertificate w mirpot vKeys rewards out ->
    firstExceptT (const ()) -- TODO fix error handling
      $ runGovernanceMIRCertificatePayStakeAddrs w mirpot vKeys rewards out
  EraBasedGovernanceMIRTransfer w ll oFp direction ->
    firstExceptT (const ()) -- TODO fix error handling
      $ runGovernanceMIRCertificateTransfer w ll oFp direction
  EraBasedGovernanceDelegationCertificateCmd stakeIdentifier delegationTarget outFp ->
    firstExceptT (const ()) -- TODO fix error handling
      $ runGovernanceDelegrationCertificate stakeIdentifier delegationTarget outFp
  EraBasedGovernanceGenesisKeyDelegationCertificate w genVk genDelegVk vrfVk out ->
    firstExceptT (const ()) -- TODO fix error handling
      $ runGovernanceGenesisKeyDelegationCertificate w genVk genDelegVk vrfVk out

runEraBasedGovernancePreConwayCmd
  :: ShelleyToBabbageEra era
  -> ExceptT () IO ()
runEraBasedGovernancePreConwayCmd _w = pure ()

runEraBasedGovernancePostConwayCmd
  :: ConwayEraOnwards era
  -> ExceptT () IO ()
runEraBasedGovernancePostConwayCmd _w = pure ()
