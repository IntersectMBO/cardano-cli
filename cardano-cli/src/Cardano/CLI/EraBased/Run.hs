{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.Run
  ( AnyEraCmdError(..)
  , runAnyEraCommand
  , runEraBasedCommand
  , runEraBasedGovernanceCmd

  , renderAnyEraCmdError
  ) where

import qualified Cardano.Api.Ledger as Ledger
import           Cardano.Api.Shelley

import           Cardano.CLI.Commands.EraBased
import           Cardano.CLI.EraBased.Options.Governance
import           Cardano.CLI.EraBased.Run.Governance

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
  => EraBasedCommand era
  -> ExceptT () IO ()
runEraBasedCommand = \case
  EraBasedGovernanceCmd cmd ->
    firstExceptT (const ()) $ runEraBasedGovernanceCmd cmd

runEraBasedGovernanceCmd :: ()
  => Ledger.EraCrypto (ShelleyLedgerEra era) ~ Ledger.StandardCrypto
  => EraBasedGovernanceCmd era
  -> ExceptT GovernanceCmdError IO ()
runEraBasedGovernanceCmd = \case
  EraBasedGovernanceMIRPayStakeAddressesCertificate w mirpot vKeys rewards out ->
    runGovernanceMIRCertificatePayStakeAddrs w mirpot vKeys rewards out
  EraBasedGovernanceMIRTransfer w ll oFp direction ->
    runGovernanceMIRCertificateTransfer w ll oFp direction
  EraBasedGovernanceDelegationCertificateCmd stakeIdentifier delegationTarget outFp ->
    runGovernanceDelegationCertificate stakeIdentifier delegationTarget outFp
  EraBasedGovernanceGenesisKeyDelegationCertificate w genVk genDelegVk vrfVk out ->
    runGovernanceGenesisKeyDelegationCertificate w genVk genDelegVk vrfVk out
