{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.Run
  ( AnyEraCmdError(..)
  , runAnyEraCommand
  , runEraBasedCommand

  , renderAnyEraCmdError
  ) where

import qualified Cardano.Api.Ledger as Ledger
import           Cardano.Api.Shelley

import           Cardano.CLI.Commands.EraBased
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
    firstExceptT (const ()) $ runGovernanceCmd cmd
