{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.Commands.Governance.DRep
  ( GovernanceDRepCmds(..)
  , renderGovernanceDRepCmds
  ) where

import           Cardano.Api

import           Cardano.CLI.Types.Key

import           Data.Text (Text)

data GovernanceDRepCmds era
  = GovernanceDRepDelegationCertificateCmd
      StakeIdentifier
      AnyDelegationTarget
      (File () Out)
  | GovernanceDRepRegistrationCertificateCmd
      AnyRegistrationTarget
      (File () Out)
  | GovernanceDRepGenerateKey
      (ConwayEraOnwards era)
      (File (VerificationKey ()) Out)
      (File (SigningKey ()) Out)

renderGovernanceDRepCmds :: ()
  => GovernanceDRepCmds era
  -> Text
renderGovernanceDRepCmds = \case
  GovernanceDRepDelegationCertificateCmd {} ->
    "governance drep delegation-certificate"
  GovernanceDRepRegistrationCertificateCmd {} ->
    "governance drep registration-certificate"
  GovernanceDRepGenerateKey{} ->
    "governance drep key-gen"
