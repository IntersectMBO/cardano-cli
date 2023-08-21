{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.Commands.Governance.DRep
  ( GovernanceDRepCmds(..)
  , renderGovernanceDRepCmds
  ) where

import           Cardano.Api

import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Key

import           Data.Text (Text)

data GovernanceDRepCmds era
  = GovernanceDRepDelegationCertificateCmd
      StakeIdentifier
      AnyDelegationTarget
      (File () Out)
  | GovernanceDRepGenerateKey
      (ConwayEraOnwards era)
      (File (VerificationKey ()) Out)
      (File (SigningKey ()) Out)
  | GovernanceDRepIdCmd
      (ConwayEraOnwards era)
      (VerificationKeyOrFile DRepKey)
      IdOutputFormat
      (Maybe (File () Out))
  | GovernanceDRepRegistrationCertificateCmd
      AnyRegistrationTarget
      (File () Out)

renderGovernanceDRepCmds :: ()
  => GovernanceDRepCmds era
  -> Text
renderGovernanceDRepCmds = \case
  GovernanceDRepDelegationCertificateCmd {} ->
    "governance drep delegation-certificate"
  GovernanceDRepGenerateKey{} ->
    "governance drep key-gen"
  GovernanceDRepIdCmd {} ->
    "governance drep id"
  GovernanceDRepRegistrationCertificateCmd {} ->
    "governance drep registration-certificate"
