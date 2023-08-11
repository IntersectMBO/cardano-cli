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
  = GovernanceDRepDelegationCertificate
      StakeIdentifier
      AnyDelegationTarget
      (File () Out)
  | GovernanceDRepRegistrationCertificate
      AnyRegistrationTarget
      (File () Out)
  | GovernanceDRepKeyGen
      (File (VerificationKey ()) Out)
      (File (SigningKey ()) Out)

renderGovernanceDRepCmds :: GovernanceDRepCmds era -> Text
renderGovernanceDRepCmds = \case
  GovernanceDRepDelegationCertificate{} ->
    "governance drep delegation-certificate"
  GovernanceDRepRegistrationCertificate{} ->
    "governance drep registration-certificate"
  GovernanceDRepKeyGen{} ->
    "governance drep key-gen"
