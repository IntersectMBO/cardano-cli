{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.Commands.Governance.Committee
  ( GovernanceCommitteeCmds(..)
  , renderGovernanceCommitteeCmds
  ) where

import           Cardano.Api

import           Cardano.CLI.Types.Key
import           Cardano.CLI.Types.Key.VerificationKey

import           Data.Text (Text)

data GovernanceCommitteeCmds era
  = GovernanceCommitteeKeyGenColdCmd
      (ConwayEraOnwards era)
      (File (VerificationKey ()) Out)
      (File (SigningKey ()) Out)
  | GovernanceCommitteeKeyGenHotCmd
      (ConwayEraOnwards era)
      (File (VerificationKey ()) Out)
      (File (SigningKey ()) Out)
  | GovernanceCommitteeKeyHashCmd -- TODO to be moved under the top-level command group "key"
      (ConwayEraOnwards era)
      AnyVerificationKeySource
  | GovernanceCommitteeCreateHotKeyAuthorizationCertificateCmd -- TODO to be moved under the top-level command group "key"
      (ConwayEraOnwards era)
      (VerificationKeyOrHashOrFile CommitteeColdKey)
      (VerificationKeyOrHashOrFile CommitteeHotKey)
      (File () Out)
  | GovernanceCommitteeCreateColdKeyResignationCertificateCmd
      (ConwayEraOnwards era)
      (VerificationKeyOrHashOrFile CommitteeColdKey)
      (File () Out)
  deriving Show

renderGovernanceCommitteeCmds :: GovernanceCommitteeCmds era -> Text
renderGovernanceCommitteeCmds = \case
  GovernanceCommitteeKeyGenColdCmd {} ->
    "governance committee key-gen-cold"
  GovernanceCommitteeKeyGenHotCmd {} ->
    "governance committee key-gen-hot"
  GovernanceCommitteeKeyHashCmd {} ->
    "governance committee key-hash"
  GovernanceCommitteeCreateHotKeyAuthorizationCertificateCmd {} ->
    "governance committee create-hot-key-authorization-certificate"
  GovernanceCommitteeCreateColdKeyResignationCertificateCmd {} ->
    "governance committee create-cold-key-resignation-certificate"
