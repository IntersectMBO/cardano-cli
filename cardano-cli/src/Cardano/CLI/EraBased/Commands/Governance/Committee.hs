{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.Commands.Governance.Committee
  ( GovernanceCommitteeCmds(..)
  , renderGovernanceCommitteeCmds
  ) where

import           Cardano.Api

import           Cardano.CLI.Types.Key

import           Data.Text (Text)

data GovernanceCommitteeCmds era
  = GovernanceCommitteeKeyGenCold
      (ConwayEraOnwards era)
      (File (VerificationKey ()) Out)
      (File (SigningKey ()) Out)
  | GovernanceCommitteeKeyGenHot
      (ConwayEraOnwards era)
      (File (VerificationKey ()) Out)
      (File (SigningKey ()) Out)
  | GovernanceCommitteeKeyHash -- TODO to be moved under the top-level command group "key"
      (ConwayEraOnwards era)
      (File (VerificationKey ()) In)
  | GovernanceCommitteeCreateHotKeyAuthorizationCertificate -- TODO to be moved under the top-level command group "key"
      (ConwayEraOnwards era)
      (VerificationKeyOrHashOrFile CommitteeColdKey)
      (VerificationKeyOrHashOrFile CommitteeHotKey)
      (File () Out)
  | GovernanceCommitteeCreateColdKeyResignationCertificate
      (ConwayEraOnwards era)
      (VerificationKeyOrHashOrFile CommitteeColdKey)
      (File () Out)
  deriving Show

renderGovernanceCommitteeCmds :: GovernanceCommitteeCmds era -> Text
renderGovernanceCommitteeCmds = \case
  GovernanceCommitteeKeyGenCold {} ->
    "governance committee key-gen-cold"
  GovernanceCommitteeKeyGenHot {} ->
    "governance committee key-gen-hot"
  GovernanceCommitteeKeyHash {} ->
    "governance committee key-hash"
  GovernanceCommitteeCreateHotKeyAuthorizationCertificate {} ->
    "governance committee create-hot-key-authorization-certificate"
  GovernanceCommitteeCreateColdKeyResignationCertificate {} ->
    "governance committee create-cold-key-resignation-certificate"
