{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.Commands.Governance.Committee
  ( GovernanceCommitteeCmds(..)
  , renderGovernanceCommitteeCmds
  ) where

import           Cardano.Api

import           Data.Text (Text)

data GovernanceCommitteeCmds era
  = GovernanceCommitteeKeyGenCold
      (ConwayEraOnwards era)
      (File (VerificationKey ()) Out)
      (File (SigningKey ()) Out)
  deriving Show

renderGovernanceCommitteeCmds :: GovernanceCommitteeCmds era -> Text
renderGovernanceCommitteeCmds = \case
  GovernanceCommitteeKeyGenCold {} ->
    "governance committee key-gen-cold"
