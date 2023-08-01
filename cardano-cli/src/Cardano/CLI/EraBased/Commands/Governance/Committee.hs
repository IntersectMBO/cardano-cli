{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.Commands.Governance.Committee
  ( GovernanceCommitteeCmds(..)
  , renderGovernanceCommitteeCmds
  ) where

import           Cardano.Api

import           Data.Text (Text)

data GovernanceCommitteeCmds era
  = GovernanceCommitteeCmdCreateColdResignationCertificate
      (File (VerificationKey ()) Out)
      (File (SigningKey ()) Out)

renderGovernanceCommitteeCmds :: GovernanceCommitteeCmds era -> Text
renderGovernanceCommitteeCmds = \case
  GovernanceCommitteeCmdCreateColdResignationCertificate {} -> "governance committee create-cold-resignation-certificate"
