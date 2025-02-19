{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.Commands.Governance.Committee
  ( GovernanceCommitteeCmds (..)
  , GovernanceCommitteeKeyGenColdCmdArgs (..)
  , GovernanceCommitteeKeyGenHotCmdArgs (..)
  , GovernanceCommitteeKeyHashCmdArgs (..)
  , GovernanceCommitteeCreateHotKeyAuthorizationCertificateCmdArgs (..)
  , GovernanceCommitteeCreateColdKeyResignationCertificateCmdArgs (..)
  , renderGovernanceCommitteeCmds
  )
where

import Cardano.Api
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Shelley

import Cardano.CLI.Types.Common (PotentiallyCheckedAnchor, ResignationMetadataUrl)
import Cardano.CLI.Types.Key
import Cardano.CLI.Types.Key.VerificationKey

import Data.Text (Text)

data GovernanceCommitteeCmds era
  = GovernanceCommitteeKeyGenColdCmd
      (GovernanceCommitteeKeyGenColdCmdArgs era)
  | GovernanceCommitteeKeyGenHotCmd
      (GovernanceCommitteeKeyGenHotCmdArgs era)
  | GovernanceCommitteeKeyHashCmd
      (GovernanceCommitteeKeyHashCmdArgs era)
  | GovernanceCommitteeCreateHotKeyAuthorizationCertificateCmd
      (GovernanceCommitteeCreateHotKeyAuthorizationCertificateCmdArgs era)
  | GovernanceCommitteeCreateColdKeyResignationCertificateCmd
      (GovernanceCommitteeCreateColdKeyResignationCertificateCmdArgs era)
  deriving Show

data GovernanceCommitteeKeyGenColdCmdArgs era
  = GovernanceCommitteeKeyGenColdCmdArgs
  { eon :: !(ConwayEraOnwards era)
  , vkeyOutFile :: !(File (VerificationKey ()) Out)
  , skeyOutFile :: !(File (SigningKey ()) Out)
  }
  deriving Show

data GovernanceCommitteeKeyGenHotCmdArgs era
  = GovernanceCommitteeKeyGenHotCmdArgs
  { eon :: !(ConwayEraOnwards era)
  , vkeyOutFile :: !(File (VerificationKey ()) Out)
  , skeyOutFile :: !(File (SigningKey ()) Out)
  }
  deriving Show

data GovernanceCommitteeKeyHashCmdArgs era
  = GovernanceCommitteeKeyHashCmdArgs
  { eon :: !(ConwayEraOnwards era)
  , vkeySource :: !AnyVerificationKeySource
  }
  deriving Show

data GovernanceCommitteeCreateHotKeyAuthorizationCertificateCmdArgs era
  = GovernanceCommitteeCreateHotKeyAuthorizationCertificateCmdArgs
  { eon :: !(ConwayEraOnwards era)
  , vkeyColdKeySource :: !(VerificationKeySource CommitteeColdKey)
  , vkeyHotKeySource :: !(VerificationKeySource CommitteeHotKey)
  , outFile :: !(File () Out)
  }
  deriving Show

data GovernanceCommitteeCreateColdKeyResignationCertificateCmdArgs era
  = GovernanceCommitteeCreateColdKeyResignationCertificateCmdArgs
  { eon :: !(ConwayEraOnwards era)
  , vkeyColdKeySource :: !(VerificationKeySource CommitteeColdKey)
  , anchor
      :: !( Maybe
              ( PotentiallyCheckedAnchor
                  ResignationMetadataUrl
                  (L.Anchor (L.EraCrypto (ShelleyLedgerEra era)))
              )
          )
  , outFile :: !(File () Out)
  }
  deriving Show

renderGovernanceCommitteeCmds :: GovernanceCommitteeCmds era -> Text
renderGovernanceCommitteeCmds =
  ("governance committee " <>) . \case
    GovernanceCommitteeKeyGenColdCmd{} ->
      "key-gen-cold"
    GovernanceCommitteeKeyGenHotCmd{} ->
      "key-gen-hot"
    GovernanceCommitteeKeyHashCmd{} ->
      "key-hash"
    GovernanceCommitteeCreateHotKeyAuthorizationCertificateCmd{} ->
      "create-hot-key-authorization-certificate"
    GovernanceCommitteeCreateColdKeyResignationCertificateCmd{} ->
      "create-cold-key-resignation-certificate"
