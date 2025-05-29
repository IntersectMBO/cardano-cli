{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.Governance.Committee.Command
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
import Cardano.Api.Experimental qualified as Exp
import Cardano.Api.Ledger qualified as L

import Cardano.CLI.Type.Common (PotentiallyCheckedAnchor, ResignationMetadataUrl)
import Cardano.CLI.Type.Key
import Cardano.CLI.Type.Key.VerificationKey

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
  { era :: !(Exp.Era era)
  , vkeyOutFile :: !(File (VerificationKey ()) Out)
  , skeyOutFile :: !(File (SigningKey ()) Out)
  }
  deriving Show

data GovernanceCommitteeKeyGenHotCmdArgs era
  = GovernanceCommitteeKeyGenHotCmdArgs
  { era :: !(Exp.Era era)
  , vkeyOutFile :: !(File (VerificationKey ()) Out)
  , skeyOutFile :: !(File (SigningKey ()) Out)
  }
  deriving Show

data GovernanceCommitteeKeyHashCmdArgs era
  = GovernanceCommitteeKeyHashCmdArgs
  { era :: !(Exp.Era era)
  , vkeySource :: !AnyVerificationKeySource
  }
  deriving Show

data GovernanceCommitteeCreateHotKeyAuthorizationCertificateCmdArgs era
  = GovernanceCommitteeCreateHotKeyAuthorizationCertificateCmdArgs
  { era :: !(Exp.Era era)
  , vkeyColdKeySource :: !(VerificationKeySource CommitteeColdKey)
  , vkeyHotKeySource :: !(VerificationKeySource CommitteeHotKey)
  , outFile :: !(File () Out)
  }
  deriving Show

data GovernanceCommitteeCreateColdKeyResignationCertificateCmdArgs era
  = GovernanceCommitteeCreateColdKeyResignationCertificateCmdArgs
  { era :: !(Exp.Era era)
  , vkeyColdKeySource :: !(VerificationKeySource CommitteeColdKey)
  , anchor
      :: !( Maybe
              ( PotentiallyCheckedAnchor
                  ResignationMetadataUrl
                  L.Anchor
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
