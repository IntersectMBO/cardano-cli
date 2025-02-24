{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.Commands.Governance.DRep
  ( GovernanceDRepCmds (..)
  , renderGovernanceDRepCmds
  , GovernanceDRepKeyGenCmdArgs (..)
  , GovernanceDRepIdCmdArgs (..)
  , GovernanceDRepRegistrationCertificateCmdArgs (..)
  , GovernanceDRepRetirementCertificateCmdArgs (..)
  , GovernanceDRepUpdateCertificateCmdArgs (..)
  , GovernanceDRepMetadataHashCmdArgs (..)
  , DRepMetadataSource (..)
  )
where

import Cardano.Api
import Cardano.Api.Ledger qualified as L

import Cardano.CLI.Commands.Hash (HashGoal)
import Cardano.CLI.Types.Common
import Cardano.CLI.Types.Key

import Data.Text (Text)

data GovernanceDRepCmds era
  = GovernanceDRepKeyGenCmd !(GovernanceDRepKeyGenCmdArgs era)
  | GovernanceDRepIdCmd !(GovernanceDRepIdCmdArgs era)
  | GovernanceDRepRegistrationCertificateCmd !(GovernanceDRepRegistrationCertificateCmdArgs era)
  | GovernanceDRepRetirementCertificateCmd !(GovernanceDRepRetirementCertificateCmdArgs era)
  | GovernanceDRepUpdateCertificateCmd !(GovernanceDRepUpdateCertificateCmdArgs era)
  | GovernanceDRepMetadataHashCmd !(GovernanceDRepMetadataHashCmdArgs era)

data GovernanceDRepKeyGenCmdArgs era
  = GovernanceDRepKeyGenCmdArgs
  { eon :: !(ConwayEraOnwards era)
  , vkeyFile :: !(File (VerificationKey ()) Out)
  , skeyFile :: !(File (SigningKey ()) Out)
  }

data GovernanceDRepIdCmdArgs era
  = GovernanceDRepIdCmdArgs
  { eon :: !(ConwayEraOnwards era)
  , vkeySource :: !(VerificationKeyOrHashOrFile DRepKey)
  , idOutputFormat :: !IdOutputFormat
  , mOutFile :: !(Maybe (File () Out))
  }

data GovernanceDRepRegistrationCertificateCmdArgs era
  = GovernanceDRepRegistrationCertificateCmdArgs
  { eon :: !(ConwayEraOnwards era)
  , drepHashSource :: !DRepHashSource
  , deposit :: !Lovelace
  , mAnchor
      :: !( Maybe
              ( PotentiallyCheckedAnchor
                  DRepMetadataUrl
                  L.Anchor
              )
          )
  , outFile :: !(File () Out)
  }

data GovernanceDRepRetirementCertificateCmdArgs era
  = GovernanceDRepRetirementCertificateCmdArgs
  { eon :: !(ConwayEraOnwards era)
  , drepHashSource :: !DRepHashSource
  , deposit :: !Lovelace
  , outFile :: !(File () Out)
  }

data GovernanceDRepUpdateCertificateCmdArgs era
  = GovernanceDRepUpdateCertificateCmdArgs
  { eon :: !(ConwayEraOnwards era)
  , drepHashSource :: !DRepHashSource
  , mAnchor
      :: Maybe
           ( PotentiallyCheckedAnchor
               DRepMetadataUrl
               L.Anchor
           )
  , outFile :: !(File () Out)
  }

data GovernanceDRepMetadataHashCmdArgs era
  = GovernanceDRepMetadataHashCmdArgs
  { eon :: !(ConwayEraOnwards era)
  , drepMetadataSource :: !DRepMetadataSource
  , hashGoal :: !(HashGoal (Hash DRepMetadata))
  }

data DRepMetadataSource
  = DrepMetadataFileIn !(DRepMetadataFile In)
  | DrepMetadataURL !L.Url
  deriving Show

renderGovernanceDRepCmds
  :: ()
  => GovernanceDRepCmds era
  -> Text
renderGovernanceDRepCmds = \case
  GovernanceDRepKeyGenCmd{} ->
    "governance drep key-gen"
  GovernanceDRepIdCmd{} ->
    "governance drep id"
  GovernanceDRepRegistrationCertificateCmd{} ->
    "governance drep registration-certificate"
  GovernanceDRepRetirementCertificateCmd{} ->
    "governance drep retirement-certificate"
  GovernanceDRepUpdateCertificateCmd{} ->
    "governance drep update-certificate"
  GovernanceDRepMetadataHashCmd{} ->
    "governance drep metadata-hash"
