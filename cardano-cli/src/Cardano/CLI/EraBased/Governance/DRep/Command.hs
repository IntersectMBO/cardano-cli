{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.Governance.DRep.Command
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
import Cardano.Api.Experimental qualified as Exp
import Cardano.Api.Ledger qualified as L

import Cardano.CLI.EraIndependent.Hash.Command (HashGoal)
import Cardano.CLI.Type.Common
import Cardano.CLI.Type.Key

import Vary

data GovernanceDRepCmds era
  = GovernanceDRepKeyGenCmd !(GovernanceDRepKeyGenCmdArgs era)
  | GovernanceDRepIdCmd !(GovernanceDRepIdCmdArgs era)
  | GovernanceDRepRegistrationCertificateCmd !(GovernanceDRepRegistrationCertificateCmdArgs era)
  | GovernanceDRepRetirementCertificateCmd !(GovernanceDRepRetirementCertificateCmdArgs era)
  | GovernanceDRepUpdateCertificateCmd !(GovernanceDRepUpdateCertificateCmdArgs era)
  | GovernanceDRepMetadataHashCmd !(GovernanceDRepMetadataHashCmdArgs era)

data GovernanceDRepKeyGenCmdArgs era
  = GovernanceDRepKeyGenCmdArgs
  { era :: !(Exp.Era era)
  , vkeyFile :: !(File (VerificationKey ()) Out)
  , skeyFile :: !(File (SigningKey ()) Out)
  }

data GovernanceDRepIdCmdArgs era
  = GovernanceDRepIdCmdArgs
  { era :: !(Exp.Era era)
  , vkeySource :: !(VerificationKeyOrHashOrFile DRepKey)
  , idOutputFormat :: !(Vary [FormatBech32, FormatHex, FormatCip129])
  , mOutFile :: !(Maybe (File () Out))
  }

data GovernanceDRepRegistrationCertificateCmdArgs era
  = GovernanceDRepRegistrationCertificateCmdArgs
  { era :: !(Exp.Era era)
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
  { era :: !(Exp.Era era)
  , drepHashSource :: !DRepHashSource
  , deposit :: !Lovelace
  , outFile :: !(File () Out)
  }

data GovernanceDRepUpdateCertificateCmdArgs era
  = GovernanceDRepUpdateCertificateCmdArgs
  { era :: !(Exp.Era era)
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
  { era :: !(Exp.Era era)
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
