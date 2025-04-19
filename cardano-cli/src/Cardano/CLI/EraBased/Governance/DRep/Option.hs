{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Cardano.CLI.EraBased.Governance.DRep.Option
  ( pGovernanceDRepCmds
  , pUpdateCertificateCmd
  )
where

import Cardano.Api
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Shelley (Hash (DRepMetadataHash))

import Cardano.CLI.EraBased.Common.Option
import Cardano.CLI.EraBased.Governance.DRep.Command
import Cardano.CLI.EraIndependent.Hash.Command (HashGoal (..))
import Cardano.CLI.Parser
import Cardano.CLI.Read
import Cardano.CLI.Type.Common hiding (CheckHash)

import Control.Applicative (Alternative ((<|>)), optional)
import Data.Foldable (asum)
import Options.Applicative (Parser)
import Options.Applicative qualified as Opt

import Vary

pGovernanceDRepCmds
  :: ()
  => ShelleyBasedEra era
  -> Maybe (Parser (GovernanceDRepCmds era))
pGovernanceDRepCmds era =
  subInfoParser
    "drep"
    ( Opt.progDesc $
        mconcat
          [ "DRep member commands."
          ]
    )
    [ pGovernanceDRepKeyGenCmd era
    , pGovernanceDRepKeyIdCmd era
    , pRegistrationCertificateCmd era
    , pRetirementCertificateCmd era
    , pUpdateCertificateCmd era
    , pGovernanceDrepMetadataHashCmd era
    ]

pGovernanceDRepKeyGenCmd
  :: ()
  => ShelleyBasedEra era
  -> Maybe (Parser (GovernanceDRepCmds era))
pGovernanceDRepKeyGenCmd era = do
  w <- forShelleyBasedEraMaybeEon era
  pure
    $ Opt.hsubparser
    $ commandWithMetavar "key-gen"
    $ Opt.info
      ( fmap GovernanceDRepKeyGenCmd $
          GovernanceDRepKeyGenCmdArgs w
            <$> pVerificationKeyFileOut
            <*> pSigningKeyFileOut
      )
    $ Opt.progDesc "Generate Delegated Representative verification and signing keys."

pGovernanceDRepKeyIdCmd
  :: ()
  => ShelleyBasedEra era
  -> Maybe (Parser (GovernanceDRepCmds era))
pGovernanceDRepKeyIdCmd era = do
  w <- forShelleyBasedEraMaybeEon era
  pure
    $ Opt.hsubparser
    $ commandWithMetavar "id"
    $ Opt.info
      ( fmap GovernanceDRepIdCmd $
          GovernanceDRepIdCmdArgs w
            <$> pDRepVerificationKeyOrHashOrFile
            <*> pDRepIdOutputFormat
            <*> optional pOutputFile
      )
    $ Opt.progDesc "Generate a drep id."

pDRepIdOutputFormat :: Parser (Vary [FormatBech32, FormatHex])
pDRepIdOutputFormat =
  asum [make (Vary.from FormatHex) "hex", make (Vary.from FormatBech32) "bech32"]
    <|> pure default_
 where
  default_ = Vary.from FormatBech32
  make format flag_ =
    Opt.flag' format $
      mconcat
        [ Opt.help $
            "Format drep id output as "
              <> flag_
              <> (if format == default_ then " (the default)." else ".")
        , Opt.long ("output-" <> flag_)
        ]

-- Registration Certificate related

pRegistrationCertificateCmd
  :: ()
  => ShelleyBasedEra era
  -> Maybe (Parser (GovernanceDRepCmds era))
pRegistrationCertificateCmd era = do
  w <- forShelleyBasedEraMaybeEon era
  pure $
    Opt.hsubparser $
      commandWithMetavar "registration-certificate" $
        Opt.info (conwayEraOnwardsConstraints w $ mkParser w) $
          Opt.progDesc "Create a registration certificate."
 where
  mkParser w =
    fmap GovernanceDRepRegistrationCertificateCmd $
      GovernanceDRepRegistrationCertificateCmdArgs w
        <$> pDRepHashSource
        <*> pKeyRegistDeposit
        <*> optional
          ( pPotentiallyCheckedAnchorData
              pMustCheckMetadataHash
              pDRepMetadata
          )
        <*> pOutputFile

pDRepMetadata :: Parser L.Anchor
pDRepMetadata =
  L.Anchor
    <$> fmap unAnchorUrl pDrepMetadataUrl
    <*> pDrepMetadataHash

pDrepMetadataUrl :: Parser AnchorUrl
pDrepMetadataUrl =
  AnchorUrl
    <$> pUrl "drep-metadata-url" "DRep anchor URL"

pExpectedDrepMetadataHash :: Parser (Hash DRepMetadata)
pExpectedDrepMetadataHash =
  pExpectedHash (DRepMetadataHash . L.extractHash . L.castSafeHash) "DRep metadata"

pDrepMetadataHash :: Parser (L.SafeHash L.AnchorData)
pDrepMetadataHash =
  Opt.option readSafeHash $
    mconcat
      [ Opt.long "drep-metadata-hash"
      , Opt.metavar "HASH"
      , Opt.help "DRep anchor data hash."
      ]

pRetirementCertificateCmd
  :: ()
  => ShelleyBasedEra era
  -> Maybe (Parser (GovernanceDRepCmds era))
pRetirementCertificateCmd era = do
  w <- forShelleyBasedEraMaybeEon era
  pure
    $ Opt.hsubparser
    $ commandWithMetavar "retirement-certificate"
    $ Opt.info
      ( fmap GovernanceDRepRetirementCertificateCmd $
          GovernanceDRepRetirementCertificateCmdArgs w
            <$> pDRepHashSource
            <*> pDrepDeposit
            <*> pOutputFile
      )
    $ Opt.progDesc "Create a DRep retirement certificate."

pUpdateCertificateCmd
  :: ()
  => ShelleyBasedEra era
  -> Maybe (Parser (GovernanceDRepCmds era))
pUpdateCertificateCmd era = do
  w <- forShelleyBasedEraMaybeEon era
  pure
    $ Opt.hsubparser
    $ commandWithMetavar "update-certificate"
    $ Opt.info
      ( fmap GovernanceDRepUpdateCertificateCmd $
          conwayEraOnwardsConstraints w $
            GovernanceDRepUpdateCertificateCmdArgs w
              <$> pDRepHashSource
              <*> optional
                ( pPotentiallyCheckedAnchorData
                    pMustCheckMetadataHash
                    pDRepMetadata
                )
              <*> pOutputFile
      )
    $ Opt.progDesc "Create a DRep update certificate."

pGovernanceDrepMetadataHashCmd
  :: ()
  => ShelleyBasedEra era
  -> Maybe (Parser (GovernanceDRepCmds era))
pGovernanceDrepMetadataHashCmd era = do
  w <- forShelleyBasedEraMaybeEon era
  pure
    $ Opt.hsubparser
    $ commandWithMetavar "metadata-hash"
    $ Opt.info
      ( fmap GovernanceDRepMetadataHashCmd $
          GovernanceDRepMetadataHashCmdArgs w
            <$> pDRepMetadataSource
            <*> pDRepHashGoal
      )
    $ Opt.progDesc
      "Calculate the hash of a metadata file, optionally checking the obtained hash against an expected value."

pDRepHashGoal :: Parser (HashGoal (Hash DRepMetadata))
pDRepHashGoal =
  asum
    [ CheckHash <$> pExpectedDrepMetadataHash
    , HashToFile <$> pOutputFile
    ]
    <|> pure HashToStdout

pDRepMetadataSource :: Parser DRepMetadataSource
pDRepMetadataSource =
  asum
    [ DrepMetadataFileIn <$> pFileInDirection "drep-metadata-file" "JSON Metadata file to hash."
    , DrepMetadataURL <$> pUrl "drep-metadata-url" "URL pointing to the JSON Metadata file to hash."
    ]
