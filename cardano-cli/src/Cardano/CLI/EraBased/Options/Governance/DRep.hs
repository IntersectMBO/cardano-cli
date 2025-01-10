{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Cardano.CLI.EraBased.Options.Governance.DRep
  ( pGovernanceDRepCmds
  , pUpdateCertificateCmd
  )
where

import           Cardano.Api
import qualified Cardano.Api.Ledger as L
import           Cardano.Api.Shelley (Hash (DRepMetadataHash))

import           Cardano.CLI.Commands.Hash (HashGoal (..))
import           Cardano.CLI.Environment
import           Cardano.CLI.EraBased.Commands.Governance.DRep
import           Cardano.CLI.EraBased.Options.Common
import           Cardano.CLI.Parser
import           Cardano.CLI.Read
import           Cardano.CLI.Types.Common hiding (CheckHash)
import           Cardano.CLI.Types.Key

import           Control.Applicative
import           Data.Foldable
import           Data.String
import           Options.Applicative (Parser)
import qualified Options.Applicative as Opt

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
    $ subParser "key-gen"
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
    $ subParser "id"
    $ Opt.info
      ( fmap GovernanceDRepIdCmd $
          GovernanceDRepIdCmdArgs w
            <$> pDRepVerificationKeyOrHashOrFile
            <*> pDRepIdOutputFormat
            <*> optional pOutputFile
      )
    $ Opt.progDesc "Generate a drep id."

pDRepIdOutputFormat :: Parser IdOutputFormat
pDRepIdOutputFormat =
  Opt.option readIdOutputFormat $
    mconcat
      [ Opt.long "output-format"
      , Opt.metavar "STRING"
      , Opt.help $
          mconcat
            [ "Optional drep id output format. Accepted output formats are \"hex\" "
            , "and \"bech32\" (default is \"bech32\")."
            ]
      , Opt.value IdOutputFormatBech32
      ]

-- Registration Certificate related

pRegistrationCertificateCmd
  :: ()
  => ShelleyBasedEra era
  -> Maybe (Parser (GovernanceDRepCmds era))
pRegistrationCertificateCmd era = do
  w <- forShelleyBasedEraMaybeEon era
  pure $
    subParser "registration-certificate" $
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

pDRepMetadata :: Parser (L.Anchor L.StandardCrypto)
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

pDrepMetadataHash :: Parser (L.SafeHash L.StandardCrypto L.AnchorData)
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
    $ subParser "retirement-certificate"
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
    $ subParser "update-certificate"
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
    $ subParser "metadata-hash"
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
