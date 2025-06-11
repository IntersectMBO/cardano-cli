{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Cardano.CLI.EraBased.Governance.DRep.Option
  ( pGovernanceDRepCmds
  , pUpdateCertificateCmd
  )
where

import Cardano.Api
import Cardano.Api.Experimental qualified as Exp
import Cardano.Api.Ledger qualified as L

import Cardano.CLI.EraBased.Common.Option
import Cardano.CLI.EraBased.Governance.DRep.Command
import Cardano.CLI.EraIndependent.Hash.Command (HashGoal (..))
import Cardano.CLI.Option.Flag
import Cardano.CLI.Parser
import Cardano.CLI.Read

import Control.Applicative (Alternative ((<|>)), optional)
import Data.Foldable (asum)
import Data.Function
import Options.Applicative (Parser)
import Options.Applicative qualified as Opt

pGovernanceDRepCmds
  :: Exp.IsEra era => Maybe (Parser (GovernanceDRepCmds era))
pGovernanceDRepCmds =
  subInfoParser
    "drep"
    ( Opt.progDesc $
        mconcat
          [ "DRep member commands."
          ]
    )
    [ pGovernanceDRepKeyGenCmd
    , pGovernanceDRepKeyIdCmd
    , pRegistrationCertificateCmd
    , pRetirementCertificateCmd
    , pUpdateCertificateCmd
    , pGovernanceDrepMetadataHashCmd
    ]

pGovernanceDRepKeyGenCmd
  :: Exp.IsEra era => Maybe (Parser (GovernanceDRepCmds era))
pGovernanceDRepKeyGenCmd = do
  pure
    $ Opt.hsubparser
    $ commandWithMetavar "key-gen"
    $ Opt.info
      ( fmap GovernanceDRepKeyGenCmd $
          GovernanceDRepKeyGenCmdArgs Exp.useEra
            <$> pVerificationKeyFileOut
            <*> pSigningKeyFileOut
      )
    $ Opt.progDesc "Generate Delegated Representative verification and signing keys."

pGovernanceDRepKeyIdCmd
  :: Exp.IsEra era => Maybe (Parser (GovernanceDRepCmds era))
pGovernanceDRepKeyIdCmd = do
  pure
    $ Opt.hsubparser
    $ commandWithMetavar "id"
    $ Opt.info
      ( fmap GovernanceDRepIdCmd $
          GovernanceDRepIdCmdArgs Exp.useEra
            <$> pDRepVerificationKeyOrHashOrFile
            <*> pFormatFlags
              "drep id output"
              [ flagFormatHex
              , flagFormatBech32 & setDefault
              , flagFormatCip129
              ]
            <*> optional pOutputFile
      )
    $ Opt.progDesc "Generate a drep id."

-- Registration Certificate related

pRegistrationCertificateCmd
  :: Exp.IsEra era => Maybe (Parser (GovernanceDRepCmds era))
pRegistrationCertificateCmd = do
  pure $
    Opt.hsubparser $
      commandWithMetavar "registration-certificate" $
        Opt.info (mkParser Exp.useEra) $
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
  :: Exp.IsEra era => Maybe (Parser (GovernanceDRepCmds era))
pRetirementCertificateCmd = do
  pure
    $ Opt.hsubparser
    $ commandWithMetavar "retirement-certificate"
    $ Opt.info
      ( fmap GovernanceDRepRetirementCertificateCmd $
          GovernanceDRepRetirementCertificateCmdArgs Exp.useEra
            <$> pDRepHashSource
            <*> pDrepDeposit
            <*> pOutputFile
      )
    $ Opt.progDesc "Create a DRep retirement certificate."

pUpdateCertificateCmd
  :: Exp.IsEra era => Maybe (Parser (GovernanceDRepCmds era))
pUpdateCertificateCmd = do
  pure
    $ Opt.hsubparser
    $ commandWithMetavar "update-certificate"
    $ Opt.info
      ( fmap GovernanceDRepUpdateCertificateCmd $
          GovernanceDRepUpdateCertificateCmdArgs Exp.useEra
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
  :: Exp.IsEra era => Maybe (Parser (GovernanceDRepCmds era))
pGovernanceDrepMetadataHashCmd = do
  pure
    $ Opt.hsubparser
    $ commandWithMetavar "metadata-hash"
    $ Opt.info
      ( fmap GovernanceDRepMetadataHashCmd $
          GovernanceDRepMetadataHashCmdArgs Exp.useEra
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
