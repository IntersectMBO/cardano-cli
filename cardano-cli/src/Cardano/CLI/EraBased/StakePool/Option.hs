{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.CLI.EraBased.StakePool.Option
  ( pStakePoolCmds
  )
where

import Cardano.Api
import Cardano.Api.Experimental
import Cardano.Api.Experimental qualified as Exp
import Cardano.Api.Experimental.Certificate (Hash (StakePoolMetadataHash), StakePoolMetadata)
import Cardano.Api.Ledger qualified as L

import Cardano.CLI.Environment (EnvCli (..))
import Cardano.CLI.EraBased.Common.Option
import Cardano.CLI.EraBased.StakePool.Command qualified as Cmd
import Cardano.CLI.EraIndependent.Hash.Command qualified as Cmd
import Cardano.CLI.Parser
import Cardano.CLI.Type.Common (CertificateFile (..), SigningKeyFile)

import Data.Foldable qualified as F
import Options.Applicative hiding (help, str)
import Options.Applicative qualified as Opt

pStakePoolCmds
  :: IsEra era
  => EnvCli
  -> Maybe (Parser (Cmd.StakePoolCmds era))
pStakePoolCmds envCli =
  subInfoParser
    "stake-pool"
    ( Opt.progDesc $
        mconcat
          [ "Stake pool commands."
          ]
    )
    [ pStakePoolRegistrationCertificateCmd envCli
    , pStakePoolBlsRegistrationCertificateCmd
    , pStakePoolDeregistrationCertificateCmd
    , Just $
        Opt.hsubparser $
          commandWithMetavar "id" $
            Opt.info pStakePoolId $
              Opt.progDesc "Build pool id from the offline key"
    , Just $
        Opt.hsubparser $
          commandWithMetavar "metadata-hash" $
            Opt.info pStakePoolMetadataHashCmd $
              Opt.progDesc $
                mconcat
                  [ "Calculate the hash of a stake pool metadata file,"
                  , " optionally checking the obtained hash against an expected value."
                  ]
    ]

pStakePoolId
  :: ()
  => Parser (Cmd.StakePoolCmds era)
pStakePoolId =
  fmap Cmd.StakePoolIdCmd $
    Cmd.StakePoolIdCmdArgs
      <$> pStakePoolVerificationKeyOrFile Nothing
      <*> pPoolIdOutputFormat
      <*> pMaybeOutputFile

pStakePoolMetadataHashCmd
  :: ()
  => Parser (Cmd.StakePoolCmds era)
pStakePoolMetadataHashCmd =
  fmap Cmd.StakePoolMetadataHashCmd $
    Cmd.StakePoolMetadataHashCmdArgs
      <$> pPoolMetadataSource
      <*> pPoolMetadataHashGoal

pPoolMetadataSource :: Parser Cmd.StakePoolMetadataSource
pPoolMetadataSource =
  F.asum
    [ Cmd.StakePoolMetadataFileIn <$> pPoolMetadataFile
    , Cmd.StakePoolMetadataURL
        <$> pUrl
          "pool-metadata-url"
          "URL pointing to the JSON Metadata file to hash. Supported schemes are: file, http, https, ipfs."
    ]

pPoolMetadataHashGoal :: Parser (Cmd.HashGoal (Hash StakePoolMetadata))
pPoolMetadataHashGoal =
  F.asum
    [ Cmd.CheckHash <$> pExpectedStakePoolMetadataHash
    , Cmd.HashToFile <$> pOutputFile
    ]
    <|> pure Cmd.HashToStdout

pExpectedStakePoolMetadataHash :: Parser (Hash StakePoolMetadata)
pExpectedStakePoolMetadataHash =
  pExpectedHash (StakePoolMetadataHash . L.extractHash . L.castSafeHash) "stake pool metadata"

pStakePoolRegistrationCertificateCmd
  :: IsEra era
  => EnvCli
  -> Maybe (Parser (Cmd.StakePoolCmds era))
pStakePoolRegistrationCertificateCmd envCli = do
  let era = useEra
  pure
    $ Opt.hsubparser
    $ commandWithMetavar "registration-certificate"
    $ Opt.info
      ( fmap Cmd.StakePoolRegistrationCertificateCmd $
          Cmd.StakePoolRegistrationCertificateCmdArgs (convert era)
            <$> pStakePoolVerificationKeyOrFile Nothing
            <*> pVrfVerificationKeyOrFile
            <*> pBlsSigningKeyFileForEra era
            <*> pPoolPledge
            <*> pPoolCost
            <*> pPoolMargin
            <*> pRewardAcctVerificationKeyOrFile
            <*> some pPoolOwnerVerificationKeyOrFile
            <*> many pPoolRelay
            <*> pValidateRelays
            <*> optional
              ( pPotentiallyCheckedAnchorData
                  pMustCheckStakeMetadataHash
                  pStakePoolMetadataReference
              )
            <*> pNetworkId envCli
            <*> pOutputFile
      )
    $ Opt.progDesc "Create a stake pool registration certificate"

-- | A pool registers its voting key from Dijkstra onwards, so the BLS signing
-- key is mandatory there and not offered at all in earlier eras.
pBlsSigningKeyFileForEra :: Exp.Era era -> Parser (Maybe (SigningKeyFile In))
pBlsSigningKeyFileForEra Exp.ConwayEra = pure Nothing
pBlsSigningKeyFileForEra Exp.DijkstraEra = Just <$> pBlsSigningKeyFile

pBlsSigningKeyFile :: Parser (SigningKeyFile In)
pBlsSigningKeyFile =
  File
    <$> parseFilePath
      "bls-signing-key-file"
      "Input filepath of the BLS signing key."

-- | Dijkstra only: the BLS key is registered from that era onwards.
pStakePoolBlsRegistrationCertificateCmd
  :: forall era
   . IsEra era
  => Maybe (Parser (Cmd.StakePoolCmds era))
pStakePoolBlsRegistrationCertificateCmd =
  case useEra @era of
    Exp.ConwayEra -> Nothing
    Exp.DijkstraEra ->
      Just
        . Opt.hsubparser
        . commandWithMetavar "bls-registration-certificate"
        . Opt.info pOffline
        $ Opt.progDesc
        $ mconcat
          [ "Create a stake pool registration certificate that keeps the pool's existing"
          , " parameters and replaces only its BLS key and proof of possession."
          ]
 where
  pOffline =
    Opt.hsubparser
      . commandWithMetavar "offline"
      . Opt.info pBlsRegistrationCertificateOffline
      $ Opt.progDesc
      $ mconcat
        [ "Create the certificate from the parameters in an existing stake pool registration certificate file."
        , " No node connection is needed."
        ]

  pBlsRegistrationCertificateOffline =
    fmap Cmd.StakePoolBlsRegistrationCertificateCmd $
      Cmd.StakePoolBlsRegistrationCertificateCmdArgs (convert $ useEra @era)
        <$> pStakePoolParametersFromCertificate
        <*> pBlsSigningKeyFile
        <*> pOutputFile

pStakePoolParametersFromCertificate :: Parser Cmd.StakePoolParametersSource
pStakePoolParametersFromCertificate =
  Cmd.StakePoolParametersFromCertificate . CertificateFile
    <$> parseFilePath
      "registration-certificate-file"
      "Input filepath of the stake pool registration certificate to copy the parameters from."

pStakePoolDeregistrationCertificateCmd
  :: IsEra era => Maybe (Parser (Cmd.StakePoolCmds era))
pStakePoolDeregistrationCertificateCmd = do
  pure
    $ Opt.hsubparser
    $ commandWithMetavar "deregistration-certificate"
    $ Opt.info
      ( fmap Cmd.StakePoolDeregistrationCertificateCmd $
          Cmd.StakePoolDeregistrationCertificateCmdArgs (convert useEra)
            <$> pStakePoolVerificationKeyOrFile Nothing
            <*> pEpochNo "The epoch number."
            <*> pOutputFile
      )
    $ Opt.progDesc "Create a stake pool deregistration certificate"
