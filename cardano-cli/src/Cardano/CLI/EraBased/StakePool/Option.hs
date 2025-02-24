{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- HLINT ignore "Use <$>" -}
{- HLINT ignore "Move brackets to avoid $" -}

module Cardano.CLI.EraBased.StakePool.Option
  ( pStakePoolCmds
  )
where

import Cardano.Api
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Shelley (Hash (StakePoolMetadataHash))

import Cardano.CLI.Environment (EnvCli (..))
import Cardano.CLI.EraBased.Common.Option
import Cardano.CLI.EraBased.StakePool.Command qualified as Cmd
import Cardano.CLI.EraIndependent.Hash.Command qualified as Cmd
import Cardano.CLI.Parser

import Data.Foldable qualified as F
import Options.Applicative hiding (help, str)
import Options.Applicative qualified as Opt

pStakePoolCmds
  :: ()
  => ShelleyBasedEra era
  -> EnvCli
  -> Maybe (Parser (Cmd.StakePoolCmds era))
pStakePoolCmds era envCli =
  subInfoParser
    "stake-pool"
    ( Opt.progDesc $
        mconcat
          [ "Stake pool commands."
          ]
    )
    [ pStakePoolRegistrationCertificateCmd era envCli
    , pStakePoolDeregistrationCertificateCmd era
    , Just $
        subParser "id" $
          Opt.info pStakePoolId $
            Opt.progDesc "Build pool id from the offline key"
    , Just $
        subParser "metadata-hash" $
          Opt.info pStakePoolMetadataHashCmd $
            Opt.progDesc
              ( "Calculate the hash of a stake pool metadata file,"
                  <> " optionally checking the obtained hash against an expected value."
              )
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
        <$> pUrl "pool-metadata-url" "URL pointing to the JSON Metadata file to hash."
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
  :: ()
  => ShelleyBasedEra era
  -> EnvCli
  -> Maybe (Parser (Cmd.StakePoolCmds era))
pStakePoolRegistrationCertificateCmd era envCli = do
  w <- forShelleyBasedEraMaybeEon era
  pure
    $ subParser "registration-certificate"
    $ Opt.info
      ( fmap Cmd.StakePoolRegistrationCertificateCmd $
          Cmd.StakePoolRegistrationCertificateCmdArgs w
            <$> pStakePoolVerificationKeyOrFile Nothing
            <*> pVrfVerificationKeyOrFile
            <*> pPoolPledge
            <*> pPoolCost
            <*> pPoolMargin
            <*> pRewardAcctVerificationKeyOrFile
            <*> some pPoolOwnerVerificationKeyOrFile
            <*> many pPoolRelay
            <*> optional
              ( pPotentiallyCheckedAnchorData
                  pMustCheckStakeMetadataHash
                  pStakePoolMetadataReference
              )
            <*> pNetworkId envCli
            <*> pOutputFile
      )
    $ Opt.progDesc "Create a stake pool registration certificate"

pStakePoolDeregistrationCertificateCmd
  :: ()
  => ShelleyBasedEra era
  -> Maybe (Parser (Cmd.StakePoolCmds era))
pStakePoolDeregistrationCertificateCmd era = do
  w <- forShelleyBasedEraMaybeEon era
  pure
    $ subParser "deregistration-certificate"
    $ Opt.info
      ( fmap Cmd.StakePoolDeregistrationCertificateCmd $
          Cmd.StakePoolDeregistrationCertificateCmdArgs w
            <$> pStakePoolVerificationKeyOrFile Nothing
            <*> pEpochNo "The epoch number."
            <*> pOutputFile
      )
    $ Opt.progDesc "Create a stake pool deregistration certificate"
