{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.EraBased.Options.StakePool
  ( pStakePoolCmds
  ) where

import           Cardano.Api

import           Cardano.CLI.Environment (EnvCli (..))
import           Cardano.CLI.EraBased.Commands.StakePool
import           Cardano.CLI.EraBased.Options.Common

import           Options.Applicative hiding (help, str)
import qualified Options.Applicative as Opt

{- HLINT ignore "Use <$>" -}
{- HLINT ignore "Move brackets to avoid $" -}

pStakePoolCmds :: ()
  => CardanoEra era
  -> EnvCli
  -> Maybe (Parser (StakePoolCmds era))
pStakePoolCmds era envCli =
  subInfoParser "stake-pool"
    ( Opt.progDesc
        $ mconcat
          [ "Stake pool commands."
          ]
    )
    [ pStakePoolRegistrationCertificateCmd era envCli
    , pStakePoolDeregistrationCertificateCmd era
    , Just
        $ subParser "id"
        $ Opt.info pStakePoolId
        $ Opt.progDesc "Build pool id from the offline key"
    , Just
        $ subParser "metadata-hash"
        $ Opt.info pStakePoolMetadataHashCmd
        $ Opt.progDesc "Print the hash of pool metadata."
    ]

pStakePoolId :: Parser (StakePoolCmds era)
pStakePoolId =
  StakePoolIdCmd
    <$> pStakePoolVerificationKeyOrFile Nothing
    <*> pPoolIdOutputFormat
    <*> pMaybeOutputFile

pStakePoolMetadataHashCmd :: Parser (StakePoolCmds era)
pStakePoolMetadataHashCmd =
  StakePoolMetadataHashCmd
    <$> pPoolMetadataFile
    <*> pMaybeOutputFile

pStakePoolRegistrationCertificateCmd :: CardanoEra era -> EnvCli -> Maybe (Parser (StakePoolCmds era))
pStakePoolRegistrationCertificateCmd era envCli = do
  w <- maybeFeatureInEra era
  pure
    $ subParser "registration-certificate"
    $ Opt.info
        ( StakePoolRegistrationCertificateCmd w
            <$> pStakePoolVerificationKeyOrFile Nothing
            <*> pVrfVerificationKeyOrFile
            <*> pPoolPledge
            <*> pPoolCost
            <*> pPoolMargin
            <*> pRewardAcctVerificationKeyOrFile
            <*> some pPoolOwnerVerificationKeyOrFile
            <*> many pPoolRelay
            <*> pStakePoolMetadataReference
            <*> pNetworkId envCli
            <*> pOutputFile
        )
    $ Opt.progDesc "Create a stake pool registration certificate"

pStakePoolDeregistrationCertificateCmd :: CardanoEra era -> Maybe (Parser (StakePoolCmds era))
pStakePoolDeregistrationCertificateCmd era = do
  w <- maybeFeatureInEra era
  pure
    $ subParser "deregistration-certificate"
    $ Opt.info
        ( StakePoolDeregistrationCertificateCmd w
            <$> pStakePoolVerificationKeyOrFile Nothing
            <*> pEpochNo "The epoch number."
            <*> pOutputFile
        )
    $ Opt.progDesc "Create a stake pool deregistration certificate"
