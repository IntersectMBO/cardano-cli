{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- HLINT ignore "Use <$>" -}
{- HLINT ignore "Move brackets to avoid $" -}

module Cardano.CLI.Legacy.Options.Pool
  ( parsePoolCmds
  ) where

import           Cardano.CLI.Environment (EnvCli (..))
import           Cardano.CLI.EraBased.Options.Common
import           Cardano.CLI.Legacy.Commands.Pool

import           Data.Foldable
import           Options.Applicative hiding (help, str)
import qualified Options.Applicative as Opt

parsePoolCmds :: EnvCli -> Parser LegacyPoolCmds
parsePoolCmds  envCli =
  asum
    [ subParser "registration-certificate"
        $ Opt.info (pStakePoolRegistrationCert envCli)
        $ Opt.progDesc "Create a stake pool registration certificate"
    , subParser "deregistration-certificate"
        $ Opt.info (pStakePoolRetirementCert envCli)
        $ Opt.progDesc "Create a stake pool deregistration certificate"
    , subParser "id"
        $ Opt.info pId
        $ Opt.progDesc "Build pool id from the offline key"
    , subParser "metadata-hash"
        $ Opt.info pPoolMetadataHashSubCmd
        $ Opt.progDesc "Print the hash of pool metadata."
    ]
  where
    pId :: Parser LegacyPoolCmds
    pId = PoolGetId <$> pStakePoolVerificationKeyOrFile <*> pPoolIdOutputFormat <*> pMaybeOutputFile

    pPoolMetadataHashSubCmd :: Parser LegacyPoolCmds
    pPoolMetadataHashSubCmd = PoolMetadataHash <$> pPoolMetadataFile <*> pMaybeOutputFile

pStakePoolRegistrationCert :: EnvCli -> Parser LegacyPoolCmds
pStakePoolRegistrationCert envCli =
  PoolRegistrationCert
    <$> pAnyShelleyBasedEra envCli
    <*> pStakePoolVerificationKeyOrFile
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

pStakePoolRetirementCert :: EnvCli -> Parser LegacyPoolCmds
pStakePoolRetirementCert envCli =
  PoolRetirementCert
    <$> pAnyShelleyBasedEra envCli
    <*> pStakePoolVerificationKeyOrFile
    <*> pEpochNo
    <*> pOutputFile
