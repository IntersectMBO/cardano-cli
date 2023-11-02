{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- HLINT ignore "Use <$>" -}
{- HLINT ignore "Move brackets to avoid $" -}

module Cardano.CLI.EraBased.Options.StakePool
  ( pStakePoolCmds
  ) where

import           Cardano.Api

import           Cardano.CLI.Environment (EnvCli (..))
import qualified Cardano.CLI.EraBased.Commands.StakePool as Cmd
import           Cardano.CLI.EraBased.Options.Common

import           Options.Applicative hiding (help, str)
import qualified Options.Applicative as Opt

pStakePoolCmds :: ()
  => CardanoEra era
  -> EnvCli
  -> Maybe (Parser (Cmd.StakePoolCmds era))
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

pStakePoolId :: ()
  => Parser (Cmd.StakePoolCmds era)
pStakePoolId =
  fmap Cmd.StakePoolIdCmd $
    Cmd.StakePoolIdCmdArgs
      <$> pStakePoolVerificationKeyOrFile Nothing
      <*> pPoolIdOutputFormat
      <*> pMaybeOutputFile

pStakePoolMetadataHashCmd :: ()
  => Parser (Cmd.StakePoolCmds era)
pStakePoolMetadataHashCmd =
  fmap Cmd.StakePoolMetadataHashCmd $
    Cmd.StakePoolMetadataHashCmdArgs
      <$> pPoolMetadataFile
      <*> pMaybeOutputFile

pStakePoolRegistrationCertificateCmd :: ()
  => CardanoEra era
  -> EnvCli
  -> Maybe (Parser (Cmd.StakePoolCmds era))
pStakePoolRegistrationCertificateCmd era envCli = do
  w <- forEraMaybeEon era
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
              <*> pStakePoolMetadataReference
              <*> pNetworkId envCli
              <*> pOutputFile
        )
    $ Opt.progDesc "Create a stake pool registration certificate"

pStakePoolDeregistrationCertificateCmd :: ()
  => CardanoEra era
  -> Maybe (Parser (Cmd.StakePoolCmds era))
pStakePoolDeregistrationCertificateCmd era = do
  w <- forEraMaybeEon era
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
