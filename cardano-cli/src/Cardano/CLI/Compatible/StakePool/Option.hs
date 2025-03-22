{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.Compatible.StakePool.Option
  ( pCompatibleStakePoolCmds
  )
where

import Cardano.Api

import Cardano.CLI.Compatible.StakePool.Command
import Cardano.CLI.Environment (EnvCli (..))
import Cardano.CLI.EraBased.Common.Option
import Cardano.CLI.Parser

import Options.Applicative hiding (help, str)
import Options.Applicative qualified as Opt

pCompatibleStakePoolCmds
  :: ()
  => ShelleyBasedEra era
  -> EnvCli
  -> Maybe (Parser (CompatibleStakePoolCmds era))
pCompatibleStakePoolCmds era envCli =
  subInfoParser
    "stake-pool"
    ( Opt.progDesc $
        mconcat
          [ "Stake pool commands."
          ]
    )
    [ pCompatibleStakePoolRegistrationCertificateCmd era envCli
    ]

pCompatibleStakePoolRegistrationCertificateCmd
  :: ()
  => ShelleyBasedEra era
  -> EnvCli
  -> Maybe (Parser (CompatibleStakePoolCmds era))
pCompatibleStakePoolRegistrationCertificateCmd era envCli = do
  w <- forShelleyBasedEraMaybeEon era
  pure
    $ Opt.hsubparser
    $ commandWithMetavar "registration-certificate"
    $ Opt.info
      ( fmap CompatibleStakePoolRegistrationCertificateCmd $
          CompatibleStakePoolRegistrationCertificateCmdArgs w
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
