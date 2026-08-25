{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.Compatible.StakePool.Option
  ( pCompatibleStakePoolCmds
  )
where

import Cardano.Api

import Cardano.CLI.Compatible.StakePool.Command
import Cardano.CLI.Environment (EnvCli (..))
import Cardano.CLI.EraBased.Common.Option
import Cardano.CLI.EraBased.StakePool.Option (pBlsKeySource)
import Cardano.CLI.Parser
import Cardano.CLI.Type.Key (BlsKeySource)

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
            <*> pMaybeBlsKeySource era
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

-- The BLS key can only be registered from the Dijkstra era onwards, and
-- the ledger keeps it optional.
pMaybeBlsKeySource :: ShelleyBasedEra era -> Parser (Maybe BlsKeySource)
pMaybeBlsKeySource = \case
  ShelleyBasedEraShelley -> pure Nothing
  ShelleyBasedEraAllegra -> pure Nothing
  ShelleyBasedEraMary -> pure Nothing
  ShelleyBasedEraAlonzo -> pure Nothing
  ShelleyBasedEraBabbage -> pure Nothing
  ShelleyBasedEraConway -> pure Nothing
  ShelleyBasedEraDijkstra -> optional pBlsKeySource
