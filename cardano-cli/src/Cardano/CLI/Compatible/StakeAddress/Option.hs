{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Cardano.CLI.Compatible.StakeAddress.Option
  ( pCompatibleStakeAddressCmds
  )
where

import Cardano.Api

import Cardano.CLI.Compatible.StakeAddress.Command
import Cardano.CLI.EraBased.Common.Option
import Cardano.CLI.Parser

import Options.Applicative
import Options.Applicative qualified as Opt

pCompatibleStakeAddressCmds
  :: ()
  => ShelleyBasedEra era
  -> Maybe (Parser (CompatibleStakeAddressCmds era))
pCompatibleStakeAddressCmds era =
  subInfoParser
    "stake-address"
    ( Opt.progDesc $
        mconcat
          [ "Stake address commands."
          ]
    )
    [ Just $ pStakeAddressRegistrationCertificateCmd era
    , Just $ pStakeAddressStakeDelegationCertificateCmd era
    ]

pStakeAddressRegistrationCertificateCmd
  :: ()
  => ShelleyBasedEra era
  -> Parser (CompatibleStakeAddressCmds era)
pStakeAddressRegistrationCertificateCmd sbe = do
  Opt.hsubparser $
    commandWithMetavar "registration-certificate" $
      Opt.info
        ( CompatibleStakeAddressRegistrationCertificateCmd sbe
            <$> pStakeIdentifier Nothing
            <*> ( case sbe of
                    ShelleyBasedEraShelley -> pure Nothing
                    ShelleyBasedEraAllegra -> pure Nothing
                    ShelleyBasedEraMary -> pure Nothing
                    ShelleyBasedEraAlonzo -> pure Nothing
                    ShelleyBasedEraBabbage -> pure Nothing
                    ShelleyBasedEraConway -> Just <$> pKeyRegistDeposit
                    ShelleyBasedEraDijkstra -> Just <$> pKeyRegistDeposit
                )
            <*> pOutputFile
        )
        desc
 where
  desc = Opt.progDesc "Create a stake address registration certificate"

pStakeAddressStakeDelegationCertificateCmd
  :: ()
  => ShelleyBasedEra era
  -> Parser (CompatibleStakeAddressCmds era)
pStakeAddressStakeDelegationCertificateCmd sbe = do
  Opt.hsubparser
    $ commandWithMetavar "stake-delegation-certificate"
    $ Opt.info
      ( CompatibleStakeAddressStakeDelegationCertificateCmd sbe
          <$> pStakeIdentifier Nothing
          <*> pStakePoolVerificationKeyOrHashOrFile Nothing
          <*> pOutputFile
      )
    $ Opt.progDesc
    $ mconcat
      [ "Create a stake address stake delegation certificate, which when submitted in a transaction "
      , "delegates stake to a stake pool."
      ]
