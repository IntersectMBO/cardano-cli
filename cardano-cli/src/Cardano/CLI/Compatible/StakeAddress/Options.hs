{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Cardano.CLI.Compatible.StakeAddress.Options
  ( pCompatibleStakeAddressCmds
  )
where

import Cardano.Api

import Cardano.CLI.Compatible.StakeAddress.Commands
import Cardano.CLI.EraBased.Options.Common
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
  caseShelleyToBabbageOrConwayEraOnwards
    ( const $
        subParser "registration-certificate" $
          Opt.info
            ( CompatibleStakeAddressRegistrationCertificateCmd sbe
                <$> pStakeIdentifier Nothing
                <*> pure Nothing
                <*> pOutputFile
            )
            desc
    )
    ( const $
        subParser "registration-certificate" $
          Opt.info
            ( CompatibleStakeAddressRegistrationCertificateCmd sbe
                <$> pStakeIdentifier Nothing
                <*> fmap Just pKeyRegistDeposit
                <*> pOutputFile
            )
            desc
    )
    sbe
 where
  desc = Opt.progDesc "Create a stake address registration certificate"

pStakeAddressStakeDelegationCertificateCmd
  :: ()
  => ShelleyBasedEra era
  -> Parser (CompatibleStakeAddressCmds era)
pStakeAddressStakeDelegationCertificateCmd sbe = do
  subParser "stake-delegation-certificate"
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
