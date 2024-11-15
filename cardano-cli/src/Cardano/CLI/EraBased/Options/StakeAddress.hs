{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Cardano.CLI.EraBased.Options.StakeAddress
  ( pStakeAddressCmds
  )
where

import           Cardano.Api

import           Cardano.CLI.Environment
import           Cardano.CLI.EraBased.Commands.StakeAddress
import           Cardano.CLI.EraBased.Options.Common
import           Cardano.CLI.Parser

import           Options.Applicative
import qualified Options.Applicative as Opt

pStakeAddressCmds
  :: ()
  => ShelleyBasedEra era
  -> EnvCli
  -> Maybe (Parser (StakeAddressCmds era))
pStakeAddressCmds era envCli =
  subInfoParser
    "stake-address"
    ( Opt.progDesc $
        mconcat
          [ "Stake address commands."
          ]
    )
    [ Just pStakeAddressKeyGenCmd
    , Just pStakeAddressKeyHashCmd
    , Just (pStakeAddressBuildCmd envCli)
    , Just (pStakeAddressRegistrationCertificateCmd era)
    , Just (pStakeAddressDeregistrationCertificateCmd era)
    , Just (pStakeAddressStakeDelegationCertificateCmd era)
    , pStakeAddressStakeAndVoteDelegationCertificateCmd era
    , pStakeAddressVoteDelegationCertificateCmd era
    , pStakeAddressRegistrationAndDelegationCertificateCmd era
    , pStakeAddressRegistrationAndVoteDelegationCertificateCmd era
    , pStakeAddressRegistrationStakeAndVoteDelegationCertificateCmd era
    ]

pStakeAddressKeyGenCmd
  :: ()
  => Parser (StakeAddressCmds era)
pStakeAddressKeyGenCmd = do
  subParser "key-gen"
    $ Opt.info
      ( StakeAddressKeyGenCmd
          <$> pKeyOutputFormat
          <*> pVerificationKeyFileOut
          <*> pSigningKeyFileOut
      )
    $ Opt.progDesc "Create a stake address key pair"

pStakeAddressKeyHashCmd
  :: ()
  => Parser (StakeAddressCmds era)
pStakeAddressKeyHashCmd =
  do
    subParser "key-hash"
    $ Opt.info
      ( StakeAddressKeyHashCmd
          <$> pStakeVerificationKeyOrFile Nothing
          <*> pMaybeOutputFile
      )
    $ Opt.progDesc "Print the hash of a stake address key"

pStakeAddressBuildCmd
  :: ()
  => EnvCli
  -> Parser (StakeAddressCmds era)
pStakeAddressBuildCmd envCli = do
  subParser "build"
    $ Opt.info
      ( StakeAddressBuildCmd
          <$> pStakeVerifier Nothing
          <*> pNetworkId envCli
          <*> pMaybeOutputFile
      )
    $ Opt.progDesc "Build a stake address"

pStakeAddressRegistrationCertificateCmd
  :: ()
  => ShelleyBasedEra era
  -> Parser (StakeAddressCmds era)
pStakeAddressRegistrationCertificateCmd sbe = do
  caseShelleyToBabbageOrConwayEraOnwards
    ( const $
        subParser "registration-certificate" $
          Opt.info
            ( StakeAddressRegistrationCertificateCmd sbe
                <$> pStakeIdentifier Nothing
                <*> pure Nothing
                <*> pOutputFile
            )
            desc
    )
    ( const $
        subParser "registration-certificate" $
          Opt.info
            ( StakeAddressRegistrationCertificateCmd sbe
                <$> pStakeIdentifier Nothing
                <*> fmap Just pKeyRegistDeposit
                <*> pOutputFile
            )
            desc
    )
    sbe
 where
  desc = Opt.progDesc "Create a stake address registration certificate"

pStakeAddressDeregistrationCertificateCmd
  :: ()
  => ShelleyBasedEra era
  -> Parser (StakeAddressCmds era)
pStakeAddressDeregistrationCertificateCmd =
  caseShelleyToBabbageOrConwayEraOnwards
    ( \shelleyToBabbage ->
        subParser "deregistration-certificate"
          $ Opt.info
            ( StakeAddressDeregistrationCertificateCmd (inject shelleyToBabbage)
                <$> pStakeIdentifier Nothing
                <*> pure Nothing
                <*> pOutputFile
            )
          $ Opt.progDesc "Create a stake address deregistration certificate"
    )
    ( \conwayOnwards ->
        subParser "deregistration-certificate"
          $ Opt.info
            ( StakeAddressDeregistrationCertificateCmd (inject conwayOnwards)
                <$> pStakeIdentifier Nothing
                <*> fmap Just pKeyRegistDeposit
                <*> pOutputFile
            )
          $ Opt.progDesc "Create a stake address deregistration certificate"
    )

pStakeAddressStakeDelegationCertificateCmd
  :: ()
  => ShelleyBasedEra era
  -> Parser (StakeAddressCmds era)
pStakeAddressStakeDelegationCertificateCmd sbe = do
  subParser "stake-delegation-certificate"
    $ Opt.info
      ( StakeAddressStakeDelegationCertificateCmd sbe
          <$> pStakeIdentifier Nothing
          <*> pStakePoolVerificationKeyOrHashOrFile Nothing
          <*> pOutputFile
      )
    $ Opt.progDesc
    $ mconcat
      [ "Create a stake address stake delegation certificate, which when submitted in a transaction "
      , "delegates stake to a stake pool."
      ]

pStakeAddressStakeAndVoteDelegationCertificateCmd
  :: ()
  => ShelleyBasedEra era
  -> Maybe (Parser (StakeAddressCmds era))
pStakeAddressStakeAndVoteDelegationCertificateCmd era = do
  w <- forShelleyBasedEraMaybeEon era
  pure
    $ subParser "stake-and-vote-delegation-certificate"
    $ Opt.info
      ( StakeAddressStakeAndVoteDelegationCertificateCmd w
          <$> pStakeIdentifier Nothing
          <*> pStakePoolVerificationKeyOrHashOrFile Nothing
          <*> pVoteDelegationTarget
          <*> pOutputFile
      )
    $ Opt.progDesc
    $ mconcat
      [ "Create a stake address stake and vote delegation certificate, which when submitted in a transaction "
      , "delegates stake to a stake pool and a DRep."
      ]

pStakeAddressVoteDelegationCertificateCmd
  :: ()
  => ShelleyBasedEra era
  -> Maybe (Parser (StakeAddressCmds era))
pStakeAddressVoteDelegationCertificateCmd era = do
  w <- forShelleyBasedEraMaybeEon era
  pure
    $ subParser "vote-delegation-certificate"
    $ Opt.info
      ( StakeAddressVoteDelegationCertificateCmd w
          <$> pStakeIdentifier Nothing
          <*> pVoteDelegationTarget
          <*> pOutputFile
      )
    $ Opt.progDesc
    $ mconcat
      [ "Create a stake address vote delegation certificate, which when submitted in a transaction "
      , "delegates stake to a DRep."
      ]

pStakeAddressRegistrationAndDelegationCertificateCmd
  :: ()
  => ShelleyBasedEra era
  -> Maybe (Parser (StakeAddressCmds era))
pStakeAddressRegistrationAndDelegationCertificateCmd era = do
  w <- forShelleyBasedEraMaybeEon era
  pure
    $ subParser "registration-and-delegation-certificate"
    $ Opt.info
      ( StakeAddressRegistrationAndDelegationCertificateCmd w
          <$> pStakeIdentifier Nothing
          <*> pStakePoolVerificationKeyOrHashOrFile Nothing
          <*> pKeyRegistDeposit
          <*> pOutputFile
      )
    $ Opt.progDesc
    $ mconcat
      [ "Create a stake address registration and delegation certificate, which when submitted in a transaction "
      , "registers a stake address and delegates stake to a stake pool."
      ]

pStakeAddressRegistrationAndVoteDelegationCertificateCmd
  :: ()
  => ShelleyBasedEra era
  -> Maybe (Parser (StakeAddressCmds era))
pStakeAddressRegistrationAndVoteDelegationCertificateCmd era = do
  w <- forShelleyBasedEraMaybeEon era
  pure
    $ subParser "registration-and-vote-delegation-certificate"
    $ Opt.info
      ( StakeAddressRegistrationAndVoteDelegationCertificateCmd w
          <$> pStakeIdentifier Nothing
          <*> pVoteDelegationTarget
          <*> pKeyRegistDeposit
          <*> pOutputFile
      )
    $ Opt.progDesc
    $ mconcat
      [ "Create a stake address registration and vote delegation certificate, which when submitted in a transaction "
      , "registers a stake address and delegates votes to a DRep or pre-defined voting option."
      ]

pStakeAddressRegistrationStakeAndVoteDelegationCertificateCmd
  :: ()
  => ShelleyBasedEra era
  -> Maybe (Parser (StakeAddressCmds era))
pStakeAddressRegistrationStakeAndVoteDelegationCertificateCmd era = do
  w <- forShelleyBasedEraMaybeEon era
  pure
    $ subParser "registration-stake-and-vote-delegation-certificate"
    $ Opt.info
      ( StakeAddressRegistrationStakeAndVoteDelegationCertificateCmd w
          <$> pStakeIdentifier Nothing
          <*> pStakePoolVerificationKeyOrHashOrFile Nothing
          <*> pVoteDelegationTarget
          <*> pKeyRegistDeposit
          <*> pOutputFile
      )
    $ Opt.progDesc
    $ mconcat
      [ "Create a stake address registration, stake delegation and vote delegation certificate, which when submitted in a transaction "
      , "registers a stake address, delgates stake to a pool, and delegates votes to a DRep or pre-defined voting option."
      ]
