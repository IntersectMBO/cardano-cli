{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Cardano.CLI.EraBased.StakeAddress.Option
  ( pStakeAddressCmds
  )
where

import Cardano.Api
import Cardano.Api.Experimental qualified as Exp

import Cardano.CLI.Environment
import Cardano.CLI.EraBased.Common.Option
import Cardano.CLI.EraBased.StakeAddress.Command
import Cardano.CLI.Parser

import Options.Applicative
import Options.Applicative qualified as Opt

pStakeAddressCmds
  :: Exp.IsEra era
  => EnvCli
  -> Maybe (Parser (StakeAddressCmds era))
pStakeAddressCmds envCli =
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
    , Just pStakeAddressRegistrationCertificateCmd
    , Just pStakeAddressDeregistrationCertificateCmd
    , Just pStakeAddressStakeDelegationCertificateCmd
    , pStakeAddressStakeAndVoteDelegationCertificateCmd
    , pStakeAddressVoteDelegationCertificateCmd
    , pStakeAddressRegistrationAndDelegationCertificateCmd
    , pStakeAddressRegistrationAndVoteDelegationCertificateCmd
    , pStakeAddressRegistrationStakeAndVoteDelegationCertificateCmd
    ]

pStakeAddressKeyGenCmd
  :: ()
  => Parser (StakeAddressCmds era)
pStakeAddressKeyGenCmd = do
  Opt.hsubparser
    $ commandWithMetavar "key-gen"
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
  Opt.hsubparser
    $ commandWithMetavar "key-hash"
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
  Opt.hsubparser
    $ commandWithMetavar "build"
    $ Opt.info
      ( StakeAddressBuildCmd
          <$> pStakeVerifier Nothing
          <*> pNetworkId envCli
          <*> pMaybeOutputFile
      )
    $ Opt.progDesc "Build a stake address"

pStakeAddressRegistrationCertificateCmd
  :: Exp.IsEra era
  => Parser (StakeAddressCmds era)
pStakeAddressRegistrationCertificateCmd = do
  Opt.hsubparser $
    commandWithMetavar "registration-certificate" $
      Opt.info
        ( StakeAddressRegistrationCertificateCmd Exp.useEra
            <$> pStakeIdentifier Nothing
            <*> pFeatured Exp.useEra pKeyRegistDeposit
            <*> pOutputFile
        )
        desc
 where
  desc = Opt.progDesc "Create a stake address registration certificate"

pStakeAddressDeregistrationCertificateCmd
  :: Exp.IsEra era
  => Parser (StakeAddressCmds era)
pStakeAddressDeregistrationCertificateCmd =
  Opt.hsubparser
    $ commandWithMetavar "deregistration-certificate"
    $ Opt.info
      ( StakeAddressDeregistrationCertificateCmd Exp.useEra
          <$> pStakeIdentifier Nothing
          <*> pFeatured Exp.useEra pKeyRegistDeposit
          <*> pOutputFile
      )
    $ Opt.progDesc "Create a stake address deregistration certificate"

pStakeAddressStakeDelegationCertificateCmd
  :: Exp.IsEra era
  => Parser (StakeAddressCmds era)
pStakeAddressStakeDelegationCertificateCmd = do
  Opt.hsubparser
    $ commandWithMetavar "stake-delegation-certificate"
    $ Opt.info
      ( StakeAddressStakeDelegationCertificateCmd Exp.useEra
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
  :: Exp.IsEra era => Maybe (Parser (StakeAddressCmds era))
pStakeAddressStakeAndVoteDelegationCertificateCmd = do
  pure
    $ Opt.hsubparser
    $ commandWithMetavar "stake-and-vote-delegation-certificate"
    $ Opt.info
      ( StakeAddressStakeAndVoteDelegationCertificateCmd (convert Exp.useEra)
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
  :: Exp.IsEra era
  => Maybe (Parser (StakeAddressCmds era))
pStakeAddressVoteDelegationCertificateCmd =
  do pure
    $ Opt.hsubparser
    $ commandWithMetavar "vote-delegation-certificate"
    $ Opt.info
      ( StakeAddressVoteDelegationCertificateCmd (convert Exp.useEra)
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
  :: Exp.IsEra era
  => Maybe (Parser (StakeAddressCmds era))
pStakeAddressRegistrationAndDelegationCertificateCmd = do
  pure
    $ Opt.hsubparser
    $ commandWithMetavar "registration-and-delegation-certificate"
    $ Opt.info
      ( StakeAddressRegistrationAndDelegationCertificateCmd (convert Exp.useEra)
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
  :: Exp.IsEra era
  => Maybe (Parser (StakeAddressCmds era))
pStakeAddressRegistrationAndVoteDelegationCertificateCmd = do
  pure
    $ Opt.hsubparser
    $ commandWithMetavar "registration-and-vote-delegation-certificate"
    $ Opt.info
      ( StakeAddressRegistrationAndVoteDelegationCertificateCmd (convert Exp.useEra)
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
  :: Exp.IsEra era
  => Maybe (Parser (StakeAddressCmds era))
pStakeAddressRegistrationStakeAndVoteDelegationCertificateCmd = do
  pure
    $ Opt.hsubparser
    $ commandWithMetavar "registration-stake-and-vote-delegation-certificate"
    $ Opt.info
      ( StakeAddressRegistrationStakeAndVoteDelegationCertificateCmd (convert Exp.useEra)
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
