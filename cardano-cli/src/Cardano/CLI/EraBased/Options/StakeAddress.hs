{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Cardano.CLI.EraBased.Options.StakeAddress
  ( pStakeAddressCmds
  ) where

import           Cardano.Api

import           Cardano.CLI.Environment
import           Cardano.CLI.EraBased.Commands.StakeAddress
import           Cardano.CLI.EraBased.Options.Common
import           Cardano.CLI.Orphans ()

import           Options.Applicative
import qualified Options.Applicative as Opt

pStakeAddressCmds :: ()
  => CardanoEra era
  -> EnvCli
  -> Maybe (Parser (StakeAddressCmds era))
pStakeAddressCmds era envCli =
  subInfoParser "stake-address"
    ( Opt.progDesc
        $ mconcat
          [ "Stake address commands."
          ]
    )
    [ pStakeAddressKeyGenCmd era
    , pStakeAddressKeyHashCmd era
    , pStakeAddressBuildCmd era envCli
    , pStakeAddressRegistrationCertificateCmd era
    , pStakeAddressDeregistrationCertificateCmd era
    , pStakeAddressStakeDelegationCertificateCmd era
    , pStakeAddressStakeAndVoteDelegationCertificateCmd era
    , pStakeAddressVoteDelegationCertificateCmd era
    ]

pStakeAddressKeyGenCmd :: ()
  => CardanoEra era
  -> Maybe (Parser (StakeAddressCmds era))
pStakeAddressKeyGenCmd era = do
  w <- maybeEonInEra era
  pure
    $ subParser "key-gen"
    $ Opt.info
        ( StakeAddressKeyGenCmd w
            <$> pKeyOutputFormat
            <*> pVerificationKeyFileOut
            <*> pSigningKeyFileOut
        )
    $ Opt.progDesc "Create a stake address key pair"

pStakeAddressKeyHashCmd :: ()
  => CardanoEra era
  -> Maybe (Parser (StakeAddressCmds era))
pStakeAddressKeyHashCmd era = do
  w <- maybeEonInEra era
  pure
    $ subParser "key-hash"
    $ Opt.info
        ( StakeAddressKeyHashCmd w
            <$> pStakeVerificationKeyOrFile Nothing
            <*> pMaybeOutputFile
        )
    $ Opt.progDesc "Print the hash of a stake address key"

pStakeAddressBuildCmd :: ()
  => CardanoEra era
  -> EnvCli
  -> Maybe (Parser (StakeAddressCmds era))
pStakeAddressBuildCmd era envCli = do
  w <- maybeEonInEra era
  pure
    $ subParser "build"
    $ Opt.info
        ( StakeAddressBuildCmd w
            <$> pStakeVerifier
            <*> pNetworkId envCli
            <*> pMaybeOutputFile
        )
    $ Opt.progDesc "Build a stake address"

pStakeAddressRegistrationCertificateCmd :: ()
  => CardanoEra era
  -> Maybe (Parser (StakeAddressCmds era))
pStakeAddressRegistrationCertificateCmd era = do
  w <- maybeEonInEra era
  pure
    $ subParser "registration-certificate"
    $ Opt.info
        ( StakeAddressRegistrationCertificateCmd w
            <$> pStakeIdentifier
            <*> optional pKeyRegistDeposit
            <*> pOutputFile
        )
    $ Opt.progDesc "Create a stake address registration certificate"

pStakeAddressDeregistrationCertificateCmd :: ()
  => CardanoEra era
  -> Maybe (Parser (StakeAddressCmds era))
pStakeAddressDeregistrationCertificateCmd era = do
  w <- maybeEonInEra era
  pure
    $ subParser "deregistration-certificate"
    $ Opt.info
        ( StakeAddressDeregistrationCertificateCmd w
            <$> pStakeIdentifier
            <*> optional pKeyRegistDeposit
            <*> pOutputFile
        )
    $ Opt.progDesc "Create a stake address deregistration certificate"

pStakeAddressStakeDelegationCertificateCmd :: ()
  => CardanoEra era
  -> Maybe (Parser (StakeAddressCmds era))
pStakeAddressStakeDelegationCertificateCmd era = do
  w <- maybeEonInEra era
  pure
    $ subParser "stake-delegation-certificate"
    $ Opt.info
        ( StakeAddressStakeDelegationCertificateCmd w
            <$> pStakeIdentifier
            <*> pStakePoolVerificationKeyOrHashOrFile Nothing
            <*> pOutputFile
        )
    $ Opt.progDesc
    $ mconcat
        [ "Create a stake address stake delegation certificate, which when submitted in a transaction "
        , "delegates stake to a stake pool."
        ]

pStakeAddressStakeAndVoteDelegationCertificateCmd :: ()
  => CardanoEra era
  -> Maybe (Parser (StakeAddressCmds era))
pStakeAddressStakeAndVoteDelegationCertificateCmd era = do
  w <- maybeEonInEra era
  pure
    $ subParser "stake-and-vote-delegation-certificate"
    $ Opt.info
        ( StakeAddressStakeAndVoteDelegationCertificateCmd w
            <$> pStakeIdentifier
            <*> pStakePoolVerificationKeyOrHashOrFile Nothing
            <*> pVoteDelegationTarget
            <*> pOutputFile
        )
    $ Opt.progDesc
    $ mconcat
        [ "Create a stake address stake and vote delegation certificate, which when submitted in a transaction "
        , "delegates stake to a stake pool and a DRep."
        ]

pStakeAddressVoteDelegationCertificateCmd :: ()
  => CardanoEra era
  -> Maybe (Parser (StakeAddressCmds era))
pStakeAddressVoteDelegationCertificateCmd era = do
  w <- maybeEonInEra era
  pure
    $ subParser "vote-delegation-certificate"
    $ Opt.info
        ( StakeAddressVoteDelegationCertificateCmd w
            <$> pStakeIdentifier
            <*> pVoteDelegationTarget
            <*> pOutputFile
        )
    $ Opt.progDesc
    $ mconcat
        [ "Create a stake address vote delegation certificate, which when submitted in a transaction "
        , "delegates stake to a stake pool and a DRep."
        ]
