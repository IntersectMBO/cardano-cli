{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- HLINT ignore "Use <$>" -}
{- HLINT ignore "Move brackets to avoid $" -}

module Cardano.CLI.Legacy.Options.StakeAddress
  ( parseStakeAddressCmds
  ) where

import           Cardano.CLI.Environment (EnvCli (..))
import           Cardano.CLI.EraBased.Options.Common
import           Cardano.CLI.Legacy.Commands.StakeAddress

import           Data.Foldable
import           Options.Applicative hiding (help, str)
import qualified Options.Applicative as Opt

parseStakeAddressCmds :: EnvCli -> Parser LegacyStakeAddressCmds
parseStakeAddressCmds envCli =
    asum
      [ subParser "key-gen"
          $ Opt.info pStakeAddressKeyGen
          $ Opt.progDesc "Create a stake address key pair"
      , subParser "build"
          $ Opt.info pStakeAddressBuild
          $ Opt.progDesc "Build a stake address"
      , subParser "key-hash"
          $ Opt.info pStakeAddressKeyHash
          $ Opt.progDesc "Print the hash of a stake address key."
      , subParser "registration-certificate"
          $ Opt.info pStakeAddressRegistrationCert
          $ Opt.progDesc "Create a stake address registration certificate"
      , subParser "deregistration-certificate"
          $ Opt.info pStakeAddressDeregistrationCert
          $ Opt.progDesc "Create a stake address deregistration certificate"
      , subParser "delegation-certificate"
          $ Opt.info pStakeAddressPoolDelegationCert
          $ Opt.progDesc "Create a stake address pool delegation certificate"
      ]
  where
    pStakeAddressKeyGen :: Parser LegacyStakeAddressCmds
    pStakeAddressKeyGen =
      StakeAddressKeyGen
        <$> pKeyOutputFormat
        <*> pVerificationKeyFileOut
        <*> pSigningKeyFileOut

    pStakeAddressKeyHash :: Parser LegacyStakeAddressCmds
    pStakeAddressKeyHash = StakeAddressKeyHash <$> pStakeVerificationKeyOrFile <*> pMaybeOutputFile

    pStakeAddressBuild :: Parser LegacyStakeAddressCmds
    pStakeAddressBuild =
      StakeAddressBuild
        <$> pStakeVerifier
        <*> pNetworkId envCli
        <*> pMaybeOutputFile

    pStakeAddressRegistrationCert :: Parser LegacyStakeAddressCmds
    pStakeAddressRegistrationCert =
      StakeRegistrationCert
        <$> pAnyShelleyBasedEra envCli
        <*> pStakeIdentifier
        <*> optional pKeyRegistDeposit
        <*> pOutputFile

    pStakeAddressDeregistrationCert :: Parser LegacyStakeAddressCmds
    pStakeAddressDeregistrationCert =
      StakeCredentialDeRegistrationCert
        <$> pAnyShelleyBasedEra envCli
        <*> pStakeIdentifier
        <*> optional pKeyRegistDeposit
        <*> pOutputFile

    pStakeAddressPoolDelegationCert :: Parser LegacyStakeAddressCmds
    pStakeAddressPoolDelegationCert =
      StakeCredentialDelegationCert
        <$> pAnyShelleyBasedEra envCli
        <*> pStakeIdentifier
        <*> pDelegationTarget
        <*> pOutputFile
