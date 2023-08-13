{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- HLINT ignore "Use <$>" -}
{- HLINT ignore "Move brackets to avoid $" -}

module Cardano.CLI.Legacy.Options.Address
  ( parseAddressCmds
  ) where

import           Cardano.CLI.Environment (EnvCli (..))
import           Cardano.CLI.EraBased.Options.Common
import           Cardano.CLI.Legacy.Commands.Address

import           Data.Foldable
import           Options.Applicative hiding (help, str)
import qualified Options.Applicative as Opt

parseAddressCmds :: EnvCli -> Parser LegacyAddressCmds
parseAddressCmds envCli =
   asum
     [ subParser "key-gen"
         (Opt.info pAddressKeyGen $ Opt.progDesc "Create an address key pair.")
     , subParser "key-hash"
         (Opt.info pAddressKeyHash $ Opt.progDesc "Print the hash of an address key.")
     , subParser "build"
         (Opt.info pAddressBuild $ Opt.progDesc "Build a Shelley payment address, with optional delegation to a stake address.")
     , subParser "info"
         (Opt.info pAddressInfo $ Opt.progDesc "Print information about an address.")
     ]
  where
    pAddressKeyGen :: Parser LegacyAddressCmds
    pAddressKeyGen =
      AddressKeyGen
        <$> pKeyOutputFormat
        <*> pAddressKeyType
        <*> pVerificationKeyFileOut
        <*> pSigningKeyFileOut

    pAddressKeyHash :: Parser LegacyAddressCmds
    pAddressKeyHash =
      AddressKeyHash
        <$> pPaymentVerificationKeyTextOrFile
        <*> pMaybeOutputFile

    pAddressBuild :: Parser LegacyAddressCmds
    pAddressBuild = AddressBuild
      <$> pPaymentVerifier
      <*> Opt.optional pStakeIdentifier
      <*> pNetworkId envCli
      <*> pMaybeOutputFile

    pAddressInfo :: Parser LegacyAddressCmds
    pAddressInfo = AddressInfo <$> pAddress <*> pMaybeOutputFile
