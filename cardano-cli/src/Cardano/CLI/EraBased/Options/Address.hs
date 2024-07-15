{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.EraBased.Options.Address
  ( pAddressCmds
  )
where

import           Cardano.Api

import           Cardano.CLI.Environment (EnvCli (..))
import           Cardano.CLI.EraBased.Commands.Address
import           Cardano.CLI.EraBased.Options.Common

import           Options.Applicative hiding (help, str)
import qualified Options.Applicative as Opt

pAddressCmds
  :: ()
  => CardanoEra era
  -> EnvCli
  -> Maybe (Parser (AddressCmds era))
pAddressCmds _ envCli =
  subInfoParser
    "address"
    ( Opt.progDesc $
        mconcat
          [ "Payment address commands."
          ]
    )
    [ Just $
        subParser "key-gen" $
          Opt.info pAddressKeyGen $
            Opt.progDesc "Create an address key pair."
    , Just $
        subParser "key-hash" $
          Opt.info pAddressKeyHash $
            Opt.progDesc "Print the hash of an address key."
    , Just $
        subParser "build" $
          Opt.info (pAddressBuild envCli) $
            Opt.progDesc "Build a Shelley payment address, with optional delegation to a stake address."
    , Just $
        subParser "info" $
          Opt.info pAddressInfo $
            Opt.progDesc "Print information about an address."
    ]

pAddressKeyGen :: Parser (AddressCmds era)
pAddressKeyGen =
  AddressKeyGen
    <$> pKeyOutputFormat
    <*> pAddressKeyType
    <*> pVerificationKeyFileOut
    <*> pSigningKeyFileOut

pAddressKeyHash :: Parser (AddressCmds era)
pAddressKeyHash =
  AddressKeyHash
    <$> pPaymentVerificationKeyTextOrFile
    <*> pMaybeOutputFile

pAddressBuild :: EnvCli -> Parser (AddressCmds era)
pAddressBuild envCli =
  AddressBuild
    <$> pPaymentVerifier
    <*> Opt.optional (pStakeIdentifier Nothing)
    <*> pNetworkId envCli
    <*> pMaybeOutputFile

pAddressInfo :: Parser (AddressCmds era)
pAddressInfo =
  AddressInfo
    <$> pAddress
    <*> pMaybeOutputFile
