{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.Commands.Genesis
  ( GenesisCmds (..)
  , GenesisCreateCmdArgs (..)
  , renderGenesisCmds
  ) where

import           Cardano.Api.Shelley

import           Cardano.Chain.Common (BlockCount)
import           Cardano.CLI.Types.Common

import           Data.Text (Text)

data GenesisCmds era
  = GenesisCreate !GenesisCreateCmdArgs
  | GenesisCreateCardano
      GenesisDir
      Word
      Word
      (Maybe SystemStart)
      (Maybe Lovelace)
      BlockCount
      Word
      Rational
      NetworkId
      FilePath
      FilePath
      FilePath
      FilePath
      (Maybe FilePath)
  | GenesisCreateStaked
      KeyOutputFormat
      GenesisDir
      Word
      Word
      Word
      Word
      (Maybe SystemStart)
      (Maybe Lovelace)
      Lovelace
      NetworkId
      Word
      Word
      Word
      (Maybe FilePath) -- ^ Relay specification filepath
  | GenesisKeyGenGenesis
      (VerificationKeyFile Out)
      (SigningKeyFile Out)
  | GenesisKeyGenDelegate
      (VerificationKeyFile Out)
      (SigningKeyFile Out)
      (OpCertCounterFile Out)
  | GenesisKeyGenUTxO
      (VerificationKeyFile Out)
      (SigningKeyFile Out)
  | GenesisCmdKeyHash
      (VerificationKeyFile In)
  | GenesisVerKey
      (VerificationKeyFile Out)
      (SigningKeyFile In)
  | GenesisTxIn
      (VerificationKeyFile In)
      NetworkId
      (Maybe (File () Out))
  | GenesisAddr
      (VerificationKeyFile In)
      NetworkId
      (Maybe (File () Out))
  | GenesisHashFile
      GenesisFile
  deriving Show

data GenesisCreateCmdArgs = GenesisCreateCmdArgs
  { keyOutputFormat :: !KeyOutputFormat
  , genesisDir :: !GenesisDir
  , numGenesisKeys :: !Word
  , numUTxOKeys :: !Word
  , mSystemStart :: !(Maybe SystemStart)
  , mSupply :: !(Maybe Lovelace)
  , network :: !NetworkId
  } deriving Show

renderGenesisCmds :: GenesisCmds era -> Text
renderGenesisCmds = \case
  GenesisCreate {} ->
    "genesis create"
  GenesisCreateCardano {} ->
    "genesis create-cardano"
  GenesisCreateStaked {} ->
    "genesis create-staked"
  GenesisKeyGenGenesis {} ->
    "genesis key-gen-genesis"
  GenesisKeyGenDelegate {} ->
    "genesis key-gen-delegate"
  GenesisKeyGenUTxO {} ->
    "genesis key-gen-utxo"
  GenesisCmdKeyHash {} ->
    "genesis key-hash"
  GenesisVerKey {} ->
    "genesis get-ver-key"
  GenesisTxIn {} ->
    "genesis initial-txin"
  GenesisAddr {} ->
    "genesis initial-addr"
  GenesisHashFile {} ->
    "genesis hash"
