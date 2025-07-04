{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Legacy.Genesis.Command
  ( LegacyGenesisCmds (..)
  , renderLegacyGenesisCmds
  )
where

import Cardano.Api

import Cardano.CLI.Type.Common
import Cardano.Ledger.BaseTypes (NonZero)

import Data.Text (Text)
import Data.Word (Word64)
import Vary (Vary)

data LegacyGenesisCmds
  = GenesisCreate
      (EraInEon ShelleyBasedEra)
      (Vary [FormatBech32, FormatTextEnvelope])
      GenesisDir
      Word
      Word
      (Maybe SystemStart)
      (Maybe Coin)
      NetworkId
  | GenesisCreateCardano
      (EraInEon ShelleyBasedEra)
      GenesisDir
      Word
      Word
      (Maybe SystemStart)
      (Maybe Coin)
      (NonZero Word64)
      Word
      Rational
      NetworkId
      FilePath
      FilePath
      FilePath
      FilePath
      (Maybe FilePath)
  | -- | Relay specification filepath
    GenesisCreateStaked
      (EraInEon ShelleyBasedEra)
      (Vary [FormatBech32, FormatTextEnvelope])
      GenesisDir
      Word
      Word
      Word
      Word
      (Maybe SystemStart)
      (Maybe Coin)
      Coin
      NetworkId
      Word
      Word
      Word
      (Maybe FilePath)
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

renderLegacyGenesisCmds :: LegacyGenesisCmds -> Text
renderLegacyGenesisCmds = \case
  GenesisCreate{} -> "genesis create"
  GenesisCreateCardano{} -> "genesis create-cardano"
  GenesisCreateStaked{} -> "genesis create-staked"
  GenesisKeyGenGenesis{} -> "genesis key-gen-genesis"
  GenesisKeyGenDelegate{} -> "genesis key-gen-delegate"
  GenesisKeyGenUTxO{} -> "genesis key-gen-utxo"
  GenesisCmdKeyHash{} -> "genesis key-hash"
  GenesisVerKey{} -> "genesis get-ver-key"
  GenesisTxIn{} -> "genesis initial-txin"
  GenesisAddr{} -> "genesis initial-addr"
  GenesisHashFile{} -> "genesis hash"
