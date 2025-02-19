{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Commands.Address
  ( AddressCmds (..)
  , renderAddressCmds
  )
where

import Cardano.Api.Shelley hiding (QueryInShelleyBasedEra (..))

import Cardano.CLI.Types.Common
import Cardano.CLI.Types.Key

import Prelude

import Data.Text (Text)

data AddressCmds
  = AddressKeyGen
      KeyOutputFormat
      AddressKeyType
      (VerificationKeyFile Out)
      (SigningKeyFile Out)
  | AddressKeyHash
      VerificationKeyTextOrFile
      (Maybe (File () Out))
  | AddressBuild
      PaymentVerifier
      (Maybe StakeIdentifier)
      NetworkId
      (Maybe (File () Out))
  | AddressInfo
      Text
      (Maybe (File () Out))
  deriving Show

renderAddressCmds :: AddressCmds -> Text
renderAddressCmds = \case
  AddressKeyGen{} -> "address key-gen"
  AddressKeyHash{} -> "address key-hash"
  AddressBuild{} -> "address build"
  AddressInfo{} -> "address info"
