{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraIndependent.Address.Command
  ( AddressCmds (..)
  , renderAddressCmds
  )
where

import Cardano.Api

import Cardano.CLI.Type.Common
import Cardano.CLI.Type.Key

import Prelude

import Vary (Vary)

data AddressCmds
  = AddressKeyGen
      (Vary [FormatBech32, FormatTextEnvelope])
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
