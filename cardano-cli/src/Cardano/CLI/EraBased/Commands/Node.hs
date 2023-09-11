{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.Commands.Node
  ( NodeCmds (..)
  , renderNodeCmds
  ) where

import           Cardano.Api.Shelley

import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Key

import           Data.Text (Text)

data NodeCmds era
  = NodeKeyGenCold
      KeyOutputFormat
      (VerificationKeyFile Out)
      (SigningKeyFile Out)
      (OpCertCounterFile Out)
  | NodeKeyGenKES
      KeyOutputFormat
      (VerificationKeyFile Out)
      (SigningKeyFile Out)
  | NodeKeyGenVRF
      KeyOutputFormat
      (VerificationKeyFile Out)
      (SigningKeyFile Out)
  | NodeKeyHashVRF
      (VerificationKeyOrFile VrfKey)
      (Maybe (File () Out))
  | NodeNewCounter
      ColdVerificationKeyOrFile
      Word
      (OpCertCounterFile InOut)
  | NodeIssueOpCert
      (VerificationKeyOrFile KesKey)
      (SigningKeyFile In)
      (OpCertCounterFile InOut)
      KESPeriod (File () Out)
  deriving Show

renderNodeCmds :: NodeCmds era -> Text
renderNodeCmds = \case
  NodeKeyGenCold {} ->
    "node key-gen"
  NodeKeyGenKES {} ->
    "node key-gen-KES"
  NodeKeyGenVRF {} ->
    "node key-gen-VRF"
  NodeKeyHashVRF {} ->
    "node key-hash-VRF"
  NodeNewCounter {} ->
    "node new-counter"
  NodeIssueOpCert{} ->
    "node issue-op-cert"
