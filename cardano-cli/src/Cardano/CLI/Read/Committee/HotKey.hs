{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cardano.CLI.Read.Committee.HotKey
  ( AnyCommitteeHotVerificationKey (..)

    -- * Read bech32 or hex encoded Committee Hot verification key
  , readCommitteeHotBech32VerificationKeyText
  , readCommitteeHotHexVerificationKeyText

    -- * Read TextEnvelope Committee Hot verification key file
  , readCommitteeHotVerificationKeyFile
  )
where

import Cardano.Api

import Cardano.CLI.Read
import Cardano.Prelude qualified as Text

import Prelude

import Data.Validation

data AnyCommitteeHotVerificationKey where
  AnyCommitteeHotVerificationKey :: VerificationKey CommitteeHotKey -> AnyCommitteeHotVerificationKey
  AnyCommitteeHotExtendedVerificationKey
    :: VerificationKey CommitteeHotExtendedKey -> AnyCommitteeHotVerificationKey

deriving instance Show AnyCommitteeHotVerificationKey

readCommitteeHotBech32VerificationKeyText
  :: Text -> Validation [Bech32DecodeError] AnyCommitteeHotVerificationKey
readCommitteeHotBech32VerificationKeyText committeeHot =
  let vkey =
        liftError return $
          AnyCommitteeHotVerificationKey
            <$> deserialiseFromBech32 committeeHot
      extendedVkey =
        liftError return $
          AnyCommitteeHotExtendedVerificationKey
            <$> deserialiseFromBech32 committeeHot
   in vkey <> extendedVkey

readCommitteeHotHexVerificationKeyText
  :: Text -> Validation [RawBytesHexError] AnyCommitteeHotVerificationKey
readCommitteeHotHexVerificationKeyText committeeHotText =
  let committeeHotBs = Text.encodeUtf8 committeeHotText
      vkey =
        liftError return $
          AnyCommitteeHotVerificationKey
            <$> deserialiseFromRawBytesHex committeeHotBs
      extendedVkey =
        liftError return $
          AnyCommitteeHotExtendedVerificationKey
            <$> deserialiseFromRawBytesHex committeeHotBs
   in vkey <> extendedVkey

readCommitteeHotVerificationKeyFile
  :: FileOrPipe -> IO (Either (FileError TextEnvelopeError) AnyCommitteeHotVerificationKey)
readCommitteeHotVerificationKeyFile = readFileOrPipeTextEnvelopeAnyOf types
 where
  types =
    [ FromSomeType (AsVerificationKey AsCommitteeHotKey) AnyCommitteeHotVerificationKey
    , FromSomeType (AsVerificationKey AsCommitteeHotExtendedKey) AnyCommitteeHotExtendedVerificationKey
    ]
