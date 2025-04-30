{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cardano.CLI.Read.DRep
  ( AnyDrepVerificationKey (..)

    -- * Read bech32 or hex encoded DRep verification key
  , readDRepBech32VerificationKeyText
  , readDRepHexVerificationKeyText

    -- * Read TextEnvelope DRep verification key file
  , readDrepVerificationKeyFile
  )
where

import Cardano.Api

import Cardano.CLI.Read
import Cardano.Prelude qualified as Text

import Prelude

import Data.Text (Text)
import Data.Validation

data AnyDrepVerificationKey where
  AnyDrepVerificationKey :: VerificationKey DRepKey -> AnyDrepVerificationKey
  AnyDrepExtendedVerificationKey :: VerificationKey DRepExtendedKey -> AnyDrepVerificationKey

deriving instance Show AnyDrepVerificationKey

readDRepBech32VerificationKeyText :: Text -> Validation [Bech32DecodeError] AnyDrepVerificationKey
readDRepBech32VerificationKeyText drep =
  let vkey =
        liftError return $
          AnyDrepVerificationKey <$> deserialiseFromBech32 drep
      extendedVkey =
        liftError return $
          AnyDrepExtendedVerificationKey <$> deserialiseFromBech32 drep
   in vkey <> extendedVkey

readDRepHexVerificationKeyText :: Text -> Validation [RawBytesHexError] AnyDrepVerificationKey
readDRepHexVerificationKeyText drepText =
  let drepBs = Text.encodeUtf8 drepText
      vkey =
        liftError return $
          AnyDrepVerificationKey <$> deserialiseFromRawBytesHex drepBs
      extendedVkey =
        liftError return $
          AnyDrepExtendedVerificationKey
            <$> deserialiseFromRawBytesHex drepBs
   in vkey <> extendedVkey

readDrepVerificationKeyFile
  :: FileOrPipe -> IO (Either (FileError TextEnvelopeError) AnyDrepVerificationKey)
readDrepVerificationKeyFile = readFileOrPipeTextEnvelopeAnyOf types
 where
  types =
    [ FromSomeType (AsVerificationKey AsDRepKey) AnyDrepVerificationKey
    , FromSomeType (AsVerificationKey AsDRepExtendedKey) AnyDrepExtendedVerificationKey
    ]
