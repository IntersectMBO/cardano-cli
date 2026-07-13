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

import Data.Validation

data AnyDrepVerificationKey where
  AnyDrepVerificationKey :: VerificationKey DRepKey -> AnyDrepVerificationKey
  AnyDrepExtendedVerificationKey :: VerificationKey DRepExtendedKey -> AnyDrepVerificationKey

deriving instance Show AnyDrepVerificationKey

readDRepBech32VerificationKeyText :: Text -> Validation [Bech32DecodeError] AnyDrepVerificationKey
readDRepBech32VerificationKeyText drep =
  let vkey =
        liftError' $
          AnyDrepVerificationKey <$> deserialiseFromBech32 drep
      extendedVkey =
        liftError' $
          AnyDrepExtendedVerificationKey <$> deserialiseFromBech32 drep
   in vkey <> extendedVkey

readDRepHexVerificationKeyText :: Text -> Validation [RawBytesHexError] AnyDrepVerificationKey
readDRepHexVerificationKeyText drepText =
  let drepBs = Text.encodeUtf8 drepText
      vkey =
        liftError' $
          AnyDrepVerificationKey <$> deserialiseFromRawBytesHex drepBs
      extendedVkey =
        liftError' $
          AnyDrepExtendedVerificationKey
            <$> deserialiseFromRawBytesHex drepBs
   in vkey <> extendedVkey

-- | Convert an 'Either' to a 'Validation', wrapping the error in a singleton list.
-- Replaces @liftError return@ from older versions of the @validation@ package.
liftError' :: Either e a -> Validation [e] a
liftError' (Left e) = Failure [e]
liftError' (Right a) = Success a

readDrepVerificationKeyFile
  :: FileOrPipe -> IO (Either (FileError TextEnvelopeError) AnyDrepVerificationKey)
readDrepVerificationKeyFile = readFileOrPipeTextEnvelopeAnyOf types
 where
  types =
    [ FromSomeType (AsVerificationKey AsDRepKey) AnyDrepVerificationKey
    , FromSomeType (AsVerificationKey AsDRepExtendedKey) AnyDrepExtendedVerificationKey
    ]
