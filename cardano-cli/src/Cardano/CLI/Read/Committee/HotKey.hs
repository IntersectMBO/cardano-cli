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
        liftError' $
          AnyCommitteeHotVerificationKey
            <$> deserialiseFromBech32 committeeHot
      extendedVkey =
        liftError' $
          AnyCommitteeHotExtendedVerificationKey
            <$> deserialiseFromBech32 committeeHot
   in vkey <> extendedVkey

readCommitteeHotHexVerificationKeyText
  :: Text -> Validation [RawBytesHexError] AnyCommitteeHotVerificationKey
readCommitteeHotHexVerificationKeyText committeeHotText =
  let committeeHotBs = Text.encodeUtf8 committeeHotText
      vkey =
        liftError' $
          AnyCommitteeHotVerificationKey
            <$> deserialiseFromRawBytesHex committeeHotBs
      extendedVkey =
        liftError' $
          AnyCommitteeHotExtendedVerificationKey
            <$> deserialiseFromRawBytesHex committeeHotBs
   in vkey <> extendedVkey

-- | Convert an 'Either' to a 'Validation', wrapping the error in a singleton list.
liftError' :: Either e a -> Validation [e] a
liftError' (Left e) = Failure [e]
liftError' (Right a) = Success a

readCommitteeHotVerificationKeyFile
  :: FileOrPipe -> IO (Either (FileError TextEnvelopeError) AnyCommitteeHotVerificationKey)
readCommitteeHotVerificationKeyFile = readFileOrPipeTextEnvelopeAnyOf types
 where
  types =
    [ FromSomeType (AsVerificationKey AsCommitteeHotKey) AnyCommitteeHotVerificationKey
    , FromSomeType (AsVerificationKey AsCommitteeHotExtendedKey) AnyCommitteeHotExtendedVerificationKey
    ]
