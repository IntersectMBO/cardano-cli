{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cardano.CLI.Read.Committee.ColdKey
  ( AnyCommitteeColdVerificationKey (..)

    -- * Read bech32 or hex encoded Committee Hot verification key
  , readCommitteeColdBech32VerificationKeyText
  , readCommitteeColdHexVerificationKeyText

    -- * Read TextEnvelope Committee Hot verification key file
  , readCommitteeColdVerificationKeyFile
  )
where

import Cardano.Api

import Cardano.CLI.Read
import Cardano.Prelude qualified as Text

import Prelude

import Data.Text (Text)
import Data.Validation

data AnyCommitteeColdVerificationKey where
  AnyCommitteeColdVerificationKey
    :: VerificationKey CommitteeColdKey -> AnyCommitteeColdVerificationKey
  AnyCommitteeColdExtendedVerificationKey
    :: VerificationKey CommitteeColdExtendedKey -> AnyCommitteeColdVerificationKey

deriving instance Show AnyCommitteeColdVerificationKey

readCommitteeColdBech32VerificationKeyText
  :: Text -> Validation [Bech32DecodeError] AnyCommitteeColdVerificationKey
readCommitteeColdBech32VerificationKeyText committeeColdText =
  let vkey =
        liftError return $
          AnyCommitteeColdVerificationKey
            <$> deserialiseFromBech32 committeeColdText
      extendedVkey =
        liftError return $
          AnyCommitteeColdExtendedVerificationKey
            <$> deserialiseFromBech32 committeeColdText
   in vkey <> extendedVkey

readCommitteeColdHexVerificationKeyText
  :: Text -> Validation [RawBytesHexError] AnyCommitteeColdVerificationKey
readCommitteeColdHexVerificationKeyText committeeColdText =
  let committeeColdBs = Text.encodeUtf8 committeeColdText
      vkey =
        liftError return $
          AnyCommitteeColdVerificationKey
            <$> deserialiseFromRawBytesHex committeeColdBs
      extendedVkey =
        liftError return $
          AnyCommitteeColdExtendedVerificationKey
            <$> deserialiseFromRawBytesHex committeeColdBs
   in vkey <> extendedVkey

readCommitteeColdVerificationKeyFile
  :: FileOrPipe -> IO (Either (FileError TextEnvelopeError) AnyCommitteeColdVerificationKey)
readCommitteeColdVerificationKeyFile = readFileOrPipeTextEnvelopeAnyOf types
 where
  types =
    [ FromSomeType (AsVerificationKey AsCommitteeColdKey) AnyCommitteeColdVerificationKey
    , FromSomeType (AsVerificationKey AsCommitteeColdExtendedKey) AnyCommitteeColdExtendedVerificationKey
    ]
