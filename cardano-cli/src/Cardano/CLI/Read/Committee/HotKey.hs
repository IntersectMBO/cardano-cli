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
import Data.Either qualified as Either

data AnyCommitteeHotVerificationKey where
  AnyCommitteeHotVerificationKey :: VerificationKey CommitteeHotKey -> AnyCommitteeHotVerificationKey
  AnyCommitteeHotExtendedVerificationKey
    :: VerificationKey CommitteeHotExtendedKey -> AnyCommitteeHotVerificationKey

deriving instance Show AnyCommitteeHotVerificationKey

readCommitteeHotBech32VerificationKeyText
  :: Text -> Validation [Bech32DecodeError] AnyCommitteeHotVerificationKey
readCommitteeHotBech32VerificationKeyText committeeHot =
  let vkey =
        Either.either (Failure . return) Success $
          AnyCommitteeHotVerificationKey
            <$> deserialiseFromBech32 committeeHot
      extendedVkey =
        Either.either (Failure . return) Success $
          AnyCommitteeHotExtendedVerificationKey
            <$> deserialiseFromBech32 committeeHot
   in vkey <> extendedVkey

readCommitteeHotHexVerificationKeyText
  :: Text -> Validation [RawBytesHexError] AnyCommitteeHotVerificationKey
readCommitteeHotHexVerificationKeyText committeeHotText =
  let committeeHotBs = Text.encodeUtf8 committeeHotText
      vkey =
        Either.either (Failure . return) Success $
          AnyCommitteeHotVerificationKey
            <$> deserialiseFromRawBytesHex committeeHotBs
      extendedVkey =
        Either.either (Failure . return) Success $
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
