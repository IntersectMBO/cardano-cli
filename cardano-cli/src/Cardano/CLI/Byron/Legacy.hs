{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.Byron.Legacy
  ( LegacyDelegateKey (..)
  , encodeLegacyDelegateKey
  , decodeLegacyDelegateKey
  )
where

import Cardano.Api (textShow)

import Cardano.Crypto.Signing (SigningKey (..))
import Cardano.Crypto.Wallet qualified as Wallet

import Codec.CBOR.Decoding qualified as D
import Codec.CBOR.Encoding qualified as E
import Control.Monad (when)
import Data.Text (Text)
import Formatting (build, formatToString)

-- | LegacyDelegateKey is a subset of the UserSecret's from the legacy codebase:
-- 1. the VSS keypair must be present
-- 2. the signing key must be present
-- 3. the rest must be absent (Nothing)
--
-- Legacy reference: https://github.com/input-output-hk/cardano-sl/blob/release/3.0.1/lib/src/Pos/Util/UserSecret.hs#L189
newtype LegacyDelegateKey = LegacyDelegateKey {lrkSigningKey :: SigningKey}

encodeXPrv :: Wallet.XPrv -> E.Encoding
encodeXPrv a = E.encodeBytes $ Wallet.unXPrv a

decodeXPrv :: D.Decoder s Wallet.XPrv
decodeXPrv =
  either (fail . formatToString build) pure . Wallet.xprv =<< D.decodeBytesCanonical

-- Stolen from: cardano-sl/binary/src/Pos/Binary/Class/Core.hs

-- | Enforces that the input size is the same as the decoded one, failing in
-- case it's not.
enforceSize :: Text -> Int -> D.Decoder s ()
enforceSize lbl requestedSize = D.decodeListLenCanonical >>= matchSize requestedSize lbl

-- Stolen from: cardano-sl/binary/src/Pos/Binary/Class/Core.hs

-- | Compare two sizes, failing if they are not equal.
matchSize :: Int -> Text -> Int -> D.Decoder s ()
matchSize requestedSize lbl actualSize =
  when (actualSize /= requestedSize) $
    fail $
      formatToString
        build
        ( lbl
            <> " failed the size check. Expected "
            <> textShow requestedSize
            <> ", found "
            <> textShow actualSize
        )

-- | Encoder for a Byron/Classic signing key.
--   Lifted from cardano-sl legacy codebase.
encodeLegacyDelegateKey :: LegacyDelegateKey -> E.Encoding
encodeLegacyDelegateKey (LegacyDelegateKey (SigningKey sk)) =
  E.encodeListLen 4
    <> E.encodeListLen 1
    <> E.encodeBytes "vss deprecated"
    <> E.encodeListLen 1
    <> encodeXPrv sk
    <> E.encodeListLenIndef
    <> E.encodeBreak
    <> E.encodeListLen 0

-- | Decoder for a Byron/Classic signing key.
--   Lifted from cardano-sl legacy codebase.
decodeLegacyDelegateKey :: D.Decoder s LegacyDelegateKey
decodeLegacyDelegateKey = do
  enforceSize "UserSecret" 4
  _ <- do
    enforceSize "vss" 1
    D.decodeBytes
  pkey <- do
    enforceSize "pkey" 1
    SigningKey <$> decodeXPrv
  _ <- do
    D.decodeListLenIndef
    D.decodeSequenceLenIndef (flip (:)) [] reverse D.decodeNull
  _ <- do
    enforceSize "wallet" 0
  pure $ LegacyDelegateKey pkey
