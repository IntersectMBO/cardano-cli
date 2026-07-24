{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Golden.Query.UTxO where

import Cardano.Api
import Cardano.Api.Ledger qualified as L
import Cardano.Api.UTxO qualified as UTxO

import Cardano.CLI.EraBased.Query.Run (filteredUTxOsToText)

import Control.Monad (forM)
import Data.ByteString (ByteString)
import Data.Text qualified as Text
import GHC.Stack (HasCallStack)
import System.FilePath ((</>))

import Test.Cardano.CLI.Util (watchdogProp)

import Hedgehog (MonadTest, Property, evalEither)
import Hedgehog.Extras.Test (propertyOnce)
import Hedgehog.Extras.Test qualified as H

goldenDir :: FilePath
goldenDir = "test/cardano-cli-golden/files/golden"

-- Regression tests for the text rendering of @query utxo@, see
-- https://github.com/IntersectMBO/cardano-cli/issues/1398. In particular,
-- outputs of eras without datums must not end in a dangling @" + "@
-- separator.

-- | Execute me with:
-- @cabal test cardano-cli-golden --test-options '-p "/golden query utxo text shelley/"'@
hprop_golden_query_utxo_text_shelley :: Property
hprop_golden_query_utxo_text_shelley =
  watchdogProp . propertyOnce $ do
    utxo <-
      mkUtxo
        ShelleyBasedEraShelley
        [
          ( "8de6d2076c030674c828d4958f4a33d8368579afe8b077d75dfa324fdb6745a1"
          , 0
          , 36_000_000_000_000_000
          , TxOutDatumNone
          )
        ]
    H.diffVsGoldenFile
      (Text.unpack $ filteredUTxOsToText ShelleyBasedEraShelley utxo)
      (goldenDir </> "shelley/query-utxo-text.out")

-- | Execute me with:
-- @cabal test cardano-cli-golden --test-options '-p "/golden query utxo text alonzo/"'@
hprop_golden_query_utxo_text_alonzo :: Property
hprop_golden_query_utxo_text_alonzo =
  watchdogProp . propertyOnce $ do
    datumHash <- evalEither $ deserialiseFromRawBytesHex exampleDatumHashHex
    utxo <-
      mkUtxo
        ShelleyBasedEraAlonzo
        [
          ( "1111111111111111111111111111111111111111111111111111111111111111"
          , 0
          , 100_000_000
          , TxOutDatumNone
          )
        ,
          ( "2222222222222222222222222222222222222222222222222222222222222222"
          , 1
          , 2_000_000
          , TxOutDatumHash AlonzoEraOnwardsAlonzo datumHash
          )
        ]
    H.diffVsGoldenFile
      (Text.unpack $ filteredUTxOsToText ShelleyBasedEraAlonzo utxo)
      (goldenDir </> "alonzo/query-utxo-text.out")

-- | Execute me with:
-- @cabal test cardano-cli-golden --test-options '-p "/golden query utxo text conway/"'@
hprop_golden_query_utxo_text_conway :: Property
hprop_golden_query_utxo_text_conway =
  watchdogProp . propertyOnce $ do
    datumHash <- evalEither $ deserialiseFromRawBytesHex exampleDatumHashHex
    utxo <-
      mkUtxo
        ShelleyBasedEraConway
        [
          ( "1111111111111111111111111111111111111111111111111111111111111111"
          , 0
          , 100_000_000
          , TxOutDatumNone
          )
        ,
          ( "2222222222222222222222222222222222222222222222222222222222222222"
          , 1
          , 2_000_000
          , TxOutDatumHash AlonzoEraOnwardsConway datumHash
          )
        ,
          ( "3333333333333333333333333333333333333333333333333333333333333333"
          , 2
          , 5_000_000
          , TxOutDatumInline BabbageEraOnwardsConway (unsafeHashableScriptData (ScriptDataNumber 42))
          )
        ]
    H.diffVsGoldenFile
      (Text.unpack $ filteredUTxOsToText ShelleyBasedEraConway utxo)
      (goldenDir </> "conway/query-utxo-text.out")

exampleDatumHashHex :: ByteString
exampleDatumHashHex = "9e478573ab81ea7a8e31891ce0648b81229f408d596a3483e6f4f9b92d3cf710"

-- | Build a UTxO with one entry per given (tx id hex, tx index, lovelace,
-- datum) tuple, all paying to the same key address.
mkUtxo
  :: HasCallStack
  => MonadTest m
  => ShelleyBasedEra era
  -> [(ByteString, Word, L.Coin, TxOutDatum CtxUTxO era)]
  -> m (UTxO era)
mkUtxo sbe entries = do
  paymentKeyHash <-
    evalEither $
      deserialiseFromRawBytesHex @(Hash PaymentKey)
        "1234567890abcdef1234567890abcdef1234567890abcdef12345678"
  let address =
        makeShelleyAddressInEra
          sbe
          (Testnet (NetworkMagic 42))
          (PaymentCredentialByKey paymentKeyHash)
          NoStakeAddress
  fmap UTxO.fromList . forM entries $ \(txIdHex, index, amount, datum) -> do
    txId' <- evalEither $ deserialiseFromRawBytesHex txIdHex
    pure
      ( TxIn txId' (TxIx index)
      , TxOut address (lovelaceToTxOutValue sbe amount) datum ReferenceScriptNone
      )
