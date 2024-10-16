{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Byron.Tx where

import           Cardano.Api
import           Cardano.Api.Byron (ATxAux)

import           Cardano.CLI.Byron.Tx

import           Control.Monad (void)
import           Data.ByteString (ByteString)

import           Test.Cardano.CLI.Util

import           Hedgehog (Property, (===))
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Test.Base as H
import           Hedgehog.Internal.Property (failWith)

{- HLINT ignore "Use camelCase" -}

hprop_byronTx_legacy :: Property
hprop_byronTx_legacy = propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
  signingKey <- noteInputFile "test/cardano-cli-golden/files/input/byron/keys/legacy.skey"
  expectedTx <- noteInputFile "test/cardano-cli-golden/files/input/byron/tx/legacy.tx"
  createdTx <- noteTempFile tempDir "tx"
  void $
    execCardanoCLI
      [ "byron"
      , "transaction"
      , "issue-utxo-expenditure"
      , "--mainnet"
      , "--byron-legacy-formats"
      , "--wallet-key"
      , signingKey
      , "--tx"
      , createdTx
      , "--txin"
      , "(796a90e0a89b292d53a6129b9f0d757429063b529d27e4f56565192a8c8da5e3,10)"
      , "--txout"
      , "(\"2657WMsDfac6eFirdvKVPVMxNVYuACd1RGM2arH3g1y1yaQCr1yYpb2jr2b2aSiDZ\",999)"
      ]

  compareByronTxs createdTx expectedTx

hprop_byronTx :: Property
hprop_byronTx = propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
  signingKey <- noteInputFile "test/cardano-cli-golden/files/input/byron/keys/byron.skey"
  expectedTx <- noteInputFile "test/cardano-cli-golden/files/input/byron/tx/normal.tx"
  createdTx <- noteTempFile tempDir "tx"
  void $
    execCardanoCLI
      [ "byron"
      , "transaction"
      , "issue-utxo-expenditure"
      , "--mainnet"
      , "--byron-formats"
      , "--wallet-key"
      , signingKey
      , "--tx"
      , createdTx
      , "--txin"
      , "(796a90e0a89b292d53a6129b9f0d757429063b529d27e4f56565192a8c8da5e3,10)"
      , "--txout"
      , "(\"2657WMsDfac6eFirdvKVPVMxNVYuACd1RGM2arH3g1y1yaQCr1yYpb2jr2b2aSiDZ\",999)"
      ]

  compareByronTxs createdTx expectedTx

getTxByteString :: FilePath -> H.PropertyT IO (ATxAux ByteString)
getTxByteString txFp = do
  eATxAuxBS <- liftIO . runExceptT $ readByronTx $ File txFp
  case eATxAuxBS of
    Left err -> failWith Nothing . docToString $ renderByronTxError err
    Right aTxAuxBS -> return aTxAuxBS

compareByronTxs :: FilePath -> FilePath -> H.PropertyT IO ()
compareByronTxs createdTx expectedTx = do
  createdATxAuxBS <- getTxByteString createdTx
  expectedATxAuxBS <- getTxByteString expectedTx

  normalByronTxToGenTx expectedATxAuxBS === normalByronTxToGenTx createdATxAuxBS
