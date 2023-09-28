{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Byron.Tx where

import Cardano.Api

import Cardano.CLI.Byron.Tx
import Cardano.Chain.UTxO (ATxAux)

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.ByteString (ByteString)
import Data.Text qualified as Text

import Test.Cardano.CLI.Util

import Hedgehog (Property, (===))
import Hedgehog qualified as H
import Hedgehog.Extras.Test.Base qualified as H
import Hedgehog.Internal.Property (failWith)

{- HLINT ignore "Use camelCase" -}

hprop_golden_byronTx_legacy :: Property
hprop_golden_byronTx_legacy = propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
  signingKey <- noteInputFile "test/cardano-cli-golden/files/golden/byron/keys/legacy.skey"
  goldenTx <- noteInputFile "test/cardano-cli-golden/files/golden/byron/tx/legacy.tx"
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

  compareByronTxs createdTx goldenTx

hprop_golden_byronTx :: Property
hprop_golden_byronTx = propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
  signingKey <- noteInputFile "test/cardano-cli-golden/files/golden/byron/keys/byron.skey"
  goldenTx <- noteInputFile "test/cardano-cli-golden/files/golden/byron/tx/normal.tx"
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

  compareByronTxs createdTx goldenTx

getTxByteString :: FilePath -> H.PropertyT IO (ATxAux ByteString)
getTxByteString txFp = do
  eATxAuxBS <- liftIO . runExceptT $ readByronTx $ File txFp
  case eATxAuxBS of
    Left err -> failWith Nothing . Text.unpack $ renderByronTxError err
    Right aTxAuxBS -> return aTxAuxBS

compareByronTxs :: FilePath -> FilePath -> H.PropertyT IO ()
compareByronTxs createdTx goldenTx = do
  createdATxAuxBS <- getTxByteString createdTx
  goldenATxAuxBS <- getTxByteString goldenTx

  normalByronTxToGenTx goldenATxAuxBS === normalByronTxToGenTx createdATxAuxBS
