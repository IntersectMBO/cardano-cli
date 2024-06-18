{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{- HLINT ignore "Use camelCase" -}

module Test.Golden.Byron.Tx where

import           Cardano.Api hiding (Error)

import           Cardano.Chain.UTxO (ATxAux)
import           Cardano.CLI.Byron.Tx

import           Test.Cardano.CLI.Polysemy

import           HaskellWorks.Polysemy
import           HaskellWorks.Polysemy.Hedgehog
import           HaskellWorks.Prelude

hprop_byronTx_legacy :: Property
hprop_byronTx_legacy = propertyOnce $ localWorkspace $ do
  signingKey <- jotPkgInputFile "test/cardano-cli-golden/files/input/byron/keys/legacy.skey"
  expectedTx <- jotPkgInputFile "test/cardano-cli-golden/files/input/byron/tx/legacy.tx"
  createdTx <- jotTempFile "tx"

  execCardanoCli_
    [ "byron", "transaction", "issue-utxo-expenditure"
    , "--mainnet"
    , "--byron-legacy-formats"
    , "--wallet-key", signingKey
    , "--tx", createdTx
    , "--txin", "(796a90e0a89b292d53a6129b9f0d757429063b529d27e4f56565192a8c8da5e3,10)"
    , "--txout", "(\"2657WMsDfac6eFirdvKVPVMxNVYuACd1RGM2arH3g1y1yaQCr1yYpb2jr2b2aSiDZ\",999)"
    ]

  void $ compareByronTxs createdTx expectedTx

hprop_byronTx :: Property
hprop_byronTx = propertyOnce $ localWorkspace $ do
  signingKey <- jotPkgInputFile "test/cardano-cli-golden/files/input/byron/keys/byron.skey"
  expectedTx <- jotPkgInputFile "test/cardano-cli-golden/files/input/byron/tx/normal.tx"
  createdTx <- jotTempFile "tx"

  execCardanoCli_
    [ "byron", "transaction", "issue-utxo-expenditure"
    , "--mainnet"
    , "--byron-formats"
    , "--wallet-key", signingKey
    , "--tx", createdTx
    , "--txin", "(796a90e0a89b292d53a6129b9f0d757429063b529d27e4f56565192a8c8da5e3,10)"
    , "--txout", "(\"2657WMsDfac6eFirdvKVPVMxNVYuACd1RGM2arH3g1y1yaQCr1yYpb2jr2b2aSiDZ\",999)"
    ]

  compareByronTxs createdTx expectedTx

getTxByteString :: ()
  => Member (Embed IO) r
  => Member (Error ByronTxError) r
  => FilePath
  -> Sem r (ATxAux ByteString)
getTxByteString txFp = do
  embed (runExceptT $ readByronTx $ File txFp)
    & onLeftM throw

compareByronTxs :: ()
  => HasCallStack
  => Member (Embed IO) r
  => Member Hedgehog r
  => FilePath
  -> FilePath
  -> Sem r ()
compareByronTxs createdTx expectedTx = do
  createdATxAuxBS <- getTxByteString createdTx & trapFail @ByronTxError
  expectedATxAuxBS <- getTxByteString expectedTx & trapFail @ByronTxError

  normalByronTxToGenTx expectedATxAuxBS === normalByronTxToGenTx createdATxAuxBS
