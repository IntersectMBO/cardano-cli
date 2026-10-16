{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Cli.Transaction.DirectDeposit where

import Cardano.Api
import Cardano.Api.Experimental qualified as Exp
import Cardano.Api.Ledger qualified as L

import Cardano.Ledger.Address qualified as L
import Cardano.Ledger.Coin qualified as L
import Cardano.Ledger.Core qualified as L
import Cardano.Ledger.Dijkstra.TxBody qualified as L

import Control.Monad (forM_, void)
import Data.Aeson qualified as Aeson
import Data.Foldable (toList)
import Data.List (isInfixOf)
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Lens.Micro ((&), (.~), (^.))
import System.Directory (doesFileExist)
import System.Exit (ExitCode (..))

import Test.Cardano.CLI.Util

import Hedgehog (Property, (===))
import Hedgehog qualified as H
import Hedgehog.Extras qualified as H
import Test.Cli.Transaction.SubTransaction
  ( buildSubTransaction
  , readDijkstraTransaction
  , signSubTransaction
  , signingKey
  , subTransactionArgs
  , transactionArgs
  , txIn
  )

account :: String
account = "stake_test17qvxuvh64q9zdqgrjt76d42eclk5wgdxtnsun4808cwg0dqxv5r99"

otherAccount :: String
otherAccount = "stake_test1uz855drx5szvz845zqcnq9dc3ez8mqdkqzy7yhmpzcqwvpg59zzfl"

accountAddress :: H.MonadTest m => m L.AccountAddress
accountAddress = toShelleyStakeAddr <$> H.evalMaybe (deserialiseAddress AsStakeAddress $ Text.pack account)

-- Decode both the top-level and signed nested transaction to catch deposits
-- being accepted by the parser but lost during construction or embedding.
hprop_cip159_direct_deposits_roundtrip :: Property
hprop_cip159_direct_deposits_roundtrip =
  watchdogProp . propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
    outFile <- H.noteTempFile tempDir "tx.body"
    subFile <- H.noteTempFile tempDir "sub.body"
    signedFile <- H.noteTempFile tempDir "sub.signed"
    let cases =
          [[(account, amount)] | amount <- [0, 2000000, 18446744073709551615]]
            ++ [[(account, 2000000), (otherAccount, 3000000)]]
    forM_ cases $ \deposits -> do
      recipients <- mapM (H.evalMaybe . deserialiseAddress AsStakeAddress . Text.pack . fst) deposits
      let args =
            concat
              [ ["--direct-deposit", recipient ++ "+" ++ show (amount :: Integer)] | (recipient, amount) <- deposits
              ]
          expected =
            L.DirectDeposits $
              Map.fromList $
                zip (map toShelleyStakeAddr recipients) (map (Coin . snd) deposits)
      Exp.UnsignedSubTx sub <- buildSubTransaction subFile args
      sub ^. L.bodyTxL . L.directDepositsTxBodyL === expected
      signSubTransaction subFile signingKey signedFile
      void $
        execCardanoCLI $
          transactionArgs "dijkstra"
            ++ args
            ++ ["--signed-sub-tx-file", signedFile, "--out-file", outFile]
      ShelleyTx _ tx <- readDijkstraTransaction outFile
      let body = tx ^. L.bodyTxL
      body ^. L.directDepositsTxBodyL === expected
      let nested = toList $ body ^. L.subTransactionsTxBodyL
      length nested === 1
      forM_ nested $ \child ->
        child ^. L.bodyTxL . L.directDepositsTxBodyL === expected

hprop_cip159_rejects_invalid_direct_deposits :: Property
hprop_cip159_rejects_invalid_direct_deposits =
  watchdogProp . propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
    outFile <- H.noteTempFile tempDir "unused"
    let invalid =
          [ ["--direct-deposit", account ++ "+" ++ value]
          | value <- ["-1", "18446744073709551616", "1garbage", "1+1 token", ""]
          ]
            ++ [["--direct-deposit", "addr_test1vp0t4dfa9ktc2uvv7sg9leafuhtwyu0xcj4q4kf5pqkpjwqhklklg+1"]]
    forM_ [transactionArgs "dijkstra", subTransactionArgs] $ \base -> do
      forM_ invalid $ \args -> do
        H.noteShow_ args
        (exitCode, _, _) <- execDetailCardanoCLI $ base ++ args ++ ["--out-file", outFile]
        exitCode === ExitFailure 1
        H.evalIO (doesFileExist outFile) >>= (=== False)
      (exitCode, _, stderr) <-
        execDetailCardanoCLI $
          base
            ++ ["--direct-deposit", account ++ "+1", "--direct-deposit", account ++ "+2", "--out-file", outFile]
      exitCode === ExitFailure 1
      H.assertWith stderr ("Duplicate direct-deposit accounts" `isInfixOf`)
      H.evalIO (doesFileExist outFile) >>= (=== False)

-- | Invalid arguments must be reported even when the node is unavailable.
hprop_cip159_build_rejects_duplicate_deposits_before_node_query :: Property
hprop_cip159_build_rejects_duplicate_deposits_before_node_query =
  watchdogProp . propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
    outFile <- H.noteTempFile tempDir "unused"
    socketPath <- H.noteTempFile tempDir "missing.socket"
    (exitCode, _, stderr) <-
      execDetailCardanoCLI
        [ "dijkstra"
        , "transaction"
        , "build"
        , "--testnet-magic"
        , "42"
        , "--socket-path"
        , socketPath
        , "--tx-in"
        , txIn
        , "--change-address"
        , "addr_test1vp0t4dfa9ktc2uvv7sg9leafuhtwyu0xcj4q4kf5pqkpjwqhklklg"
        , "--direct-deposit"
        , account ++ "+1"
        , "--direct-deposit"
        , account ++ "+2"
        , "--out-file"
        , outFile
        ]
    exitCode === ExitFailure 1
    H.assertWith stderr (("Duplicate direct-deposit accounts: " ++ account) `isInfixOf`)
    H.evalIO (doesFileExist outFile) >>= (=== False)

hprop_cip159_direct_deposit_era_gating :: Property
hprop_cip159_direct_deposit_era_gating =
  watchdogProp . propertyOnce $ do
    forM_ ["build", "build-raw", "build-estimate"] $ \command -> do
      help <- execCardanoCLI ["dijkstra", "transaction", command, "--help"]
      H.assertWith help ("--direct-deposit" `isInfixOf`)
      forM_ ["conway", "latest"] $ \era -> do
        (exitCode, _, stderr) <-
          execDetailCardanoCLI
            [era, "transaction", command, "--direct-deposit", account ++ "+1", "--out-file", "unused"]
        exitCode === ExitFailure 1
        H.assertWith stderr ("Invalid option `--direct-deposit'" `isInfixOf`)

-- | With 100 ADA of inputs and direct deposits of 20 and 30 ADA, build-estimate
-- must return 50 ADA minus the estimated fee as change. This catches deposits being
-- recorded in the body but omitted from the change calculation.
hprop_cip159_build_estimate_deducts_deposits_from_change :: Property
hprop_cip159_build_estimate_deducts_deposits_from_change =
  watchdogProp . propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
    paramsFile <- H.noteTempFile tempDir "params.json"
    outFile <- H.noteTempFile tempDir "tx.body"
    let inputValue = Coin 100000000 -- 100 ADA, expressed in lovelace.
        depositAmount = Coin 20000000 -- 20 ADA, expressed in lovelace.
        otherDepositAmount = Coin 30000000 -- 30 ADA to a different recipient.
        params =
          (L.PParams L.emptyPParamsIdentity :: L.PParams (Exp.LedgerEra Exp.DijkstraEra))
            & L.ppMaxTxSizeL .~ 16384
            & L.ppTxFeePerByteL .~ L.CoinPerByte (L.CompactCoin 44)
            & L.ppTxFeeFixedL .~ 155381
    H.evalIO $ Aeson.encodeFile paramsFile params
    -- build-estimate works offline: supply the input value explicitly. There
    -- are no --tx-out arguments, so every generated output is change.
    H.note_ "The 100 ADA input must fund deposits of 20 and 30 ADA, the estimated fee, and change."
    void $
      execCardanoCLI
        [ "dijkstra"
        , "transaction"
        , "build-estimate"
        , "--tx-in"
        , txIn
        , "--total-utxo-value"
        , show (L.unCoin inputValue)
        , "--shelley-key-witnesses"
        , "1"
        , "--protocol-params-file"
        , paramsFile
        , "--change-address"
        , "addr_test1vp0t4dfa9ktc2uvv7sg9leafuhtwyu0xcj4q4kf5pqkpjwqhklklg"
        , "--direct-deposit"
        , account ++ "+" ++ show (L.unCoin depositAmount)
        , "--direct-deposit"
        , otherAccount ++ "+" ++ show (L.unCoin otherDepositAmount)
        , "--out-file"
        , outFile
        ]
    ShelleyTx _ tx <- readDijkstraTransaction outFile
    address <- accountAddress
    otherAddress <-
      toShelleyStakeAddr <$> H.evalMaybe (deserialiseAddress AsStakeAddress $ Text.pack otherAccount)
    let body = tx ^. L.bodyTxL
        estimatedFee = body ^. L.feeTxBodyL
        changeAmount = sum [output ^. L.coinTxOutL | output <- toList $ body ^. L.outputsTxBodyL]
    -- Check that the requested deposit was included in the serialized body.
    body ^. L.directDepositsTxBodyL
      === L.DirectDeposits (Map.fromList [(address, depositAmount), (otherAddress, otherDepositAmount)])

    -- Use the fee calculated by build-estimate, since it depends on transaction
    -- size. The remaining change must be 100 ADA - 20 ADA - 30 ADA - that fee.
    H.note_ $ "Estimated fee (lovelace): " ++ show (L.unCoin estimatedFee)
    H.note_ $ "Returned change (lovelace): " ++ show (L.unCoin changeAmount)
    changeAmount === inputValue - depositAmount - otherDepositAmount - estimatedFee
