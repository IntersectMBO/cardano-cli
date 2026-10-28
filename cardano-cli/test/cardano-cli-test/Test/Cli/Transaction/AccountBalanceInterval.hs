{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- | CIP-159 account balance interval tests.
module Test.Cli.Transaction.AccountBalanceInterval where

import Cardano.Api
import Cardano.Api.Experimental qualified as Exp
import Cardano.Api.Ledger qualified as L

import Cardano.Ledger.BaseTypes (Exclusive (..), Inclusive (..))
import Cardano.Ledger.Coin qualified as L
import Cardano.Ledger.Core qualified as L
import Cardano.Ledger.Dijkstra.Scripts qualified as L
import Cardano.Ledger.Dijkstra.TxBody qualified as L

import Control.Monad (forM_, void)
import Control.Monad.Trans.Resource (ResourceT)
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
import Test.Cli.Transaction.DirectDeposit (account, accountAddress, otherAccount)
import Test.Cli.Transaction.SubTransaction
  ( buildRawSubTxArgs
  , buildSubTransaction
  , readDijkstraTransaction
  , signSubTransaction
  , signingKey
  , transactionArgs
  , txIn
  )

-- Exercise the CLI, ledger encoding, signing and embedding together: accepting
-- a flag is insufficient if a builder later discards the corresponding field.
hprop_account_balance_intervals_roundtrip :: Property
hprop_account_balance_intervals_roundtrip =
  watchdogProp . propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
    outFile <- H.noteTempFile tempDir "tx.body"
    subFile <- H.noteTempFile tempDir "sub.body"
    signedFile <- H.noteTempFile tempDir "sub.signed"
    address <- accountAddress
    otherAddress <-
      toShelleyStakeAddr <$> H.evalMaybe (deserialiseAddress AsStakeAddress $ Text.pack otherAccount)
    let amount = 100
        otherAmount = 200
        cases =
          [ exact 0
          , exact maxAmount
          , exact amount
          , (">=" ++ show amount, L.AccountBalanceLowerBound (Inclusive $ L.Coin amount))
          , ("<" ++ show otherAmount, L.AccountBalanceUpperBound (Exclusive $ L.Coin otherAmount))
          , between amount otherAmount
          , between 0 1
          , between (maxAmount - 1) maxAmount
          , between 0 maxAmount
          ]
    forM_ cases $ \(expression, interval) -> do
      H.noteShow_ expression
      let args = intervalArgs "--account-balance-interval" [(account, expression), (otherAccount, "=42")]
          startingArgs =
            intervalArgs "--starting-account-balance-interval" [(account, "=300"), (otherAccount, expression)]
          expected =
            L.AccountBalanceIntervals $
              Map.fromList [(address, interval), (otherAddress, L.AccountBalanceExact 42)]
          expectedStarting =
            L.AccountBalanceIntervals $
              Map.fromList [(address, L.AccountBalanceExact 300), (otherAddress, interval)]
      Exp.UnsignedSubTx sub <- buildSubTransaction subFile args
      sub ^. L.bodyTxL . L.accountBalanceIntervalsTxBodyL === expected
      signSubTransaction subFile signingKey signedFile
      void $
        execCardanoCLI $
          transactionArgs "dijkstra"
            ++ args
            ++ startingArgs
            ++ ["--signed-sub-tx-file", signedFile, "--out-file", outFile]
      ShelleyTx _ tx <- readDijkstraTransaction outFile
      let body = tx ^. L.bodyTxL
      body ^. L.accountBalanceIntervalsTxBodyL === expected
      body ^. L.startingAccountBalanceIntervalsTxBodyL === expectedStarting
      map (^. L.bodyTxL . L.accountBalanceIntervalsTxBodyL) (toList $ body ^. L.subTransactionsTxBodyL)
        === [expected]
 where
  maxAmount = 18446744073709551615
  exact amount = ("=" ++ show amount, L.AccountBalanceExact $ L.Coin amount)
  between lower upper =
    ( ">=" ++ show lower ++ ",<" ++ show upper
    , L.AccountBalanceBothBounds (Inclusive $ L.Coin lower) (Exclusive $ L.Coin upper)
    )

hprop_account_balance_intervals_reject_invalid_arguments :: Property
hprop_account_balance_intervals_reject_invalid_arguments =
  -- Allow time for the many CLI invocations under QEMU and Wine on Hydra.
  propertyOnce . H.runWithWatchdog_ H.WatchdogConfig{H.watchdogTimeout = 90} $ H.moduleWorkspace "tmp" $ \tempDir -> do
    outFile <- H.noteTempFile tempDir "unused"
    address <- accountAddress
    let hexAddress = Text.unpack $ serialiseToRawBytesHexText (fromShelleyStakeAddr address)
        invalid =
          map
            (account ++)
            [ ""
            , "="
            , "=-1"
            , "=18446744073709551616"
            , ">=18446744073709551616"
            , "<18446744073709551616"
            , "=1garbage"
            , "==1"
            , ">1"
            , "<=2"
            , ">=1,<=2"
            , "<2,>=1"
            , ">=1,<"
            , ">=1,<2,=1"
            , " =1"
            , ">= 1"
            , ">=1, <2"
            , "=1 "
            , "+100+null"
            , ">=null,<null"
            ]
            ++ [ "addr_test1vp0t4dfa9ktc2uvv7sg9leafuhtwyu0xcj4q4kf5pqkpjwqhklklg=1"
               , "not_an_address=1"
               , hexAddress ++ "=1"
               ]
        commands =
          [ (transactionArgs "dijkstra", "--account-balance-interval")
          , (transactionArgs "dijkstra", "--starting-account-balance-interval")
          , (buildRawSubTxArgs, "--account-balance-interval")
          ]
    forM_ commands $ \(base, flag) -> do
      forM_ invalid $ \argument ->
        void $ assertRejected outFile $ base ++ [flag, argument]
      forM_
        [ ">=0,<0"
        , ">=100,<100"
        , ">=200,<100"
        , ">=18446744073709551615,<0"
        , ">=18446744073709551615,<18446744073709551615"
        ]
        $ \expression -> do
          stderr <- assertRejected outFile $ base ++ intervalArgs flag [(account, expression)]
          H.assertWith stderr ("The lower bound must be strictly less than the upper bound" `isInfixOf`)
      stderr <- assertRejected outFile $ base ++ intervalArgs flag [(account, ">=1"), (account, "<2")]
      H.assertWith stderr (("Duplicate accounts in " ++ flag ++ ": " ++ account) `isInfixOf`)

hprop_account_balance_intervals_gating :: Property
hprop_account_balance_intervals_gating =
  watchdogProp . propertyOnce $ do
    forM_ ["build", "build-raw", "build-estimate"] $ \command -> do
      help <- execCardanoCLI ["dijkstra", "transaction", command, "--help"]
      forM_ ["--account-balance-interval", "--starting-account-balance-interval"] $ \flag -> do
        H.assertWith help (flag `isInfixOf`)
        forM_ ["conway", "latest"] $ \era ->
          assertInvalidOption flag [era, "transaction", command]
    assertInvalidOption "--starting-account-balance-interval" buildRawSubTxArgs

hprop_account_balance_intervals_validate_before_node_query :: Property
hprop_account_balance_intervals_validate_before_node_query =
  watchdogProp . propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
    outFile <- H.noteTempFile tempDir "unused"
    socketPath <- H.noteTempFile tempDir "missing.socket"
    forM_ ["--account-balance-interval", "--starting-account-balance-interval"] $ \flag -> do
      stderr <-
        assertRejected outFile $
          concat
            [ ["dijkstra", "transaction", "build"]
            , ["--testnet-magic", "42", "--socket-path", socketPath]
            , ["--tx-in", txIn]
            , ["--change-address", "addr_test1vp0t4dfa9ktc2uvv7sg9leafuhtwyu0xcj4q4kf5pqkpjwqhklklg"]
            , intervalArgs flag [(account, "=1"), (account, "=2")]
            ]
      H.assertWith stderr (("Duplicate accounts in " ++ flag ++ ": " ++ account) `isInfixOf`)

hprop_account_balance_intervals_build_estimate :: Property
hprop_account_balance_intervals_build_estimate =
  watchdogProp . propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
    paramsFile <- H.noteTempFile tempDir "params.json"
    outFile <- H.noteTempFile tempDir "tx.body"
    let inputValue = Coin 100000000
        params =
          (L.PParams L.emptyPParamsIdentity :: L.PParams (Exp.LedgerEra Exp.DijkstraEra))
            & L.ppMaxTxSizeL .~ 16384
            & L.ppTxFeePerByteL .~ L.CoinPerByte (L.CompactCoin 44)
            & L.ppTxFeeFixedL .~ 155381
    H.evalIO $ Aeson.encodeFile paramsFile params
    void $
      execCardanoCLI $
        concat
          [ ["dijkstra", "transaction", "build-estimate"]
          , ["--tx-in", txIn, "--total-utxo-value", show (L.unCoin inputValue)]
          , ["--shelley-key-witnesses", "1", "--protocol-params-file", paramsFile]
          , ["--change-address", "addr_test1vp0t4dfa9ktc2uvv7sg9leafuhtwyu0xcj4q4kf5pqkpjwqhklklg"]
          , intervalArgs "--account-balance-interval" [(account, ">=100,<200")]
          , intervalArgs "--starting-account-balance-interval" [(account, "=300")]
          , ["--out-file", outFile]
          ]
    ShelleyTx _ tx <- readDijkstraTransaction outFile
    address <- accountAddress
    let body = tx ^. L.bodyTxL
        changeAmount = sum [output ^. L.coinTxOutL | output <- toList $ body ^. L.outputsTxBodyL]
    body ^. L.accountBalanceIntervalsTxBodyL
      === L.AccountBalanceIntervals
        (Map.singleton address (L.AccountBalanceBothBounds (Inclusive 100) (Exclusive 200)))
    body ^. L.startingAccountBalanceIntervalsTxBodyL
      === L.AccountBalanceIntervals (Map.singleton address (L.AccountBalanceExact 300))
    changeAmount === inputValue - body ^. L.feeTxBodyL

intervalArgs :: String -> [(String, String)] -> [String]
intervalArgs flag entries = concat [[flag, address ++ expression] | (address, expression) <- entries]

-- Failed builds must not leave an output transaction behind.
assertRejected :: FilePath -> [String] -> H.PropertyT (ResourceT IO) String
assertRejected outFile args = do
  (exitCode, _, stderr) <- execDetailCardanoCLI $ args ++ ["--out-file", outFile]
  exitCode === ExitFailure 1
  H.evalIO (doesFileExist outFile) >>= (=== False)
  pure stderr

assertInvalidOption :: String -> [String] -> H.PropertyT (ResourceT IO) ()
assertInvalidOption flag args = do
  (exitCode, _, stderr) <- execDetailCardanoCLI $ args ++ [flag, account ++ "=1"]
  exitCode === ExitFailure 1
  H.assertWith stderr (("Invalid option `" ++ flag ++ "'") `isInfixOf`)
