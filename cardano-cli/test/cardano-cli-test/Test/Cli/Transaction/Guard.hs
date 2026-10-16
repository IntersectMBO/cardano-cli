{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Cli.Transaction.Guard where

import Cardano.Api
import Cardano.Api.Experimental qualified as Exp
import Cardano.Api.Ledger qualified as L

import Cardano.Ledger.Coin qualified as L
import Cardano.Ledger.Core qualified as L
import Cardano.Ledger.Dijkstra.TxBody qualified as L

import Control.Monad (forM_, void)
import Data.Aeson qualified as Aeson
import Data.Foldable (toList)
import Data.List (isInfixOf)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Lens.Micro ((&), (.~), (^.))
import System.Exit (ExitCode (..))

import Test.Cardano.CLI.Util

import Hedgehog (Property, (===))
import Hedgehog qualified as H
import Hedgehog.Extras qualified as H
import Test.Cli.Transaction.SubTransaction
  ( nativeScriptFile
  , plutusScriptFile
  , readDijkstraTransaction
  , transactionArgs
  , txIn
  , txOut
  , writeDijkstraProtocolParams
  )

-- Native guards need no protocol parameters or Plutus witness data.
hprop_top_level_native_guard :: Property
hprop_top_level_native_guard = watchdogProp . propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
  outFile <- H.noteTempFile tempDir "tx.body"
  void $
    execCardanoCLI $
      transactionArgs "dijkstra"
        ++ ["--guard-script-file", nativeScriptFile, "--out-file", outFile]
  ShelleyTx _ tx <- readDijkstraTransaction outFile
  toList (tx ^. L.bodyTxL . L.guardsTxBodyL)
    === map L.ScriptHashObj (Map.keys $ tx ^. L.witsTxL . L.scriptTxWitsL)
  Map.size (tx ^. L.witsTxL . L.scriptTxWitsL) === 1
  L.unRedeemers (tx ^. L.witsTxL . L.rdmrsTxWitsL) === mempty

-- Index native guards too, keep spending/guarding redeemers separate, and
-- commit guard data to the script integrity hash before signing.
hprop_top_level_guard_redeemers :: Property
hprop_top_level_guard_redeemers = watchdogProp . propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
  params <- H.noteTempFile tempDir "params.json"
  first <- H.noteTempFile tempDir "first.body"
  second <- H.noteTempFile tempDir "second.body"
  H.evalIO $ writeDijkstraProtocolParams params
  forM_ [(first, "42"), (second, "43")] $ \(outFile, value) ->
    void $
      execCardanoCLI $
        transactionArgs "dijkstra"
          ++ [ "--tx-in-script-file"
             , plutusScriptFile 4
             , "--tx-in-redeemer-value"
             , "0"
             , "--tx-in-execution-units"
             , "(1,2)"
             , "--required-signer-hash"
             , replicate 56 '0'
             , "--guard-script-file"
             , nativeScriptFile
             , "--guard-script-file"
             , plutusScriptFile 4
             , "--guard-redeemer-value"
             , value
             , "--guard-execution-units"
             , "(3,4)"
             , "--protocol-params-file"
             , params
             , "--out-file"
             , outFile
             ]
  ShelleyTx _ tx <- readDijkstraTransaction first
  ShelleyTx _ changed <- readDijkstraTransaction second
  let redeemers = L.unRedeemers $ tx ^. L.witsTxL . L.rdmrsTxWitsL
      datum = toAlonzoData . unsafeHashableScriptData . ScriptDataNumber
  Map.lookup (L.DijkstraSpending (L.AsIx 0)) redeemers
    === Just (datum 0, toAlonzoExUnits $ ExecutionUnits 1 2)
  Map.lookup (L.DijkstraGuarding (L.AsIx 1)) redeemers
    === Just (datum 42, toAlonzoExUnits $ ExecutionUnits 3 4)
  Map.size redeemers === 2
  length (tx ^. L.bodyTxL . L.guardsTxBodyL) === 3
  Map.size (tx ^. L.witsTxL . L.scriptTxWitsL) === 2
  H.assert $
    tx ^. L.bodyTxL . L.scriptIntegrityHashTxBodyL
      /= changed ^. L.bodyTxL . L.scriptIntegrityHashTxBodyL
  view <- execCardanoCLI ["debug", "transaction", "view", "--tx-file", first]
  H.assertWith view ("guarding script" `isInfixOf`)

-- Reference guards add their UTxO to reference inputs, with no inline script.
hprop_top_level_reference_guards :: Property
hprop_top_level_reference_guards = watchdogProp . propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
  params <- H.noteTempFile tempDir "params.json"
  outFile <- H.noteTempFile tempDir "tx.body"
  H.evalIO $ writeDijkstraProtocolParams params
  let reference = init txIn ++ "1"
  input <-
    H.evalEither $
      deserialiseFromRawBytesHex "63e6a9a8e58e48cc025cae04daaed9d36fc7b70bc292721d9f5057ae37b24981"
  forM_ [[], ["--guard-redeemer-value", "42", "--guard-execution-units", "(3,4)"]] $ \redeemer -> do
    void $
      execCardanoCLI $
        transactionArgs "dijkstra"
          ++ ["--guard-tx-in-reference", reference, "--guard-script-hash", replicate 56 '0']
          ++ redeemer
          ++ ["--protocol-params-file", params, "--out-file", outFile]
    ShelleyTx _ tx <- readDijkstraTransaction outFile
    tx ^. L.bodyTxL . L.referenceInputsTxBodyL === Set.singleton (toShelleyTxIn $ TxIn input (TxIx 1))
    Map.size (tx ^. L.witsTxL . L.scriptTxWitsL) === 0
    length (tx ^. L.bodyTxL . L.guardsTxBodyL) === 1
    Map.size (L.unRedeemers $ tx ^. L.witsTxL . L.rdmrsTxWitsL) === if null redeemer then 0 else 1

hprop_top_level_guard_rejects_invalid_witnesses :: Property
hprop_top_level_guard_rejects_invalid_witnesses = watchdogProp . propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
  outFile <- H.noteTempFile tempDir "unused"
  let budget = ["--guard-redeemer-value", "0", "--guard-execution-units", "(1,2)"]
      cases =
        [ ["--guard-script-file", plutusScriptFile 4]
        , ["--guard-script-file", plutusScriptFile 4, "--guard-redeemer-value", "0"]
        , ["--guard-script-file", nativeScriptFile] ++ budget
        , ["--guard-script-file", nativeScriptFile, "--guard-script-file", nativeScriptFile]
        ]
          ++ [["--guard-script-file", plutusScriptFile v] ++ budget | v <- [1 .. 3]]
  forM_ cases $ \args -> do
    (code, _, _) <- execDetailCardanoCLI $ transactionArgs "dijkstra" ++ args ++ ["--out-file", outFile]
    code === ExitFailure 1

hprop_top_level_guard_options :: Property
hprop_top_level_guard_options = watchdogProp . propertyOnce $ do
  forM_ ["build", "build-raw", "build-estimate"] $ \command -> do
    help <- execCardanoCLI ["dijkstra", "transaction", command, "--help"]
    H.assertWith help ("--guard-script-file" `isInfixOf`)
    H.assertWith help ("--guard-tx-in-reference" `isInfixOf`)
    ("--guard-execution-units" `isInfixOf` help) === (command /= "build")
    forM_ ["conway", "latest"] $ \era -> do
      (code, _, stderr) <-
        execDetailCardanoCLI [era, "transaction", command, "--guard-script-file", nativeScriptFile]
      code === ExitFailure 1
      H.assertWith stderr ("Invalid option `--guard-script-file'" `isInfixOf`)

-- Online build validates witnesses before contacting the node, and accepts
-- a redeemer without an execution budget (which is calculated by the API).
hprop_top_level_guard_build_validation :: Property
hprop_top_level_guard_build_validation = watchdogProp . propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
  outFile <- H.noteTempFile tempDir "unused"
  socket <- H.noteTempFile tempDir "missing.socket"
  forM_ [(nativeScriptFile, "does not accept a redeemer"), (plutusScriptFile 3, "must be Plutus V4")] $ \(script, message) -> do
    (code, _, stderr) <-
      execDetailCardanoCLI
        [ "dijkstra"
        , "transaction"
        , "build"
        , "--testnet-magic"
        , "42"
        , "--socket-path"
        , socket
        , "--tx-in"
        , txIn
        , "--change-address"
        , takeWhile (/= '+') txOut
        , "--guard-script-file"
        , script
        , "--guard-redeemer-value"
        , "0"
        , "--out-file"
        , outFile
        ]
    code === ExitFailure 1
    H.assertWith stderr (message `isInfixOf`)

-- Offline balancing must preserve the guarding budget and charge for the
-- extra script/redeemer bytes, even without a script spending input.
hprop_top_level_guard_build_estimate :: Property
hprop_top_level_guard_build_estimate = watchdogProp . propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
  paramsFile <- H.noteTempFile tempDir "params.json"
  plainFile <- H.noteTempFile tempDir "plain.body"
  guardFile <- H.noteTempFile tempDir "guard.body"
  let params =
        (L.PParams L.emptyPParamsIdentity :: L.PParams (Exp.LedgerEra Exp.DijkstraEra))
          & L.ppMaxTxSizeL .~ 16384
          & L.ppTxFeePerByteL .~ L.CoinPerByte (L.CompactCoin 44)
          & L.ppTxFeeFixedL .~ 155381
  H.evalIO $ Aeson.encodeFile paramsFile params
  forM_
    [ (plainFile, [])
    ,
      ( guardFile
      ,
        [ "--guard-script-file"
        , plutusScriptFile 4
        , "--guard-redeemer-value"
        , "42"
        , "--guard-execution-units"
        , "(3,4)"
        ]
      )
    ]
    $ \(outFile, guards) ->
      void $
        execCardanoCLI $
          [ "dijkstra"
          , "transaction"
          , "build-estimate"
          , "--tx-in"
          , txIn
          , "--total-utxo-value"
          , "100000000"
          , "--shelley-key-witnesses"
          , "1"
          , "--protocol-params-file"
          , paramsFile
          , "--change-address"
          , takeWhile (/= '+') txOut
          , "--out-file"
          , outFile
          ]
            ++ guards
  ShelleyTx _ plain <- readDijkstraTransaction plainFile
  ShelleyTx _ guarded <- readDijkstraTransaction guardFile
  H.assert $ guarded ^. L.bodyTxL . L.feeTxBodyL > plain ^. L.bodyTxL . L.feeTxBodyL
  let datum = toAlonzoData $ unsafeHashableScriptData $ ScriptDataNumber 42
  Map.lookup (L.DijkstraGuarding (L.AsIx 0)) (L.unRedeemers $ guarded ^. L.witsTxL . L.rdmrsTxWitsL)
    === Just (datum, toAlonzoExUnits $ ExecutionUnits 3 4)
