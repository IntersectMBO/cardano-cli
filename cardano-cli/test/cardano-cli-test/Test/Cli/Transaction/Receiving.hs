{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cli.Transaction.Receiving where

import Cardano.Api
import Cardano.Api.Experimental qualified as Exp
import Cardano.Api.Genesis qualified as Genesis
import Cardano.Api.Ledger qualified as L

import Cardano.CLI.Read qualified as Read
import Cardano.Ledger.Core qualified as Ledger
import Cardano.Ledger.Dijkstra.Genesis (dgUpgradePParams)
import Cardano.Ledger.State (verifyWitVKey)

import Control.Monad (forM_, void)
import Control.Monad.Trans.Resource (ResourceT)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.Function ((&))
import Data.List (isInfixOf)
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Lens.Micro ((.~), (^.), (^?))
import Lens.Micro.Aeson qualified as Aeson
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import Text.Read (readMaybe)

import Test.Cardano.CLI.Util

import Hedgehog (Property, PropertyT, (===))
import Hedgehog qualified as H
import Hedgehog.Extras qualified as H

type CliTest = PropertyT (ResourceT IO)

execSingleValue :: [String] -> CliTest String
execSingleValue args = Text.unpack . Text.strip . Text.pack <$> execCardanoCLI args

nativeHash, lowNativeHash, plutusHash, input, referenceInput :: String
nativeHash = "d441227553a0f1a965fee7d60a0f724b368dd1bddbc208730fccebcf"
lowNativeHash = "3530cc9ae7f2895111a99b7a02184dd7c0cea7424f1632d73951b1d7"
plutusHash = "28d467557081773a051b5f83982574abfccceb079bc8012f192505e2"
input = replicate 64 '1' <> "#0"
referenceInput = replicate 64 '2' <> "#0"

protectedScriptAddress :: String -> CliTest String
protectedScriptAddress hash = do
  scriptHash <- H.evalEither (deserialiseFromRawBytesHex (Text.encodeUtf8 (Text.pack hash)))
  addr <-
    H.evalEither $
      protectShelleyAddress $
        makeShelleyAddress (Testnet (NetworkMagic 42)) (PaymentCredentialByScript scriptHash) NoStakeAddress
  pure (Text.unpack (serialiseAddress addr))

writeDijkstraParams :: FilePath -> CliTest FilePath
writeDijkstraParams dir = do
  conwayParams :: L.PParams (Exp.LedgerEra Exp.ConwayEra) <-
    H.readJsonFileOk
      "test/cardano-cli-test/files/input/calculate-min-fee/offline-protocol-params-preview.json"
  let pp =
        Ledger.upgradePParams (dgUpgradePParams Genesis.dijkstraGenesisDefaults) conwayParams
          :: L.PParams (Exp.LedgerEra Exp.DijkstraEra)
      dijkstraParams =
        pp
          & Ledger.ppProtocolVersionL
            .~ ((Ledger.emptyPParams :: L.PParams (Exp.LedgerEra Exp.DijkstraEra)) ^. Ledger.ppProtocolVersionL)
      path = dir </> "dijkstra-params.json"
  liftIO $ LBS.writeFile path (Aeson.encode dijkstraParams)
  pure path

nativeFile :: FilePath -> CliTest FilePath
nativeFile dir = do
  let path = dir </> "native.json"
  liftIO $ writeFile path "{\"type\":\"all\",\"scripts\":[]}"
  pure path

hprop_script_address_build_supports_v4_and_preserves_native_hash :: Property
hprop_script_address_build_supports_v4_and_preserves_native_hash = watchdogProp . propertyOnce $ H.moduleWorkspace "tmp" $ \dir -> do
  native <- nativeFile dir
  let fixture name = "test/cardano-cli-test/files/input/plutus/" <> name
      v2 = dir </> "v2-always-succeeds.plutus"
  liftIO $
    writeFile
      v2
      "{\"type\":\"PlutusScriptV2\",\"description\":\"\",\"cborHex\":\"4e4d01000033222220051200120011\"}"
  forM_
    [ (native, nativeHash)
    , (fixture "v1-always-succeeds.plutus", "67f33146617a5e61936081db3b2117cbf59bd2123748f58ac9678656")
    , (v2, "793f8c8cffba081b2a56462fc219cc8fe652d6a338b62c7b134876e7")
    , (fixture "v3-always-succeeds.plutus", "186e32faa80a26810392fda6d559c7ed4721a65ce1c9d4ef3e1c87b4")
    , (fixture "v4-receiving-even-datum.plutus", plutusHash)
    ]
    $ \(script, expectedHash) -> do
      scriptHash <- H.evalEither (deserialiseFromRawBytesHex (Text.encodeUtf8 (Text.pack expectedHash)))
      let payment = PaymentCredentialByScript scriptHash
          network = Testnet (NetworkMagic 42)
          enterprise = makeShelleyAddress network payment NoStakeAddress
          base = makeShelleyAddress network payment (StakeAddressByValue (StakeCredentialByScript scriptHash))
      forM_ ["latest", "dijkstra"] $ \era -> do
        let args =
              [ era
              , "address"
              , "build"
              , "--payment-script-file"
              , script
              , "--testnet-magic"
              , "42"
              ]
        built <- execSingleValue args
        built === Text.unpack (serialiseAddress enterprise)
        builtBase <- execSingleValue (args <> ["--stake-script-file", script])
        builtBase === Text.unpack (serialiseAddress base)
        protected <- execSingleValue ["dijkstra", "address", "protect", "--address", built]
        expectedProtected <- H.evalEither (protectShelleyAddress enterprise)
        protected === Text.unpack (serialiseAddress expectedProtected)

rawArgs :: String -> FilePath -> [String]
rawArgs addr out =
  [ "dijkstra"
  , "transaction"
  , "build-raw"
  , "--tx-in"
  , input
  , "--tx-out"
  , addr <> "+2000000"
  , "--fee"
  , "200000"
  , "--out-file"
  , out
  ]

viewBody :: FilePath -> CliTest Aeson.Value
viewBody path = do
  json <- execCardanoCLI ["debug", "transaction", "view", "--tx-body-file", path, "--output-json"]
  H.evalEither (Aeson.eitherDecodeStrict' (Text.encodeUtf8 (Text.pack json)))

hprop_receiving_native_repeated_hash_and_reference_roundtrip :: Property
hprop_receiving_native_repeated_hash_and_reference_roundtrip = watchdogProp . propertyOnce $ H.moduleWorkspace "tmp" $ \dir -> do
  addr <- protectedScriptAddress nativeHash
  script <- nativeFile dir
  hash <- execSingleValue ["dijkstra", "transaction", "policyid", "--script-file", script]
  hash === nativeHash
  let body = dir </> "native.tx"
  void $
    execCardanoCLI $
      rawArgs addr body
        <> [ "--tx-out"
           , addr <> "+3000000"
           , "--receiving-output-index"
           , "0"
           , "--receiving-script-file"
           , script
           , "--receiving-output-index"
           , "1"
           , "--receiving-script-file"
           , script
           ]
  json <- viewBody body
  outputs <- H.evalMaybe (json ^? Aeson.key "outputs" . Aeson._Array)
  length outputs === 2
  H.assert $ all (\out -> out ^? Aeson.key "protected" . Aeson._Bool == Just True) outputs
  redeemers <- H.evalMaybe (json ^? Aeson.key "redeemers" . Aeson._Array)
  length redeemers === 0
  parsed <- H.evalEitherM $ liftIO $ Read.fileOrPipe body >>= Read.readFileTx
  let nativeScriptCount :: Maybe Int
      nativeScriptCount = case parsed of
        InAnyShelleyBasedEra ShelleyBasedEraDijkstra (ShelleyTx _ tx) ->
          Just (Map.size (tx ^. L.witsTxL . L.scriptTxWitsL))
        _ -> Nothing
  scriptCount <- H.evalMaybe nativeScriptCount
  scriptCount === 1
  -- Native authorization is shared by hash without per-output Plutus budgets.
  let sharedNativeBody = dir </> "shared-native.tx"
  void $
    execCardanoCLI $
      rawArgs addr sharedNativeBody
        <> ["--tx-out", addr <> "+3000000", "--receiving-output-index", "0", "--receiving-script-file", script]
  sharedNativeJson <- viewBody sharedNativeBody
  sharedNativeRedeemers <- H.evalMaybe (sharedNativeJson ^? Aeson.key "redeemers" . Aeson._Array)
  length sharedNativeRedeemers === 0
  let referenceBody = dir </> "reference.tx"
  void $
    execCardanoCLI $
      rawArgs addr referenceBody
        <> ["--receiving-output-index", "0", "--receiving-simple-script-tx-in-reference", referenceInput]
  referenceJson <- viewBody referenceBody
  references <- H.evalMaybe (referenceJson ^? Aeson.key "reference inputs" . Aeson._Array)
  Aeson.toJSON references === Aeson.toJSON [referenceInput]
  let consumedBody = dir </> "consumed-script.tx"
  void $
    execCardanoCLI $
      rawArgs addr consumedBody
        <> ["--receiving-output-index", "0", "--receiving-simple-script-tx-in-reference", input]
  consumedJson <- viewBody consumedBody
  consumedReferences <- H.evalMaybe (consumedJson ^? Aeson.key "reference inputs" . Aeson._Array)
  length consumedReferences === 0
  consumedInputs <- H.evalMaybe (consumedJson ^? Aeson.key "inputs" . Aeson._Array)
  Aeson.toJSON consumedInputs === Aeson.toJSON [input]

hprop_receiving_precise_negative_diagnostics :: Property
hprop_receiving_precise_negative_diagnostics = watchdogProp . propertyOnce $ H.moduleWorkspace "tmp" $ \dir -> do
  addr <- protectedScriptAddress nativeHash
  script <- nativeFile dir
  plutusAddr <- protectedScriptAddress plutusHash
  let args = rawArgs addr (dir </> "invalid.tx")
      witness = ["--receiving-output-index", "0", "--receiving-script-file", script]
  (missingExit, _, missingError) <- execDetailCardanoCLI args
  missingExit === ExitFailure 1
  H.assert
    ("Missing Receiving witness for protected script output index 0" `isInfixOf` missingError)
  (duplicateExit, _, duplicateError) <- execDetailCardanoCLI (args <> witness <> witness)
  duplicateExit === ExitFailure 1
  H.assert ("Duplicate --receiving-output-index" `isInfixOf` duplicateError)
  forM_ ["-1", "4294967296"] $ \badIndex -> do
    (boundsExit, _, _) <-
      execDetailCardanoCLI
        (args <> ["--receiving-output-index", badIndex, "--receiving-script-file", script])
    boundsExit === ExitFailure 1
  (unusedExit, _, unusedError) <-
    execDetailCardanoCLI
      (args <> ["--receiving-output-index", "1", "--receiving-script-file", script])
  unusedExit === ExitFailure 1
  H.assert ("Receiving" `isInfixOf` unusedError)
  scriptHash <- H.evalEither (deserialiseFromRawBytesHex (Text.encodeUtf8 (Text.pack nativeHash)))
  let ordinary =
        Text.unpack $
          serialiseAddress $
            makeShelleyAddress (Testnet (NetworkMagic 42)) (PaymentCredentialByScript scriptHash) NoStakeAddress
  (ordinaryExit, _, ordinaryError) <-
    execDetailCardanoCLI
      ( args
          <> ["--tx-out", ordinary <> "+2000000"]
          <> witness
          <> ["--receiving-output-index", "1", "--receiving-script-file", script]
      )
  ordinaryExit === ExitFailure 1
  H.assert ("absent or ineligible output index: 1" `isInfixOf` ordinaryError)
  recipient <-
    H.evalEither (deserialiseFromRawBytesHex (Text.encodeUtf8 (Text.pack (replicate 56 '3'))))
  protectedKey <-
    H.evalEither $
      protectShelleyAddress $
        makeShelleyAddress (Testnet (NetworkMagic 42)) (PaymentCredentialByKey recipient) NoStakeAddress
  (keyExit, _, keyError) <-
    execDetailCardanoCLI
      (rawArgs (Text.unpack (serialiseAddress protectedKey)) (dir </> "key-index.tx") <> witness)
  keyExit === ExitFailure 1
  H.assert ("absent or ineligible output index: 0" `isInfixOf` keyError)
  (nativeBudgetExit, _, _) <-
    execDetailCardanoCLI
      (args <> witness <> ["--receiving-execution-units", "(100000000,1000000)"])
  nativeBudgetExit === ExitFailure 1
  (oldSelectorExit, _, _) <-
    execDetailCardanoCLI
      (args <> ["--receiving-script-hash", nativeHash, "--receiving-script-file", script])
  oldSelectorExit === ExitFailure 1
  (hashExit, _, hashError) <-
    execDetailCardanoCLI
      ( rawArgs plutusAddr (dir </> "hash-invalid.tx")
          <> ["--receiving-output-index", "0", "--receiving-script-file", script]
      )
  hashExit === ExitFailure 1
  H.assert
    ("Receiving witness script hash does not match output index 0" `isInfixOf` hashError)
  (languageExit, _, languageError) <-
    execDetailCardanoCLI $
      args
        <> [ "--receiving-output-index"
           , "0"
           , "--receiving-script-file"
           , "test/cardano-cli-test/files/input/plutus/v3-always-succeeds.plutus"
           , "--receiving-redeemer-value"
           , "0"
           , "--receiving-execution-units"
           , "(100000000,1000000)"
           ]
  languageExit === ExitFailure 1
  H.assert ("Receiving scripts require Plutus V4" `isInfixOf` languageError)
  (eraExit, _, eraError) <- execDetailCardanoCLI ("conway" : drop 1 args)
  eraExit === ExitFailure 1
  H.assert ("Dijkstra" `isInfixOf` eraError)

-- Both signatures authorize the exact agreed body. Assembly adds witnesses
-- without changing that body; reusing them after changing an output fails the
-- same verification primitive as the ledger witness rule.
hprop_receiving_key_recipient_multisigner_body_binding :: Property
hprop_receiving_key_recipient_multisigner_body_binding = watchdogProp . propertyOnce $ H.moduleWorkspace "tmp" $ \dir -> do
  let senderKey = dir </> "sender.skey"
      recipientKey = dir </> "recipient.skey"
      recipientVKey = dir </> "recipient.vkey"
  void $
    execCardanoCLI
      [ "address"
      , "key-gen"
      , "--verification-key-file"
      , dir </> "sender.vkey"
      , "--signing-key-file"
      , senderKey
      ]
  void $
    execCardanoCLI
      [ "latest"
      , "address"
      , "key-gen"
      , "--verification-key-file"
      , recipientVKey
      , "--signing-key-file"
      , recipientKey
      ]
  ordinary <-
    execSingleValue
      [ "latest"
      , "address"
      , "build"
      , "--payment-verification-key-file"
      , recipientVKey
      , "--testnet-magic"
      , "42"
      ]
  protected <- execSingleValue ["latest", "address", "protect", "--address", ordinary]
  keyHash <-
    execSingleValue ["latest", "address", "key-hash", "--payment-verification-key-file", recipientVKey]
  let body = dir </> "payment.tx"
      changedBody = dir </> "changed.tx"
      signed = dir </> "signed.tx"
      senderWitness = dir </> "sender.witness"
      recipientWitness = dir </> "recipient.witness"
  void $ execCardanoCLI (rawArgs protected body)
  json <- viewBody body
  required <-
    H.evalMaybe
      (json ^? Aeson.key "required recipient payment key witnesses (protected outputs)" . Aeson._Array)
  Aeson.toJSON required === Aeson.toJSON [keyHash]
  mapM_
    ( \(key, out) ->
        void $
          execCardanoCLI
            [ "dijkstra"
            , "transaction"
            , "witness"
            , "--tx-body-file"
            , body
            , "--signing-key-file"
            , key
            , "--testnet-magic"
            , "42"
            , "--out-file"
            , out
            ]
    )
    [(senderKey, senderWitness), (recipientKey, recipientWitness)]
  void $
    execCardanoCLI
      [ "dijkstra"
      , "transaction"
      , "sign-witness"
      , "--tx-body-file"
      , body
      , "--witness-file"
      , senderWitness
      , "--witness-file"
      , recipientWitness
      , "--out-file"
      , signed
      ]
  agreedTx <- H.evalEitherM $ liftIO $ Read.fileOrPipe body >>= Read.readFileTx
  assembledTx <- H.evalEitherM $ liftIO $ Read.fileOrPipe signed >>= Read.readFileTx
  void $ execCardanoCLI (rawArgs protected changedBody <> ["--tx-out", ordinary <> "+1000000"])
  modifiedTx <- H.evalEitherM $ liftIO $ Read.fileOrPipe changedBody >>= Read.readFileTx
  case (agreedTx, assembledTx, modifiedTx) of
    ( InAnyShelleyBasedEra ShelleyBasedEraDijkstra (ShelleyTx _ agreedLedger)
      , InAnyShelleyBasedEra ShelleyBasedEraDijkstra assembled@(ShelleyTx _ assembledLedger)
      , InAnyShelleyBasedEra ShelleyBasedEraDijkstra (ShelleyTx _ modifiedLedger)
      ) -> do
        let agreedHash = fromShelleyTxId (Ledger.txIdTxBody (agreedLedger ^. Ledger.bodyTxL))
            modifiedHash = fromShelleyTxId (Ledger.txIdTxBody (modifiedLedger ^. Ledger.bodyTxL))
            TxId originalHash = agreedHash
            TxId changedHash = modifiedHash
            witnesses = getTxWitnesses assembled
        fromShelleyTxId (Ledger.txIdTxBody (assembledLedger ^. Ledger.bodyTxL)) === agreedHash
        H.assert (agreedHash /= modifiedHash)
        length witnesses === 2
        mapM_
          ( \witness -> case witness of
              ShelleyKeyWitness _ wit -> do
                H.assert (verifyWitVKey originalHash wit)
                H.assert (not (verifyWitVKey changedHash wit))
              _ -> H.failure
          )
          witnesses
    _ -> H.failure

hprop_receiving_v4_per_output_redeemer_view :: Property
hprop_receiving_v4_per_output_redeemer_view = watchdogProp . propertyOnce $ H.moduleWorkspace "tmp" $ \dir -> do
  hash <-
    execSingleValue
      [ "dijkstra"
      , "transaction"
      , "policyid"
      , "--script-file"
      , "test/cardano-cli-test/files/input/plutus/v4-receiving-even-datum.plutus"
      ]
  hash === plutusHash
  addr <- protectedScriptAddress plutusHash
  nativeAddr <- protectedScriptAddress lowNativeHash
  scriptHash <- H.evalEither (deserialiseFromRawBytesHex (Text.encodeUtf8 (Text.pack plutusHash)))
  recipientKey <-
    H.evalEither (deserialiseFromRawBytesHex (Text.encodeUtf8 (Text.pack (replicate 56 '3'))))
  protectedKey <-
    H.evalEither $
      protectShelleyAddress $
        makeShelleyAddress (Testnet (NetworkMagic 42)) (PaymentCredentialByKey recipientKey) NoStakeAddress
  let ordinaryScript =
        Text.unpack $
          serialiseAddress $
            makeShelleyAddress (Testnet (NetworkMagic 42)) (PaymentCredentialByScript scriptHash) NoStakeAddress
      protectedKeyAddr = Text.unpack (serialiseAddress protectedKey)
      native = dir </> "threshold.json"
  liftIO $ writeFile native "{\"type\":\"atLeast\",\"required\":0,\"scripts\":[]}"
  paramsFile <- writeDijkstraParams dir
  let body = dir </> "plutus.tx"
  void $
    execCardanoCLI $
      rawArgs ordinaryScript body
        <> [ "--tx-out"
           , addr <> "+2000000"
           , "--tx-out-inline-datum-value"
           , "2"
           , "--tx-out"
           , protectedKeyAddr <> "+2000000"
           , "--tx-out"
           , nativeAddr <> "+2000000"
           , "--tx-out"
           , addr <> "+3000000"
           , "--tx-out-inline-datum-value"
           , "4"
           , "--tx-in-collateral"
           , referenceInput
           , "--protocol-params-file"
           , paramsFile
           , "--receiving-output-index"
           , "1"
           , "--receiving-script-file"
           , "test/cardano-cli-test/files/input/plutus/v4-receiving-even-datum.plutus"
           , "--receiving-redeemer-value"
           , "0"
           , "--receiving-execution-units"
           , "(100000000,1000000)"
           , "--receiving-output-index"
           , "3"
           , "--receiving-script-file"
           , native
           , "--receiving-output-index"
           , "4"
           , "--receiving-script-file"
           , "test/cardano-cli-test/files/input/plutus/v4-receiving-even-datum.plutus"
           , "--receiving-redeemer-value"
           , "1"
           , "--receiving-execution-units"
           , "(200000000,2000000)"
           ]
  json <- viewBody body
  redeemers <- H.evalMaybe (json ^? Aeson.key "redeemers" . Aeson._Array)
  length redeemers === 2
  H.assert ("receiving protected output at index" `isInfixOf` show redeemers)
  forM_ [(0, 1, 1000000), (1, 4, 2000000)] $ \(redeemerIndex, outputIndex, expectedMemory) -> do
    pointer <-
      H.evalMaybe (json ^? Aeson.key "redeemers" . Aeson.nth redeemerIndex . Aeson.key "redeemer pointer")
    pointer ^? Aeson.key "kind" . Aeson._String === Just "DijkstraReceiving"
    pointer ^? Aeson.key "value" . Aeson.key "index" . Aeson._Number === Just outputIndex
    json
      ^? Aeson.key "redeemers"
        . Aeson.nth redeemerIndex
        . Aeson.key "redeemer"
        . Aeson.key "data"
        . Aeson._String
      === Just (if redeemerIndex == 0 then "I 0" else "I 1")
    json
      ^? Aeson.key "redeemers"
        . Aeson.nth redeemerIndex
        . Aeson.key "redeemer"
        . Aeson.key "execution units"
        . Aeson.key "memory"
        . Aeson._Number
      === Just expectedMemory

  -- Evaluate the genuine V4 fixture through the existing offline cost command.
  -- Repeated hashes have independent Receiving invocations. Ordinary output0,
  -- protected key output2 and native output3 remain gaps with no Plutus budget.
  fundId <- H.evalEither (deserialiseFromRawBytesHex (Text.encodeUtf8 (Text.pack (replicate 64 '1'))))
  collId <- H.evalEither (deserialiseFromRawBytesHex (Text.encodeUtf8 (Text.pack (replicate 64 '2'))))
  key <- H.evalEither (deserialiseFromRawBytesHex (Text.encodeUtf8 (Text.pack (replicate 56 '3'))))
  let fundingAddress =
        AddressInEra
          (ShelleyAddressInEra ShelleyBasedEraDijkstra)
          (makeShelleyAddress (Testnet (NetworkMagic 42)) (PaymentCredentialByKey key) NoStakeAddress)
      fundingOutput amount =
        TxOut
          fundingAddress
          (lovelaceToTxOutValue ShelleyBasedEraDijkstra amount)
          TxOutDatumNone
          ReferenceScriptNone
      utxo =
        UTxO
          ( Map.fromList
              [(TxIn fundId (TxIx 0), fundingOutput 20000000), (TxIn collId (TxIx 0), fundingOutput 3000000)]
          )
      utxoFile = dir </> "utxo.json"
      costFile = dir </> "receiving-cost.json"
  liftIO $ LBS.writeFile utxoFile (Aeson.encode utxo)
  void $
    execCardanoCLI
      [ "dijkstra"
      , "transaction"
      , "calculate-plutus-script-cost"
      , "offline"
      , "--start-time-posix"
      , "1666656000"
      , "--protocol-params-file"
      , paramsFile
      , "--utxo-file"
      , utxoFile
      , "--unsafe-extend-safe-zone"
      , "--era-history-file"
      , "test/cardano-cli-test/files/input/preview-era-history.json"
      , "--tx-file"
      , body
      , "--out-file"
      , costFile
      ]
  cost :: Aeson.Value <- H.readJsonFileOk costFile
  costs <- H.evalMaybe (cost ^? Aeson._Array)
  length costs === 2
  memory <-
    H.evalMaybe (cost ^? Aeson.nth 0 . Aeson.key "executionUnits" . Aeson.key "memory" . Aeson._Number)
  steps <-
    H.evalMaybe (cost ^? Aeson.nth 0 . Aeson.key "executionUnits" . Aeson.key "steps" . Aeson._Number)
  H.assert (memory > 0 && steps > 0)

  -- Offline estimation retains the caller supplied per-output budget while
  -- deriving fees and ordinary change through the same API builder.
  let estimateBody = dir </> "estimated.tx"
      ordinary =
        Text.unpack
          ( serialiseAddress
              (makeShelleyAddress (Testnet (NetworkMagic 42)) (PaymentCredentialByKey key) NoStakeAddress)
          )
  void $
    execCardanoCLI
      [ "dijkstra"
      , "transaction"
      , "build-estimate"
      , "--shelley-key-witnesses"
      , "1"
      , "--protocol-params-file"
      , paramsFile
      , "--total-utxo-value"
      , "20000000"
      , "--tx-in"
      , input
      , "--tx-out"
      , addr <> "+2000000"
      , "--tx-out-inline-datum-value"
      , "2"
      , "--change-address"
      , ordinary
      , "--tx-in-collateral"
      , referenceInput
      , "--tx-total-collateral"
      , "3000000"
      , "--receiving-output-index"
      , "0"
      , "--receiving-script-file"
      , "test/cardano-cli-test/files/input/plutus/v4-receiving-even-datum.plutus"
      , "--receiving-redeemer-value"
      , "0"
      , "--receiving-execution-units"
      , "(100000000,1000000)"
      , "--out-file"
      , estimateBody
      ]
  estimatedJson <- viewBody estimateBody
  estimatedJson
    ^? Aeson.key "redeemers"
      . Aeson.nth 0
      . Aeson.key "redeemer pointer"
      . Aeson.key "value"
      . Aeson.key "index"
      . Aeson._Number
    === Just 0

  let protectedChangeBody = dir </> "protected-change.tx"
  void $
    execCardanoCLI
      [ "dijkstra"
      , "transaction"
      , "build-estimate"
      , "--shelley-key-witnesses"
      , "1"
      , "--protocol-params-file"
      , paramsFile
      , "--total-utxo-value"
      , "20000000"
      , "--tx-in"
      , input
      , "--tx-out"
      , addr <> "+2000000"
      , "--tx-out-inline-datum-value"
      , "2"
      , "--change-address"
      , nativeAddr
      , "--tx-in-collateral"
      , referenceInput
      , "--tx-out-return-collateral"
      , ordinary <> "+1000000"
      , "--tx-total-collateral"
      , "3000000"
      , "--receiving-output-index"
      , "0"
      , "--receiving-script-file"
      , "test/cardano-cli-test/files/input/plutus/v4-receiving-even-datum.plutus"
      , "--receiving-redeemer-value"
      , "0"
      , "--receiving-execution-units"
      , "(100000000,1000000)"
      , "--receiving-output-index"
      , "1"
      , "--receiving-script-file"
      , native
      , "--out-file"
      , protectedChangeBody
      ]
  protectedChangeJson <- viewBody protectedChangeBody
  protectedChangeJson
    ^? Aeson.key "redeemers"
      . Aeson.nth 0
      . Aeson.key "redeemer pointer"
      . Aeson.key "value"
      . Aeson.key "index"
      . Aeson._Number
    === Just 0
  protectedChangeJson
    ^? Aeson.key "redeemers"
      . Aeson.nth 0
      . Aeson.key "redeemer"
      . Aeson.key "execution units"
      . Aeson.key "memory"
      . Aeson._Number
    === Just 1000000

hprop_receiving_dijkstra_command_help :: Property
hprop_receiving_dijkstra_command_help = watchdogProp . propertyOnce $ do
  mapM_
    ( \command -> do
        help <- execCardanoCLI ["dijkstra", "transaction", command, "--help"]
        H.assert ("--receiving-output-index" `isInfixOf` help)
        H.assert ("--receiving-script-file" `isInfixOf` help)
        H.assert ("--receiving-simple-script-tx-in-reference" `isInfixOf` help)
        H.assert ("--receiving-plutus-script-v4" `isInfixOf` help)
        conwayHelp <- execCardanoCLI ["conway", "transaction", command, "--help"]
        H.assert (not ("--receiving-output-index" `isInfixOf` conwayHelp))
    )
    ["build", "build-raw", "build-estimate"]

-- Offline estimation has no UTxO with which to inspect a native reference
-- script. Its explicit count must include the native recipient signature in
-- addition to the funding key, even though Receiving has no redeemer.
hprop_receiving_native_reference_explicit_witness_fee_count :: Property
hprop_receiving_native_reference_explicit_witness_fee_count = watchdogProp . propertyOnce $ H.moduleWorkspace "tmp" $ \dir -> do
  let vkey = dir </> "native-recipient.vkey"
      skey = dir </> "native-recipient.skey"
      script = dir </> "signature.json"
  void $
    execCardanoCLI
      ["latest", "address", "key-gen", "--verification-key-file", vkey, "--signing-key-file", skey]
  signerHash <-
    execSingleValue ["latest", "address", "key-hash", "--payment-verification-key-file", vkey]
  liftIO $
    LBS.writeFile script $
      Aeson.encode $
        Aeson.object ["type" Aeson..= ("sig" :: Text.Text), "keyHash" Aeson..= signerHash]
  hash <- execSingleValue ["dijkstra", "transaction", "policyid", "--script-file", script]
  addr <- protectedScriptAddress hash
  change <-
    execSingleValue
      ["latest", "address", "build", "--payment-verification-key-file", vkey, "--testnet-magic", "42"]
  params <- writeDijkstraParams dir
  let estimate count file =
        execCardanoCLI
          [ "dijkstra"
          , "transaction"
          , "build-estimate"
          , "--shelley-key-witnesses"
          , count
          , "--protocol-params-file"
          , params
          , "--total-utxo-value"
          , "20000000"
          , "--tx-in"
          , input
          , "--tx-out"
          , addr <> "+2000000"
          , "--change-address"
          , change
          , "--receiving-output-index"
          , "0"
          , "--receiving-simple-script-tx-in-reference"
          , referenceInput
          , "--reference-script-size"
          , "32"
          , "--out-file"
          , file
          ]
      undercounted = dir </> "one-witness.tx"
      complete = dir </> "two-witnesses.tx"
  void $ estimate "1" undercounted
  void $ estimate "2" complete
  one <- viewBody undercounted
  two <- viewBody complete
  let fee json = do
        rendered <- H.evalMaybe (json ^? Aeson.key "fee" . Aeson._String)
        firstWord <- H.evalMaybe $ case Text.words rendered of value : _ -> Just value; [] -> Nothing
        H.evalMaybe (readMaybe (Text.unpack firstWord) :: Maybe Integer)
  oneFee <- fee one
  twoFee <- fee two
  H.assert (twoFee > oneFee)
  minimumOutput <-
    execCardanoCLI
      [ "dijkstra"
      , "transaction"
      , "calculate-min-fee"
      , "--tx-body-file"
      , undercounted
      , "--protocol-params-file"
      , params
      , "--witness-count"
      , "2"
      , "--reference-script-size"
      , "32"
      ]
  minimumJson :: Aeson.Value <-
    H.evalEither (Aeson.eitherDecodeStrict' (Text.encodeUtf8 (Text.pack minimumOutput)))
  minimumFee <- H.evalMaybe (minimumJson ^? Aeson.key "fee" . Aeson._Number)
  H.assert (fromInteger oneFee < minimumFee)
  H.assert (fromInteger twoFee >= minimumFee)

  refs <- H.evalMaybe (two ^? Aeson.key "reference inputs" . Aeson._Array)
  Aeson.toJSON refs === Aeson.toJSON [referenceInput]
  redeemers <- H.evalMaybe (two ^? Aeson.key "redeemers" . Aeson._Array)
  length redeemers === 0
