{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cli.Transaction.Build where

import Cardano.Api.Ledger (hashToBytes)
import Cardano.Api.Shelley
  ( AddressAny (AddressShelley)
  , AddressInEra (AddressInEra)
  , AddressTypeInEra (ShelleyAddressInEra)
  , AsType (AsAddressAny)
  , ReferenceScript (ReferenceScriptNone)
  , ShelleyBasedEra (ShelleyBasedEraConway)
  , TxId (TxId)
  , TxIn (TxIn)
  , TxIx (TxIx)
  , TxOut (TxOut)
  , TxOutDatum (TxOutDatumNone)
  , UTxO (UTxO)
  , deserialiseAddress
  , liftIO
  , lovelaceToTxOutValue
  , writeFileJSON
  )

import Data.Aeson qualified as Aeson
import Data.ByteString.Base16 qualified as Base16
import Data.List (isInfixOf)
import Data.Map qualified as Map
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Lens.Micro ((^?))
import Lens.Micro.Aeson qualified as Aeson
import System.Exit (ExitCode (..))
import System.FilePath ((</>))

import Test.Gen.Cardano.Api.Typed (genShelleyHash)

import Test.Cardano.CLI.Util

import Hedgehog
import Hedgehog qualified as H
import Hedgehog.Extras qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

inputDir :: FilePath
inputDir = "test/cardano-cli-test/files/input/shelley/transaction"

-- | This is a test of https://github.com/IntersectMBO/cardano-cli/issues/662
-- Execute me with:
-- @cabal test cardano-cli-test --test-options '-p "/conway transaction build one voter many votes/"'@
hprop_conway_transaction_build_one_voter_many_votes :: Property
hprop_conway_transaction_build_one_voter_many_votes =
  watchdogProp . propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
    outFile <- H.noteTempFile tempDir "tx.traw"

    (exitCode, _stdout, stderr) <-
      H.noteShowM $
        execDetailCardanoCLI
          [ "conway"
          , "transaction"
          , "build-raw"
          , "--tx-in"
          , "6e8c947816e82627aeccb55300074f2894a2051332f62a1c8954e7b588a18be7#0"
          , "--tx-out"
          , "addr_test1vpfwv0ezc5g8a4mkku8hhy3y3vp92t7s3ul8g778g5yegsgalc6gc+24910487859"
          , "--invalid-hereafter"
          , "24325742"
          , "--fee"
          , "178569"
          , "--vote-file"
          , inputDir </> "vote1.drep.json"
          , "--vote-file"
          , inputDir </> "vote2.drep.json"
          , "--out-file"
          , outFile
          ]

    exitCode H.=== ExitFailure 1
    H.assertWith stderr ("This would cause ignoring some of the votes" `isInfixOf`)

-- | This is a test of https://github.com/IntersectMBO/cardano-cli/issues/904
-- Execute me with:
-- @cabal test cardano-cli-test --test-options '-p "/conway transaction build raw negative txout/"'@
hprop_conway_transaction_build_raw_negative_txout :: Property
hprop_conway_transaction_build_raw_negative_txout =
  watchdogProp . propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
    outFile <- H.noteTempFile tempDir "tx.traw"

    (exitCode, _stdout, stderr) <-
      H.noteShowM $
        execDetailCardanoCLI
          [ "conway"
          , "transaction"
          , "build-raw"
          , "--fee"
          , "200000"
          , "--tx-in"
          , "e25450233e4bedd00c8bda15c48c2d4018223bd88271e194052294c4e5be7d55#0"
          , "--tx-out"
          , "addr_test1vqfxq2s8yce3tuhjq9ulu2awuk623hzvtft9z8fh6qelzts49vuqw+-1" -- This is the negative txout
          , "--out-file"
          , outFile
          ]

    exitCode H.=== ExitFailure 1
    H.assertWith stderr ("Value must be positive in UTxO (or transaction output)" `isInfixOf`)

-- | This is a test that we allow transaction outputs to contain negative bits, if
-- the grand total is positive.
-- @cabal test cardano-cli-test --test-options '-p "/conway transaction build raw negative bits positive total txout/"'@
hprop_conway_transaction_build_raw_negative_bits_positive_total_txout :: Property
hprop_conway_transaction_build_raw_negative_bits_positive_total_txout =
  watchdogProp . propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
    outFile <- H.noteTempFile tempDir "tx.traw"

    -- This checks that the command succeeds
    H.noteShowM_ $
      execCardanoCLI
        [ "conway"
        , "transaction"
        , "build-raw"
        , "--fee"
        , "200000"
        , "--tx-in"
        , "e25450233e4bedd00c8bda15c48c2d4018223bd88271e194052294c4e5be7d55#0"
        , "--tx-out"
        , "addr_test1vqfxq2s8yce3tuhjq9ulu2awuk623hzvtft9z8fh6qelzts49vuqw+-1+3" -- Negative txout (-1) summed with positive txout (+3), so total is positive
        , "--out-file"
        , outFile
        ]

-- | This tests calculating the cost of a plutus script offline.
-- @cabal test cardano-cli-test --test-options '-p "/conway calculate plutus script cost offline/"'@
hprop_conway_calculate_plutus_script_cost_offline :: Property
hprop_conway_calculate_plutus_script_cost_offline =
  watchdogProp . propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
    randomTxIdHash <- forAll genShelleyHash
    txIx <- forAll $ Gen.integral (Range.linear 0 10)
    AddressShelley scriptAddr <-
      H.evalMaybe $
        deserialiseAddress AsAddressAny "addr_test1wqvxuvh64q9zdqgrjt76d42eclk5wgdxtnsun4808cwg0dqxy2mj0"

    unsignedTx <- H.noteTempFile tempDir "unsigned.tx"

    H.noteShowM_ $
      execCardanoCLI
        [ "conway"
        , "transaction"
        , "build-raw"
        , "--tx-in"
        , Text.unpack (Text.decodeLatin1 (Base16.encode (hashToBytes randomTxIdHash))) <> "#" <> show txIx
        , "--tx-in-script-file"
        , "test/cardano-cli-test/files/input/plutus/v3-always-succeeds.plutus"
        , "--tx-in-redeemer-value"
        , "42"
        , "--tx-in-execution-units"
        , "(30,40)"
        , "--tx-out"
        , "addr_test1wqvxuvh64q9zdqgrjt76d42eclk5wgdxtnsun4808cwg0dqxy2mj0+1000000"
        , "--fee"
        , "200000"
        , "--out-file"
        , unsignedTx
        ]

    costOutFile <- H.noteTempFile tempDir "cost-calculation.json"

    utxoFile <- H.noteTempFile tempDir "utxo.json"
    H.evalEitherM $
      liftIO $
        writeFileJSON
          utxoFile
          ( UTxO $
              Map.singleton
                (TxIn (TxId randomTxIdHash) (TxIx txIx))
                ( TxOut
                    (AddressInEra (ShelleyAddressInEra ShelleyBasedEraConway) scriptAddr)
                    (lovelaceToTxOutValue ShelleyBasedEraConway 1200000)
                    TxOutDatumNone
                    ReferenceScriptNone
                )
          )

    H.noteShowM_ $
      execCardanoCLI
        [ "conway"
        , "transaction"
        , "calculate-plutus-script-cost"
        , "offline"
        , "--start-time-posix"
        , "1666656000"
        , "--protocol-params-file"
        , "test/cardano-cli-test/files/input/protocol-params-preview.json"
        , "--utxo-file"
        , utxoFile
        , "--unsafe-extend-safe-zone"
        , "--era-history-file"
        , "test/cardano-cli-test/files/input/preview-era-history.json"
        , "--tx-file"
        , unsignedTx
        , "--out-file"
        , costOutFile
        ]

    json :: Aeson.Value <- H.readJsonFileOk costOutFile

    lovelaceCost <- H.evalMaybe $ json ^? Aeson.nth 0 . Aeson.key "lovelaceCost" . Aeson._Number
    lovelaceCost === 34

    executionUnits <- H.evalMaybe $ json ^? Aeson.nth 0 . Aeson.key "executionUnits"
    memory <- H.evalMaybe $ executionUnits ^? Aeson.key "memory" . Aeson._Number
    memory === 500

    steps <- H.evalMaybe $ executionUnits ^? Aeson.key "steps" . Aeson._Number
    steps === 64100
