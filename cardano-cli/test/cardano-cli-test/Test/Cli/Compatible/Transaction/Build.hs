{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cli.Compatible.Transaction.Build where

import Cardano.Api.Internal.Eras
import Cardano.Api.Internal.Pretty

import Control.Monad
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class
import Data.Aeson (Value)
import Data.Aeson qualified as A
import Data.Char (toLower)
import Data.String (IsString (..))
import GHC.Stack

import Test.Cardano.CLI.Util

import Hedgehog
import Hedgehog.Extras qualified as H
import Hedgehog.Extras.Test.Golden qualified as H

inputDir :: FilePath
inputDir = "test/cardano-cli-test/files/input/"

hprop_compatible_conway_transaction_build_cert_script_wit :: Property
hprop_compatible_conway_transaction_build_cert_script_wit = watchdogProp . propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
  refOutFile <- H.noteTempFile tempDir "txbody.reference.json"
  outFile <- H.noteTempFile tempDir "txbody.json"
  let eraName = map toLower . docToString $ pretty ConwayEra

  let args =
        [ "--tx-in"
        , "6e8c947816e82627aeccb55300074f2894a2051332f62a1c8954e7b588a18be7#0"
        , "--tx-out"
        , "addr_test1vpfwv0ezc5g8a4mkku8hhy3y3vp92t7s3ul8g778g5yegsgalc6gc+24910487859"
        , "--fee"
        , "178569"
        , "--certificate-file"
        , inputDir <> "certificate/stake-address-registration.json"
        , "--certificate-script-file"
        , inputDir <> "plutus/v1-always-succeeds.plutus"
        , "--certificate-redeemer-value"
        , "0"
        , "--certificate-execution-units"
        , "(0,0)"
        ]

  -- reference transaction
  _ <-
    execCardanoCLI $
      [ eraName
      , "transaction"
      , "build-raw"
      ]
        <> args
        <> [ "--out-file"
           , refOutFile
           ]

  -- tested compatible transaction
  _ <-
    execCardanoCLI $
      [ "compatible"
      , eraName
      , "transaction"
      , "signed-transaction"
      ]
        <> args
        <> [ "--out-file"
           , outFile
           ]

  assertTxFilesEqual refOutFile outFile

hprop_compatible_shelley_create_update_proposal :: Property
hprop_compatible_shelley_create_update_proposal = watchdogProp . propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
  refOutFile <- H.noteTempFile tempDir "update-proposal_allegra.reference.proposal"
  outFile <- H.noteTempFile tempDir "update_proposal_allegra.proposal"
  let eraName = map toLower . docToString $ pretty ShelleyEra

  let args =
        [ "--epoch"
        , "1"
        , "--genesis-verification-key-file"
        , inputDir <> "genesis1.vkey"
        , "--protocol-major-version"
        , "3"
        , "--protocol-minor-version"
        , "0"
        ]

  -- reference transaction
  _ <-
    execCardanoCLI $
      [ "legacy"
      , "governance"
      , "create-update-proposal"
      ]
        <> args
        <> [ "--out-file"
           , refOutFile
           ]

  -- tested compatible transaction
  _ <-
    execCardanoCLI $
      [ "compatible"
      , eraName
      , "governance"
      , "action"
      , "create-protocol-parameters-update"
      ]
        <> args
        <> [ "--out-file"
           , outFile
           ]

  H.diffFileVsGoldenFile outFile refOutFile

hprop_compatible_shelley_transaction :: Property
hprop_compatible_shelley_transaction = watchdogProp . propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
  refOutFile <- H.noteTempFile tempDir "tx.reference.json"
  outFile <- H.noteTempFile tempDir "tx.json"
  let eraName = map toLower . docToString $ pretty ShelleyEra

  let args =
        [ "--fee"
        , "5000000"
        , "--tx-in"
        , "596e9836a4f42661d66deb7993e4e5da310b688e85facc50fee2462e611a0c94#0"
        , "--tx-out"
        , "2657WMsDfac7RXyZU5nkYxPvZAh7u96FN4cp6w6581zJUR4vKUr3kofjd8MuFghFS+35999999995000000"
        , "--update-proposal-file"
        , inputDir <> "shelley/update-proposal.json"
        ]

  -- reference transaction
  void . execCardanoCLI $
    [ eraName
    , "transaction"
    , "build-raw"
    ]
      <> args
      <> [ "--out-file"
         , refOutFile
         ]

  -- tested compatible transaction
  void . execCardanoCLI $
    [ "compatible"
    , eraName
    , "transaction"
    , "signed-transaction"
    ]
      <> args
      <> [ "--out-file"
         , outFile
         ]

  assertTxFilesEqual refOutFile outFile

hprop_compatible_shelley_signed_transaction :: Property
hprop_compatible_shelley_signed_transaction = watchdogProp . propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
  refOutFile <- H.noteTempFile tempDir "tx.reference.json"
  refTxBody <- H.noteTempFile tempDir "txbody.reference.json"
  outFile <- H.noteTempFile tempDir "tx.json"
  let eraName = map toLower . docToString $ pretty ShelleyEra

  let args =
        [ "--fee"
        , "5000000"
        , "--tx-in"
        , "596e9836a4f42661d66deb7993e4e5da310b688e85facc50fee2462e611a0c94#0"
        , "--tx-out"
        , "2657WMsDfac7RXyZU5nkYxPvZAh7u96FN4cp6w6581zJUR4vKUr3kofjd8MuFghFS+35999999995000000"
        , "--update-proposal-file"
        , inputDir <> "shelley/update-proposal.json"
        ]
      signArgs =
        [ "--signing-key-file"
        , inputDir <> "delegate1.skey"
        , "--signing-key-file"
        , inputDir <> "genesis1.skey"
        , "--signing-key-file"
        , inputDir <> "byron/payment.skey"
        , "--testnet-magic"
        , "42"
        ]

  -- reference transaction
  void . execCardanoCLI $
    [ eraName
    , "transaction"
    , "build-raw"
    ]
      <> args
      <> [ "--out-file"
         , refTxBody
         ]

  -- sign reference transaction
  void . execCardanoCLI $
    [ eraName
    , "transaction"
    , "sign"
    ]
      <> signArgs
      <> [ "--tx-body-file"
         , refTxBody
         , "--out-file"
         , refOutFile
         ]

  -- tested compatible transaction
  void . execCardanoCLI $
    [ "compatible"
    , eraName
    , "transaction"
    , "signed-transaction"
    ]
      <> args
      <> signArgs
      <> [ "--out-file"
         , outFile
         ]

  assertTxFilesEqual refOutFile outFile

hprop_compatible_shelley_transaction_build_with_cert :: Property
hprop_compatible_shelley_transaction_build_with_cert = watchdogProp . propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
  refTxBody <- H.noteTempFile tempDir "txbody.reference.json"
  refOutFile <- H.noteTempFile tempDir "tx.reference.json"
  outFile <- H.noteTempFile tempDir "tx.json"
  let eraName = map toLower . docToString $ pretty ShelleyEra

  verKey <- noteTempFile tempDir "stake-verification-key-file"
  signKey <- noteTempFile tempDir "stake-signing-key-file"

  -- prepare required keys
  void $
    execCardanoCLI
      [ eraName
      , "stake-address"
      , "key-gen"
      , "--verification-key-file"
      , verKey
      , "--signing-key-file"
      , signKey
      ]

  H.assertFilesExist [verKey, signKey]

  stakeRegCert <- noteTempFile tempDir "stake-registration-certificate"
  void $
    execCardanoCLI
      [ "compatible"
      , eraName
      , "stake-address"
      , "registration-certificate"
      , "--stake-verification-key-file"
      , verKey
      , "--out-file"
      , stakeRegCert
      ]

  let args =
        [ "--fee"
        , "5000000"
        , "--tx-in"
        , "7a2a25400ae0accdf796ac5be8c2e79a5cac36cbb3ac918886d3acd776fe5fa7#0"
        , "--tx-out"
        , "2657WMsDfac5TGjRdAHui5w98RLNoEP4vtbFJUezQ5nwAFTWa4MpqD8y59Xgwv5jD+35996998492600000"
        , "--certificate-file"
        , stakeRegCert
        ]
      signArgs =
        [ "--signing-key-file"
        , signKey
        ]

  -- build reference transaction
  void . execCardanoCLI $
    [ eraName
    , "transaction"
    , "build-raw"
    ]
      <> args
      <> [ "--out-file"
         , refTxBody
         ]

  void . execCardanoCLI $
    [ eraName
    , "transaction"
    , "sign"
    ]
      <> signArgs
      <> [ "--tx-body-file"
         , refTxBody
         , "--out-file"
         , refOutFile
         ]

  -- build tested compatible transaction
  void $
    execCardanoCLI $
      [ "compatible"
      , eraName
      , "transaction"
      , "signed-transaction"
      ]
        <> args
        <> signArgs
        <> [ "--out-file"
           , outFile
           ]

  assertTxFilesEqual refOutFile outFile

assertTxFilesEqual
  :: forall m
   . (HasCallStack, MonadIO m, MonadTest m, MonadCatch m)
  => FilePath
  -- ^ expected
  -> FilePath
  -- ^ tested
  -> m ()
assertTxFilesEqual f1 f2 = withFrozenCallStack $ do
  tx1 <- viewTx f1
  tx2 <- viewTx f2

  tx1 === tx2
 where
  -- deserialise a transaction from JSON file into a Value
  viewTx :: HasCallStack => FilePath -> m Value
  viewTx f =
    withFrozenCallStack $
      H.leftFailM $
        A.eitherDecode . fromString
          <$> execCardanoCLI
            [ "debug"
            , "transaction"
            , "view"
            , "--tx-file"
            , f
            ]
