{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cli.Compatible.Transaction.Build where

import Cardano.Api.Internal.Eras
import Cardano.Api.Internal.Pretty

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

-- | Execute me with:
-- @cabal test cardano-cli-test --test-options '-p "/conway transaction build one voter many votes/"'@
hprop_compatible_conway_transaction_build_one_voter_many_votes :: Property
hprop_compatible_conway_transaction_build_one_voter_many_votes =
  watchdogProp . propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
    refOutFile <- H.noteTempFile tempDir "reference.tx.json"
    outFile <- H.noteTempFile tempDir "txbody.tx.json"
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
hprop_compatible_shelley_create_update_proposal =
  watchdogProp . propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
    refOutFile <- H.noteTempFile tempDir "ref_update-proposal_allegra.proposal"
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
        [ "compatible"
        , "allegra"
        , "governance"
        , "action"
        , "create-protocol-parameters-update"
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
