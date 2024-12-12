{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cli.Shelley.Transaction.Compatible.Build where

import           Control.Monad.Catch (MonadCatch)
import           Control.Monad.IO.Class
import           Data.Aeson (Value)
import qualified Data.Aeson as A
import           Data.String (IsString (..))
import           GHC.Stack

import           Test.Cardano.CLI.Util

import           Hedgehog
import qualified Hedgehog as H
import qualified Hedgehog.Extras as H

inputDir :: FilePath
inputDir = "test/cardano-cli-test/files/input/shelley/transaction"

-- | Execute me with:
-- @cabal test cardano-cli-test --test-options '-p "/conway transaction build one voter many votes/"'@
hprop_compatible_conway_transaction_build_one_voter_many_votes :: Property
hprop_compatible_conway_transaction_build_one_voter_many_votes = propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
  refOutFile <- H.noteTempFile tempDir "reference_tx.traw"
  outFile <- H.noteTempFile tempDir "tx.traw"

  let args =
        [ "--tx-in"
        , "6e8c947816e82627aeccb55300074f2894a2051332f62a1c8954e7b588a18be7#0"
        , "--tx-out"
        , "addr_test1vpfwv0ezc5g8a4mkku8hhy3y3vp92t7s3ul8g778g5yegsgalc6gc+24910487859"
        , "--fee"
        , "178569"
        , "--certificate-file"
        , "test/cardano-cli-golden/files/golden/shelley/stake-address/reg-certificate-2.json"
        , "--certificate-script-file"
        , "test/cardano-cli-golden/files/input/AlwaysSucceeds.plutus"
        , "--certificate-redeemer-value"
        , "0"
        , "--certificate-execution-units"
        , "(0,0)"
        ]

  _ <-
    execCardanoCLI $
      [ "conway"
      , "transaction"
      , "build-raw"
      ]
        <> args
        <> [ "--out-file"
           , refOutFile
           ]

  _ <-
    execCardanoCLI $
      [ "compatible"
      , "conway"
      , "transaction"
      , "signed-transaction"
      ]
        <> args
        <> [ "--out-file"
           , outFile
           ]

  assertTxFilesEqual refOutFile outFile
  H.failure

assertTxFilesEqual
  :: forall m. (HasCallStack, MonadIO m, MonadTest m, MonadCatch m) => FilePath -> FilePath -> m ()
assertTxFilesEqual f1 f2 = withFrozenCallStack $ do
  tx1 <- viewTx f1
  tx2 <- viewTx f2

  tx1 === tx2
 where
  viewTx :: HasCallStack => FilePath -> m Value
  viewTx f =
    withFrozenCallStack $
      H.leftFailM $
        A.eitherDecode . fromString
          <$> execCardanoCLI
            [ "debug"
            , "transaction"
            , "view"
            , "--tx-body-file"
            , f
            ]
