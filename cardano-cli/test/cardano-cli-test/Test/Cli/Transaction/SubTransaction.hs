module Test.Cli.Transaction.SubTransaction where

import Control.Monad (void)
import Data.List (isInfixOf)
import System.Exit (ExitCode (..))

import Test.Cardano.CLI.Util

import Hedgehog (Property)
import Hedgehog qualified as H
import Hedgehog.Extras qualified as H

signingKey :: FilePath
signingKey = "test/cardano-cli-golden/files/input/dijkstra/keys/utxo_keys/signing_key"

txIn :: String
txIn = "63e6a9a8e58e48cc025cae04daaed9d36fc7b70bc292721d9f5057ae37b24981#0"

txOut :: String
txOut = "addr_test1vp0t4dfa9ktc2uvv7sg9leafuhtwyu0xcj4q4kf5pqkpjwqhklklg+5000000"

-- | Sub-transactions exist from Dijkstra onwards, so the command group must not
-- parse in Conway.
-- Execute me with:
-- @cabal test cardano-cli-test --test-options '-p "/conway has no sub transaction commands/"'@
hprop_conway_has_no_sub_transaction_commands :: Property
hprop_conway_has_no_sub_transaction_commands =
  watchdogProp . propertyOnce $ do
    (exitCode, _stdout, stderr) <-
      H.noteShowM $
        execDetailCardanoCLI
          [ "conway"
          , "transaction"
          , "sub-transaction"
          , "build-raw"
          , "--tx-in"
          , txIn
          , "--tx-out"
          , txOut
          , "--out-file"
          , "unused"
          ]
    exitCode H.=== ExitFailure 1
    H.assertWith stderr ("Invalid argument `sub-transaction'" `isInfixOf`)

-- | The @--sub-transaction@ option on @build-raw@ must not parse in Conway.
-- Execute me with:
-- @cabal test cardano-cli-test --test-options '-p "/conway build raw has no sub transaction option/"'@
hprop_conway_build_raw_has_no_sub_transaction_option :: Property
hprop_conway_build_raw_has_no_sub_transaction_option =
  watchdogProp . propertyOnce $ do
    (exitCode, _stdout, stderr) <-
      H.noteShowM $
        execDetailCardanoCLI
          [ "conway"
          , "transaction"
          , "build-raw"
          , "--tx-in"
          , txIn
          , "--tx-out"
          , txOut
          , "--fee"
          , "1"
          , "--sub-transaction"
          , "unused"
          , "--out-file"
          , "unused"
          ]
    exitCode H.=== ExitFailure 1
    H.assertWith stderr ("Invalid option `--sub-transaction'" `isInfixOf`)

-- | The same sub-transaction given twice is an error rather than silently
-- collapsing into one.
-- Execute me with:
-- @cabal test cardano-cli-test --test-options '-p "/dijkstra build raw rejects duplicate sub transaction/"'@
hprop_dijkstra_build_raw_rejects_duplicate_sub_transaction :: Property
hprop_dijkstra_build_raw_rejects_duplicate_sub_transaction =
  watchdogProp . propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
    unsignedFile <- H.noteTempFile tempDir "sub-tx.unsigned"
    signedFile <- H.noteTempFile tempDir "sub-tx.signed"
    outFile <- H.noteTempFile tempDir "tx.body"
    void $
      execCardanoCLI
        [ "dijkstra"
        , "transaction"
        , "sub-transaction"
        , "build-raw"
        , "--tx-in"
        , txIn
        , "--tx-out"
        , txOut
        , "--out-file"
        , unsignedFile
        ]
    void $
      execCardanoCLI
        [ "dijkstra"
        , "transaction"
        , "sub-transaction"
        , "sign"
        , "--sub-tx-file"
        , unsignedFile
        , "--signing-key-file"
        , signingKey
        , "--out-file"
        , signedFile
        ]
    (exitCode, _stdout, stderr) <-
      H.noteShowM $
        execDetailCardanoCLI
          [ "dijkstra"
          , "transaction"
          , "build-raw"
          , "--tx-in"
          , txIn
          , "--tx-out"
          , txOut
          , "--fee"
          , "200000"
          , "--sub-transaction"
          , signedFile
          , "--sub-transaction"
          , signedFile
          , "--out-file"
          , outFile
          ]
    exitCode H.=== ExitFailure 1
    H.assertWith stderr ("was provided more than once" `isInfixOf`)

-- | Only signed sub-transactions can be embedded.
-- Execute me with:
-- @cabal test cardano-cli-test --test-options '-p "/dijkstra build raw rejects unsigned sub transaction/"'@
hprop_dijkstra_build_raw_rejects_unsigned_sub_transaction :: Property
hprop_dijkstra_build_raw_rejects_unsigned_sub_transaction =
  watchdogProp . propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
    unsignedFile <- H.noteTempFile tempDir "sub-tx.unsigned"
    outFile <- H.noteTempFile tempDir "tx.body"
    void $
      execCardanoCLI
        [ "dijkstra"
        , "transaction"
        , "sub-transaction"
        , "build-raw"
        , "--tx-in"
        , txIn
        , "--tx-out"
        , txOut
        , "--out-file"
        , unsignedFile
        ]
    (exitCode, _stdout, stderr) <-
      H.noteShowM $
        execDetailCardanoCLI
          [ "dijkstra"
          , "transaction"
          , "build-raw"
          , "--tx-in"
          , txIn
          , "--tx-out"
          , txOut
          , "--fee"
          , "200000"
          , "--sub-transaction"
          , unsignedFile
          , "--out-file"
          , outFile
          ]
    exitCode H.=== ExitFailure 1
    H.assertWith stderr ("Unwitnessed SubTx DijkstraEra" `isInfixOf`)

-- | Byron (bootstrap) witnesses need a top-level body and are refused.
-- Execute me with:
-- @cabal test cardano-cli-test --test-options '-p "/dijkstra sub transaction sign rejects byron key/"'@
hprop_dijkstra_sub_transaction_sign_rejects_byron_key :: Property
hprop_dijkstra_sub_transaction_sign_rejects_byron_key =
  watchdogProp . propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
    unsignedFile <- H.noteTempFile tempDir "sub-tx.unsigned"
    rawByronKeyFile <- H.noteTempFile tempDir "byron.raw.skey"
    byronKeyFile <- H.noteTempFile tempDir "byron.skey"
    outFile <- H.noteTempFile tempDir "sub-tx.signed"
    void $
      execCardanoCLI
        [ "dijkstra"
        , "transaction"
        , "sub-transaction"
        , "build-raw"
        , "--tx-in"
        , txIn
        , "--tx-out"
        , txOut
        , "--out-file"
        , unsignedFile
        ]
    -- A Byron signing key in the TextEnvelope format that `sign` reads
    void $
      execCardanoCLI
        [ "byron"
        , "key"
        , "keygen"
        , "--secret"
        , rawByronKeyFile
        ]
    void $
      execCardanoCLI
        [ "key"
        , "convert-byron-key"
        , "--byron-payment-key-type"
        , "--byron-signing-key-file"
        , rawByronKeyFile
        , "--out-file"
        , byronKeyFile
        ]
    (exitCode, _stdout, stderr) <-
      H.noteShowM $
        execDetailCardanoCLI
          [ "dijkstra"
          , "transaction"
          , "sub-transaction"
          , "sign"
          , "--sub-tx-file"
          , unsignedFile
          , "--signing-key-file"
          , byronKeyFile
          , "--out-file"
          , outFile
          ]
    exitCode H.=== ExitFailure 1
    H.assertWith
      stderr
      ("Byron (bootstrap) witnesses are not supported for sub-transactions" `isInfixOf`)
