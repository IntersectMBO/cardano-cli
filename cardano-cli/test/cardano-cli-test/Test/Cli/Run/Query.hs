module Test.Cli.Run.Query
  ( hprop_percentage
  , hprop_query_stake_address_info_network_id_mismatch
  )
where

import Cardano.CLI.EraBased.Query.Run qualified as Q
import Cardano.Slotting.Time (RelativeTime (..))

import Data.List (isInfixOf)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))

import Test.Cardano.CLI.Util (execDetailCardanoCLI, watchdogProp)

import Hedgehog (Property, (===))
import Hedgehog.Extras qualified as H

hprop_percentage :: Property
hprop_percentage =
  watchdogProp . H.propertyOnce $ do
    Q.percentage (RelativeTime 10) (RelativeTime 1000) (RelativeTime 1000) === "100.00"
    Q.percentage (RelativeTime 10) (RelativeTime 990) (RelativeTime 1000) === "100.00"
    Q.percentage (RelativeTime 10) (RelativeTime 980) (RelativeTime 1000) === "99.00"
    Q.percentage (RelativeTime 10) (RelativeTime 500) (RelativeTime 1000) === "51.05"
    Q.percentage (RelativeTime 10) (RelativeTime 0) (RelativeTime 1000) === "1.10"
    return ()

-- | The command must fail before attempting to connect to the node (the given
-- socket does not exist), because the mainnet stake address cannot match the
-- given testnet network id.
--
-- Execute me with:
-- @cabal test cardano-cli-test --test-options '-p "/query stake address info network id mismatch/"'@
hprop_query_stake_address_info_network_id_mismatch :: Property
hprop_query_stake_address_info_network_id_mismatch =
  watchdogProp . H.propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
    (exitCode, _stdout, stderr) <-
      H.noteShowM $
        execDetailCardanoCLI
          [ "latest"
          , "query"
          , "stake-address-info"
          , "--testnet-magic"
          , "2"
          , "--address"
          , "stake1uxqmgfzls3vn7c7qlu3fdycz2nmh5p5sl2w7t7tfetp8evqacghf3"
          , "--socket-path"
          , tempDir </> "unused.socket"
          ]

    exitCode === ExitFailure 1
    H.assertWith
      stderr
      ("is a mainnet stake address, but the command was given a testnet with network magic 2" `isInfixOf`)
