module Test.Cli.CreateTestnetData where



import           System.FilePath

import           Test.Cardano.CLI.Util (execCardanoCLI)

import           Hedgehog (Property)
import           Hedgehog.Extras (moduleWorkspace, propertyOnce)
import qualified Hedgehog.Extras as H

-- | Test case for https://github.com/IntersectMBO/cardano-cli/issues/587
-- Execute this test with:
-- @cabal test cardano-cli-test --test-options '-p "/create testnet data minimal/"'@
hprop_create_testnet_data_minimal :: Property
hprop_create_testnet_data_minimal =
  propertyOnce $ moduleWorkspace "tmp" $ \tempDir -> do

    let outputDir = tempDir </> "out"

    H.noteM_ $ execCardanoCLI
      ["conway",  "genesis", "create-testnet-data"
      , "--testnet-magic", "42"
      , "--out-dir", outputDir
      ]

    -- We test that the command doesn't crash, because otherwise
    -- execCardanoCLI would fail.
