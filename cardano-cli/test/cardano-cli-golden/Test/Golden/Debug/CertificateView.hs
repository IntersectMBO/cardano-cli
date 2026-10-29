module Test.Golden.Debug.CertificateView where

import System.FilePath ((<.>), (</>))

import Test.Cardano.CLI.Util (execCardanoCLI, propertyOnce, watchdogProp)

import Hedgehog.Extras.Test qualified as H
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

inputDir, goldenDir :: FilePath
inputDir = "test/cardano-cli-golden/files/input/certificate-view"
goldenDir = "test/cardano-cli-golden/files/golden/certificate-view"

certificates :: [FilePath]
certificates =
  [ "cc-cold-key-resignation"
  , "cc-hot-key-authorization"
  , "dijkstra-pool-registration"
  , "drep-registration"
  , "drep-retirement"
  , "drep-update"
  , "pool-registration"
  , "pool-registration-ipv6"
  , "pool-registration-multihost"
  , "pool-registration-singlehost"
  , "pool-retirement"
  , "registration-and-delegation"
  , "registration-and-vote-delegation"
  , "registration-stake-and-vote-delegation"
  , "stake-and-vote-delegation"
  , "stake-delegation"
  , "stake-deregistration"
  , "stake-registration"
  , "stake-registration-no-deposit"
  , "vote-delegation-abstain"
  , "vote-delegation-key"
  , "vote-delegation-noconfidence"
  ]

-- | Execute me with:
-- @cabal test cardano-cli-golden --test-options '-p "/debug certificate view/"'@
test_golden_debug_certificate_view :: TestTree
test_golden_debug_certificate_view =
  testGroup
    "debug certificate view"
    [ testProperty (certificate <.> format) . watchdogProp . propertyOnce $ do
        result <-
          execCardanoCLI
            [ "debug"
            , "certificate"
            , "view"
            , "--certificate-file"
            , inputDir </> certificate <.> "cert"
            , "--output-" <> format
            ]
        H.diffVsGoldenFile result (goldenDir </> certificate <.> format)
    | certificate <- certificates
    , format <- ["json", "yaml"]
    ]
