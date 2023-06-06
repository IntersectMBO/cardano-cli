{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Config.Mainnet
  ( tests
  ) where

import           Cardano.Api (File (..), initialLedgerState, renderInitialLedgerStateError)
import           Control.Monad.Trans.Except
import           Hedgehog (Property, (===))
import           System.FilePath ((</>))

import qualified Data.Aeson as J
import qualified Data.Text as T
import qualified Data.Yaml as Y
import qualified GHC.Stack as GHC
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.Process as H
import qualified System.Directory as IO

hprop_configMainnetHash :: Property
hprop_configMainnetHash = H.propertyOnce $ do
  base <- H.note =<< H.evalIO . IO.canonicalizePath =<< H.getProjectBase
  result <- H.evalIO $ runExceptT $ initialLedgerState $ File $ base </> "configuration/cardano/mainnet-config.json"
  case result of
    Right (_, _) -> return ()
    Left e -> H.failWithCustom GHC.callStack Nothing (T.unpack (renderInitialLedgerStateError e))

hprop_configMainnetYaml :: Property
hprop_configMainnetYaml = H.propertyOnce $ do
  base <- H.note =<< H.evalIO . IO.canonicalizePath =<< H.getProjectBase
  yamlResult <- H.evalIO . Y.decodeFileEither $ base </> "configuration/cardano/mainnet-config.yaml"
  yaml :: J.Value <- case yamlResult of
    Right v -> return v
    Left e -> H.failWithCustom GHC.callStack Nothing (Y.prettyPrintParseException e)
  jsonResult <- H.evalIO . J.eitherDecodeFileStrict $ base </> "configuration/cardano/mainnet-config.json"
  json  :: J.Value <- case jsonResult of
    Right v -> return v
    Left e -> H.failWithCustom GHC.callStack Nothing (show e)
  yaml === json

tests :: IO Bool
tests =
  H.checkSequential
    $ H.Group "Test.Config.Mainnet"
        [ ("hprop_configMainnetHash", hprop_configMainnetHash)
        , ("hprop_configMainnetYaml", hprop_configMainnetYaml)
        ]
