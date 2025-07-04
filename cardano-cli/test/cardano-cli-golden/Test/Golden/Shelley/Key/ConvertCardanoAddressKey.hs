{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.Key.ConvertCardanoAddressKey where

import Control.Monad (void)
import Control.Monad.Extra (forM_)
import Data.Text (Text)
import System.FilePath.Posix ((</>))

import Test.Cardano.CLI.Aeson qualified as Aeson
import Test.Cardano.CLI.Util
import Test.Cardano.CLI.Util qualified as H

import Hedgehog (Property)
import Hedgehog.Extras.Test qualified as H hiding (noteTempFile)

{- HLINT ignore "Use camelCase" -}

-- | An example signing key generated by @cardano-address@ using the
-- deprecated Byron style.
exampleByronSigningKey :: Text
exampleByronSigningKey =
  "xprv1pp72a64en2vf568jywe9azlgrqe3p2jjf9gxxeejn2fex8g889x54w6emg2egkaz2rxyc"
    <> "560fp0hrv8y0hzpuzu27zhhhgwc8t5tvrczz2jnhhjwdnd6cdjx4dxehrsr2pr406rchw"
    <> "ctfwrgpc9r7nmakvaegyz9"

-- | An example signing key generated by @cardano-address@ using the Icarus
-- style.
exampleIcarusSigningKey :: Text
exampleIcarusSigningKey =
  "xprv1yq7c6nlmxncg7txy0z6lqf3fww4vm20m60lrxttx5lr4qmkvh395m3p59v8fn4ku9mzyc"
    <> "g2rkxatgwm86uc3pvrt06e43afya6rm0s2azlpnc9yrhygl2heckeyhhtgad08c0zljpn"
    <> "c6fse2ldzyx9c86yvddxjw"

-- | An example signing key generated by @cardano-address@ using the Shelley
-- style.
exampleShelleySigningKey :: Text
exampleShelleySigningKey =
  "xprv1yq7c6nlmxncg7txy0z6lqf3fww4vm20m60lrxttx5lr4qmkvh395m3p59v8fn4ku9mzyc"
    <> "g2rkxatgwm86uc3pvrt06e43afya6rm0s2azlpnc9yrhygl2heckeyhhtgad08c0zljpn"
    <> "c6fse2ldzyx9c86yvddxjw"

-- | Test that converting a @cardano-address@ Byron signing key yields the
-- expected result.
hprop_golden_convertCardanoAddressByronSigningKey :: Property
hprop_golden_convertCardanoAddressByronSigningKey =
  watchdogProp . propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    -- `cardano-address` signing key filepath
    signingKeyFp <- noteTempFile tempDir "cardano-address-byron.skey"

    -- Converted signing key filepath
    convertedSigningKeyFp <-
      noteTempFile tempDir "converted-cardano-address-byron.skey"

    -- Write `cardano-address` signing key to disk
    H.textWriteFile signingKeyFp exampleByronSigningKey
    H.assertFilesExist [signingKeyFp]

    -- Convert the `cardano-address` signing key
    void $
      execCardanoCLI
        [ "latest"
        , "key"
        , "convert-cardano-address-key"
        , "--byron-payment-key"
        , "--signing-key-file"
        , signingKeyFp
        , "--out-file"
        , convertedSigningKeyFp
        ]

    -- Check for existence of the converted signing key file
    H.assertFilesExist [convertedSigningKeyFp]

    -- Check that the contents of the converted signing key file match that of
    -- the golden file.
    H.diffFileVsGoldenFile
      convertedSigningKeyFp
      "test/cardano-cli-golden/files/golden/shelley/keys/converted_cardano-address_keys/byron_signing_key"

-- | Test that converting a @cardano-address@ Icarus signing key yields the
-- expected result.
hprop_golden_convertCardanoAddressIcarusSigningKey :: Property
hprop_golden_convertCardanoAddressIcarusSigningKey =
  watchdogProp . propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    -- `cardano-address` signing key filepath
    signingKeyFp <- H.noteTempFile tempDir "cardano-address-icarus.skey"

    -- Converted signing key filepath
    convertedSigningKeyFp <-
      noteTempFile tempDir "converted-cardano-address-icarus.skey"

    -- Write `cardano-address` signing key to disk
    H.textWriteFile signingKeyFp exampleIcarusSigningKey
    H.assertFilesExist [signingKeyFp]

    -- Convert the `cardano-address` signing key
    void $
      execCardanoCLI
        [ "latest"
        , "key"
        , "convert-cardano-address-key"
        , "--icarus-payment-key"
        , "--signing-key-file"
        , signingKeyFp
        , "--out-file"
        , convertedSigningKeyFp
        ]

    -- Check for existence of the converted signing key file
    H.assertFilesExist [convertedSigningKeyFp]

    -- Check that the contents of the converted signing key file match that of
    -- the golden file.
    H.diffFileVsGoldenFile
      convertedSigningKeyFp
      "test/cardano-cli-golden/files/golden/shelley/keys/converted_cardano-address_keys/icarus_signing_key"

-- | Test that converting a @cardano-address@ Shelley payment signing key
-- yields the expected result.
hprop_golden_convertCardanoAddressShelleyPaymentSigningKey :: Property
hprop_golden_convertCardanoAddressShelleyPaymentSigningKey =
  watchdogProp . propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    -- `cardano-address` signing key filepath
    signingKeyFp <-
      noteTempFile tempDir "cardano-address-shelley-payment.skey"

    -- Converted signing key filepath
    convertedSigningKeyFp <-
      noteTempFile tempDir "converted-cardano-address-shelley-payment.skey"

    -- Write `cardano-address` signing key to disk
    H.textWriteFile signingKeyFp exampleShelleySigningKey
    H.assertFilesExist [signingKeyFp]

    -- Convert the `cardano-address` signing key
    void $
      execCardanoCLI
        [ "latest"
        , "key"
        , "convert-cardano-address-key"
        , "--shelley-payment-key"
        , "--signing-key-file"
        , signingKeyFp
        , "--out-file"
        , convertedSigningKeyFp
        ]

    -- Check for existence of the converted signing key file
    H.assertFilesExist [convertedSigningKeyFp]

    -- Check that the contents of the converted signing key file match that of
    -- the golden file.
    H.diffFileVsGoldenFile
      convertedSigningKeyFp
      "test/cardano-cli-golden/files/golden/shelley/keys/converted_cardano-address_keys/shelley_payment_signing_key"

-- | Test that converting a @cardano-address@ Shelley stake signing key yields
-- the expected result.
hprop_golden_convertCardanoAddressShelleyStakeSigningKey :: Property
hprop_golden_convertCardanoAddressShelleyStakeSigningKey =
  watchdogProp . propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    -- `cardano-address` signing key filepath
    signingKeyFp <-
      noteTempFile tempDir "cardano-address-shelley-stake.skey"

    -- Converted signing key filepath
    convertedSigningKeyFp <-
      H.noteTempFile tempDir "converted-cardano-address-shelley-stake.skey"

    -- Write `cardano-address` signing key to disk
    H.textWriteFile signingKeyFp exampleShelleySigningKey
    H.assertFilesExist [signingKeyFp]

    -- Convert the `cardano-address` signing key
    void $
      execCardanoCLI
        [ "latest"
        , "key"
        , "convert-cardano-address-key"
        , "--shelley-stake-key"
        , "--signing-key-file"
        , signingKeyFp
        , "--out-file"
        , convertedSigningKeyFp
        ]

    -- Check for existence of the converted signing key file
    H.assertFilesExist [convertedSigningKeyFp]

    -- Check that the contents of the converted signing key file match that of
    -- the golden file.
    H.diffFileVsGoldenFile
      convertedSigningKeyFp
      "test/cardano-cli-golden/files/golden/shelley/keys/converted_cardano-address_keys/shelley_stake_signing_key"

-- | Test that converting a @cardano-address@ CC/DRep signing key
-- yields the expected result.
-- Execute me with:
-- @cabal test cardano-cli-golden --test-options '-p "/convert cardano address cc drep/"'@
hprop_golden_convert_cardano_address_cc_drep :: Property
hprop_golden_convert_cardano_address_cc_drep = do
  let supplyValues =
        [ ("cc_cold.key", "--cc-cold-key", "Constitutional Committee Cold")
        , ("cc_hot.key", "--cc-hot-key", "Constitutional Committee Hot")
        , ("drep.key", "--drep-key", "Delegated Representative")
        ]

  watchdogProp . propertyOnce $ forM_ supplyValues $ \(filename, flag, descPrefix) -> H.moduleWorkspace "tmp" $ \tempDir -> do
    let outFile = tempDir </> "out.json"

    -- `cardano-address` signing key filepath
    signingKeyFp <-
      H.noteInputFile $ "test/cardano-cli-golden/files/input/shelley/convert-cardano-address/" <> filename

    -- Convert the `cardano-address` signing key
    H.noteShowM_ $
      execCardanoCLI
        [ "latest"
        , "key"
        , "convert-cardano-address-key"
        , flag
        , "--signing-key-file"
        , signingKeyFp
        , "--out-file"
        , outFile
        ]

    H.diffFileVsGoldenFile
      outFile
      ("test/cardano-cli-golden/files/golden/shelley/keys/converted_cardano-address_keys/" <> filename)

    Aeson.assertHasMappings [("description", descPrefix <> " Extended Signing Key")] outFile
