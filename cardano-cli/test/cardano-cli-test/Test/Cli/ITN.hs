{-# LANGUAGE OverloadedStrings #-}

module Test.Cli.ITN
  ( hprop_convertITNBIP32SigningKey
  , hprop_convertITNExtendedSigningKey
  , hprop_convertITNKeys
  , hprop_golden_bech32Decode
  )
where

import Cardano.CLI.EraIndependent.Key.Run (decodeBech32)

import Codec.Binary.Bech32 qualified as Bech32
import Control.Monad (void)
import Data.ByteString (ByteString)
import Data.ByteString.Base16 qualified as Base16
import Data.Text (Text)
import Data.Text.IO qualified as Text

import Test.Cardano.CLI.Util

import Hedgehog (Property, (===))
import Hedgehog qualified as H
import Hedgehog.Extras.Test.Base qualified as H
import Hedgehog.Extras.Test.File qualified as H

{- HLINT ignore "Reduce duplication" -}

-- | Bech32 verification key
itnVerKey :: Text
itnVerKey = "ed25519_pk1demeytzdadayd4qrqeg2raadp2eceg3mrdmefxyfxx73q60hg4xsjjyzyq"

-- | Bech32 signing key
itnSignKey :: Text
itnSignKey = "ed25519_sk1yhnetcmla9pskrvp5z5ff2v8gkenhmluy736jd6nrxrlxcgn70zsy94f7k"

-- | 1. Convert a bech32 ITN key pair to a haskell stake verification key and signing key
--   2. Derive the haskell verification key from the haskell signing key.
hprop_convertITNKeys :: Property
hprop_convertITNKeys =
  watchdogProp . propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    -- ITN input file paths
    itnVerKeyFp <- noteTempFile tempDir "itnVerKey.key"
    itnSignKeyFp <- noteTempFile tempDir "itnSignKey.key"

    -- Converted keys output file paths
    outputHaskellVerKeyFp <- noteTempFile tempDir "haskell-verification-key.key"
    outputHaskellSignKeyFp <- noteTempFile tempDir "haskell-signing-key.key"

    -- Write ITN keys to disk
    H.evalIO $ Text.writeFile itnVerKeyFp itnVerKey
    H.evalIO $ Text.writeFile itnSignKeyFp itnSignKey
    H.assertFilesExist [itnVerKeyFp, itnSignKeyFp]

    -- Generate haskell stake verification key
    void $
      execCardanoCLI
        [ "latest"
        , "key"
        , "convert-itn-key"
        , "--itn-verification-key-file"
        , itnVerKeyFp
        , "--out-file"
        , outputHaskellVerKeyFp
        ]
    -- Generate haskell signing key
    void $
      execCardanoCLI
        [ "latest"
        , "key"
        , "convert-itn-key"
        , "--itn-signing-key-file"
        , itnSignKeyFp
        , "--out-file"
        , outputHaskellSignKeyFp
        ]

    -- Check for existence of the converted ITN keys
    H.assertFilesExist [outputHaskellVerKeyFp, outputHaskellSignKeyFp]

-- | 1. Convert a bech32 ITN extended signing key to a haskell stake signing key
hprop_convertITNExtendedSigningKey :: Property
hprop_convertITNExtendedSigningKey =
  watchdogProp . propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    let itnExtendedSignKey =
          mconcat
            [ "ed25519e_sk1qpcplz38tg4fusw0fkqljzspe9qmj06ldu9lgcve99v4fphuk9a535kwj"
            , "f38hkyn0shcycyaha4k9tmjy6xgvzaz7stw5t7rqjadyjcwfyx6k"
            ]

    -- ITN input file paths
    itnSignKeyFp <- noteTempFile tempDir "itnExtendedSignKey.key"

    -- Converted keys output file paths
    outputHaskellSignKeyFp <- noteTempFile tempDir "stake-signing.key"

    -- Write ITN keys to disk
    H.evalIO $ writeFile itnSignKeyFp itnExtendedSignKey
    H.assertFilesExist [itnSignKeyFp]

    -- Generate haskell signing key
    void $
      execCardanoCLI
        [ "latest"
        , "key"
        , "convert-itn-extended-key"
        , "--itn-signing-key-file"
        , itnSignKeyFp
        , "--out-file"
        , outputHaskellSignKeyFp
        ]

    -- Check for existence of the converted ITN keys
    H.assertFilesExist [outputHaskellSignKeyFp]

-- | 1. Convert a bech32 ITN BIP32 signing key to a haskell stake signing key
hprop_convertITNBIP32SigningKey :: Property
hprop_convertITNBIP32SigningKey =
  watchdogProp . propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    let itnExtendedSignKey =
          mconcat
            [ "xprv1spkw5suj39723c40mr55gwh7j3vryjv2zdm4e47xs0deka"
            , "jcza9ud848ckdqf48md9njzc5pkujfxwu2j8wdvtxkx02n3s2qa"
            , "euhqnfx6zu9dyccpua6vf5x3kur9hsganq2kl0yw7y9hpunts0e9kc5xv3pz0yj"
            ]

    -- ITN input file paths
    itnSignKeyFp <- noteTempFile tempDir "itnBIP32SignKey.key"

    -- Converted keys output file paths
    outputHaskellSignKeyFp <- noteTempFile tempDir "stake-signing.key"

    -- Write ITN keys to disk
    H.evalIO $ writeFile itnSignKeyFp itnExtendedSignKey

    H.assertFilesExist [itnSignKeyFp]

    -- Generate haskell signing key
    void $
      execCardanoCLI
        [ "latest"
        , "key"
        , "convert-itn-bip32-key"
        , "--itn-signing-key-file"
        , itnSignKeyFp
        , "--out-file"
        , outputHaskellSignKeyFp
        ]

    -- Check for existence of the converted ITN keys
    H.assertFilesExist [outputHaskellSignKeyFp]

-- | We check our 'decodeBech32' outputs against https://slowli.github.io/bech32-buffer/
-- using 'itnVerKey' & 'itnSignKey' as inputs.
hprop_golden_bech32Decode :: Property
hprop_golden_bech32Decode =
  watchdogProp . propertyOnce $ do
    (vHumReadPart, vDataPart, _) <- H.evalEither $ decodeBech32 itnVerKey
    Just vDataPartBase16 <- pure (dataPartToBase16 vDataPart)

    (sHumReadPart, sDataPart, _) <- H.evalEither $ decodeBech32 itnSignKey
    Just sDataPartBase16 <- pure (dataPartToBase16 sDataPart)

    -- Based on https://slowli.github.io/bech32-buffer/ which are in Base16
    let expectedHumanReadPartVerificationKey = "ed25519_pk"
        expectedDataPartVerificationKey = "6e77922c4deb7a46d4030650a1f7ad0ab38ca23b1b7794988931bd1069f7454d"
        expectedHumanReadPartSigningKey = "ed25519_sk"
        expectedDataPartSigningKey = "25e795e37fe9430b0d81a0a894a98745b33beffc27a3a937531987f36113f3c5"

    -- ITN Verification key decode check
    expectedHumanReadPartVerificationKey === Bech32.humanReadablePartToText vHumReadPart
    expectedDataPartVerificationKey === vDataPartBase16

    -- ITN Signing key decode check
    expectedHumanReadPartSigningKey === Bech32.humanReadablePartToText sHumReadPart
    expectedDataPartSigningKey === sDataPartBase16
 where
  dataPartToBase16 :: Bech32.DataPart -> Maybe ByteString
  dataPartToBase16 = fmap Base16.encode . Bech32.dataPartToBytes
