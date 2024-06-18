{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Golden.Byron.SigningKeys
  ( hprop_deserialise_legacy_signing_Key
  , hprop_deserialise_nonLegacy_signing_Key
  , hprop_deserialise_NonLegacy_Signing_Key_API
  , hprop_deserialiseLegacy_Signing_Key_API
  , hprop_generate_and_read_nonlegacy_signingkeys
  , hprop_migrate_legacy_to_nonlegacy_signingkeys
  , hprop_print_legacy_signing_key_address
  , hprop_print_nonLegacy_signing_key_address
  ) where

import           Cardano.CLI.Byron.Legacy (decodeLegacyDelegateKey)
import           Cardano.CLI.Polysemy
import qualified Cardano.Crypto.Signing as Crypto

import           Codec.CBOR.Read (DeserialiseFailure, deserialiseFromBytes)

import           Test.Cardano.Api.Polysemy
import           Test.Cardano.CLI.Polysemy

import           HaskellWorks.Polysemy
import qualified HaskellWorks.Polysemy.Data.ByteString.Lazy as LBS
import           HaskellWorks.Polysemy.Hedgehog
import           HaskellWorks.Prelude

hprop_deserialise_legacy_signing_Key :: Property
hprop_deserialise_legacy_signing_Key = propertyOnce $ localWorkspace $ do
  legSkeyBs <- LBS.readFile "test/cardano-cli-golden/files/input/byron/keys/legacy.skey"
    & trapFail @IOException

  fromEither (deserialiseFromBytes decodeLegacyDelegateKey legSkeyBs)
    & trapFail @DeserialiseFailure
    & void

hprop_deserialise_nonLegacy_signing_Key :: Property
hprop_deserialise_nonLegacy_signing_Key = propertyOnce $ localWorkspace $ do
  skeyBs <- LBS.readFile "test/cardano-cli-golden/files/input/byron/keys/byron.skey"
    & trapFail @IOException

  fromEither (deserialiseFromBytes Crypto.fromCBORXPrv skeyBs)
    & trapFail @DeserialiseFailure
    & void

hprop_print_legacy_signing_key_address :: Property
hprop_print_legacy_signing_key_address = propertyOnce $ localWorkspace $ do
  legKeyFp <- jotPkgInputFile "test/cardano-cli-golden/files/input/byron/keys/legacy.skey"

  execCardanoCli_
    [ "signing-key-address", "--byron-legacy-formats"
    , "--testnet-magic", "42"
    , "--secret", legKeyFp
    ]

  execCardanoCli_
    [ "signing-key-address", "--byron-legacy-formats"
    , "--mainnet"
    , "--secret", legKeyFp
    ]

hprop_print_nonLegacy_signing_key_address :: Property
hprop_print_nonLegacy_signing_key_address = propertyOnce $ localWorkspace $ do
  nonLegKeyFp <- jotPkgInputFile "test/cardano-cli-golden/files/input/byron/keys/byron.skey"

  execCardanoCli_
    [ "signing-key-address", "--byron-formats"
    , "--testnet-magic", "42"
    , "--secret", nonLegKeyFp
    ]

  execCardanoCli_
    [ "signing-key-address", "--byron-formats"
    , "--mainnet"
    , "--secret", nonLegKeyFp
    ]

hprop_generate_and_read_nonlegacy_signingkeys :: Property
hprop_generate_and_read_nonlegacy_signingkeys = propertyOnce $ localWorkspace $ do
  byronSkey <- generateSigningKey AsByronKey

  fromEither (deserialiseFromRawBytes (AsSigningKey AsByronKey) (serialiseToRawBytes byronSkey))
    -- Failed to deserialise non-legacy Byron signing key
    & trapFail @SerialiseAsRawBytesError
    & void

hprop_migrate_legacy_to_nonlegacy_signingkeys :: Property
hprop_migrate_legacy_to_nonlegacy_signingkeys = propertyOnce $ localWorkspace $ do
  legKeyFp <- jotPkgInputFile "test/cardano-cli-golden/files/input/byron/keys/legacy.skey"
  nonLegacyKeyFp <- jotTempFile "nonlegacy.skey"

  execCardanoCli_
    [ "migrate-delegate-key-from"
    , "--from", legKeyFp
    , "--to", nonLegacyKeyFp
    ]

  readByronSigningKey NonLegacyByronKeyFormat (File nonLegacyKeyFp)
    & trapFail @ByronKeyFailure
    & void

hprop_deserialise_NonLegacy_Signing_Key_API :: Property
hprop_deserialise_NonLegacy_Signing_Key_API = propertyOnce $ localWorkspace $ do
  readByronSigningKey NonLegacyByronKeyFormat "test/cardano-cli-golden/files/input/byron/keys/byron.skey"
    & trapFail @ByronKeyFailure
    & void

hprop_deserialiseLegacy_Signing_Key_API :: Property
hprop_deserialiseLegacy_Signing_Key_API = propertyOnce $ localWorkspace $ do
  readByronSigningKey LegacyByronKeyFormat "test/cardano-cli-golden/files/input/byron/keys/legacy.skey"
    & trapFail @ByronKeyFailure
    & void
