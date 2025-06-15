{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Byron.SigningKeys
  ( tasty_deserialise_legacy_signing_Key
  , tasty_deserialise_nonLegacy_signing_Key
  , tasty_deserialise_NonLegacy_Signing_Key_API
  , tasty_deserialiseLegacy_Signing_Key_API
  , tasty_generate_and_read_nonlegacy_signingkeys
  , tasty_migrate_legacy_to_nonlegacy_signingkeys
  , tasty_print_legacy_signing_key_address
  , tasty_print_nonLegacy_signing_key_address
  )
where

import Cardano.Api.Byron
import Cardano.Api.Key
import Cardano.Api.Monad.Error
import Cardano.Api.Serialise.Raw

import Cardano.CLI.Byron.Key (readByronSigningKey)
import Cardano.CLI.Byron.Legacy (decodeLegacyDelegateKey)
import Cardano.CLI.Type.Common
import Cardano.Crypto.Signing qualified as Crypto

import Codec.CBOR.Read (deserialiseFromBytes)
import Control.Monad (void)
import Data.ByteString.Lazy qualified as LB

import Test.Cardano.CLI.Util

import Hedgehog (success)
import Hedgehog qualified as H
import Hedgehog.Extras (UnitIO)
import Hedgehog.Extras.Test.Base qualified as H
import Hedgehog.Internal.Property (failWith)

tasty_deserialise_legacy_signing_Key :: UnitIO ()
tasty_deserialise_legacy_signing_Key = do
  legSkeyBs <- H.evalIO $ LB.readFile "test/cardano-cli-golden/files/input/byron/keys/legacy.skey"
  case deserialiseFromBytes decodeLegacyDelegateKey legSkeyBs of
    Left deSerFail -> failWith Nothing $ show deSerFail
    Right _ -> success

tasty_deserialise_nonLegacy_signing_Key :: UnitIO ()
tasty_deserialise_nonLegacy_signing_Key = do
  skeyBs <- H.evalIO $ LB.readFile "test/cardano-cli-golden/files/input/byron/keys/byron.skey"
  case deserialiseFromBytes Crypto.fromCBORXPrv skeyBs of
    Left deSerFail -> failWith Nothing $ show deSerFail
    Right _ -> success

tasty_print_legacy_signing_key_address :: UnitIO ()
tasty_print_legacy_signing_key_address = do
  let legKeyFp = "test/cardano-cli-golden/files/input/byron/keys/legacy.skey"

  void $
    execCardanoCLI
      [ "signing-key-address"
      , "--byron-legacy-formats"
      , "--testnet-magic"
      , "42"
      , "--secret"
      , legKeyFp
      ]

  void $
    execCardanoCLI
      [ "signing-key-address"
      , "--byron-legacy-formats"
      , "--mainnet"
      , "--secret"
      , legKeyFp
      ]

tasty_print_nonLegacy_signing_key_address :: UnitIO ()
tasty_print_nonLegacy_signing_key_address = do
  let nonLegKeyFp = "test/cardano-cli-golden/files/input/byron/keys/byron.skey"

  void $
    execCardanoCLI
      [ "signing-key-address"
      , "--byron-formats"
      , "--testnet-magic"
      , "42"
      , "--secret"
      , nonLegKeyFp
      ]

  void $
    execCardanoCLI
      [ "signing-key-address"
      , "--byron-formats"
      , "--mainnet"
      , "--secret"
      , nonLegKeyFp
      ]

tasty_generate_and_read_nonlegacy_signingkeys :: UnitIO ()
tasty_generate_and_read_nonlegacy_signingkeys = do
  byronSkey <- H.evalIO $ generateSigningKey AsByronKey
  case deserialiseFromRawBytes (AsSigningKey AsByronKey) (serialiseToRawBytes byronSkey) of
    Left _ -> failWith Nothing "Failed to deserialise non-legacy Byron signing key. "
    Right _ -> success

tasty_migrate_legacy_to_nonlegacy_signingkeys :: UnitIO ()
tasty_migrate_legacy_to_nonlegacy_signingkeys =
  H.moduleWorkspace "tmp" $ \tempDir -> do
    let legKeyFp = "test/cardano-cli-golden/files/input/byron/keys/legacy.skey"
    nonLegacyKeyFp <- noteTempFile tempDir "nonlegacy.skey"

    void $
      execCardanoCLI
        [ "migrate-delegate-key-from"
        , "--from"
        , legKeyFp
        , "--to"
        , nonLegacyKeyFp
        ]

    eSignKey <-
      H.evalIO . runExceptT . readByronSigningKey NonLegacyByronKeyFormat $
        File nonLegacyKeyFp

    case eSignKey of
      Left err -> failWith Nothing $ show err
      Right _ -> success

tasty_deserialise_NonLegacy_Signing_Key_API :: UnitIO ()
tasty_deserialise_NonLegacy_Signing_Key_API = do
  eFailOrWit <-
    H.evalIO . runExceptT $
      readByronSigningKey
        NonLegacyByronKeyFormat
        "test/cardano-cli-golden/files/input/byron/keys/byron.skey"
  case eFailOrWit of
    Left keyFailure -> failWith Nothing $ show keyFailure
    Right _ -> success

tasty_deserialiseLegacy_Signing_Key_API :: UnitIO ()
tasty_deserialiseLegacy_Signing_Key_API = do
  eFailOrWit <-
    H.evalIO . runExceptT $
      readByronSigningKey
        LegacyByronKeyFormat
        "test/cardano-cli-golden/files/input/byron/keys/legacy.skey"
  case eFailOrWit of
    Left keyFailure -> failWith Nothing $ show keyFailure
    Right _ -> success
