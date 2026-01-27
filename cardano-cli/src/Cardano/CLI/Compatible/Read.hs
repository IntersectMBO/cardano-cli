{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.CLI.Compatible.Read
  ( AnyPlutusScript (..)
  , readFilePlutusScript
  , readFileSimpleScript
  )
where

import Cardano.Api as Api

import Cardano.CLI.Compatible.Exception
import Cardano.CLI.Read (readFileCli)
import Cardano.CLI.Type.Error.PlutusScriptDecodeError
import Cardano.CLI.Type.Error.ScriptDecodeError

import Prelude

import Data.Aeson qualified as Aeson
import Data.Bifunctor
import Data.ByteString qualified as BS
import Data.Text qualified as Text

readFileSimpleScript
  :: FilePath
  -> CIO e (Script SimpleScript')
readFileSimpleScript file = do
  scriptBytes <- readFileCli file
  fromEitherCli $
    deserialiseSimpleScript scriptBytes

deserialiseSimpleScript
  :: BS.ByteString
  -> Either ScriptDecodeError (Script SimpleScript')
deserialiseSimpleScript bs =
  case deserialiseFromJSON bs of
    Left _ ->
      -- In addition to the TextEnvelope format, we also try to
      -- deserialize the JSON representation of SimpleScripts.
      case Aeson.eitherDecodeStrict' bs of
        Left err -> Left (ScriptDecodeSimpleScriptError $ JsonDecodeError err)
        Right script -> Right $ SimpleScript script
    Right te ->
      case deserialiseFromTextEnvelopeAnyOf [teType'] te of
        Left err -> Left (ScriptDecodeTextEnvelopeError err)
        Right script -> Right script
 where
  teType' :: FromSomeType HasTextEnvelope (Script SimpleScript')
  teType' = FromSomeType (AsScript AsSimpleScript) id

data AnyPlutusScript where
  AnyPlutusScript
    :: IsPlutusScriptLanguage lang => PlutusScriptVersion lang -> PlutusScript lang -> AnyPlutusScript

readFilePlutusScript
  :: FilePath
  -> CIO e AnyPlutusScript
readFilePlutusScript plutusScriptFp = do
  bs <-
    readFileCli plutusScriptFp
  fromEitherCli $ deserialisePlutusScript bs

deserialisePlutusScript
  :: BS.ByteString
  -> Either PlutusScriptDecodeError AnyPlutusScript
deserialisePlutusScript bs = do
  te <- first PlutusScriptJsonDecodeError $ deserialiseFromJSON bs
  case teType te of
    TextEnvelopeType s -> case s of
      "PlutusScriptV1" -> deserialiseAnyPlutusScriptVersion PlutusScriptV1 te
      "PlutusScriptV2" -> deserialiseAnyPlutusScriptVersion PlutusScriptV2 te
      "PlutusScriptV3" -> deserialiseAnyPlutusScriptVersion PlutusScriptV3 te
      unknownScriptVersion ->
        Left . PlutusScriptDecodeErrorUnknownVersion $ Text.pack unknownScriptVersion
 where
  deserialiseAnyPlutusScriptVersion
    :: IsPlutusScriptLanguage lang
    => PlutusScriptVersion lang
    -> TextEnvelope
    -> Either PlutusScriptDecodeError AnyPlutusScript
  deserialiseAnyPlutusScriptVersion lang tEnv =
    first PlutusScriptDecodeTextEnvelopeError $
      deserialiseFromTextEnvelopeAnyOf [teTypes (AnyPlutusScriptVersion lang)] tEnv

  teTypes :: AnyPlutusScriptVersion -> FromSomeType HasTextEnvelope AnyPlutusScript
  teTypes =
    \case
      AnyPlutusScriptVersion PlutusScriptV1 ->
        FromSomeType (AsPlutusScript AsPlutusScriptV1) (AnyPlutusScript PlutusScriptV1)
      AnyPlutusScriptVersion PlutusScriptV2 ->
        FromSomeType (AsPlutusScript AsPlutusScriptV2) (AnyPlutusScript PlutusScriptV2)
      AnyPlutusScriptVersion PlutusScriptV3 ->
        FromSomeType (AsPlutusScript AsPlutusScriptV3) (AnyPlutusScript PlutusScriptV3)
      AnyPlutusScriptVersion PlutusScriptV4 ->
        FromSomeType (AsPlutusScript AsPlutusScriptV4) (AnyPlutusScript PlutusScriptV4)
