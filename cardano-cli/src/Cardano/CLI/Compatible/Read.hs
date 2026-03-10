{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.CLI.Compatible.Read
  ( readFilePlutusScript
  , readFileSimpleScript
  )
where

import Cardano.Api as Api
import Cardano.Api.Experimental.Plutus qualified as Exp

import Cardano.CLI.Compatible.Exception
import Cardano.CLI.Read (readFileCli)
import Cardano.CLI.Type.Error.ScriptDecodeError

import Prelude

import Data.Aeson qualified as Aeson
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

readFilePlutusScript
  :: forall era e
   . ShelleyBasedEra era
  -> FilePath
  -> CIO e (Exp.AnyPlutusScript (ShelleyLedgerEra era))
readFilePlutusScript sbe plutusScriptFp = do
  bs <- readFileCli plutusScriptFp
  te <- fromEitherCli $ deserialiseFromJSON bs
  let scriptBs = teRawCBOR te
      TextEnvelopeType anyScriptType = teType te
  case Exp.textToPlutusLanguage (Text.pack anyScriptType) of
    Just lang ->
      fromEitherCli
        ( shelleyBasedEraConstraints sbe (Exp.decodeAnyPlutusScript scriptBs lang)
            :: Either DecoderError (Exp.AnyPlutusScript (ShelleyLedgerEra era))
        )
    Nothing ->
      throwCliError $ "Unsupported script language: " <> anyScriptType
