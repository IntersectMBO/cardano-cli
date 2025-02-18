{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.Script.Read.Common
  ( -- * Plutus Script Related
    readScriptDataOrFile
  , readScriptRedeemerOrFile
  , readFilePlutusScript

    -- * Simple Script Related
  , readFileSimpleScript
  )
where

import Cardano.Api as Api

import Cardano.CLI.EraBased.Script.Types
import Cardano.CLI.Types.Common
import Cardano.CLI.Types.Errors.PlutusScriptDecodeError
import Cardano.CLI.Types.Errors.ScriptDataError
import Cardano.CLI.Types.Errors.ScriptDecodeError

import Prelude

import Data.Aeson qualified as Aeson
import Data.Bifunctor
import Data.ByteString qualified as BS
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Text qualified as Text

deserialisePlutusScript
  :: BS.ByteString
  -> Either PlutusScriptDecodeError AnyPlutusScript
deserialisePlutusScript bs = do
  te <- first PlutusScriptJsonDecodeError $ deserialiseFromJSON AsTextEnvelope bs
  case teType te of
    TextEnvelopeType s -> case s of
      sVer@"PlutusScriptV1" -> deserialiseAnyPlutusScriptVersion sVer PlutusScriptV1 te
      sVer@"PlutusScriptV2" -> deserialiseAnyPlutusScriptVersion sVer PlutusScriptV2 te
      sVer@"PlutusScriptV3" -> deserialiseAnyPlutusScriptVersion sVer PlutusScriptV3 te
      unknownScriptVersion ->
        Left . PlutusScriptDecodeErrorUnknownVersion $ Text.pack unknownScriptVersion
 where
  deserialiseAnyPlutusScriptVersion
    :: IsPlutusScriptLanguage lang
    => String
    -> PlutusScriptVersion lang
    -> TextEnvelope
    -> Either PlutusScriptDecodeError AnyPlutusScript
  deserialiseAnyPlutusScriptVersion v lang tEnv =
    if v == show lang
      then
        first PlutusScriptDecodeTextEnvelopeError $
          deserialiseFromTextEnvelopeAnyOf [teTypes (AnyPlutusScriptVersion lang)] tEnv
      else Left $ PlutusScriptDecodeErrorVersionMismatch (Text.pack v) (AnyPlutusScriptVersion lang)

  teTypes :: AnyPlutusScriptVersion -> FromSomeType HasTextEnvelope AnyPlutusScript
  teTypes =
    \case
      AnyPlutusScriptVersion PlutusScriptV1 ->
        FromSomeType (AsPlutusScript AsPlutusScriptV1) (AnyPlutusScript PlutusScriptV1)
      AnyPlutusScriptVersion PlutusScriptV2 ->
        FromSomeType (AsPlutusScript AsPlutusScriptV2) (AnyPlutusScript PlutusScriptV2)
      AnyPlutusScriptVersion PlutusScriptV3 ->
        FromSomeType (AsPlutusScript AsPlutusScriptV3) (AnyPlutusScript PlutusScriptV3)

deserialiseSimpleScript
  :: BS.ByteString
  -> Either ScriptDecodeError (Script SimpleScript')
deserialiseSimpleScript bs =
  case deserialiseFromJSON AsTextEnvelope bs of
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
  :: MonadIOTransError (FileError PlutusScriptDecodeError) t m
  => FilePath
  -> t m AnyPlutusScript
readFilePlutusScript plutusScriptFp = do
  bs <-
    handleIOExceptionsLiftWith (FileIOError plutusScriptFp) . liftIO $
      BS.readFile plutusScriptFp
  modifyError (FileError plutusScriptFp) $
    hoistEither $
      deserialisePlutusScript bs

readFileSimpleScript
  :: MonadIOTransError (FileError ScriptDecodeError) t m
  => FilePath
  -> t m (Script SimpleScript')
readFileSimpleScript file = do
  scriptBytes <- handleIOExceptionsLiftWith (FileIOError file) . liftIO $ BS.readFile file
  modifyError (FileError file) $
    hoistEither $
      deserialiseSimpleScript scriptBytes

readScriptDataOrFile
  :: MonadIO m
  => ScriptDataOrFile
  -> ExceptT ScriptDataError m HashableScriptData
readScriptDataOrFile (ScriptDataValue d) = return d
readScriptDataOrFile (ScriptDataJsonFile fp) = do
  sDataBs <- handleIOExceptT (ScriptDataErrorFile . FileIOError fp) $ LBS.readFile fp
  sDataValue <- hoistEither . first (ScriptDataErrorJsonParse fp) $ Aeson.eitherDecode sDataBs
  hoistEither
    . first ScriptDataErrorJsonBytes
    $ scriptDataJsonToHashable ScriptDataJsonDetailedSchema sDataValue
readScriptDataOrFile (ScriptDataCborFile fp) = do
  origBs <- handleIOExceptT (ScriptDataErrorFile . FileIOError fp) (BS.readFile fp)
  hSd <-
    firstExceptT (ScriptDataErrorMetadataDecode fp) $
      hoistEither $
        deserialiseFromCBOR AsHashableScriptData origBs
  firstExceptT (ScriptDataErrorValidation fp) $
    hoistEither $
      validateScriptData $
        getScriptData hSd
  return hSd

readScriptRedeemerOrFile
  :: ScriptRedeemerOrFile
  -> ExceptT ScriptDataError IO ScriptRedeemer
readScriptRedeemerOrFile = readScriptDataOrFile
