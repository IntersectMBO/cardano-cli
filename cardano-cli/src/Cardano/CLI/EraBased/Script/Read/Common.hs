{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.CLI.EraBased.Script.Read.Common
  ( -- * Plutus Script Related
    readScriptDataOrFile

    -- * Simple Script Related
  , readFileSimpleScript
  )
where

import Cardano.Api as Api

import Cardano.CLI.Compatible.Exception
import Cardano.CLI.Read (readFileCli)
import Cardano.CLI.Type.Common
import Cardano.CLI.Type.Error.ScriptDataError
import Cardano.CLI.Type.Error.ScriptDecodeError

import Prelude

import Data.Aeson qualified as Aeson
import Data.Bifunctor
import Data.ByteString qualified as BS
import Data.ByteString.Lazy.Char8 qualified as LBS

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

readFileSimpleScript
  :: FilePath
  -> CIO e (Script SimpleScript')
readFileSimpleScript file = do
  scriptBytes <- readFileCli file
  fromEitherCli $
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
