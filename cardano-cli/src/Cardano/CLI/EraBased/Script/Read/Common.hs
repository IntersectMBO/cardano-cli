{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.EraBased.Script.Read.Common
  ( -- * Plutus Script Related
    readScriptDataOrFile

    -- * Simple Script Related
  , readFileSimpleScript
  )
where

import Cardano.Api as Api
import Cardano.Api.Experimental (obtainCommonConstraints)
import Cardano.Api.Experimental qualified as Exp

import Cardano.CLI.Compatible.Exception
import Cardano.CLI.Read (readFileCli)
import Cardano.CLI.Type.Common
import Cardano.CLI.Type.Error.ScriptDataError

import Prelude

import Data.Aeson qualified as Aeson
import Data.Bifunctor
import Data.ByteString qualified as BS
import Data.ByteString.Lazy.Char8 qualified as LBS

readFileSimpleScript
  :: forall era e
   . FilePath
  -> Exp.Era era
  -> CIO e (Exp.SimpleScript (Exp.LedgerEra era))
readFileSimpleScript file era = do
  scriptBytes <- readFileCli file
  obtainCommonConstraints era $
    fromEitherCli $
      Exp.deserialiseSimpleScript scriptBytes

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
