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
import Cardano.Ledger.Core qualified as L

import Prelude

import Data.Aeson qualified as Aeson
import Data.Bifunctor
import Data.ByteString qualified as BS
import Data.ByteString.Lazy.Char8 qualified as LBS

-- TODO: Update to handle hex script bytes directly as well!
readFileSimpleScript
  :: forall era e
   . FilePath
  -> Exp.Era era
  -> CIO e (Exp.SimpleScript (Exp.LedgerEra era))
readFileSimpleScript file era = do
  bs <- readFileCli file
  case deserialiseFromJSON bs of
    Left _ -> do
      -- In addition to the TextEnvelope format, we also try to
      -- deserialize the JSON representation of SimpleScripts..
      script :: SimpleScript <- fromEitherCli $ Aeson.eitherDecodeStrict' bs
      let s :: L.NativeScript (Exp.LedgerEra era) = obtainCommonConstraints era $ toAllegraTimelock script
      return $ obtainCommonConstraints (era :: Exp.Era era) $ Exp.SimpleScript s
    Right te -> do
      let scriptBs = teRawCBOR te
      obtainCommonConstraints era $
        fromEitherCli $
          Exp.deserialiseSimpleScript scriptBs

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
