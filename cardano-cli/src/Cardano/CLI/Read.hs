{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.Read
  ( -- * Metadata
    MetadataError(..)
  , renderMetadataError
  , readFileTxMetadata
  , readTxMetadata

    -- * Script
  , ScriptWitnessError(..)
  , renderScriptWitnessError
  , readScriptDataOrFile
  , readScriptWitness
  , readScriptWitnessFiles
  , readScriptWitnessFilesThruple
  , ScriptDecodeError (..)
  , deserialiseScriptInAnyLang
  , readFileScriptInAnyLang

  -- * Script data (datums and redeemers)
  , ScriptDataError(..)
  , readScriptDatumOrFile
  , readScriptRedeemerOrFile
  , renderScriptDataError

  -- * Tx
  , CddlError(..)
  , CddlTx(..)
  , IncompleteTx(..)
  , readFileTx
  , readFileTxBody
  , readCddlTx -- For testing purposes

  -- * Tx witnesses
  , ReadWitnessSigningDataError(..)
  , renderReadWitnessSigningDataError
  , SomeSigningWitness(..)
  , ByronOrShelleyWitness(..)
  , ShelleyBootstrapWitnessSigningKeyData(..)
  , CddlWitnessError(..)
  , readFileTxKeyWitness
  , readWitnessSigningData

  -- * Required signer
  , RequiredSignerError(..)
  , categoriseSomeSigningWitness
  , readRequiredSigner

  -- * Governance related
  , ConstitutionError(..)
  , ProposalError(..)
  , VoteError (..)
  , readTxGovernanceActions
  , constitutionHashSourceToHash
  , readProposal

  -- * FileOrPipe
  , FileOrPipe
  , fileOrPipe
  , fileOrPipePath
  , fileOrPipeCache
  , readFileOrPipe

  -- * Stake credentials
  , getStakeCredentialFromVerifier
  , getStakeCredentialFromIdentifier
  , getStakeAddressFromVerifier

  , readVotingProceduresFiles
  , readVotingProceduresFile

  -- * DRep credentials
  , getDRepCredentialFromVerKeyHashOrFile

  -- * Committee credentials
  , getCommitteeColdCredentialFromVerKeyHashOrFile
  , getCommitteeHotCredentialFromVerKeyHashOrFile

  , ReadSafeHashError(..)
  , readHexAsSafeHash
  , readSafeHash

  , scriptHashReader

  -- * Update proposals
  , readTxUpdateProposal

  -- * Vote related
  , readVoteDelegationTarget
  ) where

import           Cardano.Api as Api
import qualified Cardano.Api.Ledger as L
import           Cardano.Api.Pretty
import           Cardano.Api.Shelley as Api

import qualified Cardano.Binary as CBOR
import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Errors.DelegationError
import           Cardano.CLI.Types.Errors.ScriptDecodeError
import           Cardano.CLI.Types.Errors.StakeCredentialError
import           Cardano.CLI.Types.Governance
import           Cardano.CLI.Types.Key
import qualified Cardano.Crypto.Hash.Class as Crypto
import qualified Cardano.Ledger.BaseTypes as L
import qualified Cardano.Ledger.BaseTypes as Ledger
import qualified Cardano.Ledger.Conway.Governance as Ledger
import qualified Cardano.Ledger.Credential as Ledger
import qualified Cardano.Ledger.Crypto as Crypto
import qualified Cardano.Ledger.Crypto as Ledger
import qualified Cardano.Ledger.Keys as Ledger
import qualified Cardano.Ledger.SafeHash as L
import qualified Cardano.Ledger.SafeHash as Ledger

import           Prelude

import           Control.Exception (bracket, displayException)
import           Control.Monad (forM, unless)
import           Control.Monad.IO.Class
import           Control.Monad.Trans (MonadTrans (..))
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Except.Extra
import qualified Data.Aeson as Aeson
import           Data.Bifunctor
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Function ((&))
import           Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text
import           Data.Word
import           GHC.IO.Handle (hClose, hIsSeekable)
import           GHC.IO.Handle.FD (openFileBlocking)
import qualified Options.Applicative as Opt
import           System.IO (IOMode (ReadMode))

-- Metadata

data MetadataError
  = MetadataErrorFile (FileError ())
  | MetadataErrorJsonParseError !FilePath !String
  | MetadataErrorConversionError !FilePath !TxMetadataJsonError
  | MetadataErrorValidationError !FilePath ![(Word64, TxMetadataRangeError)]
  | MetadataErrorDecodeError !FilePath !CBOR.DecoderError
  | MetadataErrorNotAvailableInEra AnyCardanoEra
  deriving Show

renderMetadataError :: MetadataError -> Doc ann
renderMetadataError = \case
  MetadataErrorFile fileErr ->
    prettyError fileErr
  MetadataErrorJsonParseError fp jsonErr ->
    "Invalid JSON format in file: " <> pshow fp <>
              "\nJSON parse error: " <> pretty jsonErr
  MetadataErrorConversionError fp metadataErr ->
    "Error reading metadata at: " <> pshow fp <>
              "\n" <> prettyError metadataErr
  MetadataErrorValidationError fp errs ->
    mconcat
      [ "Error validating transaction metadata at: " <> pretty fp <> "\n"
      , mconcat $ List.intersperse "\n"
          [ "key " <> pshow k <> ":" <> prettyError valErr
          | (k, valErr) <- errs
          ]
      ]
  MetadataErrorDecodeError fp metadataErr ->
    "Error decoding CBOR metadata at: " <> pshow fp <>
              " Error: " <> pshow metadataErr
  MetadataErrorNotAvailableInEra e ->
    "Transaction metadata not supported in " <> pretty (renderEra e)

readTxMetadata :: CardanoEra era
               -> TxMetadataJsonSchema
               -> [MetadataFile]
               -> IO (Either MetadataError (TxMetadataInEra era))
readTxMetadata _ _ [] = return $ Right TxMetadataNone
readTxMetadata era schema files = cardanoEraConstraints era $ runExceptT $ do
  supported <- forEraMaybeEon era
    & hoistMaybe (MetadataErrorNotAvailableInEra $ AnyCardanoEra era)
  metadata  <- mapM (readFileTxMetadata schema) files
  pure $ TxMetadataInEra supported $ mconcat metadata

readFileTxMetadata
  :: TxMetadataJsonSchema
  -> MetadataFile
  -> ExceptT MetadataError IO TxMetadata
readFileTxMetadata mapping (MetadataFileJSON fp) = do
  bs <- handleIOExceptT (MetadataErrorFile . FileIOError (unFile fp))
          $ LBS.readFile (unFile fp)
  v <- firstExceptT (MetadataErrorJsonParseError (unFile fp))
          $ hoistEither $ Aeson.eitherDecode' bs
  txMetadata' <- firstExceptT (MetadataErrorConversionError (unFile fp))
                  . hoistEither $ metadataFromJson mapping v
  firstExceptT (MetadataErrorValidationError (unFile fp))
    . hoistEither $ do
      validateTxMetadata txMetadata'
      return txMetadata'
readFileTxMetadata _ (MetadataFileCBOR fp) = do
  bs <- handleIOExceptT (MetadataErrorFile . FileIOError (unFile fp))
          $ BS.readFile (unFile fp)
  txMetadata' <- firstExceptT (MetadataErrorDecodeError (unFile fp))
                  . hoistEither $ deserialiseFromCBOR AsTxMetadata bs
  firstExceptT (MetadataErrorValidationError (unFile fp))
    . hoistEither $ do
      validateTxMetadata txMetadata'
      return txMetadata'

-- Script witnesses/ Scripts

data ScriptWitnessError
  = ScriptWitnessErrorFile (FileError ScriptDecodeError)
  | ScriptWitnessErrorScriptLanguageNotSupportedInEra AnyScriptLanguage AnyCardanoEra
  | ScriptWitnessErrorExpectedSimple !FilePath !AnyScriptLanguage
  | ScriptWitnessErrorExpectedPlutus !FilePath !AnyScriptLanguage
  | ScriptWitnessErrorReferenceScriptsNotSupportedInEra !AnyCardanoEra
  | ScriptWitnessErrorScriptData ScriptDataError

renderScriptWitnessError :: ScriptWitnessError -> Doc ann
renderScriptWitnessError = \case
  ScriptWitnessErrorFile err ->
    prettyError err
  ScriptWitnessErrorScriptLanguageNotSupportedInEra (AnyScriptLanguage lang) anyEra ->
    "The script language " <> pshow lang <> " is not supported in the " <>
    pretty (renderEra anyEra) <> " era."
  ScriptWitnessErrorExpectedSimple file (AnyScriptLanguage lang) ->
    pretty file <> ": expected a script in the simple script language, " <>
    "but it is actually using " <> pshow lang <> ". Alternatively, to use " <>
    "a Plutus script, you must also specify the redeemer " <>
    "(datum if appropriate) and script execution units."
  ScriptWitnessErrorExpectedPlutus file (AnyScriptLanguage lang) ->
    pretty file <> ": expected a script in the Plutus script language, " <>
    "but it is actually using " <> pshow lang <> "."
  ScriptWitnessErrorReferenceScriptsNotSupportedInEra anyEra ->
    "Reference scripts not supported in era: " <> pretty (renderEra anyEra)
  ScriptWitnessErrorScriptData sDataError ->
    renderScriptDataError sDataError

readScriptWitnessFiles
  :: CardanoEra era
  -> [(a, Maybe (ScriptWitnessFiles ctx))]
  -> ExceptT ScriptWitnessError IO [(a, Maybe (ScriptWitness ctx era))]
readScriptWitnessFiles era = mapM readSwitFile
 where
  readSwitFile (tIn, Just switFile) = do
      sWit <- readScriptWitness era switFile
      return (tIn, Just sWit)
  readSwitFile (tIn, Nothing) = return (tIn, Nothing)

readScriptWitnessFilesThruple
  :: CardanoEra era
  -> [(a, b, Maybe (ScriptWitnessFiles ctx))]
  -> ExceptT ScriptWitnessError IO [(a, b, Maybe (ScriptWitness ctx era))]
readScriptWitnessFilesThruple era = mapM readSwitFile
 where
  readSwitFile (tIn, b, Just switFile) = do
      sWit <- readScriptWitness era switFile
      return (tIn, b, Just sWit)
  readSwitFile (tIn, b, Nothing) = return (tIn, b, Nothing)

readScriptWitness
  :: CardanoEra era
  -> ScriptWitnessFiles witctx
  -> ExceptT ScriptWitnessError IO (ScriptWitness witctx era)
readScriptWitness era (SimpleScriptWitnessFile (ScriptFile scriptFile)) = do
    script@(ScriptInAnyLang lang _) <- firstExceptT ScriptWitnessErrorFile $
                                         readFileScriptInAnyLang scriptFile
    ScriptInEra langInEra script'   <- validateScriptSupportedInEra era script
    case script' of
      SimpleScript sscript ->
        return . SimpleScriptWitness langInEra $ SScript sscript

      -- If the supplied cli flags were for a simple script (i.e. the user did
      -- not supply the datum, redeemer or ex units), but the script file turns
      -- out to be a valid plutus script, then we must fail.
      PlutusScript{} ->
        left $ ScriptWitnessErrorExpectedSimple
                 scriptFile
                 (AnyScriptLanguage lang)

readScriptWitness era (PlutusScriptWitnessFiles
                          (ScriptFile scriptFile)
                          datumOrFile
                          redeemerOrFile
                          execUnits) = do
    script@(ScriptInAnyLang lang _) <- firstExceptT ScriptWitnessErrorFile $
                                         readFileScriptInAnyLang scriptFile
    ScriptInEra langInEra script'   <- validateScriptSupportedInEra era script
    case script' of
      PlutusScript version pscript -> do
        datum <- firstExceptT ScriptWitnessErrorScriptData
                   $ readScriptDatumOrFile datumOrFile
        redeemer <- firstExceptT ScriptWitnessErrorScriptData
                   $ readScriptRedeemerOrFile redeemerOrFile
        return $ PlutusScriptWitness
                   langInEra version (PScript pscript)
                   datum
                   redeemer
                   execUnits

      -- If the supplied cli flags were for a plutus script (i.e. the user did
      -- supply the datum, redeemer and ex units), but the script file turns
      -- out to be a valid simple script, then we must fail.
      SimpleScript{} ->
        left $ ScriptWitnessErrorExpectedPlutus
                 scriptFile
                 (AnyScriptLanguage lang)

readScriptWitness era (PlutusReferenceScriptWitnessFiles refTxIn
                          anyScrLang@(AnyScriptLanguage anyScriptLanguage)
                          datumOrFile redeemerOrFile execUnits mPid) = do
  caseByronToAlonzoOrBabbageEraOnwards
    ( const $ left
        $ ScriptWitnessErrorReferenceScriptsNotSupportedInEra
        $ cardanoEraConstraints era (AnyCardanoEra era)
    )
    ( const $
        case scriptLanguageSupportedInEra era anyScriptLanguage of
          Just sLangInEra ->
            case languageOfScriptLanguageInEra sLangInEra of
              SimpleScriptLanguage ->
                -- TODO: We likely need another datatype eg data ReferenceScriptWitness lang
                -- in order to make this branch unrepresentable.
                error "readScriptWitness: Should not be possible to specify a simple script"
              PlutusScriptLanguage version -> do
                datum <- firstExceptT ScriptWitnessErrorScriptData
                          $ readScriptDatumOrFile    datumOrFile
                redeemer <- firstExceptT ScriptWitnessErrorScriptData
                              $ readScriptRedeemerOrFile redeemerOrFile
                return $ PlutusScriptWitness
                          sLangInEra
                          version
                          (PReferenceScript refTxIn (unPolicyId <$> mPid))
                          datum redeemer execUnits
          Nothing ->
            left $ ScriptWitnessErrorScriptLanguageNotSupportedInEra anyScrLang (anyCardanoEra era)
    )
    era
readScriptWitness era (SimpleReferenceScriptWitnessFiles refTxIn
                         anyScrLang@(AnyScriptLanguage anyScriptLanguage) mPid) = do
  caseByronToAlonzoOrBabbageEraOnwards
    ( const $ left
        $ ScriptWitnessErrorReferenceScriptsNotSupportedInEra
        $ cardanoEraConstraints era (AnyCardanoEra era)
    )
    ( const $
        case scriptLanguageSupportedInEra era anyScriptLanguage of
          Just sLangInEra ->
            case languageOfScriptLanguageInEra sLangInEra of
              SimpleScriptLanguage ->
                return . SimpleScriptWitness sLangInEra
                      $ SReferenceScript refTxIn (unPolicyId <$> mPid)
              PlutusScriptLanguage{} ->
                error "readScriptWitness: Should not be possible to specify a plutus script"
          Nothing ->
            left $ ScriptWitnessErrorScriptLanguageNotSupportedInEra anyScrLang (anyCardanoEra era)
    )
    era

validateScriptSupportedInEra :: CardanoEra era
                             -> ScriptInAnyLang
                             -> ExceptT ScriptWitnessError IO (ScriptInEra era)
validateScriptSupportedInEra era script@(ScriptInAnyLang lang _) =
    case toScriptInEra era script of
      Nothing -> left $ ScriptWitnessErrorScriptLanguageNotSupportedInEra
                          (AnyScriptLanguage lang) (anyCardanoEra era)
      Just script' -> pure script'

data ScriptDataError =
    ScriptDataErrorFile (FileError ())
  | ScriptDataErrorJsonParse !FilePath !String
  | ScriptDataErrorConversion !FilePath !ScriptDataJsonError
  | ScriptDataErrorValidation !FilePath !ScriptDataRangeError
  | ScriptDataErrorMetadataDecode !FilePath !CBOR.DecoderError
  | ScriptDataErrorJsonBytes !ScriptDataJsonBytesError

renderScriptDataError :: ScriptDataError -> Doc ann
renderScriptDataError = \case
  ScriptDataErrorFile err ->
    prettyError err
  ScriptDataErrorJsonParse fp jsonErr->
    "Invalid JSON format in file: " <> pshow fp <> "\nJSON parse error: " <> pretty jsonErr
  ScriptDataErrorConversion fp sDataJsonErr->
    "Error reading metadata at: " <> pshow fp <> "\n" <> prettyError sDataJsonErr
  ScriptDataErrorValidation fp sDataRangeErr->
    "Error validating script data at: " <> pshow fp <> ":\n" <> prettyError sDataRangeErr
  ScriptDataErrorMetadataDecode fp decoderErr->
    "Error decoding CBOR metadata at: " <> pshow fp <> " Error: " <> pshow decoderErr
  ScriptDataErrorJsonBytes e->
    prettyError e


readScriptDatumOrFile :: ScriptDatumOrFile witctx
                      -> ExceptT ScriptDataError IO (ScriptDatum witctx)
readScriptDatumOrFile (ScriptDatumOrFileForTxIn df) = ScriptDatumForTxIn <$>
                                                        readScriptDataOrFile df
readScriptDatumOrFile InlineDatumPresentAtTxIn      = pure InlineScriptDatum
readScriptDatumOrFile NoScriptDatumOrFileForMint    = pure NoScriptDatumForMint
readScriptDatumOrFile NoScriptDatumOrFileForStake   = pure NoScriptDatumForStake

readScriptRedeemerOrFile :: ScriptRedeemerOrFile
                         -> ExceptT ScriptDataError IO ScriptRedeemer
readScriptRedeemerOrFile = readScriptDataOrFile

readScriptDataOrFile :: ScriptDataOrFile
                     -> ExceptT ScriptDataError IO HashableScriptData
readScriptDataOrFile (ScriptDataValue d) = return d
readScriptDataOrFile (ScriptDataJsonFile fp) = do
  sDataBs <- handleIOExceptT (ScriptDataErrorFile . FileIOError fp) $ LBS.readFile fp
  sDataValue <- hoistEither . first (ScriptDataErrorJsonParse fp) $ Aeson.eitherDecode sDataBs
  hoistEither
    . first ScriptDataErrorJsonBytes
    $ scriptDataJsonToHashable ScriptDataJsonDetailedSchema sDataValue

readScriptDataOrFile (ScriptDataCborFile fp) = do
  origBs <- handleIOExceptT (ScriptDataErrorFile . FileIOError fp) (BS.readFile fp)
  hSd <- firstExceptT (ScriptDataErrorMetadataDecode fp)
          $ hoistEither $ deserialiseFromCBOR AsHashableScriptData origBs
  firstExceptT (ScriptDataErrorValidation fp)
          $ hoistEither $ validateScriptData $ getScriptData hSd
  return hSd

-- | Read a script file. The file can either be in the text envelope format
-- wrapping the binary representation of any of the supported script languages,
-- or alternatively it can be a JSON format file for one of the simple script
-- language versions.
--
readFileScriptInAnyLang :: FilePath
                        -> ExceptT (FileError ScriptDecodeError) IO
                                   ScriptInAnyLang
readFileScriptInAnyLang file = do
  scriptBytes <- handleIOExceptT (FileIOError file) $ BS.readFile file
  firstExceptT (FileError file) $ hoistEither $
    deserialiseScriptInAnyLang scriptBytes


deserialiseScriptInAnyLang :: BS.ByteString
                           -> Either ScriptDecodeError ScriptInAnyLang
deserialiseScriptInAnyLang bs =
    -- Accept either the text envelope format wrapping the binary serialisation,
    -- or accept the simple script language in its JSON format.
    --
    case deserialiseFromJSON AsTextEnvelope bs of
      Left _   ->
        -- In addition to the TextEnvelope format, we also try to
        -- deserialize the JSON representation of SimpleScripts.
        case Aeson.eitherDecodeStrict' bs of
          Left  err    -> Left (ScriptDecodeSimpleScriptError $ JsonDecodeError err)
          Right script -> Right $ ScriptInAnyLang SimpleScriptLanguage $ SimpleScript script

      Right te ->
        case deserialiseFromTextEnvelopeAnyOf textEnvTypes te of
          Left  err    -> Left (ScriptDecodeTextEnvelopeError err)
          Right script -> Right script

  where
    -- TODO: Think of a way to get type checker to warn when there is a missing
    -- script version.
    textEnvTypes :: [FromSomeType HasTextEnvelope ScriptInAnyLang]
    textEnvTypes =
      [ FromSomeType (AsScript AsSimpleScript)
                     (ScriptInAnyLang SimpleScriptLanguage)

      , FromSomeType (AsScript AsPlutusScriptV1)
                     (ScriptInAnyLang (PlutusScriptLanguage PlutusScriptV1))

      , FromSomeType (AsScript AsPlutusScriptV2)
                     (ScriptInAnyLang (PlutusScriptLanguage PlutusScriptV2))
      ]

-- Tx & TxBody

newtype CddlTx = CddlTx {unCddlTx :: InAnyCardanoEra Tx} deriving (Show, Eq)

readFileTx :: FileOrPipe -> IO (Either CddlError (InAnyCardanoEra Tx))
readFileTx file = do
  eAnyTx <- readFileInAnyCardanoEra AsTx file
  case eAnyTx of
    Left e -> fmap unCddlTx <$> acceptTxCDDLSerialisation file e
    Right tx -> return $ Right tx

-- IncompleteCddlFormattedTx is an CDDL formatted tx or partial tx
-- (respectively needs additional witnesses or totally unwitnessed)
-- while UnwitnessedCliFormattedTxBody is CLI formatted TxBody and
-- needs to be key witnessed.

data IncompleteTx
  = UnwitnessedCliFormattedTxBody (InAnyCardanoEra TxBody)
  | IncompleteCddlFormattedTx (InAnyCardanoEra Tx)

readFileTxBody :: FileOrPipe -> IO (Either CddlError IncompleteTx)
readFileTxBody file = do
  eTxBody <- readFileInAnyCardanoEra AsTxBody file
  case eTxBody of
    Left e -> fmap (IncompleteCddlFormattedTx . unCddlTx) <$> acceptTxCDDLSerialisation file e
    Right txBody -> return $ Right $ UnwitnessedCliFormattedTxBody txBody

data CddlError = CddlErrorTextEnv
                   !(FileError TextEnvelopeError)
                   !(FileError TextEnvelopeCddlError)
               | CddlIOError (FileError TextEnvelopeError)
               deriving Show

instance Error CddlError where
  prettyError = \case
    CddlErrorTextEnv textEnvErr cddlErr ->
      "Failed to decode neither the cli's serialisation format nor the ledger's " <>
      "CDDL serialisation format. TextEnvelope error: " <> prettyError textEnvErr <> "\n" <>
      "TextEnvelopeCddl error: " <> prettyError cddlErr
    CddlIOError e ->
      prettyError e

acceptTxCDDLSerialisation
  :: FileOrPipe
  -> FileError TextEnvelopeError
  -> IO (Either CddlError CddlTx)
acceptTxCDDLSerialisation file err =
  case err of
   e@(FileError _ (TextEnvelopeDecodeError _)) ->
      first (CddlErrorTextEnv e) <$> readCddlTx file
   e@(FileError _ (TextEnvelopeAesonDecodeError _)) ->
      first (CddlErrorTextEnv e) <$> readCddlTx file
   e@(FileError _ (TextEnvelopeTypeError _ _)) ->
      first (CddlErrorTextEnv e) <$> readCddlTx file
   e@FileErrorTempFile{} -> return . Left $ CddlIOError e
   e@FileDoesNotExistError{} -> return . Left $ CddlIOError e
   e@FileIOError{} -> return . Left $ CddlIOError e

readCddlTx :: FileOrPipe -> IO (Either (FileError TextEnvelopeCddlError) CddlTx)
readCddlTx = readFileOrPipeTextEnvelopeCddlAnyOf teTypes
 where
    teTypes = [ FromCDDLTx "Witnessed Tx ByronEra" CddlTx
              , FromCDDLTx "Witnessed Tx ShelleyEra" CddlTx
              , FromCDDLTx "Witnessed Tx AllegraEra" CddlTx
              , FromCDDLTx "Witnessed Tx MaryEra" CddlTx
              , FromCDDLTx "Witnessed Tx AlonzoEra" CddlTx
              , FromCDDLTx "Witnessed Tx BabbageEra" CddlTx
              , FromCDDLTx "Witnessed Tx ConwayEra" CddlTx
              , FromCDDLTx "Unwitnessed Tx ByronEra" CddlTx
              , FromCDDLTx "Unwitnessed Tx ShelleyEra" CddlTx
              , FromCDDLTx "Unwitnessed Tx AllegraEra" CddlTx
              , FromCDDLTx "Unwitnessed Tx MaryEra" CddlTx
              , FromCDDLTx "Unwitnessed Tx AlonzoEra" CddlTx
              , FromCDDLTx "Unwitnessed Tx BabbageEra" CddlTx
              , FromCDDLTx "Unwitnessed Tx ConwayEra" CddlTx
              ]

-- Tx witnesses

newtype CddlWitness = CddlWitness { unCddlWitness :: InAnyCardanoEra KeyWitness}

readFileTxKeyWitness :: FilePath
                -> IO (Either CddlWitnessError (InAnyCardanoEra KeyWitness))
readFileTxKeyWitness fp = do
  file <- fileOrPipe fp
  eWitness <- readFileInAnyCardanoEra AsKeyWitness file
  case eWitness of
    Left e -> fmap unCddlWitness <$> acceptKeyWitnessCDDLSerialisation e
    Right keyWit -> return $ Right keyWit

data CddlWitnessError
  = CddlWitnessErrorTextEnv
      (FileError TextEnvelopeError)
      (FileError TextEnvelopeCddlError)
  | CddlWitnessIOError (FileError TextEnvelopeError)
  deriving Show

instance Error CddlWitnessError where
  prettyError = \case
    CddlWitnessErrorTextEnv teErr cddlErr ->
      "Failed to decode neither the cli's serialisation format nor the ledger's \
      \CDDL serialisation format. TextEnvelope error: " <> prettyError teErr <> "\n" <>
      "TextEnvelopeCddl error: " <> prettyError cddlErr
    CddlWitnessIOError fileE ->
      prettyError fileE


-- TODO: This is a stop gap to avoid modifying the TextEnvelope
-- related functions. We intend to remove this after fully deprecating
-- the cli's serialisation format
acceptKeyWitnessCDDLSerialisation
  :: FileError TextEnvelopeError
  -> IO (Either CddlWitnessError CddlWitness)
acceptKeyWitnessCDDLSerialisation err =
  case err of
    e@(FileError fp (TextEnvelopeDecodeError _)) ->
      first (CddlWitnessErrorTextEnv e) <$> readCddlWitness fp
    e@(FileError fp (TextEnvelopeAesonDecodeError _)) ->
      first (CddlWitnessErrorTextEnv e) <$> readCddlWitness fp
    e@(FileError fp (TextEnvelopeTypeError _ _)) ->
      first (CddlWitnessErrorTextEnv e) <$> readCddlWitness fp
    e@FileErrorTempFile{} -> return . Left $ CddlWitnessIOError e
    e@FileDoesNotExistError{} -> return . Left $ CddlWitnessIOError e
    e@FileIOError{} -> return . Left $ CddlWitnessIOError e

readCddlWitness
  :: FilePath
  -> IO (Either (FileError TextEnvelopeCddlError) CddlWitness)
readCddlWitness fp = do
  readFileTextEnvelopeCddlAnyOf teTypes fp
 where
  teTypes = [ FromCDDLWitness "TxWitness ShelleyEra" CddlWitness
            , FromCDDLWitness "TxWitness AllegraEra" CddlWitness
            , FromCDDLWitness "TxWitness MaryEra" CddlWitness
            , FromCDDLWitness "TxWitness AlonzoEra" CddlWitness
            , FromCDDLWitness "TxWitness BabbageEra" CddlWitness
            , FromCDDLWitness "TxWitness ConwayEra" CddlWitness
            ]

-- Witness handling

data SomeSigningWitness
  = AByronSigningWitness                    (SigningKey ByronKey) (Maybe (Address ByronAddr))
  | APaymentSigningWitness                  (SigningKey PaymentKey)
  | APaymentExtendedSigningWitness          (SigningKey PaymentExtendedKey)
  | AStakeSigningWitness                    (SigningKey StakeKey)
  | AStakeExtendedSigningWitness            (SigningKey StakeExtendedKey)
  | AStakePoolSigningWitness                (SigningKey StakePoolKey)
  | AGenesisSigningWitness                  (SigningKey GenesisKey)
  | AGenesisExtendedSigningWitness          (SigningKey GenesisExtendedKey)
  | AGenesisDelegateSigningWitness          (SigningKey GenesisDelegateKey)
  | AGenesisDelegateExtendedSigningWitness  (SigningKey GenesisDelegateExtendedKey)
  | AGenesisUTxOSigningWitness              (SigningKey GenesisUTxOKey)
  | ADRepSigningWitness                     (SigningKey DRepKey)
  | ACommitteeColdSigningWitness            (SigningKey CommitteeColdKey)
  | ACommitteeHotSigningWitness             (SigningKey CommitteeHotKey)
  deriving Show


-- | Data required for constructing a Shelley bootstrap witness.
data ShelleyBootstrapWitnessSigningKeyData
  = ShelleyBootstrapWitnessSigningKeyData
      !(SigningKey ByronKey)
      -- ^ Byron signing key.
      !(Maybe (Address ByronAddr))
      -- ^ An optionally specified Byron address.
      --
      -- If specified, both the network ID and derivation path are extracted
      -- from the address and used in the construction of the Byron witness.

-- | Some kind of Byron or Shelley witness.
data ByronOrShelleyWitness
  = AByronWitness !ShelleyBootstrapWitnessSigningKeyData
  | AShelleyKeyWitness !ShelleyWitnessSigningKey

categoriseSomeSigningWitness :: SomeSigningWitness -> ByronOrShelleyWitness
categoriseSomeSigningWitness swsk =
  case swsk of
    AByronSigningWitness                    sk addr -> AByronWitness      (ShelleyBootstrapWitnessSigningKeyData  sk addr)
    APaymentSigningWitness                  sk      -> AShelleyKeyWitness (WitnessPaymentKey                      sk)
    APaymentExtendedSigningWitness          sk      -> AShelleyKeyWitness (WitnessPaymentExtendedKey              sk)
    AStakeSigningWitness                    sk      -> AShelleyKeyWitness (WitnessStakeKey                        sk)
    AStakeExtendedSigningWitness            sk      -> AShelleyKeyWitness (WitnessStakeExtendedKey                sk)
    AStakePoolSigningWitness                sk      -> AShelleyKeyWitness (WitnessStakePoolKey                    sk)
    AGenesisSigningWitness                  sk      -> AShelleyKeyWitness (WitnessGenesisKey                      sk)
    AGenesisExtendedSigningWitness          sk      -> AShelleyKeyWitness (WitnessGenesisExtendedKey              sk)
    AGenesisDelegateSigningWitness          sk      -> AShelleyKeyWitness (WitnessGenesisDelegateKey              sk)
    AGenesisDelegateExtendedSigningWitness  sk      -> AShelleyKeyWitness (WitnessGenesisDelegateExtendedKey      sk)
    AGenesisUTxOSigningWitness              sk      -> AShelleyKeyWitness (WitnessGenesisUTxOKey                  sk)
    ADRepSigningWitness                     sk      -> AShelleyKeyWitness (WitnessPaymentKey $ castDrep           sk)
    ACommitteeColdSigningWitness            sk      -> AShelleyKeyWitness (WitnessCommitteeColdKey                sk)
    ACommitteeHotSigningWitness             sk      -> AShelleyKeyWitness (WitnessCommitteeHotKey                 sk)

-- TODO: Conway era - Add constrctor for SigningKey DrepKey to ShelleyWitnessSigningKey
castDrep :: SigningKey DRepKey -> SigningKey PaymentKey
castDrep (DRepSigningKey sk) = PaymentSigningKey sk

data ReadWitnessSigningDataError
  = ReadWitnessSigningDataSigningKeyDecodeError !(FileError InputDecodeError)
  | ReadWitnessSigningDataScriptError !(FileError JsonDecodeError)
  | ReadWitnessSigningDataSigningKeyAndAddressMismatch
  -- ^ A Byron address was specified alongside a non-Byron signing key.
  deriving Show

-- | Render an error message for a 'ReadWitnessSigningDataError'.
renderReadWitnessSigningDataError :: ReadWitnessSigningDataError -> Doc ann
renderReadWitnessSigningDataError = \case
  ReadWitnessSigningDataSigningKeyDecodeError fileErr ->
    "Error reading signing key: " <> prettyError fileErr
  ReadWitnessSigningDataScriptError fileErr ->
    "Error reading script: " <> prettyError fileErr
  ReadWitnessSigningDataSigningKeyAndAddressMismatch ->
    "Only a Byron signing key may be accompanied by a Byron address."

readWitnessSigningData
  :: WitnessSigningData
  -> IO (Either ReadWitnessSigningDataError SomeSigningWitness)
readWitnessSigningData (KeyWitnessSigningData skFile mbByronAddr) = do
    eRes <- first ReadWitnessSigningDataSigningKeyDecodeError
             <$> readKeyFileAnyOf bech32FileTypes textEnvFileTypes skFile
    return $ do
      res <- eRes
      case (res, mbByronAddr) of
        (AByronSigningWitness _ _, Just _) -> pure res
        (AByronSigningWitness _ _, Nothing) -> pure res
        (_, Nothing) -> pure res
        (_, Just _) ->
          -- A Byron address should only be specified along with a Byron signing key.
          Left ReadWitnessSigningDataSigningKeyAndAddressMismatch
  where
    textEnvFileTypes =
      [ FromSomeType (AsSigningKey AsByronKey                   ) (`AByronSigningWitness` mbByronAddr)
      , FromSomeType (AsSigningKey AsPaymentKey                 ) APaymentSigningWitness
      , FromSomeType (AsSigningKey AsPaymentExtendedKey         ) APaymentExtendedSigningWitness
      , FromSomeType (AsSigningKey AsStakeKey                   ) AStakeSigningWitness
      , FromSomeType (AsSigningKey AsStakeExtendedKey           ) AStakeExtendedSigningWitness
      , FromSomeType (AsSigningKey AsStakePoolKey               ) AStakePoolSigningWitness
      , FromSomeType (AsSigningKey AsGenesisKey                 ) AGenesisSigningWitness
      , FromSomeType (AsSigningKey AsGenesisExtendedKey         ) AGenesisExtendedSigningWitness
      , FromSomeType (AsSigningKey AsGenesisDelegateKey         ) AGenesisDelegateSigningWitness
      , FromSomeType (AsSigningKey AsGenesisDelegateExtendedKey ) AGenesisDelegateExtendedSigningWitness
      , FromSomeType (AsSigningKey AsGenesisUTxOKey             ) AGenesisUTxOSigningWitness
      , FromSomeType (AsSigningKey AsDRepKey                    ) ADRepSigningWitness
      , FromSomeType (AsSigningKey AsCommitteeColdKey           ) ACommitteeColdSigningWitness
      , FromSomeType (AsSigningKey AsCommitteeHotKey            ) ACommitteeHotSigningWitness
      ]

    bech32FileTypes =
      [ FromSomeType (AsSigningKey AsPaymentKey         ) APaymentSigningWitness
      , FromSomeType (AsSigningKey AsPaymentExtendedKey ) APaymentExtendedSigningWitness
      , FromSomeType (AsSigningKey AsStakeKey           ) AStakeSigningWitness
      , FromSomeType (AsSigningKey AsStakeExtendedKey   ) AStakeExtendedSigningWitness
      , FromSomeType (AsSigningKey AsStakePoolKey       ) AStakePoolSigningWitness
      ]

-- Required signers

data RequiredSignerError
  = RequiredSignerErrorFile (FileError InputDecodeError)
  | RequiredSignerErrorByronKey (SigningKeyFile In)
  deriving Show

instance Error RequiredSignerError where
  prettyError = \case
    RequiredSignerErrorFile e ->
      prettyError e
    RequiredSignerErrorByronKey (File byronSkeyfile) ->
      "Byron witnesses cannot be used for required signers: " <> pretty byronSkeyfile

readRequiredSigner :: RequiredSigner -> IO (Either RequiredSignerError (Hash PaymentKey))
readRequiredSigner (RequiredSignerHash h) = return $ Right h
readRequiredSigner (RequiredSignerSkeyFile skFile) = do
  eKeyWit <- first RequiredSignerErrorFile <$> readKeyFileAnyOf bech32FileTypes textEnvFileTypes skFile
  return $ do
    keyWit <- eKeyWit
    case categoriseSomeSigningWitness keyWit of
      AByronWitness _ ->
        Left $ RequiredSignerErrorByronKey skFile
      AShelleyKeyWitness skey ->
        return . getHash $ toShelleySigningKey skey
 where
   textEnvFileTypes =
     [ FromSomeType (AsSigningKey AsPaymentKey        ) APaymentSigningWitness
     , FromSomeType (AsSigningKey AsPaymentExtendedKey) APaymentExtendedSigningWitness
     , FromSomeType (AsSigningKey AsStakePoolKey      ) AStakePoolSigningWitness
     , FromSomeType (AsSigningKey AsGenesisDelegateKey) AGenesisDelegateSigningWitness
     ]
   bech32FileTypes = []

   getHash :: ShelleySigningKey -> Hash PaymentKey
   getHash (ShelleyExtendedSigningKey sk) =
     let extSKey = PaymentExtendedSigningKey sk
         payVKey = castVerificationKey $ getVerificationKey extSKey
     in verificationKeyHash payVKey
   getHash (ShelleyNormalSigningKey sk) =
     verificationKeyHash . getVerificationKey $ PaymentSigningKey sk

data VoteError
  = VoteErrorFile (FileError TextEnvelopeError)
  | VoteErrorTextNotUnicode Text.UnicodeException
  deriving Show

instance Error VoteError where
  prettyError = \case
    VoteErrorFile e ->
      prettyError e
    VoteErrorTextNotUnicode e ->
      "Vote text file not UTF8-encoded: " <> pretty (displayException e)

readVotingProceduresFiles :: ()
  => ConwayEraOnwards era
  -> [VoteFile In]
  -> IO (Either VoteError (VotingProcedures era))
readVotingProceduresFiles w = \case
  [] -> return $ Right $ VotingProcedures $ Ledger.VotingProcedures Map.empty
  files -> runExceptT $ do
    vpss <- forM files (ExceptT . readVotingProceduresFile w)

    pure $ foldl unsafeMergeVotingProcedures emptyVotingProcedures vpss

readTxUpdateProposal :: ()
  => ShelleyToBabbageEra era
  -> UpdateProposalFile
  -> ExceptT (FileError TextEnvelopeError) IO (TxUpdateProposal era)
readTxUpdateProposal w (UpdateProposalFile upFp) = do
  TxUpdateProposal w <$> newExceptT (readFileTextEnvelope AsUpdateProposal (File upFp))

readVotingProceduresFile :: ()
  => ConwayEraOnwards era
  -> VoteFile In
  -> IO (Either VoteError (VotingProcedures era))
readVotingProceduresFile w fp =
  conwayEraOnwardsConstraints w
    $ first VoteErrorFile <$> readFileTextEnvelope AsVotingProcedures fp

data ConstitutionError
  = ConstitutionErrorFile (FileError TextEnvelopeError)
  | ConstitutionNotSupportedInEra AnyCardanoEra
  | ConstitutionNotUnicodeError Text.UnicodeException
  deriving Show

data ProposalError
  = ProposalErrorFile (FileError TextEnvelopeError)
  | ProposalNotSupportedInEra AnyCardanoEra
  | ProposalNotUnicodeError Text.UnicodeException
  deriving Show

readTxGovernanceActions
  :: CardanoEra era
  -> [ProposalFile In]
  -> IO (Either ConstitutionError [Proposal era])
readTxGovernanceActions _ [] = return $ Right []
readTxGovernanceActions era files = runExceptT $ do
  w <- forEraMaybeEon era
    & hoistMaybe (ConstitutionNotSupportedInEra $ cardanoEraConstraints era $ AnyCardanoEra era)
  newExceptT $ sequence <$> mapM (fmap (first ConstitutionErrorFile) . readProposal w) files

readProposal
  :: ConwayEraOnwards era
  -> ProposalFile In
  -> IO (Either (FileError TextEnvelopeError) (Proposal era))
readProposal w fp =
    conwayEraOnwardsConstraints w (readFileTextEnvelope AsProposal fp)

constitutionHashSourceToHash :: ()
  => ConstitutionHashSource
  -> ExceptT ConstitutionError IO (Ledger.SafeHash Ledger.StandardCrypto Ledger.AnchorData)
constitutionHashSourceToHash constitutionHashSource = do
  case constitutionHashSource of
    ConstitutionHashSourceFile fp  -> do
      cBs <- liftIO $ BS.readFile $ unFile fp
      _utf8EncodedText <- firstExceptT ConstitutionNotUnicodeError . hoistEither $ Text.decodeUtf8' cBs
      pure $ Ledger.hashAnchorData $ Ledger.AnchorData cBs

    ConstitutionHashSourceText c -> do
      pure $ Ledger.hashAnchorData $ Ledger.AnchorData $ Text.encodeUtf8 c

    ConstitutionHashSourceHash h ->
      pure h

-- Misc

readFileInAnyCardanoEra
  :: ( HasTextEnvelope (thing ByronEra)
     , HasTextEnvelope (thing ShelleyEra)
     , HasTextEnvelope (thing AllegraEra)
     , HasTextEnvelope (thing MaryEra)
     , HasTextEnvelope (thing AlonzoEra)
     , HasTextEnvelope (thing BabbageEra)
     , HasTextEnvelope (thing ConwayEra)
     )
  => (forall era. AsType era -> AsType (thing era))
  -> FileOrPipe
  -> IO (Either (FileError TextEnvelopeError) (InAnyCardanoEra thing))
readFileInAnyCardanoEra asThing =
 readFileOrPipeTextEnvelopeAnyOf
   [ FromSomeType (asThing AsByronEra)   (InAnyCardanoEra ByronEra)
   , FromSomeType (asThing AsShelleyEra) (InAnyCardanoEra ShelleyEra)
   , FromSomeType (asThing AsAllegraEra) (InAnyCardanoEra AllegraEra)
   , FromSomeType (asThing AsMaryEra)    (InAnyCardanoEra MaryEra)
   , FromSomeType (asThing AsAlonzoEra)  (InAnyCardanoEra AlonzoEra)
   , FromSomeType (asThing AsBabbageEra) (InAnyCardanoEra BabbageEra)
   , FromSomeType (asThing AsConwayEra)  (InAnyCardanoEra ConwayEra)
   ]

-- | We need a type for handling files that may be actually be things like
-- pipes. Currently the CLI makes no guarantee that a "file" will only
-- be read once. This is a problem for a user who who expects to be able to pass
-- a pipe. To handle this, we have a type for representing either files or pipes
-- where the contents will be saved in memory if what we're reading is a pipe (so
-- it can be re-read later). Unfortunately this means we can't easily stream data
-- from pipes, but at present that's not an issue.
data FileOrPipe = FileOrPipe FilePath (IORef (Maybe LBS.ByteString))


instance Show FileOrPipe where
    show (FileOrPipe fp _) = show fp

fileOrPipe :: FilePath -> IO FileOrPipe
fileOrPipe fp = FileOrPipe fp <$> newIORef Nothing

-- | Get the path backing a FileOrPipe. This should primarily be used when
-- generating error messages for a user. A user should not call directly
-- call a function like readFile on the result of this function
fileOrPipePath :: FileOrPipe -> FilePath
fileOrPipePath (FileOrPipe fp _) = fp

fileOrPipeCache :: FileOrPipe -> IO (Maybe LBS.ByteString)
fileOrPipeCache (FileOrPipe _ c) = readIORef c

-- | Get the contents of a file or pipe. This function reads the entire
-- contents of the file or pipe, and is blocking.
readFileOrPipe :: FileOrPipe -> IO LBS.ByteString
readFileOrPipe (FileOrPipe fp cacheRef) = do
    cached <- readIORef cacheRef
    case cached of
      Just dat -> pure dat
      Nothing -> bracket
        (openFileBlocking fp ReadMode)
        hClose
        (\handle -> do
          -- An arbitrary block size.
          let blockSize = 4096
          let go acc = do
                next <- BS.hGet handle blockSize
                if BS.null next
                then pure acc
                else go (acc <> Builder.byteString next)
          contents <- go mempty
          let dat = Builder.toLazyByteString contents
          -- If our file is not seekable, it's likely a pipe, so we need to
          -- save the result for subsequent calls
          seekable <- hIsSeekable handle
          unless seekable (writeIORef cacheRef (Just dat))
          pure dat)

readFileOrPipeTextEnvelopeAnyOf
  :: [FromSomeType HasTextEnvelope b]
  -> FileOrPipe
  -> IO (Either (FileError TextEnvelopeError) b)
readFileOrPipeTextEnvelopeAnyOf types file = do
    let path = fileOrPipePath file
    runExceptT $ do
      content <- handleIOExceptT (FileIOError path) $ readFileOrPipe file
      firstExceptT (FileError path) $ hoistEither $ do
        te <- first TextEnvelopeAesonDecodeError $ Aeson.eitherDecode' content
        deserialiseFromTextEnvelopeAnyOf types te

readFileOrPipeTextEnvelopeCddlAnyOf
  :: [FromSomeTypeCDDL TextEnvelopeCddl b]
  -> FileOrPipe
  -> IO (Either (FileError TextEnvelopeCddlError) b)
readFileOrPipeTextEnvelopeCddlAnyOf types file = do
  let path = fileOrPipePath file
  runExceptT $ do
    te <- newExceptT $ readTextEnvelopeCddlFromFileOrPipe file
    firstExceptT (FileError path) $ hoistEither $ do
      deserialiseFromTextEnvelopeCddlAnyOf types te

readTextEnvelopeCddlFromFileOrPipe
  :: FileOrPipe
  -> IO (Either (FileError TextEnvelopeCddlError) TextEnvelopeCddl)
readTextEnvelopeCddlFromFileOrPipe file = do
  let path = fileOrPipePath file
  runExceptT $ do
    bs <- handleIOExceptT (FileIOError path) $
            readFileOrPipe file
    firstExceptT (FileError path . TextEnvelopeCddlAesonDecodeError path)
      . hoistEither $ Aeson.eitherDecode' bs

----------------------------------------------------------------------------------------------------

getStakeCredentialFromVerifier :: ()
  => StakeVerifier
  -> ExceptT StakeCredentialError IO StakeCredential
getStakeCredentialFromVerifier = \case
  StakeVerifierScriptFile (ScriptFile sFile) -> do
    ScriptInAnyLang _ script <-
      readFileScriptInAnyLang sFile
        & firstExceptT StakeCredentialScriptDecodeError
    pure $ StakeCredentialByScript $ hashScript script

  StakeVerifierKey stakeVerKeyOrFile -> do
    stakeVerKey <-
      ExceptT (readVerificationKeyOrFile AsStakeKey stakeVerKeyOrFile)
        & firstExceptT StakeCredentialInputDecodeError
    pure $ StakeCredentialByKey $ verificationKeyHash stakeVerKey

getStakeCredentialFromIdentifier :: ()
  => StakeIdentifier
  -> ExceptT StakeCredentialError IO StakeCredential
getStakeCredentialFromIdentifier = \case
  StakeIdentifierAddress stakeAddr -> pure $ stakeAddressCredential stakeAddr
  StakeIdentifierVerifier stakeVerifier -> getStakeCredentialFromVerifier stakeVerifier

getStakeAddressFromVerifier :: ()
  => NetworkId
  -> StakeVerifier
  -> ExceptT StakeCredentialError IO StakeAddress
getStakeAddressFromVerifier networkId stakeVerifier =
  makeStakeAddress networkId <$> getStakeCredentialFromVerifier stakeVerifier

getDRepCredentialFromVerKeyHashOrFile :: ()
  => VerificationKeyOrHashOrFile DRepKey
  -> ExceptT (FileError InputDecodeError) IO (Ledger.Credential Ledger.DRepRole Ledger.StandardCrypto)
getDRepCredentialFromVerKeyHashOrFile = \case
  VerificationKeyOrFile verKeyOrFile -> do
    drepVerKey <-
      ExceptT (readVerificationKeyOrFile AsDRepKey verKeyOrFile)
    pure . Ledger.KeyHashObj . unDRepKeyHash $ verificationKeyHash drepVerKey
  VerificationKeyHash kh -> pure . Ledger.KeyHashObj $ unDRepKeyHash kh

getCommitteeColdCredentialFromVerKeyHashOrFile :: ()
  => VerificationKeyOrHashOrFile CommitteeColdKey
  -> ExceptT (FileError InputDecodeError) IO (Ledger.Credential Ledger.ColdCommitteeRole Ledger.StandardCrypto)
getCommitteeColdCredentialFromVerKeyHashOrFile = \case
  VerificationKeyOrFile verKeyOrFile -> do
    commmitteeColdVerKey <-
      ExceptT (readVerificationKeyOrFile AsCommitteeColdKey verKeyOrFile)
    let CommitteeColdKeyHash kh = verificationKeyHash commmitteeColdVerKey
    pure $ Ledger.KeyHashObj kh
  VerificationKeyHash (CommitteeColdKeyHash kh) -> pure $ Ledger.KeyHashObj kh

getCommitteeHotCredentialFromVerKeyHashOrFile :: ()
  => VerificationKeyOrHashOrFile CommitteeHotKey
  -> ExceptT (FileError InputDecodeError) IO (Ledger.Credential Ledger.HotCommitteeRole Ledger.StandardCrypto)
getCommitteeHotCredentialFromVerKeyHashOrFile = \case
  VerificationKeyOrFile verKeyOrFile -> do
    commmitteeHotVerKey <-
      ExceptT (readVerificationKeyOrFile AsCommitteeHotKey verKeyOrFile)
    let CommitteeHotKeyHash kh = verificationKeyHash commmitteeHotVerKey
    pure $ Ledger.KeyHashObj kh
  VerificationKeyHash (CommitteeHotKeyHash kh) -> pure $ Ledger.KeyHashObj kh

data ReadSafeHashError
  = ReadSafeHashErrorNotHex ByteString String
  | ReadSafeHashErrorInvalidHash Text

renderReadSafeHashError :: ReadSafeHashError -> Text
renderReadSafeHashError = \case
  ReadSafeHashErrorNotHex bs err ->
    "Error reading anchor data hash: Invalid hex: " <> Text.decodeUtf8 bs <> "\n" <> Text.pack err
  ReadSafeHashErrorInvalidHash err ->
    "Error reading anchor data hash: " <> err

readHexAsSafeHash :: ()
  => Text
  -> Either ReadSafeHashError (L.SafeHash Crypto.StandardCrypto L.AnchorData)
readHexAsSafeHash hex = do
  let bs = Text.encodeUtf8 hex

  raw <- Base16.decode bs & first (ReadSafeHashErrorNotHex bs)

  case Crypto.hashFromBytes raw of
    Just a -> Right (L.unsafeMakeSafeHash a)
    Nothing -> Left $ ReadSafeHashErrorInvalidHash "Unable to read hash"

readSafeHash :: Opt.ReadM (L.SafeHash Crypto.StandardCrypto L.AnchorData)
readSafeHash =
  Opt.eitherReader $ \s ->
    readHexAsSafeHash (Text.pack s)
      & first (Text.unpack . renderReadSafeHashError)

scriptHashReader :: Opt.ReadM ScriptHash
scriptHashReader = Opt.eitherReader $ Right . fromString

readVoteDelegationTarget :: ()
  => VoteDelegationTarget
  -> ExceptT DelegationError IO (L.DRep Ledger.StandardCrypto)
readVoteDelegationTarget voteDelegationTarget =
  case voteDelegationTarget of
    VoteDelegationTargetOfDRep drepHashSource -> do
      drepHash <- case drepHashSource of
        DRepHashSourceScript (ScriptHash scriptHash) ->
          pure $ Ledger.ScriptHashObj scriptHash
        DRepHashSourceVerificationKey drepVKeyOrHashOrFile -> do
          DRepKeyHash drepKeyHash <-
            lift (readVerificationKeyOrHashOrTextEnvFile AsDRepKey drepVKeyOrHashOrFile)
              & onLeft (left . DelegationDRepReadError)
          pure $ Ledger.KeyHashObj drepKeyHash
      pure $ L.DRepCredential drepHash
    VoteDelegationTargetOfAbstain ->
      pure L.DRepAlwaysAbstain
    VoteDelegationTargetOfNoConfidence ->
      pure L.DRepAlwaysNoConfidence
