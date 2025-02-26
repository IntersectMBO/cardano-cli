{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.CLI.Read
  ( -- * Metadata
    MetadataError (..)
  , renderMetadataError
  , readFileTxMetadata
  , readTxMetadata

    -- * Script
  , ScriptWitnessError (..)
  , renderScriptWitnessError
  , ScriptDecodeError (..)
  , deserialiseScriptInAnyLang
  , readFileScriptInAnyLang
  , PlutusScriptDecodeError (..)

    -- * Script data (datums and redeemers)
  , ScriptDataError (..)
  , renderScriptDataError

    -- * Tx
  , CddlError (..)
  , CddlTx (..)
  , IncompleteCddlTxBody (..)
  , readFileTx
  , readFileTxBody
  , readCddlTx -- For testing purposes
  , txTextEnvelopeTypes -- For testing purposes

    -- * Tx witnesses
  , ReadWitnessSigningDataError (..)
  , renderReadWitnessSigningDataError
  , SomeSigningWitness (..)
  , ByronOrShelleyWitness (..)
  , ShelleyBootstrapWitnessSigningKeyData (..)
  , CddlWitnessError (..)
  , readFileTxKeyWitness
  , readWitnessSigningData
  , txWitnessTextEnvelopeTypes -- For testing purposes

    -- * Required signer
  , RequiredSignerError (..)
  , categoriseSomeSigningWitness
  , readRequiredSigner

    -- * Governance related
  , ConstitutionError (..)
  , ProposalError (..)
  , VoteError (..)
  , readTxGovernanceActions
  , constitutionHashSourceToHash
  , readProposal
  , CostModelsError (..)
  , readCostModels

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

    -- * DRep credentials
  , getDRepCredentialFromVerKeyHashOrFile
  , ReadSafeHashError (..)
  , readHexAsSafeHash
  , readSafeHash
  , scriptHashReader

    -- * Update proposals
  , readTxUpdateProposal

    -- * Vote related
  , readVoteDelegationTarget
  , readVerificationKeyOrHashOrFileOrScript
  , readVerificationKeySource

    -- * Genesis hashes
  , readShelleyOnwardsGenesisAndHash
  )
where

import Cardano.Api as Api
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Shelley as Api

import Cardano.Binary qualified as CBOR
import Cardano.CLI.EraBased.Script.Proposal.Read
import Cardano.CLI.EraBased.Script.Proposal.Type
  ( CliProposalScriptRequirements
  , ProposalScriptWitness
  )
import Cardano.CLI.EraBased.Script.Type
import Cardano.CLI.EraBased.Script.Vote.Read
import Cardano.CLI.EraBased.Script.Vote.Type
import Cardano.CLI.Type.Common
import Cardano.CLI.Type.Error.DelegationError
import Cardano.CLI.Type.Error.PlutusScriptDecodeError
import Cardano.CLI.Type.Error.ScriptDataError
import Cardano.CLI.Type.Error.ScriptDecodeError
import Cardano.CLI.Type.Error.StakeCredentialError
import Cardano.CLI.Type.Governance
import Cardano.CLI.Type.Key
import Cardano.Crypto.Hash qualified as Crypto

import Prelude

import Control.Exception (bracket)
import Control.Monad (forM, unless, when)
import Data.Aeson qualified as Aeson
import Data.Bifunctor
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Function ((&))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List qualified as List
import Data.Proxy (Proxy (..))
import Data.String
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.Encoding.Error qualified as Text
import Data.Word
import GHC.IO.Handle (hClose, hIsSeekable)
import GHC.IO.Handle.FD (openFileBlocking)
import Options.Applicative qualified as Opt
import System.IO (IOMode (ReadMode))

-- Metadata

data MetadataError
  = MetadataErrorFile (FileError ())
  | MetadataErrorJsonParseError !FilePath !String
  | MetadataErrorConversionError !FilePath !TxMetadataJsonError
  | MetadataErrorValidationError !FilePath ![(Word64, TxMetadataRangeError)]
  | MetadataErrorDecodeError !FilePath !CBOR.DecoderError
  deriving Show

renderMetadataError :: MetadataError -> Doc ann
renderMetadataError = \case
  MetadataErrorFile fileErr ->
    prettyError fileErr
  MetadataErrorJsonParseError fp jsonErr ->
    "Invalid JSON format in file: "
      <> pshow fp
      <> "\nJSON parse error: "
      <> pretty jsonErr
  MetadataErrorConversionError fp metadataErr ->
    "Error reading metadata at: "
      <> pshow fp
      <> "\n"
      <> prettyError metadataErr
  MetadataErrorValidationError fp errs ->
    mconcat
      [ "Error validating transaction metadata at: " <> pretty fp <> "\n"
      , mconcat $
          List.intersperse
            "\n"
            [ "key " <> pshow k <> ":" <> prettyError valErr
            | (k, valErr) <- errs
            ]
      ]
  MetadataErrorDecodeError fp metadataErr ->
    "Error decoding CBOR metadata at: "
      <> pshow fp
      <> " Error: "
      <> pshow metadataErr

readTxMetadata
  :: ShelleyBasedEra era
  -> TxMetadataJsonSchema
  -> [MetadataFile]
  -> IO (Either MetadataError (TxMetadataInEra era))
readTxMetadata _ _ [] = return $ Right TxMetadataNone
readTxMetadata era schema files = runExceptT $ do
  metadata <- mapM (readFileTxMetadata schema) files
  pure $ TxMetadataInEra era $ mconcat metadata

readFileTxMetadata
  :: TxMetadataJsonSchema
  -> MetadataFile
  -> ExceptT MetadataError IO TxMetadata
readFileTxMetadata mapping (MetadataFileJSON fp) = do
  bs <-
    handleIOExceptT (MetadataErrorFile . FileIOError (unFile fp)) $
      LBS.readFile (unFile fp)
  v <-
    firstExceptT (MetadataErrorJsonParseError (unFile fp)) $
      hoistEither $
        Aeson.eitherDecode' bs
  txMetadata' <-
    firstExceptT (MetadataErrorConversionError (unFile fp))
      . hoistEither
      $ metadataFromJson mapping v
  firstExceptT (MetadataErrorValidationError (unFile fp))
    . hoistEither
    $ do
      validateTxMetadata txMetadata'
      return txMetadata'
readFileTxMetadata _ (MetadataFileCBOR fp) = do
  bs <-
    handleIOExceptT (MetadataErrorFile . FileIOError (unFile fp)) $
      BS.readFile (unFile fp)
  txMetadata' <-
    firstExceptT (MetadataErrorDecodeError (unFile fp))
      . hoistEither
      $ deserialiseFromCBOR AsTxMetadata bs
  firstExceptT (MetadataErrorValidationError (unFile fp))
    . hoistEither
    $ do
      validateTxMetadata txMetadata'
      return txMetadata'

-- Script witnesses/ Scripts

data ScriptWitnessError
  = ScriptWitnessErrorFile (FileError ScriptDecodeError)
  | ScriptWitnessErrorScriptLanguageNotSupportedInEra AnyScriptLanguage AnyCardanoEra
  | ScriptWitnessErrorExpectedSimple !FilePath !AnyScriptLanguage
  | ScriptWitnessErrorExpectedPlutus !FilePath !AnyScriptLanguage
  | ScriptWitnessErrorReferenceScriptsNotSupportedInEra !AnyShelleyBasedEra
  | ScriptWitnessErrorScriptData ScriptDataError
  deriving Show

renderScriptWitnessError :: ScriptWitnessError -> Doc ann
renderScriptWitnessError = \case
  ScriptWitnessErrorFile err ->
    prettyError err
  ScriptWitnessErrorScriptLanguageNotSupportedInEra (AnyScriptLanguage lang) anyEra ->
    "The script language "
      <> pshow lang
      <> " is not supported in the "
      <> pretty anyEra
      <> " era."
  ScriptWitnessErrorExpectedSimple file (AnyScriptLanguage lang) ->
    pretty file
      <> ": expected a script in the simple script language, "
      <> "but it is actually using "
      <> pshow lang
      <> ". Alternatively, to use "
      <> "a Plutus script, you must also specify the redeemer "
      <> "(datum if appropriate) and script execution units."
  ScriptWitnessErrorExpectedPlutus file (AnyScriptLanguage lang) ->
    pretty file
      <> ": expected a script in the Plutus script language, "
      <> "but it is actually using "
      <> pshow lang
      <> "."
  ScriptWitnessErrorReferenceScriptsNotSupportedInEra anyEra ->
    "Reference scripts not supported in era: " <> pshow anyEra
  ScriptWitnessErrorScriptData sDataError ->
    renderScriptDataError sDataError

readVerificationKeyOrHashOrFileOrScript
  :: MonadIOTransError (Either (FileError ScriptDecodeError) (FileError InputDecodeError)) t m
  => Key keyrole
  => AsType keyrole
  -> (Hash keyrole -> L.KeyHash kr L.StandardCrypto)
  -> VerificationKeyOrHashOrFileOrScript keyrole
  -> t m (L.Credential kr L.StandardCrypto)
readVerificationKeyOrHashOrFileOrScript asType extractHash = \case
  VkhfsScript (File fp) -> do
    ScriptInAnyLang _lang script <-
      modifyError Left $
        readFileScriptInAnyLang fp
    pure . L.ScriptHashObj . toShelleyScriptHash $ hashScript script
  VkhfsKeyHashFile vkOrHashOrFp ->
    fmap (L.KeyHashObj . extractHash) . modifyError Right $
      readVerificationKeyOrHashOrTextEnvFile asType vkOrHashOrFp

readVerificationKeySource
  :: MonadIOTransError (Either (FileError ScriptDecodeError) (FileError InputDecodeError)) t m
  => Key keyrole
  => AsType keyrole
  -> (Hash keyrole -> L.KeyHash kr L.StandardCrypto)
  -> VerificationKeySource keyrole
  -> t m (L.Credential kr L.StandardCrypto)
readVerificationKeySource asType extractHash = \case
  VksScriptHash (ScriptHash scriptHash) ->
    pure $ L.ScriptHashObj scriptHash
  VksScript (File fp) -> do
    ScriptInAnyLang _lang script <-
      modifyError Left $
        readFileScriptInAnyLang fp
    pure . L.ScriptHashObj . toShelleyScriptHash $ hashScript script
  VksKeyHashFile vKeyOrHashOrFile ->
    fmap (L.KeyHashObj . extractHash) . modifyError Right $
      readVerificationKeyOrHashOrTextEnvFile asType vKeyOrHashOrFile

-- | Read a script file. The file can either be in the text envelope format
-- wrapping the binary representation of any of the supported script languages,
-- or alternatively it can be a JSON format file for one of the simple script
-- language versions.
readFileScriptInAnyLang
  :: MonadIOTransError (FileError ScriptDecodeError) t m
  => FilePath
  -> t m ScriptInAnyLang
readFileScriptInAnyLang file = do
  scriptBytes <- handleIOExceptionsLiftWith (FileIOError file) . liftIO $ BS.readFile file
  modifyError (FileError file) $
    hoistEither $
      deserialiseScriptInAnyLang scriptBytes

deserialiseScriptInAnyLang
  :: BS.ByteString
  -> Either ScriptDecodeError ScriptInAnyLang
deserialiseScriptInAnyLang bs =
  -- Accept either the text envelope format wrapping the binary serialisation,
  -- or accept the simple script language in its JSON format.
  --
  case deserialiseFromJSON AsTextEnvelope bs of
    Left _ ->
      -- In addition to the TextEnvelope format, we also try to
      -- deserialize the JSON representation of SimpleScripts.
      case Aeson.eitherDecodeStrict' bs of
        Left err -> Left (ScriptDecodeSimpleScriptError $ JsonDecodeError err)
        Right script -> Right $ ScriptInAnyLang SimpleScriptLanguage $ SimpleScript script
    Right te ->
      case deserialiseFromTextEnvelopeAnyOf textEnvTypes te of
        Left err -> Left (ScriptDecodeTextEnvelopeError err)
        Right script -> Right script
 where
  -- TODO: Think of a way to get type checker to warn when there is a missing
  -- script version.
  textEnvTypes :: [FromSomeType HasTextEnvelope ScriptInAnyLang]
  textEnvTypes = fromSomeTypeSimpleScript : fromSomeTypePlutusScripts

fromSomeTypeSimpleScript :: FromSomeType HasTextEnvelope ScriptInAnyLang
fromSomeTypeSimpleScript =
  FromSomeType
    (AsScript AsSimpleScript)
    (ScriptInAnyLang SimpleScriptLanguage)

fromSomeTypePlutusScripts :: [FromSomeType HasTextEnvelope ScriptInAnyLang]
fromSomeTypePlutusScripts =
  let allPlutusVersions :: [AnyPlutusScriptVersion] = [minBound .. maxBound]
   in [plutusScriptVersionFromSomeType v | AnyPlutusScriptVersion v <- allPlutusVersions]
 where
  plutusScriptVersionFromSomeType
    :: IsPlutusScriptLanguage lang
    => PlutusScriptVersion lang -> FromSomeType HasTextEnvelope ScriptInAnyLang
  plutusScriptVersionFromSomeType v =
    FromSomeType
      (AsScript $ proxyToAsType (Proxy :: Proxy lang))
      (ScriptInAnyLang $ PlutusScriptLanguage v)

-- Tx & TxBody

newtype CddlTx = CddlTx {unCddlTx :: InAnyShelleyBasedEra Tx} deriving (Show, Eq)

readFileTx :: FileOrPipe -> IO (Either (FileError TextEnvelopeCddlError) (InAnyShelleyBasedEra Tx))
readFileTx file = do
  cddlTxOrErr <- readCddlTx file
  case cddlTxOrErr of
    Left e -> return $ Left e
    Right cddlTx -> do
      InAnyShelleyBasedEra sbe tx <- pure $ unCddlTx cddlTx
      return $ Right $ inAnyShelleyBasedEra sbe tx

newtype IncompleteCddlTxBody
  = IncompleteCddlTxBody {unIncompleteCddlTxBody :: InAnyShelleyBasedEra TxBody}

readFileTxBody :: FileOrPipe -> IO (Either (FileError TextEnvelopeCddlError) IncompleteCddlTxBody)
readFileTxBody file = do
  cddlTxOrErr <- readCddlTx file
  case cddlTxOrErr of
    Left e -> return $ Left e
    Right cddlTx -> do
      InAnyShelleyBasedEra sbe tx <- pure $ unCddlTx cddlTx
      return $ Right $ IncompleteCddlTxBody $ inAnyShelleyBasedEra sbe $ getTxBody tx

data CddlError
  = CddlErrorTextEnv
      !(FileError TextEnvelopeError)
      !(FileError TextEnvelopeCddlError)
  | CddlIOError (FileError TextEnvelopeError)
  deriving Show

instance Error CddlError where
  prettyError = \case
    CddlErrorTextEnv textEnvErr cddlErr ->
      "Failed to decode the ledger's CDDL serialisation format. "
        <> "TextEnvelope error: "
        <> prettyError textEnvErr
        <> "\n"
        <> "TextEnvelopeCddl error: "
        <> prettyError cddlErr
    CddlIOError e ->
      prettyError e

readCddlTx :: FileOrPipe -> IO (Either (FileError TextEnvelopeCddlError) CddlTx)
readCddlTx =
  readFileOrPipeTextEnvelopeCddlAnyOf $
    map (`FromCDDLTx` CddlTx) txTextEnvelopeTypes

txTextEnvelopeTypes :: [Text]
txTextEnvelopeTypes =
  [ let TextEnvelopeType d = shelleyBasedEraConstraints sbe $ textEnvelopeType (proxyToAsType (makeTxProxy sbe))
     in T.pack d
  | AnyShelleyBasedEra sbe <- [minBound .. maxBound]
  ]
 where
  makeTxProxy :: ShelleyBasedEra era -> Proxy (Tx era)
  makeTxProxy _ = Proxy

-- Tx witnesses

newtype CddlWitness = CddlWitness {unCddlWitness :: InAnyShelleyBasedEra KeyWitness}

readFileTxKeyWitness
  :: FilePath
  -> IO (Either CddlWitnessError (InAnyShelleyBasedEra KeyWitness))
readFileTxKeyWitness fp = do
  file <- fileOrPipe fp
  eWitness <- readFileInAnyShelleyBasedEra AsKeyWitness file
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
      "Failed to decode the ledger's CDDL serialisation format. TextEnvelope error: "
        <> prettyError teErr
        <> "\n"
        <> "TextEnvelopeCddl error: "
        <> prettyError cddlErr
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
  readFileTextEnvelopeCddlAnyOf (map (`FromCDDLWitness` CddlWitness) txWitnessTextEnvelopeTypes) fp

txWitnessTextEnvelopeTypes :: [Text]
txWitnessTextEnvelopeTypes =
  [ let TextEnvelopeType d = shelleyBasedEraConstraints sbe $ textEnvelopeType (proxyToAsType (makeWitnessProxy sbe))
     in T.pack d
  | AnyShelleyBasedEra sbe <- [minBound .. maxBound]
  ]
 where
  makeWitnessProxy :: ShelleyBasedEra era -> Proxy (KeyWitness era)
  makeWitnessProxy _ = Proxy

-- Witness handling

data SomeSigningWitness
  = AByronSigningWitness (SigningKey ByronKey) (Maybe (Address ByronAddr))
  | APaymentSigningWitness (SigningKey PaymentKey)
  | APaymentExtendedSigningWitness (SigningKey PaymentExtendedKey)
  | AStakeSigningWitness (SigningKey StakeKey)
  | AStakeExtendedSigningWitness (SigningKey StakeExtendedKey)
  | AStakePoolSigningWitness (SigningKey StakePoolKey)
  | AGenesisSigningWitness (SigningKey GenesisKey)
  | AGenesisExtendedSigningWitness (SigningKey GenesisExtendedKey)
  | AGenesisDelegateSigningWitness (SigningKey GenesisDelegateKey)
  | AGenesisDelegateExtendedSigningWitness (SigningKey GenesisDelegateExtendedKey)
  | AGenesisUTxOSigningWitness (SigningKey GenesisUTxOKey)
  | ADRepSigningWitness (SigningKey DRepKey)
  | ADRepExtendedSigningWitness (SigningKey DRepExtendedKey)
  | ACommitteeColdSigningWitness (SigningKey CommitteeColdKey)
  | ACommitteeColdExtendedSigningWitness (SigningKey CommitteeColdExtendedKey)
  | ACommitteeHotSigningWitness (SigningKey CommitteeHotKey)
  | ACommitteeHotExtendedSigningWitness (SigningKey CommitteeHotExtendedKey)
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
    AByronSigningWitness sk addr -> AByronWitness (ShelleyBootstrapWitnessSigningKeyData sk addr)
    APaymentSigningWitness sk -> AShelleyKeyWitness (WitnessPaymentKey sk)
    APaymentExtendedSigningWitness sk -> AShelleyKeyWitness (WitnessPaymentExtendedKey sk)
    AStakeSigningWitness sk -> AShelleyKeyWitness (WitnessStakeKey sk)
    AStakeExtendedSigningWitness sk -> AShelleyKeyWitness (WitnessStakeExtendedKey sk)
    AStakePoolSigningWitness sk -> AShelleyKeyWitness (WitnessStakePoolKey sk)
    AGenesisSigningWitness sk -> AShelleyKeyWitness (WitnessGenesisKey sk)
    AGenesisExtendedSigningWitness sk -> AShelleyKeyWitness (WitnessGenesisExtendedKey sk)
    AGenesisDelegateSigningWitness sk -> AShelleyKeyWitness (WitnessGenesisDelegateKey sk)
    AGenesisDelegateExtendedSigningWitness sk -> AShelleyKeyWitness (WitnessGenesisDelegateExtendedKey sk)
    AGenesisUTxOSigningWitness sk -> AShelleyKeyWitness (WitnessGenesisUTxOKey sk)
    ADRepSigningWitness sk -> AShelleyKeyWitness (WitnessDRepKey sk)
    ADRepExtendedSigningWitness sk -> AShelleyKeyWitness (WitnessDRepExtendedKey sk)
    ACommitteeColdSigningWitness sk -> AShelleyKeyWitness (WitnessCommitteeColdKey sk)
    ACommitteeColdExtendedSigningWitness sk -> AShelleyKeyWitness (WitnessCommitteeColdExtendedKey sk)
    ACommitteeHotSigningWitness sk -> AShelleyKeyWitness (WitnessCommitteeHotKey sk)
    ACommitteeHotExtendedSigningWitness sk -> AShelleyKeyWitness (WitnessCommitteeHotExtendedKey sk)

data ReadWitnessSigningDataError
  = ReadWitnessSigningDataSigningKeyDecodeError !(FileError InputDecodeError)
  | ReadWitnessSigningDataScriptError !(FileError JsonDecodeError)
  | -- | A Byron address was specified alongside a non-Byron signing key.
    ReadWitnessSigningDataSigningKeyAndAddressMismatch
  deriving Show

instance Error ReadWitnessSigningDataError where
  prettyError = \case
    ReadWitnessSigningDataSigningKeyDecodeError fileErr ->
      prettyError fileErr
    ReadWitnessSigningDataScriptError fileErr ->
      prettyError fileErr
    ReadWitnessSigningDataSigningKeyAndAddressMismatch ->
      "Only a Byron signing key may be accompanied by a Byron address."

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
  eRes <-
    first ReadWitnessSigningDataSigningKeyDecodeError
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
  -- If you update these variables, consider updating the ones with the same
  -- names in Cardano.CLI.Type.Key
  textEnvFileTypes =
    [ FromSomeType (AsSigningKey AsByronKey) (`AByronSigningWitness` mbByronAddr)
    , FromSomeType (AsSigningKey AsPaymentKey) APaymentSigningWitness
    , FromSomeType (AsSigningKey AsPaymentExtendedKey) APaymentExtendedSigningWitness
    , FromSomeType (AsSigningKey AsStakeKey) AStakeSigningWitness
    , FromSomeType (AsSigningKey AsStakeExtendedKey) AStakeExtendedSigningWitness
    , FromSomeType (AsSigningKey AsStakePoolKey) AStakePoolSigningWitness
    , FromSomeType (AsSigningKey AsGenesisKey) AGenesisSigningWitness
    , FromSomeType (AsSigningKey AsGenesisExtendedKey) AGenesisExtendedSigningWitness
    , FromSomeType (AsSigningKey AsGenesisDelegateKey) AGenesisDelegateSigningWitness
    , FromSomeType (AsSigningKey AsGenesisDelegateExtendedKey) AGenesisDelegateExtendedSigningWitness
    , FromSomeType (AsSigningKey AsGenesisUTxOKey) AGenesisUTxOSigningWitness
    , FromSomeType (AsSigningKey AsDRepKey) ADRepSigningWitness
    , FromSomeType (AsSigningKey AsDRepExtendedKey) ADRepExtendedSigningWitness
    , FromSomeType (AsSigningKey AsCommitteeColdKey) ACommitteeColdSigningWitness
    , FromSomeType (AsSigningKey AsCommitteeColdExtendedKey) ACommitteeColdExtendedSigningWitness
    , FromSomeType (AsSigningKey AsCommitteeHotKey) ACommitteeHotSigningWitness
    , FromSomeType (AsSigningKey AsCommitteeHotExtendedKey) ACommitteeHotExtendedSigningWitness
    ]

  bech32FileTypes =
    [ FromSomeType (AsSigningKey AsPaymentKey) APaymentSigningWitness
    , FromSomeType (AsSigningKey AsPaymentExtendedKey) APaymentExtendedSigningWitness
    , FromSomeType (AsSigningKey AsStakeKey) AStakeSigningWitness
    , FromSomeType (AsSigningKey AsStakeExtendedKey) AStakeExtendedSigningWitness
    , FromSomeType (AsSigningKey AsStakePoolKey) AStakePoolSigningWitness
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
  eKeyWit <-
    first RequiredSignerErrorFile <$> readKeyFileAnyOf bech32FileTypes textEnvFileTypes skFile
  return $ do
    keyWit <- eKeyWit
    case categoriseSomeSigningWitness keyWit of
      AByronWitness _ ->
        Left $ RequiredSignerErrorByronKey skFile
      AShelleyKeyWitness skey ->
        return . getHash $ toShelleySigningKey skey
 where
  textEnvFileTypes =
    [ FromSomeType (AsSigningKey AsPaymentKey) APaymentSigningWitness
    , FromSomeType (AsSigningKey AsPaymentExtendedKey) APaymentExtendedSigningWitness
    , FromSomeType (AsSigningKey AsStakePoolKey) AStakePoolSigningWitness
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

newtype VoteError
  = VoteErrorFile (FileError CliScriptWitnessError)

instance Show VoteError where
  show = show . prettyError

instance Error VoteError where
  prettyError = \case
    VoteErrorFile e ->
      prettyError e

-- Because the 'Voter' type is contained only in the 'VotingProcedures'
-- type, we must read a single vote as 'VotingProcedures'. The cli will
-- not read vote files with multiple votes in them because this will
-- complicate the code further in terms of contructing the redeemer map
-- when it comes to script witnessed votes.
readVotingProceduresFiles
  :: ConwayEraOnwards era
  -> [(VoteFile In, Maybe CliVoteScriptRequirements)]
  -> IO (Either VoteError [(VotingProcedures era, Maybe (VoteScriptWitness era))])
readVotingProceduresFiles w = \case
  [] -> return $ return []
  files ->
    runExceptT $ firstExceptT VoteErrorFile $ forM files (readVoteScriptWitness w)

readTxUpdateProposal
  :: ()
  => ShelleyToBabbageEra era
  -> UpdateProposalFile
  -> ExceptT (FileError TextEnvelopeError) IO (TxUpdateProposal era)
readTxUpdateProposal w (UpdateProposalFile upFp) = do
  TxUpdateProposal w <$> newExceptT (readFileTextEnvelope AsUpdateProposal (File upFp))

data ConstitutionError
  = ConstitutionErrorFile (FileError TextEnvelopeError)
  | ConstitutionNotSupportedInEra AnyCardanoEra
  | ConstitutionNotUnicodeError Text.UnicodeException
  deriving Show

data ProposalError
  = ProposalErrorFile (FileError CliScriptWitnessError)
  | ProposalNotSupportedInEra AnyCardanoEra
  deriving Show

instance Error ProposalError where
  prettyError = pshow

readTxGovernanceActions
  :: ShelleyBasedEra era
  -> [(ProposalFile In, Maybe CliProposalScriptRequirements)]
  -> IO (Either ProposalError [(Proposal era, Maybe (ProposalScriptWitness era))])
readTxGovernanceActions _ [] = return $ Right []
readTxGovernanceActions era files = runExceptT $ do
  w <-
    forShelleyBasedEraMaybeEon era
      & hoistMaybe
        ( ProposalNotSupportedInEra $
            cardanoEraConstraints (toCardanoEra era) $
              AnyCardanoEra (toCardanoEra era)
        )
  newExceptT $ sequence <$> mapM (readProposal w) files

readProposal
  :: ConwayEraOnwards era
  -> (ProposalFile In, Maybe CliProposalScriptRequirements)
  -> IO (Either ProposalError (Proposal era, Maybe (ProposalScriptWitness era)))
readProposal w (fp, mScriptWit) = do
  runExceptT $
    firstExceptT ProposalErrorFile $
      readProposalScriptWitness w (fp, mScriptWit)

constitutionHashSourceToHash
  :: ()
  => ConstitutionHashSource
  -> ExceptT ConstitutionError IO (L.SafeHash L.StandardCrypto L.AnchorData)
constitutionHashSourceToHash constitutionHashSource = do
  case constitutionHashSource of
    ConstitutionHashSourceFile fp -> do
      cBs <- liftIO $ BS.readFile $ unFile fp
      _utf8EncodedText <- firstExceptT ConstitutionNotUnicodeError . hoistEither $ Text.decodeUtf8' cBs
      pure $ L.hashAnchorData $ L.AnchorData cBs
    ConstitutionHashSourceText c -> do
      pure $ L.hashAnchorData $ L.AnchorData $ Text.encodeUtf8 c
    ConstitutionHashSourceHash h ->
      pure h

data CostModelsError
  = CostModelsErrorReadFile (FileError ())
  | CostModelsErrorJSONDecode FilePath String
  | CostModelsErrorEmpty FilePath
  deriving Show

instance Error CostModelsError where
  prettyError = \case
    CostModelsErrorReadFile e ->
      "Cannot read cost model: " <> prettyError e
    CostModelsErrorJSONDecode fp err ->
      "Error decoding JSON cost model at " <> pshow fp <> ": " <> pretty err <> formatExplanation
    CostModelsErrorEmpty fp ->
      "The decoded cost model was empty at: " <> pshow fp <> formatExplanation
   where
    formatExplanation =
      vsep
        [ ""
        , "The expected format of the cost models file is "
        , "{"
        , "  \"PlutusV1\" : <costModel>,"
        , "  \"PlutusV2\" : <costModel>,"
        , "  \"PlutusV3\" : <costModel>,"
        , "}"
        , "where each of the three entries may be ommited, and a <cost model> is either an ordered list of parameter values like"
        , "[205665, 812, 1, ...]"
        , "or a map like"
        , "{ \"addInteger-cpu-arguments-intercept\": 205665, \"addInteger-cpu-arguments-slope\": 812, \"addInteger-memory-arguments-intercept\": 1, ... }"
        , "In both cases, the cost model must be complete, i.e. it must specify all parameters that are needed for the specific Plutus version."
        , "It's not specified what will happen if you provide more parameters than necessary."
        ]

readCostModels
  :: File L.CostModels In
  -> ExceptT CostModelsError IO L.CostModels
readCostModels (File fp) = do
  bytes <- handleIOExceptT (CostModelsErrorReadFile . FileIOError fp) $ LBS.readFile fp
  costModels <- firstExceptT (CostModelsErrorJSONDecode fp) . except $ Aeson.eitherDecode bytes
  when (null $ fromAlonzoCostModels costModels) $ throwE $ CostModelsErrorEmpty fp
  return costModels

-- Misc

readFileInAnyShelleyBasedEra
  :: ( HasTextEnvelope (thing ShelleyEra)
     , HasTextEnvelope (thing AllegraEra)
     , HasTextEnvelope (thing MaryEra)
     , HasTextEnvelope (thing AlonzoEra)
     , HasTextEnvelope (thing BabbageEra)
     , HasTextEnvelope (thing ConwayEra)
     )
  => (forall era. AsType era -> AsType (thing era))
  -> FileOrPipe
  -> IO (Either (FileError TextEnvelopeError) (InAnyShelleyBasedEra thing))
readFileInAnyShelleyBasedEra asThing =
  readFileOrPipeTextEnvelopeAnyOf
    [ FromSomeType (asThing AsShelleyEra) (InAnyShelleyBasedEra ShelleyBasedEraShelley)
    , FromSomeType (asThing AsAllegraEra) (InAnyShelleyBasedEra ShelleyBasedEraAllegra)
    , FromSomeType (asThing AsMaryEra) (InAnyShelleyBasedEra ShelleyBasedEraMary)
    , FromSomeType (asThing AsAlonzoEra) (InAnyShelleyBasedEra ShelleyBasedEraAlonzo)
    , FromSomeType (asThing AsBabbageEra) (InAnyShelleyBasedEra ShelleyBasedEraBabbage)
    , FromSomeType (asThing AsConwayEra) (InAnyShelleyBasedEra ShelleyBasedEraConway)
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
    Nothing ->
      bracket
        (openFileBlocking fp ReadMode)
        hClose
        ( \handle -> do
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
            pure dat
        )

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
  :: [FromSomeTypeCDDL TextEnvelope b]
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
  -> IO (Either (FileError TextEnvelopeCddlError) TextEnvelope)
readTextEnvelopeCddlFromFileOrPipe file = do
  let path = fileOrPipePath file
  runExceptT $ do
    bs <-
      handleIOExceptT (FileIOError path) $
        readFileOrPipe file
    firstExceptT (FileError path . TextEnvelopeCddlAesonDecodeError path)
      . hoistEither
      $ Aeson.eitherDecode' bs

----------------------------------------------------------------------------------------------------

getStakeCredentialFromVerifier
  :: ()
  => StakeVerifier
  -> ExceptT StakeCredentialError IO StakeCredential
getStakeCredentialFromVerifier = \case
  StakeVerifierScriptFile (File sFile) -> do
    ScriptInAnyLang _ script <-
      readFileScriptInAnyLang sFile
        & firstExceptT StakeCredentialScriptDecodeError
    pure $ StakeCredentialByScript $ hashScript script
  StakeVerifierKey stakeVerKeyOrFile -> do
    stakeVerKeyHash <-
      modifyError StakeCredentialInputDecodeError $
        readVerificationKeyOrHashOrFile AsStakeKey stakeVerKeyOrFile
    pure $ StakeCredentialByKey stakeVerKeyHash

getStakeCredentialFromIdentifier
  :: ()
  => StakeIdentifier
  -> ExceptT StakeCredentialError IO StakeCredential
getStakeCredentialFromIdentifier = \case
  StakeIdentifierAddress stakeAddr -> pure $ stakeAddressCredential stakeAddr
  StakeIdentifierVerifier stakeVerifier -> getStakeCredentialFromVerifier stakeVerifier

getStakeAddressFromVerifier
  :: ()
  => NetworkId
  -> StakeVerifier
  -> ExceptT StakeCredentialError IO StakeAddress
getStakeAddressFromVerifier networkId stakeVerifier =
  makeStakeAddress networkId <$> getStakeCredentialFromVerifier stakeVerifier

getDRepCredentialFromVerKeyHashOrFile
  :: ()
  => MonadIOTransError (FileError InputDecodeError) t m
  => VerificationKeyOrHashOrFile DRepKey
  -> t m (L.Credential L.DRepRole L.StandardCrypto)
getDRepCredentialFromVerKeyHashOrFile = \case
  VerificationKeyOrFile verKeyOrFile -> do
    drepVerKey <- readVerificationKeyOrFile AsDRepKey verKeyOrFile
    pure . L.KeyHashObj . unDRepKeyHash $ verificationKeyHash drepVerKey
  VerificationKeyHash kh -> pure . L.KeyHashObj $ unDRepKeyHash kh

data ReadSafeHashError
  = ReadSafeHashErrorNotHex ByteString String
  | ReadSafeHashErrorInvalidHash Text

renderReadSafeHashError :: ReadSafeHashError -> Text
renderReadSafeHashError = \case
  ReadSafeHashErrorNotHex bs err ->
    "Error reading anchor data hash: Invalid hex: " <> Text.decodeUtf8 bs <> "\n" <> Text.pack err
  ReadSafeHashErrorInvalidHash err ->
    "Error reading anchor data hash: " <> err

readHexAsSafeHash
  :: ()
  => Text
  -> Either ReadSafeHashError (L.SafeHash L.StandardCrypto L.AnchorData)
readHexAsSafeHash hex = do
  let bs = Text.encodeUtf8 hex

  raw <- Base16.decode bs & first (ReadSafeHashErrorNotHex bs)

  case Crypto.hashFromBytes raw of
    Just a -> Right (L.unsafeMakeSafeHash a)
    Nothing -> Left $ ReadSafeHashErrorInvalidHash "Unable to read hash"

readSafeHash :: Opt.ReadM (L.SafeHash L.StandardCrypto L.AnchorData)
readSafeHash =
  Opt.eitherReader $ \s ->
    readHexAsSafeHash (Text.pack s)
      & first (Text.unpack . renderReadSafeHashError)

scriptHashReader :: Opt.ReadM ScriptHash
scriptHashReader = Opt.eitherReader $ Right . fromString

readVoteDelegationTarget
  :: ()
  => VoteDelegationTarget
  -> ExceptT DelegationError IO (L.DRep L.StandardCrypto)
readVoteDelegationTarget voteDelegationTarget =
  case voteDelegationTarget of
    VoteDelegationTargetOfDRep drepHashSource ->
      modifyError DelegationDRepReadError $
        L.DRepCredential <$> readDRepCredential drepHashSource
    VoteDelegationTargetOfAbstain ->
      pure L.DRepAlwaysAbstain
    VoteDelegationTargetOfNoConfidence ->
      pure L.DRepAlwaysNoConfidence

--- | Read the given file and hashes its content using 'Blake2b_256'
readShelleyOnwardsGenesisAndHash
  :: MonadIO m
  => FilePath
  -- ^ The file to read
  -> m (Crypto.Hash Crypto.Blake2b_256 BS.ByteString)
readShelleyOnwardsGenesisAndHash path = do
  content <- liftIO $ BS.readFile path
  return $ Crypto.hashWith id content
