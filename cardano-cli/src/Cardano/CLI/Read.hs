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
    readFileTxMetadata
  , readTxMetadata

    -- * Script
  , ScriptDecodeError (..)
  , deserialiseScriptInAnyLang
  , readFileScriptInAnyLang
  , PlutusScriptDecodeError (..)

    -- * Script data (datums and redeemers)
  , ScriptDataError (..)
  , renderScriptDataError

    -- * Tx
  , IncompleteTxBody (..)
  , readFileTx
  , readFileTxBody
  , readTx -- For testing purposes

    -- * Tx witnesses
  , ReadWitnessSigningDataError (..)
  , renderReadWitnessSigningDataError
  , SomeSigningWitness (..)
  , ByronOrShelleyWitness (..)
  , mkShelleyBootstrapWitness
  , ShelleyBootstrapWitnessSigningKeyData (..)
  , readFileTxKeyWitness
  , readWitnessSigningData
  , txWitnessTextEnvelopeTypes -- For testing purposes

    -- * Required signer
  , RequiredSignerError (..)
  , categoriseSomeSigningWitness
  , readRequiredSigner

    -- * Governance related
  , ConstitutionError (..)
  , VoteError (..)
  , CostModelsError (..)
  , readCostModels

    -- * FileOrPipe
  , FileOrPipe
  , fileOrPipe
  , fileOrPipePath
  , fileOrPipeCache
  , readFileOrPipe
  , readFileOrPipeTextEnvelopeAnyOf

    -- * Stake credentials
  , getStakeCredentialFromVerifier
  , getStakeCredentialFromIdentifier
  , getStakeAddressFromVerifier

    -- * Stake pool credentials
  , getHashFromStakePoolKeyHashSource
  , getVerificationKeyFromStakePoolVerificationKeySource

    -- * DRep credentials
  , ReadSafeHashError (..)
  , readHexAsSafeHash
  , readSafeHash
  , scriptHashReader

    -- * Update proposals
  , readTxUpdateProposal

    -- * Vote related
  , readVoteDelegationTarget
  , readVerificationKeySource

    -- * Genesis hashes
  , readShelleyOnwardsGenesisAndHash
  , readFileCli

    -- * Plutus
  , readFilePlutusScript

    -- * utilities
  , readerFromParsecParser
  )
where

import Cardano.Api as Api
import Cardano.Api.Byron (ByronKey)
import Cardano.Api.Byron qualified as Byron
import Cardano.Api.Experimental qualified as Exp
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Parser.Text qualified as P

import Cardano.CLI.Compatible.Exception
import Cardano.CLI.EraBased.Script.Type
import Cardano.CLI.Type.Common
import Cardano.CLI.Type.Error.BootstrapWitnessError
import Cardano.CLI.Type.Error.PlutusScriptDecodeError
import Cardano.CLI.Type.Error.ScriptDataError
import Cardano.CLI.Type.Error.ScriptDecodeError
import Cardano.CLI.Type.Governance
import Cardano.CLI.Type.Key
import Cardano.Crypto.Hash qualified as Crypto
import Cardano.Ledger.Api qualified as L

import RIO (readFileBinary)
import Prelude

import Control.Exception (bracket)
import Control.Monad (unless, when)
import Data.Aeson qualified as Aeson
import Data.Bifunctor
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Function ((&))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.Encoding.Error qualified as Text
import GHC.IO.Handle (hClose, hIsSeekable)
import GHC.IO.Handle.FD (openFileBlocking)
import GHC.Stack
import Options.Applicative qualified as Opt
import System.IO (IOMode (ReadMode))

-- Metadata

readTxMetadata
  :: Exp.Era era
  -> TxMetadataJsonSchema
  -> [MetadataFile]
  -> CIO e (TxMetadataInEra era)
readTxMetadata _ _ [] = return TxMetadataNone
readTxMetadata era schema files = do
  metadata <- mapM (readFileTxMetadata schema) files
  pure $ TxMetadataInEra (convert era) $ mconcat metadata

readFileTxMetadata
  :: TxMetadataJsonSchema
  -> MetadataFile
  -> CIO e TxMetadata
readFileTxMetadata mapping (MetadataFileJSON fp) = do
  bs <- readFileCli (unFile fp)
  v <-
    fromEitherCli $
      Aeson.eitherDecode' $
        LBS.fromStrict bs
  txMetadata' <-
    fromEitherCli $
      metadataFromJson mapping v

  fromEitherCli $ validateTxMetadata txMetadata'

  return txMetadata'
readFileTxMetadata _ (MetadataFileCBOR fp) = do
  bs <- readFileCli (unFile fp)
  txMetadata' <-
    fromEitherCli $
      deserialiseFromCBOR AsTxMetadata bs
  fromEitherCli $
    do
      validateTxMetadata txMetadata'
      return txMetadata'

-- Script witnesses/ Scripts

readVerificationKeySource
  :: Key keyrole
  => (Hash keyrole -> L.KeyHash kr)
  -> VerificationKeySource keyrole
  -> CIO e (L.Credential kr)
readVerificationKeySource extractHash = \case
  VksScriptHash (ScriptHash scriptHash) ->
    pure $ L.ScriptHashObj scriptHash
  VksScript (File fp) -> do
    ScriptInAnyLang _lang script <-
      readFileScriptInAnyLang fp
    pure . L.ScriptHashObj . toShelleyScriptHash $ hashScript script
  VksKeyHashFile vKeyOrHashOrFile ->
    (L.KeyHashObj . extractHash <$> readVerificationKeyOrHashOrTextEnvFile vKeyOrHashOrFile)

-- | Read a script file. The file can either be in the text envelope format
-- wrapping the binary representation of any of the supported script languages,
-- or alternatively it can be a JSON format file for one of the simple script
-- language versions.
readFileScriptInAnyLang
  :: MonadIO m
  => FilePath
  -> m ScriptInAnyLang
readFileScriptInAnyLang file = do
  scriptBytes <-
    readFileCli
      file
  fromEitherCli $ deserialiseScriptInAnyLang scriptBytes

deserialiseScriptInAnyLang
  :: BS.ByteString
  -> Either ScriptDecodeError ScriptInAnyLang
deserialiseScriptInAnyLang bs =
  -- Accept either the text envelope format wrapping the binary serialisation,
  -- or accept the simple script language in its JSON format.
  --
  case deserialiseFromJSON bs of
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

readFileTx :: FileOrPipe -> IO (Either (FileError TextEnvelopeError) (InAnyShelleyBasedEra Tx))
readFileTx file = do
  cddlTxOrErr <- readTx file
  case cddlTxOrErr of
    Left e -> return $ Left e
    Right cddlTx -> do
      InAnyShelleyBasedEra sbe tx <- pure cddlTx
      return $ Right $ inAnyShelleyBasedEra sbe tx

newtype IncompleteTxBody
  = IncompleteTxBody {unIncompleteTxBody :: InAnyShelleyBasedEra TxBody}

readFileTxBody :: FileOrPipe -> IO (Either (FileError TextEnvelopeError) IncompleteTxBody)
readFileTxBody file = do
  cddlTxOrErr <- readTx file
  case cddlTxOrErr of
    Left e -> return $ Left e
    Right cddlTx -> do
      InAnyShelleyBasedEra sbe tx <- pure cddlTx
      return $ Right $ IncompleteTxBody $ inAnyShelleyBasedEra sbe $ getTxBody tx

readTx :: FileOrPipe -> IO (Either (FileError TextEnvelopeError) (InAnyShelleyBasedEra Tx))
readTx =
  readFileOrPipeTextEnvelopeAnyOf fromSomeShelleyTx

fromSomeShelleyTx :: [FromSomeType HasTextEnvelope (InAnyShelleyBasedEra Tx)]
fromSomeShelleyTx =
  [ shelleyBasedEraConstraints sbe $ FromSomeType (makeTxProxy sbe) (InAnyShelleyBasedEra sbe)
  | AnyShelleyBasedEra sbe <- [minBound .. maxBound]
  ]
 where
  makeTxProxy :: HasTypeProxy era => ShelleyBasedEra era -> AsType (Tx era)
  makeTxProxy _ = AsTx (proxyToAsType (Proxy :: Proxy era))

-- Tx witnesses

readFileTxKeyWitness
  :: FilePath
  -> IO (Either (FileError TextEnvelopeError) (InAnyShelleyBasedEra KeyWitness))
readFileTxKeyWitness fp = do
  file <- fileOrPipe fp
  readFileInAnyShelleyBasedEra AsKeyWitness file

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
  | AStakePoolExtendedSigningWitness (SigningKey StakePoolExtendedKey)
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

-- | Construct a Shelley bootstrap witness (i.e. a Byron key witness in the
-- Shelley era).
mkShelleyBootstrapWitness
  :: ()
  => ShelleyBasedEra era
  -> Maybe NetworkId
  -> L.TxBody (ShelleyLedgerEra era)
  -> ShelleyBootstrapWitnessSigningKeyData
  -> Either BootstrapWitnessError (KeyWitness era)
mkShelleyBootstrapWitness _ Nothing _ (ShelleyBootstrapWitnessSigningKeyData _ Nothing) =
  Left MissingNetworkIdOrByronAddressError
mkShelleyBootstrapWitness sbe (Just nw) txBody (ShelleyBootstrapWitnessSigningKeyData skey Nothing) =
  Right $ makeShelleyBasedBootstrapWitness sbe (Byron.WitnessNetworkId nw) txBody skey
mkShelleyBootstrapWitness sbe _ txBody (ShelleyBootstrapWitnessSigningKeyData skey (Just addr)) =
  Right $ makeShelleyBasedBootstrapWitness sbe (Byron.WitnessByronAddress addr) txBody skey

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
    AStakePoolExtendedSigningWitness sk -> AShelleyKeyWitness (WitnessStakePoolExtendedKey sk)
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
      <$> readFormattedFileAnyOf bech32FileTypes textEnvFileTypes skFile
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
    , FromSomeType (AsSigningKey AsStakePoolExtendedKey) AStakePoolExtendedSigningWitness
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
    , FromSomeType (AsSigningKey AsStakePoolExtendedKey) AStakePoolExtendedSigningWitness
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
    first RequiredSignerErrorFile <$> readFormattedFileAnyOf bech32FileTypes textEnvFileTypes skFile
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
    , FromSomeType (AsSigningKey AsStakePoolExtendedKey) AStakePoolExtendedSigningWitness
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

readTxUpdateProposal
  :: ()
  => ShelleyToBabbageEra era
  -> UpdateProposalFile
  -> ExceptT (FileError TextEnvelopeError) IO (TxUpdateProposal era)
readTxUpdateProposal w (UpdateProposalFile upFp) = do
  TxUpdateProposal w <$> newExceptT (readFileTextEnvelope (File upFp))

newtype ConstitutionError
  = ConstitutionNotUnicodeError Text.UnicodeException
  deriving Show

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

----------------------------------------------------------------------------------------------------

getStakeCredentialFromVerifier
  :: ()
  => StakeVerifier
  -> CIO e StakeCredential
getStakeCredentialFromVerifier = \case
  StakeVerifierScriptFile (File sFile) -> do
    ScriptInAnyLang _ script <-
      readFileScriptInAnyLang sFile
    pure $ StakeCredentialByScript $ hashScript script
  StakeVerifierKey stakeVerKeyOrFile -> do
    stakeVerKeyHash <-
      readVerificationKeyOrHashOrFile stakeVerKeyOrFile
    pure $ StakeCredentialByKey stakeVerKeyHash

getStakeCredentialFromIdentifier
  :: ()
  => StakeIdentifier
  -> CIO e StakeCredential
getStakeCredentialFromIdentifier = \case
  StakeIdentifierAddress stakeAddr -> pure $ stakeAddressCredential stakeAddr
  StakeIdentifierVerifier stakeVerifier -> getStakeCredentialFromVerifier stakeVerifier

getStakeAddressFromVerifier
  :: ()
  => NetworkId
  -> StakeVerifier
  -> CIO e StakeAddress
getStakeAddressFromVerifier networkId stakeVerifier =
  makeStakeAddress networkId <$> getStakeCredentialFromVerifier stakeVerifier

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
  -> Either ReadSafeHashError (L.SafeHash L.AnchorData)
readHexAsSafeHash hex = do
  let bs = Text.encodeUtf8 hex

  raw <- Base16.decode bs & first (ReadSafeHashErrorNotHex bs)

  case Crypto.hashFromBytes raw of
    Just a -> Right (L.unsafeMakeSafeHash a)
    Nothing -> Left $ ReadSafeHashErrorInvalidHash "Unable to read hash"

readSafeHash :: Opt.ReadM (L.SafeHash L.AnchorData)
readSafeHash =
  Opt.eitherReader $ \s ->
    readHexAsSafeHash (Text.pack s)
      & first (Text.unpack . renderReadSafeHashError)

scriptHashReader :: Opt.ReadM ScriptHash
scriptHashReader = readerFromParsecParser parseScriptHash

readVoteDelegationTarget
  :: ()
  => VoteDelegationTarget
  -> CIO e L.DRep
readVoteDelegationTarget voteDelegationTarget =
  case voteDelegationTarget of
    VoteDelegationTargetOfDRep drepHashSource ->
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

-- | Get the hash from a stake pool key hash source
getHashFromStakePoolKeyHashSource
  :: MonadIO m => StakePoolKeyHashSource -> m (Hash StakePoolKey)
getHashFromStakePoolKeyHashSource hashSource =
  case hashSource of
    StakePoolKeyHashSource vkeySource ->
      anyStakePoolVerificationKeyHash <$> getVerificationKeyFromStakePoolVerificationKeySource vkeySource
    StakePoolKeyHashLiteral hash -> pure hash

-- | Get the verification key from a stake pool verification key source
getVerificationKeyFromStakePoolVerificationKeySource
  :: MonadIO m => StakePoolVerificationKeySource -> m AnyStakePoolVerificationKey
getVerificationKeyFromStakePoolVerificationKeySource = \case
  StakePoolVerificationKeyFromFile (File file) -> do
    f <- liftIO $ fileOrPipe file
    fromEitherIOCli $ readStakePoolVerificationKeyFile f
  StakePoolVerificationKeyFromLiteral keyLiteral -> pure keyLiteral
 where
  readStakePoolVerificationKeyFile
    :: FileOrPipe -> IO (Either (FileError TextEnvelopeError) AnyStakePoolVerificationKey)
  readStakePoolVerificationKeyFile = readFileOrPipeTextEnvelopeAnyOf types
   where
    types =
      [ FromSomeType (AsVerificationKey AsStakePoolKey) AnyStakePoolNormalVerificationKey
      , FromSomeType (AsVerificationKey AsStakePoolExtendedKey) AnyStakePoolExtendedVerificationKey
      ]

readFileCli :: (HasCallStack, MonadIO m) => FilePath -> m ByteString
readFileCli = withFrozenCallStack . readFileBinary

readerFromParsecParser :: P.Parser a -> Opt.ReadM a
readerFromParsecParser p = Opt.eitherReader (P.runParser p . T.pack)

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
      AnyPlutusScriptVersion PlutusScriptV4 ->
        FromSomeType (AsPlutusScript AsPlutusScriptV4) (AnyPlutusScript PlutusScriptV4)
