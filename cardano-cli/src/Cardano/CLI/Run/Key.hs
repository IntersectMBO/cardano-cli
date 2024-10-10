{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.Run.Key
  ( runKeyCmds
  , runConvertByronGenesisVKeyCmd
  , runConvertByronKeyCmd
  , runConvertCardanoAddressKeyCmd
  , runConvertITNBip32KeyCmd
  , runConvertITNExtendedKeyCmd
  , runConvertITNKeyCmd
  , runNonExtendedKeyCmd
  , runVerificationKeyCmd
  , ccColdSkeyDesc
  , ccColdVkeyDesc
  , ccHotSkeyDesc
  , ccHotVkeyDesc
  , drepSkeyDesc
  , drepVkeyDesc
  , genesisVkeyDesc
  , genesisVkeyDelegateDesc
  , stakeVkeyDesc
  , paymentVkeyDesc

    -- * Exports for testing
  , decodeBech32
  )
where

import           Cardano.Api
import qualified Cardano.Api.Byron as ByronApi
import           Cardano.Api.Crypto.Ed25519Bip32 (xPrvFromBytes)
import qualified Cardano.Api.Ledger as L

import qualified Cardano.CLI.Byron.Key as Byron
import qualified Cardano.CLI.Commands.Key as Cmd
import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Errors.CardanoAddressSigningKeyConversionError
import           Cardano.CLI.Types.Errors.ItnKeyConversionError
import           Cardano.CLI.Types.Errors.KeyCmdError
import           Cardano.CLI.Types.Key
import qualified Cardano.Crypto.DSIGN as DSIGN
import qualified Cardano.Crypto.Signing as Byron
import qualified Cardano.Crypto.Signing as Byron.Crypto
import qualified Cardano.Crypto.Signing as Crypto
import qualified Cardano.Crypto.Wallet as Crypto

import qualified Codec.Binary.Bech32 as Bech32
import qualified Control.Exception as Exception
import           Data.Bifunctor (Bifunctor (..))
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Function
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           System.Exit (exitFailure)

-- Note on these constants:
-- https://github.com/IntersectMBO/cardano-cli/pull/416#discussion_r1378789737

ccColdSkeyDesc :: TextEnvelopeDescr
ccColdSkeyDesc = "Constitutional Committee Cold Signing Key"

ccColdExtendedSkeyDesc :: TextEnvelopeDescr
ccColdExtendedSkeyDesc = "Constitutional Committee Cold Extended Signing Key"

ccColdVkeyDesc :: TextEnvelopeDescr
ccColdVkeyDesc = "Constitutional Committee Cold Verification Key"

ccHotExtendedSkeyDesc :: TextEnvelopeDescr
ccHotExtendedSkeyDesc = "Constitutional Committee Hot Extended Signing Key"

ccHotSkeyDesc :: TextEnvelopeDescr
ccHotSkeyDesc = "Constitutional Committee Hot Signing Key"

ccHotVkeyDesc :: TextEnvelopeDescr
ccHotVkeyDesc = "Constitutional Committee Hot Verification Key"

drepSkeyDesc :: TextEnvelopeDescr
drepSkeyDesc = "Delegated Representative Signing Key"

drepExtendedSkeyDesc :: TextEnvelopeDescr
drepExtendedSkeyDesc = "Delegated Representative Extended Signing Key"

drepVkeyDesc :: TextEnvelopeDescr
drepVkeyDesc = "Delegated Representative Verification Key"

genesisVkeyDesc :: TextEnvelopeDescr
genesisVkeyDesc = "Genesis Verification Key"

genesisVkeyDelegateDesc :: TextEnvelopeDescr
genesisVkeyDelegateDesc = "Genesis delegate operator key"

paymentVkeyDesc :: TextEnvelopeDescr
paymentVkeyDesc = "Payment Verification Key"

stakeVkeyDesc :: TextEnvelopeDescr
stakeVkeyDesc = "Stake Verification Key"

runKeyCmds
  :: ()
  => Cmd.KeyCmds
  -> ExceptT KeyCmdError IO ()
runKeyCmds = \case
  Cmd.KeyVerificationKeyCmd cmd ->
    runVerificationKeyCmd cmd
  Cmd.KeyNonExtendedKeyCmd cmd ->
    runNonExtendedKeyCmd cmd
  Cmd.KeyConvertByronKeyCmd cmd ->
    runConvertByronKeyCmd cmd
  Cmd.KeyConvertByronGenesisVKeyCmd cmd ->
    runConvertByronGenesisVKeyCmd cmd
  Cmd.KeyConvertITNKeyCmd cmd ->
    runConvertITNKeyCmd cmd
  Cmd.KeyConvertITNExtendedKeyCmd cmd ->
    runConvertITNExtendedKeyCmd cmd
  Cmd.KeyConvertITNBip32KeyCmd cmd ->
    runConvertITNBip32KeyCmd cmd
  Cmd.KeyConvertCardanoAddressKeyCmd cmd ->
    runConvertCardanoAddressKeyCmd cmd

runVerificationKeyCmd
  :: ()
  => Cmd.KeyVerificationKeyCmdArgs
  -> ExceptT KeyCmdError IO ()
runVerificationKeyCmd
  Cmd.KeyVerificationKeyCmdArgs
    { Cmd.skeyFile = skf
    , Cmd.vkeyFile = vkf
    } = do
    ssk <- firstExceptT KeyCmdReadKeyFileError $ readSigningKeyFile skf
    withSomeSigningKey ssk $ \sk ->
      let vk = getVerificationKey sk
       in firstExceptT KeyCmdWriteFileError . newExceptT $
            writeLazyByteStringFile vkf $
              textEnvelopeToJSON Nothing vk

runNonExtendedKeyCmd
  :: Cmd.KeyNonExtendedKeyCmdArgs
  -> ExceptT KeyCmdError IO ()
runNonExtendedKeyCmd
  Cmd.KeyNonExtendedKeyCmdArgs
    { Cmd.extendedVkeyFileIn = evkf
    , Cmd.nonExtendedVkeyFileOut = vkf
    } =
    writeExtendedVerificationKey =<< readExtendedVerificationKeyFile evkf
   where
    -- TODO: Expose a function specifically for this purpose
    -- and explain the extended verification keys can be converted
    -- to their non-extended counterparts however this is NOT the case
    -- for extended signing keys

    writeExtendedVerificationKey
      :: SomeAddressVerificationKey
      -> ExceptT KeyCmdError IO ()
    writeExtendedVerificationKey ssk =
      case ssk of
        APaymentExtendedVerificationKey vk ->
          writeToDisk vkf (Just paymentVkeyDesc) (castVerificationKey vk :: VerificationKey PaymentKey)
        ADRepExtendedVerificationKey vk ->
          writeToDisk vkf (Just drepVkeyDesc) (castVerificationKey vk :: VerificationKey DRepKey)
        ACommitteeColdExtendedVerificationKey vk ->
          writeToDisk vkf (Just ccColdVkeyDesc) (castVerificationKey vk :: VerificationKey CommitteeColdKey)
        ACommitteeHotExtendedVerificationKey vk ->
          writeToDisk vkf (Just ccHotVkeyDesc) (castVerificationKey vk :: VerificationKey CommitteeHotKey)
        AStakeExtendedVerificationKey vk ->
          writeToDisk vkf (Just stakeVkeyDesc) (castVerificationKey vk :: VerificationKey StakeKey)
        AGenesisExtendedVerificationKey vk ->
          writeToDisk vkf (Just genesisVkeyDesc) (castVerificationKey vk :: VerificationKey GenesisKey)
        AGenesisDelegateExtendedVerificationKey vk ->
          writeToDisk
            vkf
            (Just genesisVkeyDelegateDesc)
            (castVerificationKey vk :: VerificationKey GenesisDelegateKey)
        -- Non-extended keys are below and cause failure.
        vk@AByronVerificationKey{} -> goFail vk
        vk@APaymentVerificationKey{} -> goFail vk
        vk@AGenesisUTxOVerificationKey{} -> goFail vk
        vk@AKesVerificationKey{} -> goFail vk
        vk@AVrfVerificationKey{} -> goFail vk
        vk@AStakeVerificationKey{} -> goFail vk
        vk@ADRepVerificationKey{} -> goFail vk
        vk@ACommitteeColdVerificationKey{} -> goFail vk
        vk@ACommitteeHotVerificationKey{} -> goFail vk
     where
      goFail nonExtendedKey = left $ KeyCmdExpectedExtendedVerificationKey nonExtendedKey

    writeToDisk
      :: Key keyrole
      => File content Out
      -> Maybe TextEnvelopeDescr
      -> VerificationKey keyrole
      -> ExceptT KeyCmdError IO ()
    writeToDisk vkf' descr vk =
      firstExceptT KeyCmdWriteFileError . newExceptT $
        writeLazyByteStringFile vkf' $
          textEnvelopeToJSON descr vk

readExtendedVerificationKeyFile
  :: VerificationKeyFile In
  -> ExceptT KeyCmdError IO SomeAddressVerificationKey
readExtendedVerificationKeyFile evkfile = do
  vKey <-
    firstExceptT KeyCmdVerificationKeyReadError
      . newExceptT
      $ readVerificationKeyTextOrFileAnyOf
      $ VktofVerificationKeyFile evkfile
  case vKey of
    k@APaymentExtendedVerificationKey{} -> return k
    k@ADRepExtendedVerificationKey{} -> return k
    k@ACommitteeColdExtendedVerificationKey{} -> return k
    k@ACommitteeHotExtendedVerificationKey{} -> return k
    k@AStakeExtendedVerificationKey{} -> return k
    k@AGenesisExtendedVerificationKey{} -> return k
    k@AGenesisDelegateExtendedVerificationKey{} -> return k
    -- Non-extended keys are below and cause failure.
    k@AByronVerificationKey{} -> goFail k
    k@APaymentVerificationKey{} -> goFail k
    k@AGenesisUTxOVerificationKey{} -> goFail k
    k@AKesVerificationKey{} -> goFail k
    k@AVrfVerificationKey{} -> goFail k
    k@AStakeVerificationKey{} -> goFail k
    k@ADRepVerificationKey{} -> goFail k
    k@ACommitteeColdVerificationKey{} -> goFail k
    k@ACommitteeHotVerificationKey{} -> goFail k
 where
  goFail k = left $ KeyCmdExpectedExtendedVerificationKey k

runConvertByronKeyCmd
  :: Cmd.KeyConvertByronKeyCmdArgs
  -> ExceptT KeyCmdError IO ()
runConvertByronKeyCmd
  Cmd.KeyConvertByronKeyCmdArgs
    { Cmd.mPassword = mPwd
    , Cmd.byronKeyType
    , Cmd.someKeyFileIn = inFile
    , Cmd.someKeyFileOut = outFile
    } =
    case (byronKeyType, inFile) of
      (ByronPaymentKey format, ASigningKeyFile skeyPathOld) ->
        convertByronSigningKey mPwd format convert skeyPathOld outFile
       where
        convert :: Byron.SigningKey -> SigningKey ByronKey
        convert = ByronSigningKey
      (ByronGenesisKey format, ASigningKeyFile skeyPathOld) ->
        convertByronSigningKey mPwd format convert skeyPathOld outFile
       where
        convert :: Byron.SigningKey -> SigningKey GenesisExtendedKey
        convert (Byron.SigningKey xsk) = GenesisExtendedSigningKey xsk
      (ByronDelegateKey format, ASigningKeyFile skeyPathOld) ->
        convertByronSigningKey mPwd format convert skeyPathOld outFile
       where
        convert :: Byron.SigningKey -> SigningKey GenesisDelegateExtendedKey
        convert (Byron.SigningKey xsk) = GenesisDelegateExtendedSigningKey xsk
      (ByronPaymentKey NonLegacyByronKeyFormat, AVerificationKeyFile vkeyPathOld) ->
        convertByronVerificationKey convert vkeyPathOld outFile
       where
        convert :: Byron.VerificationKey -> VerificationKey ByronKey
        convert = ByronVerificationKey
      (ByronGenesisKey NonLegacyByronKeyFormat, AVerificationKeyFile vkeyPathOld) ->
        convertByronVerificationKey convert vkeyPathOld outFile
       where
        convert :: Byron.VerificationKey -> VerificationKey GenesisExtendedKey
        convert (Byron.VerificationKey xvk) = GenesisExtendedVerificationKey xvk
      (ByronDelegateKey NonLegacyByronKeyFormat, AVerificationKeyFile vkeyPathOld) ->
        convertByronVerificationKey convert vkeyPathOld outFile
       where
        convert :: Byron.VerificationKey -> VerificationKey GenesisDelegateExtendedKey
        convert (Byron.VerificationKey xvk) =
          GenesisDelegateExtendedVerificationKey xvk
      (ByronPaymentKey LegacyByronKeyFormat, AVerificationKeyFile{}) ->
        legacyVerificationKeysNotSupported
      (ByronGenesisKey LegacyByronKeyFormat, AVerificationKeyFile{}) ->
        legacyVerificationKeysNotSupported
      (ByronDelegateKey LegacyByronKeyFormat, AVerificationKeyFile{}) ->
        legacyVerificationKeysNotSupported

legacyVerificationKeysNotSupported :: ExceptT e IO a
legacyVerificationKeysNotSupported =
  liftIO $ do
    putStrLn $
      "convert keys: byron legacy format not supported for "
        ++ "verification keys. Convert the signing key and then get the "
        ++ "verification key."
    exitFailure

convertByronSigningKey
  :: forall keyrole
   . Key keyrole
  => Maybe Text
  -- ^ Password (if applicable)
  -> ByronKeyFormat
  -> (Byron.SigningKey -> SigningKey keyrole)
  -> SigningKeyFile In
  -- ^ Input file: old format
  -> File () Out
  -- ^ Output file: new format
  -> ExceptT KeyCmdError IO ()
convertByronSigningKey mPwd byronFormat convert skeyPathOld skeyPathNew = do
  sKey <-
    firstExceptT KeyCmdByronKeyFailure $
      Byron.readByronSigningKey byronFormat skeyPathOld

  -- Account for password protected legacy Byron keys
  unprotectedSk <- case sKey of
    ByronApi.AByronSigningKeyLegacy (ByronSigningKeyLegacy sk@(Crypto.SigningKey xprv)) ->
      case mPwd of
        -- Change password to empty string
        Just pwd ->
          return . Crypto.SigningKey $
            Crypto.xPrvChangePass (Text.encodeUtf8 pwd) (Text.encodeUtf8 "") xprv
        Nothing -> return sk
    ByronApi.AByronSigningKey (ByronSigningKey sk) -> return sk

  let sk' :: SigningKey keyrole
      sk' = convert unprotectedSk

  firstExceptT KeyCmdWriteFileError . newExceptT $
    writeLazyByteStringFile skeyPathNew $
      textEnvelopeToJSON Nothing sk'

convertByronVerificationKey
  :: forall keyrole
   . Key keyrole
  => (Byron.VerificationKey -> VerificationKey keyrole)
  -> VerificationKeyFile In
  -- ^ Input file: old format
  -> File () Out
  -- ^ Output file: new format
  -> ExceptT KeyCmdError IO ()
convertByronVerificationKey convert vkeyPathOld vkeyPathNew = do
  vk <-
    firstExceptT KeyCmdByronKeyFailure $
      Byron.readPaymentVerificationKey vkeyPathOld

  let vk' :: VerificationKey keyrole
      vk' = convert vk

  firstExceptT KeyCmdWriteFileError . newExceptT $
    writeLazyByteStringFile vkeyPathNew $
      textEnvelopeToJSON Nothing vk'

runConvertByronGenesisVKeyCmd
  :: Cmd.KeyConvertByronGenesisVKeyCmdArgs
  -> ExceptT KeyCmdError IO ()
runConvertByronGenesisVKeyCmd
  Cmd.KeyConvertByronGenesisVKeyCmdArgs
    { Cmd.vkey = VerificationKeyBase64 b64ByronVKey
    , Cmd.vkeyFileOut = vkeyPathNew
    } = do
    vk <-
      firstExceptT (KeyCmdByronKeyParseError . textShow)
        . hoistEither
        . Byron.Crypto.parseFullVerificationKey
        . Text.pack
        $ b64ByronVKey

    let vk' :: VerificationKey GenesisKey
        vk' = convert vk

    firstExceptT KeyCmdWriteFileError . newExceptT $
      writeLazyByteStringFile vkeyPathNew $
        textEnvelopeToJSON Nothing vk'
   where
    convert :: Byron.VerificationKey -> VerificationKey GenesisKey
    convert (Byron.VerificationKey xvk) =
      castVerificationKey (GenesisExtendedVerificationKey xvk)

--------------------------------------------------------------------------------
-- ITN verification/signing key conversion to Haskell verficiation/signing keys
--------------------------------------------------------------------------------

runConvertITNKeyCmd
  :: Cmd.KeyConvertITNKeyCmdArgs
  -> ExceptT KeyCmdError IO ()
runConvertITNKeyCmd
  Cmd.KeyConvertITNKeyCmdArgs
    { Cmd.itnKeyFile
    , Cmd.outFile
    } =
    case itnKeyFile of
      AVerificationKeyFile (File vk) -> do
        bech32publicKey <-
          firstExceptT KeyCmdItnKeyConvError . newExceptT $
            readFileITNKey vk
        vkey <-
          hoistEither
            . first KeyCmdItnKeyConvError
            $ convertITNVerificationKey bech32publicKey
        firstExceptT KeyCmdWriteFileError . newExceptT $
          writeLazyByteStringFile outFile $
            textEnvelopeToJSON Nothing vkey
      ASigningKeyFile (File sk) -> do
        bech32privateKey <-
          firstExceptT KeyCmdItnKeyConvError . newExceptT $
            readFileITNKey sk
        skey <-
          hoistEither
            . first KeyCmdItnKeyConvError
            $ convertITNSigningKey bech32privateKey
        firstExceptT KeyCmdWriteFileError . newExceptT $
          writeLazyByteStringFile outFile $
            textEnvelopeToJSON Nothing skey

runConvertITNExtendedKeyCmd
  :: ()
  => Cmd.KeyConvertITNExtendedKeyCmdArgs
  -> ExceptT KeyCmdError IO ()
runConvertITNExtendedKeyCmd
  Cmd.KeyConvertITNExtendedKeyCmdArgs
    { Cmd.itnPrivKeyFile
    , Cmd.outFile
    } =
    case itnPrivKeyFile of
      AVerificationKeyFile _ ->
        left KeyCmdWrongKeyTypeError
      ASigningKeyFile (File sk) -> do
        bech32privateKey <- firstExceptT KeyCmdItnKeyConvError . newExceptT $ readFileITNKey sk
        skey <-
          convertITNExtendedSigningKey bech32privateKey
            & first KeyCmdItnKeyConvError
            & hoistEither
        firstExceptT KeyCmdWriteFileError . newExceptT $
          writeLazyByteStringFile outFile $
            textEnvelopeToJSON Nothing skey

runConvertITNBip32KeyCmd
  :: ()
  => Cmd.KeyConvertITNBip32KeyCmdArgs
  -> ExceptT KeyCmdError IO ()
runConvertITNBip32KeyCmd
  Cmd.KeyConvertITNBip32KeyCmdArgs
    { Cmd.itnPrivKeyFile
    , Cmd.outFile
    } =
    case itnPrivKeyFile of
      AVerificationKeyFile _ ->
        left KeyCmdWrongKeyTypeError
      ASigningKeyFile (File sk) -> do
        bech32privateKey <- firstExceptT KeyCmdItnKeyConvError . newExceptT $ readFileITNKey sk
        skey <-
          convertITNBIP32SigningKey bech32privateKey
            & first KeyCmdItnKeyConvError
            & hoistEither
        firstExceptT KeyCmdWriteFileError . newExceptT $
          writeLazyByteStringFile outFile $
            textEnvelopeToJSON Nothing skey

-- | Convert public ed25519 key to a Shelley stake verification key
convertITNVerificationKey :: Text -> Either ItnKeyConversionError (VerificationKey StakeKey)
convertITNVerificationKey pubKey = do
  (_, _, keyBS) <- first ItnKeyBech32DecodeError (decodeBech32 pubKey)
  case DSIGN.rawDeserialiseVerKeyDSIGN keyBS of
    Just verKey -> Right . StakeVerificationKey $ L.VKey verKey
    Nothing -> Left $ ItnVerificationKeyDeserialisationError keyBS

-- | Convert private ed22519 key to a Shelley signing key.
convertITNSigningKey :: Text -> Either ItnKeyConversionError (SigningKey StakeKey)
convertITNSigningKey privKey = do
  (_, _, keyBS) <- first ItnKeyBech32DecodeError (decodeBech32 privKey)
  case DSIGN.rawDeserialiseSignKeyDSIGN keyBS of
    Just signKey -> Right $ StakeSigningKey signKey
    Nothing -> Left $ ItnSigningKeyDeserialisationError keyBS

-- | Convert extended private ed22519 key to a Shelley signing key
-- Extended private key = 64 bytes,
-- Public key = 32 bytes.
convertITNExtendedSigningKey :: Text -> Either ItnKeyConversionError (SigningKey StakeExtendedKey)
convertITNExtendedSigningKey privKey = do
  (_, _, privkeyBS) <- first ItnKeyBech32DecodeError (decodeBech32 privKey)
  let dummyChainCode = BS.replicate 32 0
  case xPrvFromBytes $ BS.concat [privkeyBS, dummyChainCode] of
    Just xprv -> Right $ StakeExtendedSigningKey xprv
    Nothing -> Left $ ItnSigningKeyDeserialisationError privkeyBS

-- BIP32 Private key = 96 bytes (64 bytes extended private key + 32 bytes chaincode)
-- BIP32 Public Key = 64 Bytes
convertITNBIP32SigningKey :: Text -> Either ItnKeyConversionError (SigningKey StakeExtendedKey)
convertITNBIP32SigningKey privKey = do
  (_, _, privkeyBS) <- first ItnKeyBech32DecodeError (decodeBech32 privKey)
  case xPrvFromBytes privkeyBS of
    Just xprv -> Right $ StakeExtendedSigningKey xprv
    Nothing -> Left $ ItnSigningKeyDeserialisationError privkeyBS

readFileITNKey :: FilePath -> IO (Either ItnKeyConversionError Text)
readFileITNKey fp = do
  eStr <- Exception.try $ readFile fp
  case eStr of
    Left e -> return . Left $ ItnReadBech32FileError fp e
    Right str -> return . Right . Text.concat $ Text.words $ Text.pack str

--------------------------------------------------------------------------------
-- `cardano-address` extended signing key conversions
--------------------------------------------------------------------------------

runConvertCardanoAddressKeyCmd
  :: ()
  => Cmd.KeyConvertCardanoAddressKeyCmdArgs
  -> ExceptT KeyCmdError IO ()
runConvertCardanoAddressKeyCmd
  Cmd.KeyConvertCardanoAddressKeyCmdArgs
    { cardanoAddressKeyType = keyType
    , skeyFileIn = skFile
    , skeyFileOut = outFile
    } = do
    sKey <-
      firstExceptT KeyCmdCardanoAddressSigningKeyFileError
        . newExceptT
        $ readSomeCardanoAddressSigningKeyFile keyType skFile
    firstExceptT KeyCmdWriteFileError . newExceptT $
      writeSomeCardanoAddressSigningKeyFile outFile sKey

-- | Some kind of signing key that was converted from a @cardano-address@
-- signing key.
data SomeCardanoAddressSigningKey
  = ACardanoAddrShelleyPaymentSigningKey !(SigningKey PaymentExtendedKey)
  | ACardanoAddrShelleyStakeSigningKey !(SigningKey StakeExtendedKey)
  | ACardanoAddrByronSigningKey !(SigningKey ByronKey)
  | ACardanoAddrCommitteeColdKey !(SigningKey CommitteeColdExtendedKey)
  | ACardanoAddrCommitteeHotKey !(SigningKey CommitteeHotExtendedKey)
  | ACardanoAddrDRepKey !(SigningKey DRepExtendedKey)

-- | Decode a Bech32-encoded string.
decodeBech32
  :: Text
  -> Either Bech32DecodeError (Bech32.HumanReadablePart, Bech32.DataPart, ByteString)
decodeBech32 bech32Str =
  case Bech32.decodeLenient bech32Str of
    Left err -> Left (Bech32DecodingError err)
    Right (hrPart, dataPart) ->
      case Bech32.dataPartToBytes dataPart of
        Nothing ->
          Left $ Bech32DataPartToBytesError (Bech32.dataPartToText dataPart)
        Just bs -> Right (hrPart, dataPart, bs)

-- | Convert a Ed25519 BIP32 extended signing key (96 bytes) to a @cardano-crypto@
-- style extended signing key.
--
-- Note that both the ITN and @cardano-address@ use this key format.
convertBip32SigningKey
  :: ByteString
  -> Either CardanoAddressSigningKeyConversionError Crypto.XPrv
convertBip32SigningKey signingKeyBs =
  case xPrvFromBytes signingKeyBs of
    Just xPrv -> Right xPrv
    Nothing ->
      Left $ CardanoAddressSigningKeyDeserialisationError signingKeyBs

-- | Read a file containing a Bech32-encoded Ed25519 BIP32 extended signing
-- key.
readBech32Bip32SigningKeyFile
  :: SigningKeyFile In
  -> IO (Either (FileError CardanoAddressSigningKeyConversionError) Crypto.XPrv)
readBech32Bip32SigningKeyFile (File fp) = do
  eStr <- Exception.try $ readFile fp
  case eStr of
    Left e -> pure . Left $ FileIOError fp e
    Right str ->
      case decodeBech32 (Text.concat $ Text.words $ Text.pack str) of
        Left err ->
          pure $
            Left $
              FileError fp (CardanoAddressSigningKeyBech32DecodeError err)
        Right (_hrPart, _dataPart, bs) ->
          pure $ first (FileError fp) (convertBip32SigningKey bs)

-- | Read a file containing a Bech32-encoded @cardano-address@ extended
-- signing key.
readSomeCardanoAddressSigningKeyFile
  :: CardanoAddressKeyType
  -> SigningKeyFile In
  -> IO (Either (FileError CardanoAddressSigningKeyConversionError) SomeCardanoAddressSigningKey)
readSomeCardanoAddressSigningKeyFile keyType skFile = do
  xPrv <- readBech32Bip32SigningKeyFile skFile
  pure (toSomeCardanoAddressSigningKey <$> xPrv)
 where
  toSomeCardanoAddressSigningKey :: Crypto.XPrv -> SomeCardanoAddressSigningKey
  toSomeCardanoAddressSigningKey xPrv =
    case keyType of
      CardanoAddressShelleyPaymentKey -> ACardanoAddrShelleyPaymentSigningKey (PaymentExtendedSigningKey xPrv)
      CardanoAddressShelleyStakeKey -> ACardanoAddrShelleyStakeSigningKey (StakeExtendedSigningKey xPrv)
      CardanoAddressIcarusPaymentKey -> ACardanoAddrByronSigningKey $ ByronSigningKey (Byron.SigningKey xPrv)
      CardanoAddressByronPaymentKey -> ACardanoAddrByronSigningKey $ ByronSigningKey (Byron.SigningKey xPrv)
      CardanoAddressCommitteeColdKey -> ACardanoAddrCommitteeColdKey (CommitteeColdExtendedSigningKey xPrv)
      CardanoAddressCommitteeHotKey -> ACardanoAddrCommitteeHotKey (CommitteeHotExtendedSigningKey xPrv)
      CardanoAddressDRepKey -> ACardanoAddrDRepKey (DRepExtendedSigningKey xPrv)

-- | Write a text envelope formatted file containing a @cardano-address@
-- extended signing key, but converted to a format supported by @cardano-cli@.
writeSomeCardanoAddressSigningKeyFile
  :: File direction Out
  -> SomeCardanoAddressSigningKey
  -> IO (Either (FileError ()) ())
writeSomeCardanoAddressSigningKeyFile outFile =
  \case
    ACardanoAddrShelleyPaymentSigningKey sk -> go Nothing sk
    ACardanoAddrShelleyStakeSigningKey sk -> go Nothing sk
    ACardanoAddrByronSigningKey sk -> go Nothing sk
    ACardanoAddrCommitteeColdKey sk -> go (Just ccColdExtendedSkeyDesc) sk
    ACardanoAddrCommitteeHotKey sk -> go (Just ccHotExtendedSkeyDesc) sk
    ACardanoAddrDRepKey sk -> go (Just drepExtendedSkeyDesc) sk
 where
  go envelope sk = writeLazyByteStringFile outFile $ textEnvelopeToJSON envelope sk
