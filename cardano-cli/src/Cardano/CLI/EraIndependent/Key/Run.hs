{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.CLI.EraIndependent.Key.Run
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
  , stakePoolVKeyDesc
  , paymentVkeyDesc

    -- * Exports for testing
  , decodeBech32
  )
where

import Cardano.Api
import Cardano.Api.Byron
  ( ByronKey
  , SigningKey (ByronSigningKey, ByronSigningKeyLegacy)
  , VerificationKey (ByronVerificationKey)
  )
import Cardano.Api.Byron qualified as ByronApi
import Cardano.Api.Crypto.Ed25519Bip32 (xPrvFromBytes)
import Cardano.Api.Ledger qualified as L

import Cardano.CLI.Byron.Key qualified as Byron
import Cardano.CLI.Compatible.Exception
import Cardano.CLI.EraIndependent.Key.Command qualified as Cmd
import Cardano.CLI.Orphan ()
import Cardano.CLI.Run.Mnemonic qualified as Mnemonic
import Cardano.CLI.Type.Common
import Cardano.CLI.Type.Error.CardanoAddressSigningKeyConversionError
import Cardano.CLI.Type.Error.ItnKeyConversionError
import Cardano.CLI.Type.Error.KeyCmdError
import Cardano.CLI.Type.Key
import Cardano.Crypto.DSIGN qualified as DSIGN
import Cardano.Crypto.Signing qualified as Byron
import Cardano.Crypto.Signing qualified as Byron.Crypto
import Cardano.Crypto.Signing qualified as Crypto
import Cardano.Crypto.Wallet qualified as Crypto

import Codec.Binary.Bech32 qualified as Bech32
import Control.Exception qualified as Exception
import Data.Bifunctor (Bifunctor (..))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Function
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import System.Exit (exitFailure)

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

stakePoolVKeyDesc :: TextEnvelopeDescr
stakePoolVKeyDesc = "Stake Pool Operator Verification Key"

runKeyCmds
  :: ()
  => Cmd.KeyCmds
  -> CIO e ()
runKeyCmds = \case
  Cmd.KeyVerificationKeyCmd cmd ->
    runVerificationKeyCmd cmd
  Cmd.KeyNonExtendedKeyCmd cmd ->
    runNonExtendedKeyCmd cmd
  Cmd.KeyGenerateMnemonicCmd cmd ->
    runGenerateMnemonicCmd cmd
  Cmd.KeyExtendedSigningKeyFromMnemonicCmd cmd ->
    runExtendedSigningKeyFromMnemonicCmd cmd
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
  -> CIO e ()
runVerificationKeyCmd
  Cmd.KeyVerificationKeyCmdArgs
    { Cmd.skeyFile = skf
    , Cmd.vkeyFile = vkf
    } = do
    ssk <- fromExceptTCli $ readSigningKeyFile skf
    withSomeSigningKey ssk $ \sk -> do
      let vk = getVerificationKey sk
      fromEitherIOCli @(FileError ()) $
        writeLazyByteStringFile vkf $
          textEnvelopeToJSON Nothing vk

runNonExtendedKeyCmd
  :: Cmd.KeyNonExtendedKeyCmdArgs
  -> CIO e ()
runNonExtendedKeyCmd
  Cmd.KeyNonExtendedKeyCmdArgs
    { Cmd.extendedVkeyFileIn = evkf
    , Cmd.nonExtendedVkeyFileOut = vkf
    } =
    writeExtendedVerificationKey =<< fromExceptTCli (readExtendedVerificationKeyFile evkf)
   where
    -- TODO: Expose a function specifically for this purpose
    -- and explain the extended verification keys can be converted
    -- to their non-extended counterparts however this is NOT the case
    -- for extended signing keys

    writeExtendedVerificationKey
      :: SomeAddressVerificationKey
      -> CIO e ()
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
        AStakePoolExtendedVerificationKey vk ->
          writeToDisk
            vkf
            (Just stakePoolVKeyDesc)
            (castVerificationKey vk :: VerificationKey StakePoolKey)
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
        vk@AStakePoolVerificationKey{} -> goFail vk
        vk@AStakeVerificationKey{} -> goFail vk
        vk@ADRepVerificationKey{} -> goFail vk
        vk@ACommitteeColdVerificationKey{} -> goFail vk
        vk@ACommitteeHotVerificationKey{} -> goFail vk
     where
      goFail nonExtendedKey = throwCliError $ KeyCmdExpectedExtendedVerificationKey nonExtendedKey

    writeToDisk vkf' descr vk =
      fromEitherIOCli @(FileError ()) $
        writeLazyByteStringFile vkf' $
          textEnvelopeToJSON descr vk

runGenerateMnemonicCmd :: Cmd.KeyGenerateMnemonicCmdArgs -> CIO e ()
runGenerateMnemonicCmd
  Cmd.KeyGenerateMnemonicCmdArgs
    { mnemonicOutputFormat
    , mnemonicWords
    } = fromExceptTCli $ Mnemonic.generateMnemonic mnemonicWords mnemonicOutputFormat

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
    k@AStakePoolExtendedVerificationKey{} -> return k
    k@AStakeExtendedVerificationKey{} -> return k
    k@AGenesisExtendedVerificationKey{} -> return k
    k@AGenesisDelegateExtendedVerificationKey{} -> return k
    -- Non-extended keys are below and cause failure.
    k@AByronVerificationKey{} -> goFail k
    k@APaymentVerificationKey{} -> goFail k
    k@AGenesisUTxOVerificationKey{} -> goFail k
    k@AKesVerificationKey{} -> goFail k
    k@AVrfVerificationKey{} -> goFail k
    k@AStakePoolVerificationKey{} -> goFail k
    k@AStakeVerificationKey{} -> goFail k
    k@ADRepVerificationKey{} -> goFail k
    k@ACommitteeColdVerificationKey{} -> goFail k
    k@ACommitteeHotVerificationKey{} -> goFail k
 where
  goFail k = left $ KeyCmdExpectedExtendedVerificationKey k

runExtendedSigningKeyFromMnemonicCmd
  :: Cmd.KeyExtendedSigningKeyFromMnemonicArgs
  -> CIO e ()
runExtendedSigningKeyFromMnemonicCmd
  Cmd.KeyExtendedSigningKeyFromMnemonicArgs
    { keyOutputFormat
    , derivedExtendedSigningKeyType
    , derivationAccountNo
    , mnemonicSource
    , signingKeyFileOut
    } =
    fromExceptTCli $
      Mnemonic.extendedSigningKeyFromMnemonicImpl
        keyOutputFormat
        derivedExtendedSigningKeyType
        derivationAccountNo
        mnemonicSource
        signingKeyFileOut

runConvertByronKeyCmd
  :: Cmd.KeyConvertByronKeyCmdArgs
  -> CIO e ()
runConvertByronKeyCmd
  Cmd.KeyConvertByronKeyCmdArgs
    { Cmd.mPassword = mPwd
    , Cmd.byronKeyType
    , Cmd.someKeyFileIn = inFile
    , Cmd.someKeyFileOut = outFile
    } =
    case (byronKeyType, inFile) of
      (ByronPaymentKey format, ASigningKeyFile skeyPathOld) ->
        convertByronSigningKey mPwd format toSigningKey skeyPathOld outFile
       where
        toSigningKey :: Byron.SigningKey -> SigningKey ByronKey
        toSigningKey = ByronSigningKey
      (ByronGenesisKey format, ASigningKeyFile skeyPathOld) ->
        convertByronSigningKey mPwd format toSigningKey skeyPathOld outFile
       where
        toSigningKey :: Byron.SigningKey -> SigningKey GenesisExtendedKey
        toSigningKey (Byron.SigningKey xsk) = GenesisExtendedSigningKey xsk
      (ByronDelegateKey format, ASigningKeyFile skeyPathOld) ->
        convertByronSigningKey mPwd format toSigningKey skeyPathOld outFile
       where
        toSigningKey :: Byron.SigningKey -> SigningKey GenesisDelegateExtendedKey
        toSigningKey (Byron.SigningKey xsk) = GenesisDelegateExtendedSigningKey xsk
      (ByronPaymentKey NonLegacyByronKeyFormat, AVerificationKeyFile vkeyPathOld) ->
        convertByronVerificationKey toVerificationKey vkeyPathOld outFile
       where
        toVerificationKey :: Byron.VerificationKey -> VerificationKey ByronKey
        toVerificationKey = ByronVerificationKey
      (ByronGenesisKey NonLegacyByronKeyFormat, AVerificationKeyFile vkeyPathOld) ->
        convertByronVerificationKey toVerificationKey vkeyPathOld outFile
       where
        toVerificationKey :: Byron.VerificationKey -> VerificationKey GenesisExtendedKey
        toVerificationKey (Byron.VerificationKey xvk) = GenesisExtendedVerificationKey xvk
      (ByronDelegateKey NonLegacyByronKeyFormat, AVerificationKeyFile vkeyPathOld) ->
        convertByronVerificationKey toVerificationKey vkeyPathOld outFile
       where
        toVerificationKey :: Byron.VerificationKey -> VerificationKey GenesisDelegateExtendedKey
        toVerificationKey (Byron.VerificationKey xvk) =
          GenesisDelegateExtendedVerificationKey xvk
      (ByronPaymentKey LegacyByronKeyFormat, AVerificationKeyFile{}) ->
        legacyVerificationKeysNotSupported
      (ByronGenesisKey LegacyByronKeyFormat, AVerificationKeyFile{}) ->
        legacyVerificationKeysNotSupported
      (ByronDelegateKey LegacyByronKeyFormat, AVerificationKeyFile{}) ->
        legacyVerificationKeysNotSupported

legacyVerificationKeysNotSupported :: CIO e ()
legacyVerificationKeysNotSupported =
  liftIO $ do
    putStrLn $
      "convert keys: byron legacy format not supported for "
        ++ "verification keys. Convert the signing key and then get the "
        ++ "verification key."
    exitFailure

convertByronSigningKey
  :: forall keyrole e
   . Key keyrole
  => Maybe Text
  -- ^ Password (if applicable)
  -> ByronKeyFormat
  -> (Byron.SigningKey -> SigningKey keyrole)
  -> SigningKeyFile In
  -- ^ Input file: old format
  -> File () Out
  -- ^ Output file: new format
  -> CIO e ()
convertByronSigningKey mPwd byronFormat convertFun skeyPathOld skeyPathNew = do
  sKey <-
    fromExceptTCli $
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
      sk' = convertFun unprotectedSk

  fromEitherIOCli @(FileError ()) $
    writeLazyByteStringFile skeyPathNew $
      textEnvelopeToJSON Nothing sk'

convertByronVerificationKey
  :: forall keyrole e
   . Key keyrole
  => (Byron.VerificationKey -> VerificationKey keyrole)
  -> VerificationKeyFile In
  -- ^ Input file: old format
  -> File () Out
  -- ^ Output file: new format
  -> CIO e ()
convertByronVerificationKey convertFun vkeyPathOld vkeyPathNew = do
  vk <-
    fromExceptTCli $
      Byron.readPaymentVerificationKey vkeyPathOld

  let vk' :: VerificationKey keyrole
      vk' = convertFun vk

  fromEitherIOCli @(FileError ()) $
    writeLazyByteStringFile vkeyPathNew $
      textEnvelopeToJSON Nothing vk'

runConvertByronGenesisVKeyCmd
  :: Cmd.KeyConvertByronGenesisVKeyCmdArgs
  -> CIO e ()
runConvertByronGenesisVKeyCmd
  Cmd.KeyConvertByronGenesisVKeyCmdArgs
    { Cmd.vkey = VerificationKeyBase64 b64ByronVKey
    , Cmd.vkeyFileOut = vkeyPathNew
    } = do
    vk <-
      fromEitherCli
        . first textShow
        . Byron.Crypto.parseFullVerificationKey
        . Text.pack
        $ b64ByronVKey

    let vk' :: VerificationKey GenesisKey
        vk' = convertFun vk

    fromEitherIOCli @(FileError ()) $
      writeLazyByteStringFile vkeyPathNew $
        textEnvelopeToJSON Nothing vk'
   where
    convertFun :: Byron.VerificationKey -> VerificationKey GenesisKey
    convertFun (Byron.VerificationKey xvk) =
      castVerificationKey (GenesisExtendedVerificationKey xvk)

--------------------------------------------------------------------------------
-- ITN verification/signing key conversion to Haskell verficiation/signing keys
--------------------------------------------------------------------------------

runConvertITNKeyCmd
  :: Cmd.KeyConvertITNKeyCmdArgs
  -> CIO e ()
runConvertITNKeyCmd
  Cmd.KeyConvertITNKeyCmdArgs
    { Cmd.itnKeyFile
    , Cmd.outFile
    } =
    case itnKeyFile of
      AVerificationKeyFile (File vk) -> do
        bech32publicKey <-
          fromEitherIOCli $
            readFileITNKey vk
        vkey <-
          fromEitherCli $
            convertITNVerificationKey bech32publicKey
        fromEitherIOCli @(FileError ()) $
          writeLazyByteStringFile outFile $
            textEnvelopeToJSON Nothing vkey
      ASigningKeyFile (File sk) -> do
        bech32privateKey <-
          fromEitherIOCli $
            readFileITNKey sk
        skey <-
          fromEitherCli $
            convertITNSigningKey bech32privateKey
        fromEitherIOCli @(FileError ()) $
          writeLazyByteStringFile outFile $
            textEnvelopeToJSON Nothing skey

runConvertITNExtendedKeyCmd
  :: ()
  => Cmd.KeyConvertITNExtendedKeyCmdArgs
  -> CIO e ()
runConvertITNExtendedKeyCmd
  Cmd.KeyConvertITNExtendedKeyCmdArgs
    { Cmd.itnPrivKeyFile
    , Cmd.outFile
    } =
    case itnPrivKeyFile of
      AVerificationKeyFile _ ->
        throwCliError KeyCmdWrongKeyTypeError
      ASigningKeyFile (File sk) -> do
        bech32privateKey <- fromEitherIOCli $ readFileITNKey sk
        skey <-
          convertITNExtendedSigningKey bech32privateKey
            & fromEitherCli
        fromEitherIOCli @(FileError ()) $
          writeLazyByteStringFile outFile $
            textEnvelopeToJSON Nothing skey

runConvertITNBip32KeyCmd
  :: ()
  => Cmd.KeyConvertITNBip32KeyCmdArgs
  -> CIO e ()
runConvertITNBip32KeyCmd
  Cmd.KeyConvertITNBip32KeyCmdArgs
    { Cmd.itnPrivKeyFile
    , Cmd.outFile
    } =
    case itnPrivKeyFile of
      AVerificationKeyFile _ ->
        throwCliError KeyCmdWrongKeyTypeError
      ASigningKeyFile (File sk) -> do
        bech32privateKey <- fromEitherIOCli $ readFileITNKey sk
        skey <-
          fromEitherCli $
            convertITNBIP32SigningKey bech32privateKey

        fromEitherIOCli @(FileError ()) $
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
  -> CIO e ()
runConvertCardanoAddressKeyCmd
  Cmd.KeyConvertCardanoAddressKeyCmdArgs
    { cardanoAddressKeyType = keyType
    , skeyFileIn = skFile
    , skeyFileOut = outFile
    } = do
    sKey <-
      fromEitherIOCli $
        readSomeCardanoAddressSigningKeyFile keyType skFile
    fromEitherIOCli $
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
