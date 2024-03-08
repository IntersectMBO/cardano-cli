{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.EraBased.Run.Address
  ( runAddressCmds

  , runAddressBuildCmd
  , runAddressKeyGenCmd
  , runAddressKeyHashCmd
  ) where

import           Cardano.Api
import           Cardano.Api.Shelley

import           Cardano.CLI.EraBased.Commands.Address
import           Cardano.CLI.EraBased.Run.Address.Info
import qualified Cardano.CLI.EraBased.Run.Key as Key
import           Cardano.CLI.Read
import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Errors.AddressCmdError
import           Cardano.CLI.Types.Key (PaymentVerifier (..), StakeIdentifier (..),
                   StakeVerifier (..), VerificationKeyTextOrFile, generateKeyPair,
                   readVerificationKeyOrHashOrFile, readVerificationKeyTextOrFileAnyOf)

import qualified Data.ByteString.Char8 as BS
import           Data.Function
import qualified Data.Text.IO as Text

runAddressCmds :: ()
  => AddressCmds era
  -> ExceptT AddressCmdError IO ()
runAddressCmds = \case
  AddressKeyGen fmt kt vkf skf ->
    runAddressKeyGenCmd fmt kt vkf skf
  AddressKeyHash vkf mOFp ->
    runAddressKeyHashCmd vkf mOFp
  AddressBuild paymentVerifier mbStakeVerifier nw mOutFp ->
    runAddressBuildCmd paymentVerifier mbStakeVerifier nw mOutFp
  AddressInfo txt mOFp ->
    runAddressInfoCmd txt mOFp & firstExceptT AddressCmdAddressInfoError

runAddressKeyGenCmd
  :: KeyOutputFormat
  -> AddressKeyType
  -> VerificationKeyFile Out
  -> SigningKeyFile Out
  -> ExceptT AddressCmdError IO ()
runAddressKeyGenCmd fmt kt vkf skf = case kt of
  AddressKeyShelley         -> generateAndWriteKeyFiles fmt  AsPaymentKey          vkf skf
  AddressKeyShelleyExtended -> generateAndWriteKeyFiles fmt  AsPaymentExtendedKey  vkf skf
  AddressKeyByron           -> generateAndWriteByronKeyFiles AsByronKey            vkf skf

generateAndWriteByronKeyFiles :: ()
  => Key keyrole
  => HasTypeProxy keyrole
  => AsType keyrole
  -> VerificationKeyFile Out
  -> SigningKeyFile Out
  -> ExceptT AddressCmdError IO ()
generateAndWriteByronKeyFiles asType vkf skf = do
  uncurry (writeByronPaymentKeyFiles vkf skf) =<< liftIO (generateKeyPair asType)

generateAndWriteKeyFiles :: ()
  => Key keyrole
  => HasTypeProxy keyrole
  => SerialiseAsBech32 (SigningKey keyrole)
  => SerialiseAsBech32 (VerificationKey keyrole)
  => KeyOutputFormat
  -> AsType keyrole
  -> VerificationKeyFile Out
  -> SigningKeyFile Out
  -> ExceptT AddressCmdError IO ()
generateAndWriteKeyFiles fmt asType vkf skf = do
  uncurry (writePaymentKeyFiles fmt vkf skf) =<< liftIO (generateKeyPair asType)

writePaymentKeyFiles
  :: Key keyrole
  => SerialiseAsBech32 (SigningKey keyrole)
  => SerialiseAsBech32 (VerificationKey keyrole)
  => KeyOutputFormat
  -> VerificationKeyFile Out
  -> SigningKeyFile Out
  -> VerificationKey keyrole
  -> SigningKey keyrole
  -> ExceptT AddressCmdError IO ()
writePaymentKeyFiles fmt vkeyPath skeyPath vkey skey = do
  firstExceptT AddressCmdWriteFileError $ do
    case fmt of
      KeyOutputFormatTextEnvelope ->
        newExceptT
          $ writeLazyByteStringFile skeyPath
          $ textEnvelopeToJSON (Just skeyDesc) skey
      KeyOutputFormatBech32 ->
        newExceptT
          $ writeTextFile skeyPath
          $ serialiseToBech32 skey

    case fmt of
      KeyOutputFormatTextEnvelope ->
        newExceptT
          $ writeLazyByteStringFile vkeyPath
          $ textEnvelopeToJSON (Just Key.paymentVkeyDesc) vkey
      KeyOutputFormatBech32 ->
        newExceptT
          $ writeTextFile vkeyPath
          $ serialiseToBech32 vkey

  where
    skeyDesc :: TextEnvelopeDescr
    skeyDesc = "Payment Signing Key"

writeByronPaymentKeyFiles
   :: Key keyrole
   => VerificationKeyFile Out
   -> SigningKeyFile Out
   -> VerificationKey keyrole
   -> SigningKey keyrole
   -> ExceptT AddressCmdError IO ()
writeByronPaymentKeyFiles vkeyPath skeyPath vkey skey = do
  firstExceptT AddressCmdWriteFileError $ do
    -- No bech32 encoding for Byron keys
    newExceptT $ writeLazyByteStringFile skeyPath $ textEnvelopeToJSON (Just skeyDesc) skey
    newExceptT $ writeLazyByteStringFile vkeyPath $ textEnvelopeToJSON (Just Key.paymentVkeyDesc) vkey
  where
    skeyDesc :: TextEnvelopeDescr
    skeyDesc = "Payment Signing Key"

runAddressKeyHashCmd :: VerificationKeyTextOrFile
                  -> Maybe (File () Out)
                  -> ExceptT AddressCmdError IO ()
runAddressKeyHashCmd vkeyTextOrFile mOutputFp = do
  vkey <- firstExceptT AddressCmdVerificationKeyTextOrFileError $
             newExceptT $ readVerificationKeyTextOrFileAnyOf vkeyTextOrFile

  let hexKeyHash = foldSomeAddressVerificationKey
                     (serialiseToRawBytesHex . verificationKeyHash) vkey

  case mOutputFp of
    Just (File fpath) -> liftIO $ BS.writeFile fpath hexKeyHash
    Nothing -> liftIO $ BS.putStrLn hexKeyHash


runAddressBuildCmd :: PaymentVerifier
                -> Maybe StakeIdentifier
                -> NetworkId
                -> Maybe (File () Out)
                -> ExceptT AddressCmdError IO ()
runAddressBuildCmd paymentVerifier mbStakeVerifier nw mOutFp = do
  outText <- case paymentVerifier of
    PaymentVerifierKey payVkeyTextOrFile -> do
      payVKey <- firstExceptT AddressCmdVerificationKeyTextOrFileError $
         newExceptT $ readVerificationKeyTextOrFileAnyOf payVkeyTextOrFile

      addr <- case payVKey of
        AByronVerificationKey vk ->
          return (AddressByron (makeByronAddress nw vk))

        APaymentVerificationKey vk ->
          AddressShelley <$> buildShelleyAddress vk mbStakeVerifier nw

        APaymentExtendedVerificationKey vk ->
          AddressShelley <$> buildShelleyAddress (castVerificationKey vk) mbStakeVerifier nw

        AGenesisUTxOVerificationKey vk ->
          AddressShelley <$> buildShelleyAddress (castVerificationKey vk) mbStakeVerifier nw
        nonPaymentKey ->
          left $ AddressCmdExpectedPaymentVerificationKey nonPaymentKey
      return $ serialiseAddress (addr :: AddressAny)

    PaymentVerifierScriptFile (File fp) -> do
      ScriptInAnyLang _lang script <-
        firstExceptT AddressCmdReadScriptFileError $
          readFileScriptInAnyLang fp

      let payCred = PaymentCredentialByScript (hashScript script)

      stakeAddressReference <- maybe (return NoStakeAddress) makeStakeAddressRef mbStakeVerifier

      return $ serialiseAddress . makeShelleyAddress nw payCred $ stakeAddressReference

  case mOutFp of
    Just (File fpath) -> liftIO $ Text.writeFile fpath outText
    Nothing           -> liftIO $ Text.putStr          outText

makeStakeAddressRef
  :: StakeIdentifier
  -> ExceptT AddressCmdError IO StakeAddressReference
makeStakeAddressRef stakeIdentifier =
  case stakeIdentifier of
    StakeIdentifierVerifier stakeVerifier ->
      case stakeVerifier of
        StakeVerifierKey stkVkeyOrFile -> do
          stakeVKeyHash <- firstExceptT AddressCmdReadKeyFileError $
            newExceptT $ readVerificationKeyOrHashOrFile AsStakeKey stkVkeyOrFile

          return . StakeAddressByValue $ StakeCredentialByKey stakeVKeyHash

        StakeVerifierScriptFile (File fp) -> do
          ScriptInAnyLang _lang script <-
            firstExceptT AddressCmdReadScriptFileError $
              readFileScriptInAnyLang fp

          let stakeCred = StakeCredentialByScript (hashScript script)
          return (StakeAddressByValue stakeCred)
    StakeIdentifierAddress stakeAddr ->
        pure $ StakeAddressByValue $ stakeAddressCredential stakeAddr

buildShelleyAddress
  :: VerificationKey PaymentKey
  -> Maybe StakeIdentifier
  -> NetworkId
  -> ExceptT AddressCmdError IO (Address ShelleyAddr)
buildShelleyAddress vkey mbStakeVerifier nw =
  makeShelleyAddress nw (PaymentCredentialByKey (verificationKeyHash vkey)) <$> maybe (return NoStakeAddress) makeStakeAddressRef mbStakeVerifier


--
-- Handling the variety of address key types
--


foldSomeAddressVerificationKey :: ()
  => (forall keyrole. Key keyrole => VerificationKey keyrole -> a)
  -> SomeAddressVerificationKey
  -> a
foldSomeAddressVerificationKey f = \case
  AByronVerificationKey                   vk -> f vk
  APaymentVerificationKey                 vk -> f vk
  APaymentExtendedVerificationKey         vk -> f vk
  AGenesisUTxOVerificationKey             vk -> f vk
  AKesVerificationKey                     vk -> f vk
  AGenesisDelegateExtendedVerificationKey vk -> f vk
  AGenesisExtendedVerificationKey         vk -> f vk
  AVrfVerificationKey                     vk -> f vk
  AStakeVerificationKey                   vk -> f vk
  AStakeExtendedVerificationKey           vk -> f vk
  ADRepVerificationKey                    vk -> f vk
  ADRepExtendedVerificationKey            vk -> f vk
