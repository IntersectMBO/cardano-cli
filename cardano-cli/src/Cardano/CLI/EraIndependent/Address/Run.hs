{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{- HLINT ignore "Redundant id" -}

module Cardano.CLI.EraIndependent.Address.Run
  ( runAddressCmds
  , runAddressBuildCmd
  , runAddressKeyGenCmd
  , runAddressKeyHashCmd
  , buildShelleyAddress
  , generateAndWriteKeyFiles
  )
where

import Cardano.Api
import Cardano.Api.Shelley

import Cardano.CLI.EraIndependent.Address.Command
import Cardano.CLI.EraIndependent.Address.Info.Run
import Cardano.CLI.EraIndependent.Key.Run qualified as Key
import Cardano.CLI.Read
import Cardano.CLI.Type.Common
import Cardano.CLI.Type.Error.AddressCmdError
import Cardano.CLI.Type.Key
  ( PaymentVerifier (..)
  , StakeIdentifier (..)
  , StakeVerifier (..)
  , VerificationKeyTextOrFile
  , generateKeyPair
  , readVerificationKeyOrHashOrFile
  , readVerificationKeyTextOrFileAnyOf
  )

import Control.Monad (void)
import Data.ByteString.Char8 qualified as BS
import Data.Function
import Data.Text.IO qualified as Text
import Vary (Vary)
import Vary qualified

runAddressCmds
  :: ()
  => AddressCmds
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
  :: Vary [FormatBech32, FormatTextEnvelope]
  -> AddressKeyType
  -> VerificationKeyFile Out
  -> SigningKeyFile Out
  -> ExceptT AddressCmdError IO ()
runAddressKeyGenCmd fmt kt vkf skf = case kt of
  AddressKeyShelley -> void $ generateAndWriteKeyFiles fmt AsPaymentKey vkf skf
  AddressKeyShelleyExtended -> void $ generateAndWriteKeyFiles fmt AsPaymentExtendedKey vkf skf
  AddressKeyByron -> generateAndWriteByronKeyFiles AsByronKey vkf skf

generateAndWriteByronKeyFiles
  :: Key keyrole
  => HasTypeProxy keyrole
  => AsType keyrole
  -> VerificationKeyFile Out
  -> SigningKeyFile Out
  -> ExceptT AddressCmdError IO ()
generateAndWriteByronKeyFiles asType' vkf skf = do
  uncurry (writeByronPaymentKeyFiles vkf skf) =<< generateKeyPair asType'

generateAndWriteKeyFiles
  :: ()
  => Key keyrole
  => HasTypeProxy keyrole
  => SerialiseAsBech32 (SigningKey keyrole)
  => SerialiseAsBech32 (VerificationKey keyrole)
  => Vary [FormatBech32, FormatTextEnvelope]
  -> AsType keyrole
  -> VerificationKeyFile Out
  -> SigningKeyFile Out
  -> ExceptT AddressCmdError IO (VerificationKey keyrole, SigningKey keyrole)
generateAndWriteKeyFiles fmt asType' vkf skf = do
  (vk, sk) <- generateKeyPair asType'
  writePaymentKeyFiles fmt vkf skf vk sk
  return (vk, sk)

writePaymentKeyFiles
  :: Key keyrole
  => SerialiseAsBech32 (SigningKey keyrole)
  => SerialiseAsBech32 (VerificationKey keyrole)
  => Vary [FormatBech32, FormatTextEnvelope]
  -> VerificationKeyFile Out
  -> SigningKeyFile Out
  -> VerificationKey keyrole
  -> SigningKey keyrole
  -> ExceptT AddressCmdError IO ()
writePaymentKeyFiles fmt vkeyPath skeyPath vkey skey = do
  firstExceptT AddressCmdWriteFileError $ do
    fmt
      & ( id
            . Vary.on
              ( \FormatBech32 ->
                  newExceptT
                    . writeTextFile skeyPath
                    $ serialiseToBech32 skey
              )
            . Vary.on
              ( \FormatTextEnvelope ->
                  newExceptT
                    . writeLazyByteStringFile skeyPath
                    $ textEnvelopeToJSON (Just skeyDesc) skey
              )
            $ Vary.exhaustiveCase
        )

    fmt
      & ( id
            . Vary.on
              ( \FormatBech32 ->
                  newExceptT
                    . writeTextFile vkeyPath
                    $ serialiseToBech32 vkey
              )
            . Vary.on
              ( \FormatTextEnvelope ->
                  newExceptT
                    . writeLazyByteStringFile vkeyPath
                    $ textEnvelopeToJSON (Just Key.paymentVkeyDesc) vkey
              )
            $ Vary.exhaustiveCase
        )
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

runAddressKeyHashCmd
  :: VerificationKeyTextOrFile
  -> Maybe (File () Out)
  -> ExceptT AddressCmdError IO ()
runAddressKeyHashCmd vkeyTextOrFile mOutputFp = do
  vkey <-
    firstExceptT AddressCmdVerificationKeyTextOrFileError $
      newExceptT $
        readVerificationKeyTextOrFileAnyOf vkeyTextOrFile

  let hexKeyHash =
        mapSomeAddressVerificationKey
          (serialiseToRawBytesHex . verificationKeyHash)
          vkey

  case mOutputFp of
    Just (File fpath) -> liftIO $ BS.writeFile fpath hexKeyHash
    Nothing -> liftIO $ BS.putStrLn hexKeyHash

runAddressBuildCmd
  :: PaymentVerifier
  -> Maybe StakeIdentifier
  -> NetworkId
  -> Maybe (File () Out)
  -> ExceptT AddressCmdError IO ()
runAddressBuildCmd paymentVerifier mbStakeVerifier nw mOutFp = do
  outText <- case paymentVerifier of
    PaymentVerifierKey payVkeyTextOrFile -> do
      payVKey <-
        firstExceptT AddressCmdVerificationKeyTextOrFileError $
          newExceptT $
            readVerificationKeyTextOrFileAnyOf payVkeyTextOrFile

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
    Nothing -> liftIO $ Text.putStr outText

makeStakeAddressRef
  :: StakeIdentifier
  -> ExceptT AddressCmdError IO StakeAddressReference
makeStakeAddressRef stakeIdentifier =
  case stakeIdentifier of
    StakeIdentifierVerifier stakeVerifier ->
      case stakeVerifier of
        StakeVerifierKey stkVkeyOrFile -> do
          stakeVKeyHash <-
            modifyError AddressCmdReadKeyFileError $
              readVerificationKeyOrHashOrFile stkVkeyOrFile
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
  makeShelleyAddress nw (PaymentCredentialByKey (verificationKeyHash vkey))
    <$> maybe (return NoStakeAddress) makeStakeAddressRef mbStakeVerifier
