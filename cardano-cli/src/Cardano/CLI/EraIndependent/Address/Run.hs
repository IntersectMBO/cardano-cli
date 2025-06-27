{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
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

import Cardano.CLI.Compatible.Exception
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
  -> CIO e ()
runAddressCmds = \case
  AddressKeyGen fmt kt vkf skf ->
    runAddressKeyGenCmd fmt kt vkf skf
  AddressKeyHash vkf mOFp ->
    runAddressKeyHashCmd vkf mOFp
  AddressBuild paymentVerifier mbStakeVerifier nw mOutFp ->
    runAddressBuildCmd paymentVerifier mbStakeVerifier nw mOutFp
  AddressInfo txt mOFp ->
    runAddressInfoCmd txt mOFp

runAddressKeyGenCmd
  :: Vary [FormatBech32, FormatTextEnvelope]
  -> AddressKeyType
  -> VerificationKeyFile Out
  -> SigningKeyFile Out
  -> CIO e ()
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
  -> CIO e ()
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
  -> CIO e (VerificationKey keyrole, SigningKey keyrole)
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
  -> CIO e ()
writePaymentKeyFiles fmt vkeyPath skeyPath vkey skey = do
  fmt
    & ( id
          . Vary.on
            ( \FormatBech32 -> do
                fromEitherIOCli @(FileError ()) $
                  writeTextFile skeyPath $
                    serialiseToBech32 skey
            )
          . Vary.on
            ( \FormatTextEnvelope -> do
                fromEitherIOCli @(FileError ()) $
                  writeLazyByteStringFile skeyPath $
                    textEnvelopeToJSON (Just skeyDesc) skey
            )
          $ Vary.exhaustiveCase
      )

  fmt
    & ( id
          . Vary.on
            ( \FormatBech32 -> do
                fromEitherIOCli @(FileError ()) $
                  writeTextFile vkeyPath $
                    serialiseToBech32 vkey
            )
          . Vary.on
            ( \FormatTextEnvelope -> do
                fromEitherIOCli @(FileError ()) $
                  writeLazyByteStringFile vkeyPath $
                    textEnvelopeToJSON (Just Key.paymentVkeyDesc) vkey
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
  -> CIO e ()
writeByronPaymentKeyFiles vkeyPath skeyPath vkey skey = do
  -- No bech32 encoding for Byron keys
  fromEitherIOCli @(FileError ()) $
    writeLazyByteStringFile skeyPath $
      textEnvelopeToJSON (Just skeyDesc) skey

  fromEitherIOCli @(FileError ()) $
    writeLazyByteStringFile vkeyPath $
      textEnvelopeToJSON (Just Key.paymentVkeyDesc) vkey
 where
  skeyDesc :: TextEnvelopeDescr
  skeyDesc = "Payment Signing Key"

runAddressKeyHashCmd
  :: VerificationKeyTextOrFile
  -> Maybe (File () Out)
  -> CIO e ()
runAddressKeyHashCmd vkeyTextOrFile mOutputFp = do
  vkey <-
    fromEitherIOCli $
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
  -> CIO e ()
runAddressBuildCmd paymentVerifier mbStakeVerifier nw mOutFp = do
  outText <- case paymentVerifier of
    PaymentVerifierKey payVkeyTextOrFile -> do
      payVKey <-
        fromEitherIOCli $
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
          throwCliError $ AddressCmdExpectedPaymentVerificationKey nonPaymentKey
      return $ serialiseAddress (addr :: AddressAny)
    PaymentVerifierScriptFile (File fp) -> do
      ScriptInAnyLang _lang script <-
        readFileScriptInAnyLang fp

      let payCred = PaymentCredentialByScript (hashScript script)

      stakeAddressReference <-
        maybe (return NoStakeAddress) makeStakeAddressRef mbStakeVerifier

      return $ serialiseAddress . makeShelleyAddress nw payCred $ stakeAddressReference

  case mOutFp of
    Just (File fpath) -> liftIO $ Text.writeFile fpath outText
    Nothing -> liftIO $ Text.putStr outText

makeStakeAddressRef
  :: StakeIdentifier
  -> CIO e StakeAddressReference
makeStakeAddressRef stakeIdentifier =
  case stakeIdentifier of
    StakeIdentifierVerifier stakeVerifier ->
      case stakeVerifier of
        StakeVerifierKey stkVkeyOrFile -> do
          stakeVKeyHash <-
            readVerificationKeyOrHashOrFile stkVkeyOrFile
          return . StakeAddressByValue $ StakeCredentialByKey stakeVKeyHash
        StakeVerifierScriptFile (File fp) -> do
          ScriptInAnyLang _lang script <-
            readFileScriptInAnyLang fp

          let stakeCred = StakeCredentialByScript (hashScript script)
          return (StakeAddressByValue stakeCred)
    StakeIdentifierAddress stakeAddr ->
      pure $ StakeAddressByValue $ stakeAddressCredential stakeAddr

buildShelleyAddress
  :: VerificationKey PaymentKey
  -> Maybe StakeIdentifier
  -> NetworkId
  -> CIO e (Address ShelleyAddr)
buildShelleyAddress vkey mbStakeVerifier nw =
  makeShelleyAddress nw (PaymentCredentialByKey (verificationKeyHash vkey))
    <$> maybe (return NoStakeAddress) makeStakeAddressRef mbStakeVerifier
