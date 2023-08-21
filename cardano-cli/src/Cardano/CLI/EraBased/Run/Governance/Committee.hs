{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.Run.Governance.Committee
  ( runGovernanceCommitteeCmds
  , GovernanceCommitteeError(..)
  ) where

import           Cardano.Api
import           Cardano.Api.Shelley

import           Cardano.CLI.EraBased.Commands.Governance.Committee
import           Cardano.CLI.Types.Errors.GovernanceCommitteeError
import           Cardano.CLI.Types.Key
import           Cardano.CLI.Types.Key.VerificationKey

import           Control.Monad.Except (ExceptT)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans (lift)
import           Control.Monad.Trans.Except.Extra
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.Function

runGovernanceCommitteeCmds :: ()
  => GovernanceCommitteeCmds era
  -> ExceptT GovernanceCommitteeError IO ()
runGovernanceCommitteeCmds = \case
  GovernanceCommitteeKeyGenCold era vk sk ->
    runGovernanceCommitteeKeyGenCold era vk sk
  GovernanceCommitteeKeyGenHot era vk sk ->
    runGovernanceCommitteeKeyGenHot era vk sk
  GovernanceCommitteeKeyHash era vk ->
    runGovernanceCommitteeKeyHash era vk
  GovernanceCommitteeCreateHotKeyAuthorizationCertificate w cvk hvk out ->
    runGovernanceCommitteeCreateHotKeyAuthorizationCertificate w cvk hvk out
  GovernanceCommitteeCreateColdKeyResignationCertificate w cvk out ->
    runGovernanceCommitteeColdKeyResignationCertificate w cvk out

runGovernanceCommitteeKeyGenCold :: ()
  => ConwayEraOnwards era
  -> File (VerificationKey ()) Out
  -> File (SigningKey ()) Out
  -> ExceptT GovernanceCommitteeError IO ()
runGovernanceCommitteeKeyGenCold _w vkeyPath skeyPath = do
  skey <- liftIO $ generateSigningKey AsCommitteeColdKey

  let vkey = getVerificationKey skey

  writeLazyByteStringFile skeyPath (textEnvelopeToJSON (Just skeyDesc) skey)
    & onLeft (left . GovernanceCommitteeCmdWriteFileError)

  writeLazyByteStringFile vkeyPath (textEnvelopeToJSON (Just vkeyDesc) vkey)
    & onLeft (left . GovernanceCommitteeCmdWriteFileError)

  where
    skeyDesc :: TextEnvelopeDescr
    skeyDesc = "Constitutional Committee Cold Signing Key"

    vkeyDesc :: TextEnvelopeDescr
    vkeyDesc = "Constitutional Committee Cold Verification Key"

runGovernanceCommitteeKeyGenHot :: ()
  => ConwayEraOnwards era
  -> File (VerificationKey ()) Out
  -> File (SigningKey ()) Out
  -> ExceptT GovernanceCommitteeError IO ()
runGovernanceCommitteeKeyGenHot _w vkeyPath skeyPath = do
  skey <- liftIO $ generateSigningKey AsCommitteeHotKey

  let vkey = getVerificationKey skey

  firstExceptT GovernanceCommitteeCmdWriteFileError
    . newExceptT
    $ writeLazyByteStringFile skeyPath
    $ textEnvelopeToJSON (Just skeyDesc) skey

  firstExceptT GovernanceCommitteeCmdWriteFileError
    . newExceptT
    $ writeLazyByteStringFile vkeyPath
    $ textEnvelopeToJSON (Just vkeyDesc) vkey

  where
    skeyDesc :: TextEnvelopeDescr
    skeyDesc = "Constitutional Committee Hot Signing Key"

    vkeyDesc :: TextEnvelopeDescr
    vkeyDesc = "Constitutional Committee Hot Verification Key"

data SomeCommitteeKey f
  = ACommitteeHotKey  (f CommitteeHotKey)
  | ACommitteeColdKey (f CommitteeColdKey)

runGovernanceCommitteeKeyHash :: ()
  => ConwayEraOnwards era
  -> AnyVerificationKeySource
  -> ExceptT GovernanceCommitteeError IO ()
runGovernanceCommitteeKeyHash _w vkeySource = do
  vkey <-
    case vkeySource of
      AnyVerificationKeySourceOfText vkText -> do
        let asTypes =
              [ FromSomeType (AsVerificationKey AsCommitteeHotKey ) ACommitteeHotKey
              , FromSomeType (AsVerificationKey AsCommitteeColdKey) ACommitteeColdKey
              ]
        pure (deserialiseAnyOfFromBech32 asTypes (unAnyVerificationKeyText vkText))
          & onLeft (left . GovernanceCommitteeCmdKeyDecodeError . InputBech32DecodeError)
      AnyVerificationKeySourceOfFile vkeyPath -> do
        let asTypes =
              [ FromSomeType (AsVerificationKey AsCommitteeHotKey ) ACommitteeHotKey
              , FromSomeType (AsVerificationKey AsCommitteeColdKey) ACommitteeColdKey
              ]
        readFileTextEnvelopeAnyOf asTypes vkeyPath
          & firstExceptT GovernanceCommitteeCmdTextEnvReadFileError . newExceptT

  liftIO $ BS.putStrLn (renderKeyHash vkey)

  where
    renderKeyHash :: SomeCommitteeKey VerificationKey -> ByteString
    renderKeyHash = \case
      ACommitteeHotKey  vk -> renderVerificationKeyHash vk
      ACommitteeColdKey vk -> renderVerificationKeyHash vk

    renderVerificationKeyHash :: Key keyrole => VerificationKey keyrole -> ByteString
    renderVerificationKeyHash = serialiseToRawBytesHex
                              . verificationKeyHash

runGovernanceCommitteeCreateHotKeyAuthorizationCertificate :: ()
  => ConwayEraOnwards era
  -> VerificationKeyOrHashOrFile CommitteeColdKey
  -> VerificationKeyOrHashOrFile CommitteeHotKey
  -> File () Out
  -> ExceptT GovernanceCommitteeError IO ()
runGovernanceCommitteeCreateHotKeyAuthorizationCertificate w coldVkOrHashOrFp hotVkOrHashOrFp oFp =
  conwayEraOnwardsConstraints w $ do
    CommitteeColdKeyHash coldVKHash <-
      lift (readVerificationKeyOrHashOrTextEnvFile AsCommitteeColdKey coldVkOrHashOrFp)
        & onLeft (left . GovernanceCommitteeCmdKeyReadError)

    CommitteeHotKeyHash hotVkHash <-
      lift (readVerificationKeyOrHashOrTextEnvFile AsCommitteeHotKey hotVkOrHashOrFp)
        & onLeft (left . GovernanceCommitteeCmdKeyReadError)

    makeCommitteeHotKeyAuthorizationCertificate (CommitteeHotKeyAuthorizationRequirements w coldVKHash hotVkHash)
      & textEnvelopeToJSON (Just genKeyDelegCertDesc)
      & writeLazyByteStringFile oFp
      & firstExceptT GovernanceCommitteeCmdTextEnvWriteError . newExceptT

  where
    genKeyDelegCertDesc :: TextEnvelopeDescr
    genKeyDelegCertDesc = "Constitutional Committee Hot Key Registration Certificate"

runGovernanceCommitteeColdKeyResignationCertificate :: ()
  => ConwayEraOnwards era
  -> VerificationKeyOrHashOrFile CommitteeColdKey
  -> File () Out
  -> ExceptT GovernanceCommitteeError IO ()
runGovernanceCommitteeColdKeyResignationCertificate w coldVkOrHashOrFp oFp =
  conwayEraOnwardsConstraints w $ do
    CommitteeColdKeyHash coldVKHash <-
      lift (readVerificationKeyOrHashOrTextEnvFile AsCommitteeColdKey coldVkOrHashOrFp)
        & onLeft (left . GovernanceCommitteeCmdKeyReadError)

    makeCommitteeColdkeyResignationCertificate (CommitteeColdkeyResignationRequirements w coldVKHash)
      & textEnvelopeToJSON (Just genKeyDelegCertDesc)
      & writeLazyByteStringFile oFp
      & firstExceptT GovernanceCommitteeCmdTextEnvWriteError . newExceptT

  where
    genKeyDelegCertDesc :: TextEnvelopeDescr
    genKeyDelegCertDesc = "Constitutional Committee Cold Key Resignation Certificate"
