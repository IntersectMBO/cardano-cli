{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.Run.Governance.Committee
  ( runGovernanceCommitteeCmds
  , GovernanceCommitteeError(..)
  ) where

import           Cardano.Api

import           Cardano.CLI.EraBased.Commands.Governance.Committee

import           Control.Monad.Except (ExceptT)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Except.Extra
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.Function

data GovernanceCommitteeError
  = GovernanceCommitteeCmdWriteFileError (FileError ())
  | GovernanceCommitteeCmdTextEnvReadFileError (FileError TextEnvelopeError)
  deriving Show

instance Error GovernanceCommitteeError where
  displayError = \case
    GovernanceCommitteeCmdWriteFileError e -> "Cannot write file: " <> displayError e
    GovernanceCommitteeCmdTextEnvReadFileError e -> "Cannot read file: " <> displayError e

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
  -> File (VerificationKey ()) In
  -> ExceptT GovernanceCommitteeError IO ()
runGovernanceCommitteeKeyHash _w vkeyPath = do
  vkey <-
    readFileTextEnvelopeAnyOf
      [ FromSomeType (AsVerificationKey AsCommitteeHotKey ) ACommitteeHotKey
      , FromSomeType (AsVerificationKey AsCommitteeColdKey) ACommitteeColdKey
      ]
      vkeyPath
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
