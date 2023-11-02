{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Cardano.CLI.EraBased.Run.Governance.Committee
  ( runGovernanceCommitteeCmds
  , GovernanceCommitteeError(..)
  ) where

import           Cardano.Api
import           Cardano.Api.Shelley

import           Cardano.CLI.EraBased.Commands.Governance.Committee
import qualified Cardano.CLI.EraBased.Commands.Governance.Committee as Cmd
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
  GovernanceCommitteeKeyGenColdCmd cmd ->
    runGovernanceCommitteeKeyGenCold cmd
  GovernanceCommitteeKeyGenHotCmd cmd ->
    runGovernanceCommitteeKeyGenHot cmd
  GovernanceCommitteeKeyHashCmd cmd ->
    runGovernanceCommitteeKeyHash cmd
  GovernanceCommitteeCreateHotKeyAuthorizationCertificateCmd cmd ->
    runGovernanceCommitteeCreateHotKeyAuthorizationCertificate cmd
  GovernanceCommitteeCreateColdKeyResignationCertificateCmd cmd ->
    runGovernanceCommitteeColdKeyResignationCertificate cmd

runGovernanceCommitteeKeyGenCold :: ()
  => Cmd.GovernanceCommitteeKeyGenColdCmdArgs era
  -> ExceptT GovernanceCommitteeError IO ()
runGovernanceCommitteeKeyGenCold
    Cmd.GovernanceCommitteeKeyGenColdCmdArgs
      { Cmd.vkeyOutFile = vkeyPath
      , Cmd.skeyOutFile = skeyPath
      } = do
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
  => Cmd.GovernanceCommitteeKeyGenHotCmdArgs era
  -> ExceptT GovernanceCommitteeError IO ()
runGovernanceCommitteeKeyGenHot
    Cmd.GovernanceCommitteeKeyGenHotCmdArgs
      { Cmd.eon         = _eon
      , Cmd.vkeyOutFile = vkeyPath
      , Cmd.skeyOutFile = skeyPath
      } = do
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
  => Cmd.GovernanceCommitteeKeyHashCmdArgs era
  -> ExceptT GovernanceCommitteeError IO ()
runGovernanceCommitteeKeyHash
    Cmd.GovernanceCommitteeKeyHashCmdArgs
      { Cmd.vkeySource
      } = do
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
  => Cmd.GovernanceCommitteeCreateHotKeyAuthorizationCertificateCmdArgs era
  -> ExceptT GovernanceCommitteeError IO ()
runGovernanceCommitteeCreateHotKeyAuthorizationCertificate
    Cmd.GovernanceCommitteeCreateHotKeyAuthorizationCertificateCmdArgs
      { Cmd.eon = eon
      , Cmd.vkeyColdKeySource = coldVkOrHashOrFp
      , Cmd.vkeyHotKeySource = hotVkOrHashOrFp
      , Cmd.outFile = oFp
      } =
  conwayEraOnwardsConstraints eon $ do
    CommitteeColdKeyHash coldVKHash <-
      lift (readVerificationKeyOrHashOrTextEnvFile AsCommitteeColdKey coldVkOrHashOrFp)
        & onLeft (left . GovernanceCommitteeCmdKeyReadError)

    CommitteeHotKeyHash hotVkHash <-
      lift (readVerificationKeyOrHashOrTextEnvFile AsCommitteeHotKey hotVkOrHashOrFp)
        & onLeft (left . GovernanceCommitteeCmdKeyReadError)

    makeCommitteeHotKeyAuthorizationCertificate (CommitteeHotKeyAuthorizationRequirements eon coldVKHash hotVkHash)
      & textEnvelopeToJSON (Just genKeyDelegCertDesc)
      & writeLazyByteStringFile oFp
      & firstExceptT GovernanceCommitteeCmdTextEnvWriteError . newExceptT

  where
    genKeyDelegCertDesc :: TextEnvelopeDescr
    genKeyDelegCertDesc = "Constitutional Committee Hot Key Registration Certificate"

runGovernanceCommitteeColdKeyResignationCertificate :: ()
  => Cmd.GovernanceCommitteeCreateColdKeyResignationCertificateCmdArgs era
  -> ExceptT GovernanceCommitteeError IO ()
runGovernanceCommitteeColdKeyResignationCertificate
    Cmd.GovernanceCommitteeCreateColdKeyResignationCertificateCmdArgs
      { Cmd.eon               = w
      , Cmd.vkeyColdKeySource = coldVkOrHashOrFp
      , Cmd.anchor            = anchor
      , Cmd.outFile           = oFp
      } =
  conwayEraOnwardsConstraints w $ do
    CommitteeColdKeyHash coldVKHash <-
      lift (readVerificationKeyOrHashOrTextEnvFile AsCommitteeColdKey coldVkOrHashOrFp)
        & onLeft (left . GovernanceCommitteeCmdKeyReadError)

    makeCommitteeColdkeyResignationCertificate (CommitteeColdkeyResignationRequirements w coldVKHash anchor)
      & textEnvelopeToJSON (Just genKeyDelegCertDesc)
      & writeLazyByteStringFile oFp
      & firstExceptT GovernanceCommitteeCmdTextEnvWriteError . newExceptT

  where
    genKeyDelegCertDesc :: TextEnvelopeDescr
    genKeyDelegCertDesc = "Constitutional Committee Cold Key Resignation Certificate"
