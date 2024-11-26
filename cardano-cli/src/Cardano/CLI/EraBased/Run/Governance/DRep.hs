{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- HLINT ignore "Use let" -}

module Cardano.CLI.EraBased.Run.Governance.DRep
  ( runGovernanceDRepCmds
  , runGovernanceDRepKeyGenCmd
  )
where

import           Cardano.Api
import qualified Cardano.Api.Ledger as L

import qualified Cardano.CLI.Commands.Hash as Cmd
import qualified Cardano.CLI.EraBased.Commands.Governance.DRep as Cmd
import           Cardano.CLI.Run.Hash (allSchemes, carryHashChecks, getByteStringFromURL)
import qualified Cardano.CLI.Run.Key as Key
import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Errors.CmdError
import           Cardano.CLI.Types.Errors.GovernanceCmdError
import           Cardano.CLI.Types.Errors.HashCmdError (FetchURLError)
import           Cardano.CLI.Types.Errors.RegistrationError
import           Cardano.CLI.Types.Key

import           Control.Monad (void)
import           Data.ByteString (ByteString)
import           Data.Function
import qualified Data.Text.Encoding as Text

runGovernanceDRepCmds
  :: ()
  => Cmd.GovernanceDRepCmds era
  -> ExceptT CmdError IO ()
runGovernanceDRepCmds = \case
  Cmd.GovernanceDRepKeyGenCmd args ->
    void $
      runGovernanceDRepKeyGenCmd args
        & firstExceptT (CmdGovernanceCmdError . GovernanceCmdWriteFileError)
  Cmd.GovernanceDRepIdCmd args ->
    runGovernanceDRepIdCmd args
      & firstExceptT CmdGovernanceCmdError
  Cmd.GovernanceDRepRegistrationCertificateCmd args ->
    runGovernanceDRepRegistrationCertificateCmd args
      & firstExceptT CmdRegistrationError
  Cmd.GovernanceDRepRetirementCertificateCmd args ->
    runGovernanceDRepRetirementCertificateCmd args
      & firstExceptT CmdGovernanceCmdError
  Cmd.GovernanceDRepUpdateCertificateCmd args ->
    runGovernanceDRepUpdateCertificateCmd args
      & firstExceptT CmdGovernanceCmdError
  Cmd.GovernanceDRepMetadataHashCmd args ->
    runGovernanceDRepMetadataHashCmd args
      & firstExceptT CmdGovernanceCmdError

runGovernanceDRepKeyGenCmd
  :: ()
  => Cmd.GovernanceDRepKeyGenCmdArgs era
  -> ExceptT (FileError ()) IO (VerificationKey DRepKey, SigningKey DRepKey)
runGovernanceDRepKeyGenCmd
  Cmd.GovernanceDRepKeyGenCmdArgs
    { vkeyFile
    , skeyFile
    } = do
    (vkey, skey) <- liftIO $ generateKeyPair AsDRepKey
    newExceptT $ writeLazyByteStringFile skeyFile (textEnvelopeToJSON (Just Key.drepSkeyDesc) skey)
    newExceptT $ writeLazyByteStringFile vkeyFile (textEnvelopeToJSON (Just Key.drepVkeyDesc) vkey)
    return (vkey, skey)

runGovernanceDRepIdCmd
  :: ()
  => Cmd.GovernanceDRepIdCmdArgs era
  -> ExceptT GovernanceCmdError IO ()
runGovernanceDRepIdCmd
  Cmd.GovernanceDRepIdCmdArgs
    { vkeySource
    , idOutputFormat
    , mOutFile
    } = do
    drepVerKey <-
      modifyError ReadFileError $
        readVerificationKeyOrTextEnvFile AsDRepKey vkeySource

    content <-
      pure $ case idOutputFormat of
        IdOutputFormatHex -> serialiseToRawBytesHex $ verificationKeyHash drepVerKey
        IdOutputFormatBech32 -> Text.encodeUtf8 $ serialiseToBech32 $ verificationKeyHash drepVerKey

    lift (writeByteStringOutput mOutFile content)
      & onLeft (left . WriteFileError)

--------------------------------------------------------------------------------

-- Registration Certificate related

runGovernanceDRepRegistrationCertificateCmd
  :: ()
  => Cmd.GovernanceDRepRegistrationCertificateCmdArgs era
  -> ExceptT RegistrationError IO ()
runGovernanceDRepRegistrationCertificateCmd
  Cmd.GovernanceDRepRegistrationCertificateCmdArgs
    { eon = w
    , drepHashSource
    , deposit
    , mAnchor
    , outFile
    } =
    conwayEraOnwardsConstraints w $ do
      drepCred <- modifyError RegistrationReadError $ readDRepCredential drepHashSource

      mapM_
        (withExceptT RegistrationDRepHashCheckError . carryHashChecks)
        mAnchor

      let req = DRepRegistrationRequirements w drepCred deposit
          registrationCert =
            makeDrepRegistrationCertificate
              req
              (pcaAnchor <$> mAnchor)
          description = Just @TextEnvelopeDescr "DRep Key Registration Certificate"

      firstExceptT RegistrationWriteFileError
        . newExceptT
        . writeLazyByteStringFile outFile
        $ conwayEraOnwardsConstraints w
        $ textEnvelopeToJSON description registrationCert

runGovernanceDRepRetirementCertificateCmd
  :: ()
  => Cmd.GovernanceDRepRetirementCertificateCmdArgs era
  -> ExceptT GovernanceCmdError IO ()
runGovernanceDRepRetirementCertificateCmd
  Cmd.GovernanceDRepRetirementCertificateCmdArgs
    { eon = w
    , drepHashSource
    , deposit
    , outFile
    } =
    conwayEraOnwardsConstraints w $ do
      drepCredential <- modifyError GovernanceCmdKeyReadError $ readDRepCredential drepHashSource
      makeDrepUnregistrationCertificate (DRepUnregistrationRequirements w drepCredential deposit)
        & writeFileTextEnvelope outFile (Just genKeyDelegCertDesc)
        & modifyError GovernanceCmdTextEnvWriteError . newExceptT
   where
    genKeyDelegCertDesc :: TextEnvelopeDescr
    genKeyDelegCertDesc = "DRep Retirement Certificate"

runGovernanceDRepUpdateCertificateCmd
  :: ()
  => Cmd.GovernanceDRepUpdateCertificateCmdArgs era
  -> ExceptT GovernanceCmdError IO ()
runGovernanceDRepUpdateCertificateCmd
  Cmd.GovernanceDRepUpdateCertificateCmdArgs
    { eon = w
    , drepHashSource
    , mAnchor
    , outFile
    } =
    conwayEraOnwardsConstraints w $ do
      mapM_
        (withExceptT GovernanceDRepHashCheckError . carryHashChecks)
        mAnchor
      drepCredential <- modifyError GovernanceCmdKeyReadError $ readDRepCredential drepHashSource
      let updateCertificate =
            makeDrepUpdateCertificate
              (DRepUpdateRequirements w drepCredential)
              (pcaAnchor <$> mAnchor)
      firstExceptT GovernanceCmdTextEnvWriteError . newExceptT $
        writeFileTextEnvelope outFile (Just "DRep Update Certificate") updateCertificate

runGovernanceDRepMetadataHashCmd
  :: ()
  => Cmd.GovernanceDRepMetadataHashCmdArgs era
  -> ExceptT GovernanceCmdError IO ()
runGovernanceDRepMetadataHashCmd
  Cmd.GovernanceDRepMetadataHashCmdArgs
    { drepMetadataSource
    , hashGoal
    } = do
    metadataBytes <- case drepMetadataSource of
      Cmd.DrepMetadataFileIn metadataFile ->
        firstExceptT ReadFileError . newExceptT $ readByteStringFile metadataFile
      Cmd.DrepMetadataURL urlText ->
        fetchURLToGovernanceCmdError $ getByteStringFromURL allSchemes $ L.urlToText urlText
    let (_metadata, metadataHash) = hashDRepMetadata metadataBytes
    case hashGoal of
      Cmd.CheckHash expectedHash
        | metadataHash /= expectedHash ->
            left $ GovernanceCmdHashMismatchError expectedHash metadataHash
        | otherwise -> liftIO $ putStrLn "Hashes match!"
      Cmd.HashToFile outFile -> writeOutput (Just outFile) metadataHash
      Cmd.HashToStdout -> writeOutput Nothing metadataHash
   where
    writeOutput
      :: MonadIO m
      => Maybe (File content Out)
      -> Hash DRepMetadata
      -> ExceptT GovernanceCmdError m ()
    writeOutput mOutFile =
      firstExceptT WriteFileError
        . newExceptT
        . writeByteStringOutput mOutFile
        . serialiseToRawBytesHex

    fetchURLToGovernanceCmdError
      :: ExceptT FetchURLError IO ByteString -> ExceptT GovernanceCmdError IO ByteString
    fetchURLToGovernanceCmdError = withExceptT GovernanceCmdFetchURLError
