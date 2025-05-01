{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- HLINT ignore "Redundant id" -}
{- HLINT ignore "Use let" -}

module Cardano.CLI.EraBased.Governance.DRep.Run
  ( runGovernanceDRepCmds
  , runGovernanceDRepKeyGenCmd
  )
where

import Cardano.Api
import Cardano.Api.Ledger qualified as L

import Cardano.CLI.EraBased.Governance.DRep.Command qualified as Cmd
import Cardano.CLI.EraIndependent.Hash.Command qualified as Cmd
import Cardano.CLI.EraIndependent.Hash.Internal.Common
  ( allSchemes
  , carryHashChecks
  , getByteStringFromURL
  )
import Cardano.CLI.EraIndependent.Key.Run qualified as Key
import Cardano.CLI.Type.Common
import Cardano.CLI.Type.Error.CmdError
import Cardano.CLI.Type.Error.GovernanceCmdError
import Cardano.CLI.Type.Error.HashCmdError (FetchURLError)
import Cardano.CLI.Type.Error.RegistrationError
import Cardano.CLI.Type.Key

import Control.Monad (void)
import Data.ByteString (ByteString)
import Data.Function
import Data.Text.Encoding qualified as Text
import Vary qualified

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
    drepVerKeyHash <-
      modifyError ReadFileError $
        readVerificationKeyOrHashOrTextEnvFile AsDRepKey vkeySource

    content <-
      pure $
        idOutputFormat
          & ( id
                . Vary.on
                  ( \FormatBech32 ->
                      Text.encodeUtf8 $ serialiseToBech32 drepVerKeyHash
                  )
                . Vary.on
                  ( \FormatHex ->
                      serialiseToRawBytesHex drepVerKeyHash
                  )
                $ Vary.exhaustiveCase
            )

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
          description = Just $ hashSourceToDescription drepHashSource "Registration Certificate"

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
        & writeFileTextEnvelope
          outFile
          (Just $ hashSourceToDescription drepHashSource "Retirement Certificate")
        & modifyError GovernanceCmdTextEnvWriteError . newExceptT

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
        writeFileTextEnvelope
          outFile
          (Just $ hashSourceToDescription drepHashSource "Update Certificate")
          updateCertificate

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

hashSourceToDescription :: DRepHashSource -> TextEnvelopeDescr -> TextEnvelopeDescr
hashSourceToDescription source what =
  ( case source of
      DRepHashSourceScript _ -> "DRep Script"
      DRepHashSourceVerificationKey _ -> "DRep Key"
  )
    <> " "
    <> what
