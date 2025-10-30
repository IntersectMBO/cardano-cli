{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.CLI.EraBased.Governance.DRep.Run
  ( runGovernanceDRepCmds
  , runGovernanceDRepKeyGenCmd
  )
where

import Cardano.Api hiding
  ( makeDrepRegistrationCertificate
  , makeDrepUnregistrationCertificate
  , makeDrepUpdateCertificate
  )
import Cardano.Api.Experimental (obtainCommonConstraints)
import Cardano.Api.Experimental qualified as Exp
import Cardano.Api.Ledger qualified as L

import Cardano.CLI.Compatible.Exception
import Cardano.CLI.EraBased.Governance.DRep.Command qualified as Cmd
import Cardano.CLI.EraIndependent.Hash.Command qualified as Cmd
import Cardano.CLI.EraIndependent.Hash.Internal.Common
  ( allSchemes
  , carryHashChecks
  , getByteStringFromURL
  )
import Cardano.CLI.EraIndependent.Key.Run qualified as Key
import Cardano.CLI.Type.Common
import Cardano.CLI.Type.Error.GovernanceCmdError
import Cardano.CLI.Type.Key

import Control.Monad (void)
import Data.Function
import Data.Text.Encoding qualified as Text
import Vary qualified

runGovernanceDRepCmds
  :: ()
  => Cmd.GovernanceDRepCmds era
  -> CIO e ()
runGovernanceDRepCmds = \case
  Cmd.GovernanceDRepKeyGenCmd args ->
    void $
      runGovernanceDRepKeyGenCmd args
  Cmd.GovernanceDRepIdCmd args ->
    runGovernanceDRepIdCmd args
  Cmd.GovernanceDRepRegistrationCertificateCmd args ->
    runGovernanceDRepRegistrationCertificateCmd args
  Cmd.GovernanceDRepRetirementCertificateCmd args ->
    runGovernanceDRepRetirementCertificateCmd args
  Cmd.GovernanceDRepUpdateCertificateCmd args ->
    runGovernanceDRepUpdateCertificateCmd args
  Cmd.GovernanceDRepMetadataHashCmd args ->
    runGovernanceDRepMetadataHashCmd args

runGovernanceDRepKeyGenCmd
  :: ()
  => Cmd.GovernanceDRepKeyGenCmdArgs era
  -> CIO e (VerificationKey DRepKey, SigningKey DRepKey)
runGovernanceDRepKeyGenCmd
  Cmd.GovernanceDRepKeyGenCmdArgs
    { vkeyFile
    , skeyFile
    } = do
    (vkey, skey) <- generateKeyPair AsDRepKey
    fromEitherIOCli @(FileError ()) $
      writeLazyByteStringFile skeyFile (textEnvelopeToJSON (Just Key.drepSkeyDesc) skey)

    fromEitherIOCli @(FileError ()) $
      writeLazyByteStringFile vkeyFile (textEnvelopeToJSON (Just Key.drepVkeyDesc) vkey)

    return (vkey, skey)

runGovernanceDRepIdCmd
  :: ()
  => Cmd.GovernanceDRepIdCmdArgs era
  -> CIO e ()
runGovernanceDRepIdCmd
  Cmd.GovernanceDRepIdCmdArgs
    { vkeySource
    , idOutputFormat
    , mOutFile
    } = do
    drepVerKeyHash <-
      readVerificationKeyOrHashOrTextEnvFile vkeySource

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
                . Vary.on
                  ( \FormatCip129 ->
                      let DRepKeyHash kh = drepVerKeyHash
                          keyCredential = L.KeyHashObj kh
                       in Text.encodeUtf8 $ serialiseToBech32Cip129 keyCredential
                  )
                $ Vary.exhaustiveCase
            )

    fromEitherIOCli @(FileError ()) $ writeByteStringOutput mOutFile content

--------------------------------------------------------------------------------

-- Registration Certificate related

runGovernanceDRepRegistrationCertificateCmd
  :: ()
  => forall era e
   . Cmd.GovernanceDRepRegistrationCertificateCmdArgs era
  -> CIO e ()
runGovernanceDRepRegistrationCertificateCmd
  Cmd.GovernanceDRepRegistrationCertificateCmdArgs
    { era = w :: Exp.Era era
    , drepHashSource
    , deposit
    , mAnchor
    , outFile
    } = do
    drepCred <- readDRepCredential drepHashSource

    mapM_
      (fromExceptTCli . carryHashChecks)
      mAnchor
    let
      registrationCert =
        obtainCommonConstraints w $
          Exp.makeDrepRegistrationCertificate drepCred deposit (pcaAnchor <$> mAnchor)
          :: Exp.Certificate (Exp.LedgerEra era)
      description = Just $ hashSourceToDescription drepHashSource "Registration Certificate"

    fromEitherIOCli @(FileError ()) $
      writeLazyByteStringFile outFile $
        obtainCommonConstraints w $
          textEnvelopeToJSON description registrationCert

runGovernanceDRepRetirementCertificateCmd
  :: forall era e
   . Cmd.GovernanceDRepRetirementCertificateCmdArgs era
  -> CIO e ()
runGovernanceDRepRetirementCertificateCmd
  Cmd.GovernanceDRepRetirementCertificateCmdArgs
    { era = w
    , drepHashSource
    , deposit
    , outFile
    } = Exp.obtainCommonConstraints w $ do
    drepCredential <- readDRepCredential drepHashSource
    let unregCert =
          Exp.makeDrepUnregistrationCertificate
            drepCredential
            deposit
            :: Exp.Certificate (Exp.LedgerEra era)
    obtainCommonConstraints w $
      fromExceptTCli $
        newExceptT $
          writeFileTextEnvelope
            outFile
            (Just $ hashSourceToDescription drepHashSource "Retirement Certificate")
            unregCert

runGovernanceDRepUpdateCertificateCmd
  :: forall era e
   . Cmd.GovernanceDRepUpdateCertificateCmdArgs era
  -> CIO e ()
runGovernanceDRepUpdateCertificateCmd
  Cmd.GovernanceDRepUpdateCertificateCmdArgs
    { era = w
    , drepHashSource
    , mAnchor
    , outFile
    } = Exp.obtainCommonConstraints w $ do
    mapM_
      (fromExceptTCli . carryHashChecks)
      mAnchor
    drepCredential <- readDRepCredential drepHashSource
    let updateCertificate =
          obtainCommonConstraints w $
            Exp.makeDrepUpdateCertificate
              drepCredential
              (pcaAnchor <$> mAnchor)
            :: Exp.Certificate (Exp.LedgerEra era)

    fromExceptTCli . newExceptT $
      obtainCommonConstraints w $
        writeFileTextEnvelope
          outFile
          (Just $ hashSourceToDescription drepHashSource "Update Certificate")
          updateCertificate

runGovernanceDRepMetadataHashCmd
  :: ()
  => Cmd.GovernanceDRepMetadataHashCmdArgs era
  -> CIO e ()
runGovernanceDRepMetadataHashCmd
  Cmd.GovernanceDRepMetadataHashCmdArgs
    { drepMetadataSource
    , hashGoal
    } = do
    metadataBytes <- case drepMetadataSource of
      Cmd.DrepMetadataFileIn metadataFile ->
        fromEitherIOCli @(FileError ()) $ readByteStringFile metadataFile
      Cmd.DrepMetadataURL urlText ->
        fromExceptTCli $
          getByteStringFromURL allSchemes $
            L.urlToText urlText
    let (_metadata, metadataHash) = hashDRepMetadata metadataBytes
    case hashGoal of
      Cmd.CheckHash expectedHash
        | metadataHash /= expectedHash ->
            throwCliError $ GovernanceCmdHashMismatchError expectedHash metadataHash
        | otherwise -> liftIO $ putStrLn "Hashes match!"
      Cmd.HashToFile outFile -> fromExceptTCli $ writeOutput (Just outFile) metadataHash
      Cmd.HashToStdout -> fromExceptTCli $ writeOutput Nothing metadataHash
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

hashSourceToDescription :: DRepHashSource -> TextEnvelopeDescr -> TextEnvelopeDescr
hashSourceToDescription source what =
  ( case source of
      DRepHashSourceScript _ -> "DRep Script"
      DRepHashSourceVerificationKey _ -> "DRep Key"
  )
    <> " "
    <> what
