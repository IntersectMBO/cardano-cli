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
  ) where

import           Cardano.Api
import           Cardano.Api.Ledger (Credential (KeyHashObj))
import qualified Cardano.Api.Ledger as Ledger
import           Cardano.Api.Shelley

import qualified Cardano.CLI.EraBased.Commands.Governance.DRep as Cmd
import qualified Cardano.CLI.EraBased.Run.Key as Key
import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Errors.CmdError
import           Cardano.CLI.Types.Errors.GovernanceCmdError
import           Cardano.CLI.Types.Errors.RegistrationError
import           Cardano.CLI.Types.Key

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Except.Extra
import           Data.Function
import qualified Data.Text.Encoding as Text

runGovernanceDRepCmds :: ()
  => Cmd.GovernanceDRepCmds era
  -> ExceptT CmdError IO ()
runGovernanceDRepCmds = \case
  Cmd.GovernanceDRepKeyGenCmd args ->
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

  Cmd.GovernanceDRepMetadataHashCmd args ->
    runGovernanceDRepMetadataHashCmd args
      & firstExceptT CmdGovernanceCmdError

runGovernanceDRepKeyGenCmd :: ()
  => Cmd.GovernanceDRepKeyGenCmdArgs era
  -> ExceptT (FileError ()) IO ()
runGovernanceDRepKeyGenCmd
    Cmd.GovernanceDRepKeyGenCmdArgs
      { vkeyFile
      , skeyFile
      } = do
  skey <- liftIO $ generateSigningKey AsDRepKey
  let vkey = getVerificationKey skey
  newExceptT $ writeLazyByteStringFile skeyFile (textEnvelopeToJSON (Just skeyDesc) skey)
  newExceptT $ writeLazyByteStringFile vkeyFile (textEnvelopeToJSON (Just Key.drepVkeyDesc) vkey)
  where
    skeyDesc :: TextEnvelopeDescr
    skeyDesc = "Delegate Representative Signing Key"

runGovernanceDRepIdCmd :: ()
  => Cmd.GovernanceDRepIdCmdArgs era
  -> ExceptT GovernanceCmdError IO ()
runGovernanceDRepIdCmd
    Cmd.GovernanceDRepIdCmdArgs
      { vkeySource
      , idOutputFormat
      , mOutFile
      } = do
  drepVerKey <-
    lift (readVerificationKeyOrTextEnvFile AsDRepKey vkeySource)
      & onLeft (left . ReadFileError)

  content <-
    pure $ case idOutputFormat of
      IdOutputFormatHex -> serialiseToRawBytesHex $ verificationKeyHash drepVerKey
      IdOutputFormatBech32 -> Text.encodeUtf8 $ serialiseToBech32 $ verificationKeyHash drepVerKey

  lift (writeByteStringOutput mOutFile content)
    & onLeft (left . WriteFileError)

--------------------------------------------------------------------------------

-- Registration Certificate related

runGovernanceDRepRegistrationCertificateCmd :: ()
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
    drepCred <-
      case drepHashSource of
        DRepHashSourceScript (ScriptHash scriptHash) ->
          return $ Ledger.ScriptHashObj scriptHash
        DRepHashSourceVerificationKey drepVkeyHashSource -> do
          DRepKeyHash drepKeyHash <-
            firstExceptT RegistrationReadError
              . newExceptT
              $ readVerificationKeyOrHashOrFile AsDRepKey drepVkeyHashSource
          return $ Ledger.KeyHashObj $ conwayEraOnwardsConstraints w drepKeyHash
    let req = DRepRegistrationRequirements w drepCred deposit
        registrationCert = makeDrepRegistrationCertificate req mAnchor
        description = Just @TextEnvelopeDescr "DRep Key Registration Certificate"

    firstExceptT RegistrationWriteFileError
      . newExceptT
      . writeLazyByteStringFile outFile
      $ conwayEraOnwardsConstraints w
      $ textEnvelopeToJSON description registrationCert

runGovernanceDRepRetirementCertificateCmd :: ()
  => Cmd.GovernanceDRepRetirementCertificateCmdArgs era
  -> ExceptT GovernanceCmdError IO ()
runGovernanceDRepRetirementCertificateCmd
    Cmd.GovernanceDRepRetirementCertificateCmdArgs
      { eon = w
      , vkeyHashSource
      , deposit
      , outFile
      } =
  conwayEraOnwardsConstraints w $ do
    DRepKeyHash drepKeyHash <- firstExceptT GovernanceCmdKeyReadError
      . newExceptT
      $ readVerificationKeyOrHashOrFile AsDRepKey vkeyHashSource
    makeDrepUnregistrationCertificate (DRepUnregistrationRequirements w (KeyHashObj drepKeyHash) deposit)
      & writeFileTextEnvelope outFile (Just genKeyDelegCertDesc)
      & firstExceptT GovernanceCmdTextEnvWriteError . newExceptT

  where
    genKeyDelegCertDesc :: TextEnvelopeDescr
    genKeyDelegCertDesc = "DRep Retirement Certificate"

runGovernanceDRepMetadataHashCmd :: ()
  => Cmd.GovernanceDRepMetadataHashCmdArgs era
  -> ExceptT GovernanceCmdError IO ()
runGovernanceDRepMetadataHashCmd
    Cmd.GovernanceDRepMetadataHashCmdArgs
      { metadataFile
      , mOutFile
      } = do
  metadataBytes <- firstExceptT ReadFileError $ newExceptT (readByteStringFile metadataFile)
  (_metadata, metadataHash) <-
    firstExceptT GovernanceCmdDRepMetadataValidationError
     . hoistEither
     $ validateAndHashDRepMetadata metadataBytes
  firstExceptT WriteFileError
    . newExceptT
    . writeByteStringOutput mOutFile
    . serialiseToRawBytesHex
    $ metadataHash
