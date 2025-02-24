{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Cardano.CLI.EraBased.Governance.Committee.Run
  ( runGovernanceCommitteeCmds
  , runGovernanceCommitteeKeyGenCold
  , runGovernanceCommitteeKeyGenHot
  , GovernanceCommitteeError (..)
  )
where

import Cardano.Api
import Cardano.Api.Shelley

import Cardano.CLI.EraBased.Governance.Committee.Command
import Cardano.CLI.EraBased.Governance.Committee.Command qualified as Cmd
import Cardano.CLI.EraIndependent.Hash.Internal.Common (carryHashChecks)
import Cardano.CLI.EraIndependent.Key.Run qualified as Key
import Cardano.CLI.Read (readVerificationKeySource)
import Cardano.CLI.Type.Common (PotentiallyCheckedAnchor (..))
import Cardano.CLI.Type.Error.GovernanceCommitteeError
import Cardano.CLI.Type.Key.VerificationKey

import Control.Monad (void)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Function

runGovernanceCommitteeCmds
  :: ()
  => GovernanceCommitteeCmds era
  -> ExceptT GovernanceCommitteeError IO ()
runGovernanceCommitteeCmds = \case
  GovernanceCommitteeKeyGenColdCmd cmd ->
    void $ runGovernanceCommitteeKeyGenCold cmd
  GovernanceCommitteeKeyGenHotCmd cmd ->
    void $ runGovernanceCommitteeKeyGenHot cmd
  GovernanceCommitteeKeyHashCmd cmd ->
    runGovernanceCommitteeKeyHash cmd
  GovernanceCommitteeCreateHotKeyAuthorizationCertificateCmd cmd ->
    runGovernanceCommitteeCreateHotKeyAuthorizationCertificate cmd
  GovernanceCommitteeCreateColdKeyResignationCertificateCmd cmd ->
    runGovernanceCommitteeColdKeyResignationCertificate cmd

runGovernanceCommitteeKeyGenCold
  :: ()
  => Cmd.GovernanceCommitteeKeyGenColdCmdArgs era
  -> ExceptT GovernanceCommitteeError IO (VerificationKey CommitteeColdKey, SigningKey CommitteeColdKey)
runGovernanceCommitteeKeyGenCold
  Cmd.GovernanceCommitteeKeyGenColdCmdArgs
    { Cmd.vkeyOutFile = vkeyPath
    , Cmd.skeyOutFile = skeyPath
    } = do
    skey <- generateSigningKey AsCommitteeColdKey
    let vkey = getVerificationKey skey

    void $ firstExceptT GovernanceCommitteeCmdWriteFileError $ do
      void $ writeLazyByteStringFile skeyPath (textEnvelopeToJSON (Just Key.ccColdSkeyDesc) skey)
      writeLazyByteStringFile vkeyPath (textEnvelopeToJSON (Just Key.ccColdVkeyDesc) vkey)

    return (vkey, skey)

runGovernanceCommitteeKeyGenHot
  :: ()
  => Cmd.GovernanceCommitteeKeyGenHotCmdArgs era
  -> ExceptT GovernanceCommitteeError IO (VerificationKey CommitteeHotKey, SigningKey CommitteeHotKey)
runGovernanceCommitteeKeyGenHot
  Cmd.GovernanceCommitteeKeyGenHotCmdArgs
    { Cmd.eon = _eon
    , Cmd.vkeyOutFile = vkeyPath
    , Cmd.skeyOutFile = skeyPath
    } = do
    skey <- generateSigningKey AsCommitteeHotKey

    let vkey = getVerificationKey skey

    void $ firstExceptT GovernanceCommitteeCmdWriteFileError $ do
      void $ writeLazyByteStringFile skeyPath $ textEnvelopeToJSON (Just Key.ccHotSkeyDesc) skey
      writeLazyByteStringFile vkeyPath $ textEnvelopeToJSON (Just Key.ccHotVkeyDesc) vkey

    return (vkey, skey)

data SomeCommitteeKey
  = ACommitteeHotKey (VerificationKey CommitteeHotKey)
  | ACommitteeHotExtendedKey (VerificationKey CommitteeHotExtendedKey)
  | ACommitteeColdKey (VerificationKey CommitteeColdKey)
  | ACommitteeColdExtendedKey (VerificationKey CommitteeColdExtendedKey)

runGovernanceCommitteeKeyHash
  :: ()
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
                [ FromSomeType (AsVerificationKey AsCommitteeHotKey) ACommitteeHotKey
                , FromSomeType (AsVerificationKey AsCommitteeHotExtendedKey) ACommitteeHotExtendedKey
                , FromSomeType (AsVerificationKey AsCommitteeColdKey) ACommitteeColdKey
                , FromSomeType (AsVerificationKey AsCommitteeColdExtendedKey) ACommitteeColdExtendedKey
                ]
          pure (deserialiseAnyOfFromBech32 asTypes (unAnyVerificationKeyText vkText))
            & onLeft (left . GovernanceCommitteeCmdKeyDecodeError . InputBech32DecodeError)
        AnyVerificationKeySourceOfFile vkeyPath -> do
          let asTypes =
                [ FromSomeType (AsVerificationKey AsCommitteeHotKey) ACommitteeHotKey
                , FromSomeType (AsVerificationKey AsCommitteeHotExtendedKey) ACommitteeHotExtendedKey
                , FromSomeType (AsVerificationKey AsCommitteeColdKey) ACommitteeColdKey
                , FromSomeType (AsVerificationKey AsCommitteeColdExtendedKey) ACommitteeColdExtendedKey
                ]
          readFileTextEnvelopeAnyOf asTypes vkeyPath
            & firstExceptT GovernanceCommitteeCmdTextEnvReadFileError . newExceptT

    liftIO $ BS.putStrLn (renderKeyHash vkey)
   where
    renderKeyHash :: SomeCommitteeKey -> ByteString
    renderKeyHash = \case
      ACommitteeHotKey vk -> renderVerificationKeyHash vk
      ACommitteeHotExtendedKey vk -> renderVerificationKeyHash vk
      ACommitteeColdKey vk -> renderVerificationKeyHash vk
      ACommitteeColdExtendedKey vk -> renderVerificationKeyHash vk

    renderVerificationKeyHash :: Key keyrole => VerificationKey keyrole -> ByteString
    renderVerificationKeyHash =
      serialiseToRawBytesHex
        . verificationKeyHash

runGovernanceCommitteeCreateHotKeyAuthorizationCertificate
  :: ()
  => Cmd.GovernanceCommitteeCreateHotKeyAuthorizationCertificateCmdArgs era
  -> ExceptT GovernanceCommitteeError IO ()
runGovernanceCommitteeCreateHotKeyAuthorizationCertificate
  Cmd.GovernanceCommitteeCreateHotKeyAuthorizationCertificateCmdArgs
    { Cmd.eon = eon
    , Cmd.vkeyColdKeySource
    , Cmd.vkeyHotKeySource
    , Cmd.outFile = oFp
    } =
    conwayEraOnwardsConstraints eon $ do
      let mapError' = modifyError $ either GovernanceCommitteeCmdScriptReadError GovernanceCommitteeCmdKeyReadError
      hotCred <-
        mapError' $
          readVerificationKeySource AsCommitteeHotKey unCommitteeHotKeyHash vkeyHotKeySource
      coldCred <-
        mapError' $
          readVerificationKeySource AsCommitteeColdKey unCommitteeColdKeyHash vkeyColdKeySource

      makeCommitteeHotKeyAuthorizationCertificate
        (CommitteeHotKeyAuthorizationRequirements eon coldCred hotCred)
        & textEnvelopeToJSON (Just genKeyDelegCertDesc)
        & writeLazyByteStringFile oFp
        & firstExceptT GovernanceCommitteeCmdTextEnvWriteError . newExceptT
   where
    genKeyDelegCertDesc :: TextEnvelopeDescr
    genKeyDelegCertDesc = "Constitutional Committee Hot Key Registration Certificate"

runGovernanceCommitteeColdKeyResignationCertificate
  :: ()
  => Cmd.GovernanceCommitteeCreateColdKeyResignationCertificateCmdArgs era
  -> ExceptT GovernanceCommitteeError IO ()
runGovernanceCommitteeColdKeyResignationCertificate
  Cmd.GovernanceCommitteeCreateColdKeyResignationCertificateCmdArgs
    { Cmd.eon
    , Cmd.vkeyColdKeySource
    , Cmd.anchor
    , Cmd.outFile
    } =
    conwayEraOnwardsConstraints eon $ do
      let modifyError' = modifyError $ either GovernanceCommitteeCmdScriptReadError GovernanceCommitteeCmdKeyReadError
      coldVKeyCred <-
        modifyError' $
          readVerificationKeySource AsCommitteeColdKey unCommitteeColdKeyHash vkeyColdKeySource

      mapM_
        (withExceptT GovernanceCommitteeHashCheckError . carryHashChecks)
        anchor

      makeCommitteeColdkeyResignationCertificate
        (CommitteeColdkeyResignationRequirements eon coldVKeyCred (pcaAnchor <$> anchor))
        & textEnvelopeToJSON (Just genKeyDelegCertDesc)
        & writeLazyByteStringFile outFile
        & firstExceptT GovernanceCommitteeCmdTextEnvWriteError . newExceptT
   where
    genKeyDelegCertDesc :: TextEnvelopeDescr
    genKeyDelegCertDesc = "Constitutional Committee Cold Key Resignation Certificate"
