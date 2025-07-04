{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.CLI.EraBased.Governance.Committee.Run
  ( runGovernanceCommitteeCmds
  , runGovernanceCommitteeKeyGenCold
  , runGovernanceCommitteeKeyGenHot
  )
where

import Cardano.Api
import Cardano.Api.Experimental (obtainCommonConstraints)

import Cardano.CLI.Compatible.Exception
import Cardano.CLI.EraBased.Governance.Committee.Command
import Cardano.CLI.EraBased.Governance.Committee.Command qualified as Cmd
import Cardano.CLI.EraIndependent.Hash.Internal.Common (carryHashChecks)
import Cardano.CLI.EraIndependent.Key.Run qualified as Key
import Cardano.CLI.Orphan ()
import Cardano.CLI.Read (readVerificationKeySource)
import Cardano.CLI.Type.Common (PotentiallyCheckedAnchor (..))
import Cardano.CLI.Type.Key.VerificationKey

import Control.Monad (void)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Function

runGovernanceCommitteeCmds
  :: ()
  => GovernanceCommitteeCmds era
  -> CIO e ()
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
  :: Cmd.GovernanceCommitteeKeyGenColdCmdArgs era
  -> CIO e (VerificationKey CommitteeColdKey, SigningKey CommitteeColdKey)
runGovernanceCommitteeKeyGenCold
  Cmd.GovernanceCommitteeKeyGenColdCmdArgs
    { Cmd.vkeyOutFile = vkeyPath
    , Cmd.skeyOutFile = skeyPath
    } = do
    skey <- generateSigningKey AsCommitteeColdKey
    let vkey = getVerificationKey skey

    fromEitherIOCli @(FileError ()) $
      writeLazyByteStringFile skeyPath (textEnvelopeToJSON (Just Key.ccColdSkeyDesc) skey)
    fromEitherIOCli @(FileError ()) $
      writeLazyByteStringFile vkeyPath (textEnvelopeToJSON (Just Key.ccColdVkeyDesc) vkey)

    return (vkey, skey)

runGovernanceCommitteeKeyGenHot
  :: Cmd.GovernanceCommitteeKeyGenHotCmdArgs era
  -> CIO e (VerificationKey CommitteeHotKey, SigningKey CommitteeHotKey)
runGovernanceCommitteeKeyGenHot
  Cmd.GovernanceCommitteeKeyGenHotCmdArgs
    { Cmd.era = _eon
    , Cmd.vkeyOutFile = vkeyPath
    , Cmd.skeyOutFile = skeyPath
    } = do
    skey <- generateSigningKey AsCommitteeHotKey

    let vkey = getVerificationKey skey

    fromEitherIOCli @(FileError ()) $
      writeLazyByteStringFile skeyPath $
        textEnvelopeToJSON (Just Key.ccHotSkeyDesc) skey
    fromEitherIOCli @(FileError ()) $
      writeLazyByteStringFile vkeyPath $
        textEnvelopeToJSON (Just Key.ccHotVkeyDesc) vkey

    return (vkey, skey)

data SomeCommitteeKey
  = ACommitteeHotKey (VerificationKey CommitteeHotKey)
  | ACommitteeHotExtendedKey (VerificationKey CommitteeHotExtendedKey)
  | ACommitteeColdKey (VerificationKey CommitteeColdKey)
  | ACommitteeColdExtendedKey (VerificationKey CommitteeColdExtendedKey)

runGovernanceCommitteeKeyHash
  :: ()
  => Cmd.GovernanceCommitteeKeyHashCmdArgs era
  -> CIO e ()
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
          fromEitherCli . deserialiseAnyOfFromBech32 asTypes $ unAnyVerificationKeyText vkText
        AnyVerificationKeySourceOfFile vkeyPath -> do
          let asTypes =
                [ FromSomeType (AsVerificationKey AsCommitteeHotKey) ACommitteeHotKey
                , FromSomeType (AsVerificationKey AsCommitteeHotExtendedKey) ACommitteeHotExtendedKey
                , FromSomeType (AsVerificationKey AsCommitteeColdKey) ACommitteeColdKey
                , FromSomeType (AsVerificationKey AsCommitteeColdExtendedKey) ACommitteeColdExtendedKey
                ]
          fromEitherIOCli $ readFileTextEnvelopeAnyOf asTypes vkeyPath

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
  -> CIO e ()
runGovernanceCommitteeCreateHotKeyAuthorizationCertificate
  Cmd.GovernanceCommitteeCreateHotKeyAuthorizationCertificateCmdArgs
    { Cmd.era = eon
    , Cmd.vkeyColdKeySource
    , Cmd.vkeyHotKeySource
    , Cmd.outFile = oFp
    } =
    obtainCommonConstraints eon $ do
      hotCred <-
        readVerificationKeySource unCommitteeHotKeyHash vkeyHotKeySource
      coldCred <-
        readVerificationKeySource unCommitteeColdKeyHash vkeyColdKeySource
      fromEitherIOCli @(FileError ()) $
        makeCommitteeHotKeyAuthorizationCertificate
          (CommitteeHotKeyAuthorizationRequirements (convert eon) coldCred hotCred)
          & textEnvelopeToJSON (Just genKeyDelegCertDesc)
          & writeLazyByteStringFile oFp
   where
    genKeyDelegCertDesc :: TextEnvelopeDescr
    genKeyDelegCertDesc = "Constitutional Committee Hot Key Registration Certificate"

runGovernanceCommitteeColdKeyResignationCertificate
  :: ()
  => Cmd.GovernanceCommitteeCreateColdKeyResignationCertificateCmdArgs era
  -> CIO e ()
runGovernanceCommitteeColdKeyResignationCertificate
  Cmd.GovernanceCommitteeCreateColdKeyResignationCertificateCmdArgs
    { Cmd.era
    , Cmd.vkeyColdKeySource
    , Cmd.anchor
    , Cmd.outFile
    } =
    obtainCommonConstraints era $ do
      coldVKeyCred <-
        readVerificationKeySource unCommitteeColdKeyHash vkeyColdKeySource

      mapM_
        (fromExceptTCli . carryHashChecks)
        anchor

      fromEitherIOCli @(FileError ()) $
        makeCommitteeColdkeyResignationCertificate
          (CommitteeColdkeyResignationRequirements (convert era) coldVKeyCred (pcaAnchor <$> anchor))
          & textEnvelopeToJSON (Just genKeyDelegCertDesc)
          & writeLazyByteStringFile outFile
   where
    genKeyDelegCertDesc :: TextEnvelopeDescr
    genKeyDelegCertDesc = "Constitutional Committee Cold Key Resignation Certificate"
