{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.Run.Governance.Committee
  ( runGovernanceCommitteeCmds
  ) where

import           Cardano.Api

import           Cardano.CLI.EraBased.Commands.Governance.Committee

import           Control.Monad.Except (ExceptT)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Except.Extra
import           Data.Function

newtype GovernanceCommitteeError
  = GovernanceCommitteeCmdWriteFileError (FileError ())

runGovernanceCommitteeCmds :: ()
  => GovernanceCommitteeCmds era
  -> ExceptT GovernanceCommitteeError IO ()
runGovernanceCommitteeCmds = \case
  GovernanceCommitteeKeyGenCold era vk sk ->
    runGovernanceCommitteeKeyGenCold era vk sk
  GovernanceCommitteeKeyGenHot era vk sk ->
    runGovernanceCommitteeKeyGenHot era vk sk

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
