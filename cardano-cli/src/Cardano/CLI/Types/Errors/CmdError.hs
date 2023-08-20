{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Types.Errors.CmdError
  ( CmdError(..)
  , EraBasedDelegationError(..)
  , EraBasedRegistrationError(..)
  , GovernanceActionsError(..)
  , GovernanceCommitteeError(..)
  ) where

import           Cardano.Api

import           Cardano.CLI.Types.Errors.GovernanceCmdError
import           Cardano.CLI.Types.Errors.GovernanceVoteCmdError
import           Cardano.CLI.Types.Errors.StakeAddressRegistrationError
import           Cardano.CLI.Types.Errors.StakeCredentialError

import           Data.Text.Encoding.Error
import           GHC.Generics (Generic)

data CmdError
  = CmdEraBasedRegistrationError !EraBasedRegistrationError
  | CmdEraDelegationError        !EraBasedDelegationError
  | CmdGovernanceActionError     !GovernanceActionsError
  | CmdGovernanceCmdError        !GovernanceCmdError
  | CmdGovernanceCommitteeError  !GovernanceCommitteeError
  | CmdGovernanceVoteError       !GovernanceVoteCmdError
  deriving Show

instance Error CmdError where
  displayError = \case
    CmdGovernanceCmdError e -> displayError e
    CmdEraDelegationError e -> displayError e
    CmdEraBasedRegistrationError e -> displayError e
    CmdGovernanceVoteError e -> displayError e
    CmdGovernanceCommitteeError e -> displayError e
    CmdGovernanceActionError e -> displayError e

data GovernanceActionsError
  = GovernanceActionsCmdNonUtf8EncodedConstitution UnicodeException
  | GovernanceActionsCmdReadFileError (FileError InputDecodeError)
  | GovernanceActionsCmdReadTextEnvelopeFileError (FileError TextEnvelopeError)
  | GovernanceActionsCmdWriteFileError (FileError ())
  deriving Show

instance Error GovernanceActionsError where
  displayError = \case
    GovernanceActionsCmdNonUtf8EncodedConstitution e ->
      "Cannot read constitution: " <> show e
    GovernanceActionsCmdReadFileError e ->
      "Cannot read file: " <> displayError e
    GovernanceActionsCmdReadTextEnvelopeFileError e ->
      "Cannot read text envelope file: " <> displayError e
    GovernanceActionsCmdWriteFileError e ->
      "Cannot write file: " <> displayError e

data EraBasedDelegationError
  = EraBasedDelegReadError !(FileError InputDecodeError)
  | EraBasedCertificateWriteFileError !(FileError ())
  | EraBasedDRepReadError !(FileError InputDecodeError)
  | EraBasedDelegationStakeCredentialError !StakeCredentialError
  deriving (Show, Generic)

instance Error EraBasedDelegationError where
  displayError = \case
    EraBasedDelegReadError e ->
      "Cannot read delegation target: " <> displayError e
    EraBasedDelegationStakeCredentialError e ->
      "Cannot get stake credential: " <> displayError e
    EraBasedCertificateWriteFileError e ->
      "Cannot write certificate: " <> displayError e
    EraBasedDRepReadError e ->
      "Cannot read DRep key: " <> displayError e

data EraBasedRegistrationError
  = EraBasedRegistReadError !(FileError InputDecodeError)
  | EraBasedRegistWriteFileError !(FileError ())
  | EraBasedRegistrationStakeCredentialError !StakeCredentialError
  | EraBasedRegistStakeError !StakeAddressRegistrationError
  deriving Show

instance Error EraBasedRegistrationError where
  displayError = \case
    EraBasedRegistReadError e ->
      "Cannot read registration certificate: " <> displayError e
    EraBasedRegistWriteFileError e ->
      "Cannot write registration certificate: " <> displayError e
    EraBasedRegistrationStakeCredentialError e ->
      "Cannot read stake credential: " <> displayError e
    EraBasedRegistStakeError e ->
      "Stake address registation error: " <> displayError e

data GovernanceCommitteeError
  = GovernanceCommitteeCmdKeyDecodeError InputDecodeError
  | GovernanceCommitteeCmdKeyReadError (FileError InputDecodeError)
  | GovernanceCommitteeCmdTextEnvReadFileError (FileError TextEnvelopeError)
  | GovernanceCommitteeCmdTextEnvWriteError (FileError ())
  | GovernanceCommitteeCmdWriteFileError (FileError ())
  deriving Show

instance Error GovernanceCommitteeError where
  displayError = \case
    GovernanceCommitteeCmdKeyDecodeError e ->
      "Cannot decode key: " <> displayError e
    GovernanceCommitteeCmdKeyReadError e ->
      "Cannot read key: " <> displayError e
    GovernanceCommitteeCmdWriteFileError e ->
      "Cannot write file: " <> displayError e
    GovernanceCommitteeCmdTextEnvReadFileError e ->
      "Cannot read text envelope file: " <> displayError e
    GovernanceCommitteeCmdTextEnvWriteError e ->
      "Cannot write text envelope file: " <> displayError e
