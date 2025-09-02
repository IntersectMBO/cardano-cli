{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Type.Error.GovernanceCmdError where

import Cardano.Api

data GovernanceCmdError
  = -- Voting related
    WriteFileError (FileError ())
  | -- Governance action related
    GovernanceCmdEmptyUpdateProposalError
  | GovernanceCmdMIRCertificateKeyRewardMistmach
      !FilePath
      !Int
      -- ^ Number of stake verification keys
      !Int
      -- ^ Number of reward amounts
  | GovernanceCmdEmptyCostModel !FilePath
  | -- | Maximum answer index
    GovernanceCmdPollOutOfBoundAnswer
      !Int
  | GovernanceCmdPollInvalidChoice
  | GovernanceCmdHashMismatchError
      !(Hash DRepMetadata)
      -- ^ Expected hash
      !(Hash DRepMetadata)
      -- ^ Actual hash
  | GovernanceCmdTextEnvReadError !(FileError TextEnvelopeError)
  | GovernanceCmdTextEnvCddlReadError !(FileError TextEnvelopeCddlError)
  | GovernanceCmdVerifyPollError !GovernancePollError
  deriving Show

instance Error GovernanceCmdError where
  prettyError = \case
    WriteFileError fileError ->
      prettyError fileError
    GovernanceCmdEmptyUpdateProposalError ->
      "Empty update proposals are not allowed."
    GovernanceCmdMIRCertificateKeyRewardMistmach fp nStakeVerKeys nRewards ->
      "Error creating the MIR certificate at: "
        <> pretty fp
        <> " The number of staking keys: "
        <> pshow nStakeVerKeys
        <> " and the number of reward amounts: "
        <> pshow nRewards
        <> " are not equivalent."
    GovernanceCmdEmptyCostModel fp ->
      "The decoded cost model was empty at: " <> pretty fp
    GovernanceCmdPollOutOfBoundAnswer maxIdx ->
      "Poll answer out of bounds. Choices are between 0 and " <> pshow maxIdx
    GovernanceCmdPollInvalidChoice ->
      "Invalid choice. Please choose from the available answers."
    GovernanceCmdHashMismatchError (DRepMetadataHash expectedHash) (DRepMetadataHash actualHash) ->
      "Hashes do not match!"
        <> "\nExpected:"
          <+> pretty (show expectedHash)
        <> "\n  Actual:"
          <+> pretty (show actualHash)
    GovernanceCmdTextEnvReadError fileError ->
      "Cannot read text envelope: " <> prettyError fileError
    GovernanceCmdTextEnvCddlReadError fileError ->
      "Cannot read text cddl envelope: " <> prettyError fileError
    GovernanceCmdVerifyPollError pollError ->
      pretty $ renderGovernancePollError pollError
