{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Type.Error.GovernanceCmdError where

import Cardano.Api
import Cardano.Api.Shelley

import Cardano.Binary (DecoderError)
import Cardano.CLI.Read
import Cardano.CLI.Type.Error.HashCmdError (FetchURLError, HashCheckError)
import Cardano.CLI.Type.Error.StakeAddressCmdError

import Control.Exception (displayException)
import Data.List qualified as List
import Data.Text (Text)
import Data.Text.Lazy.Builder qualified as TL
import Formatting.Buildable qualified as B

data GovernanceCmdError
  = -- Voting related
    StakeCredGovCmdError StakeAddressCmdError
  | VotingCredentialDecodeGovCmdEror DecoderError
  | WriteFileError (FileError ())
  | ReadFileError (FileError InputDecodeError)
  | -- Governance action related
    GovernanceCmdConstitutionError ConstitutionError
  | GovernanceCmdProposalError ProposalError
  | GovernanceCmdTextEnvReadError !(FileError TextEnvelopeError)
  | GovernanceCmdTextEnvCddlReadError !(FileError TextEnvelopeCddlError)
  | GovernanceCmdCddlError !CddlError
  | GovernanceCmdKeyReadError !(FileError InputDecodeError)
  | GovernanceCmdCostModelReadError !(FileError ())
  | GovernanceCmdTextEnvWriteError !(FileError ())
  | GovernanceCmdEmptyUpdateProposalError
  | GovernanceCmdMIRCertificateKeyRewardMistmach
      !FilePath
      !Int
      -- ^ Number of stake verification keys
      !Int
      -- ^ Number of reward amounts
  | GovernanceCmdCostModelsJsonDecodeErr !FilePath !Text
  | GovernanceCmdEmptyCostModel !FilePath
  | -- | Expected key types
    GovernanceCmdUnexpectedKeyType
      ![TextEnvelopeType]
  | -- | Maximum answer index
    GovernanceCmdPollOutOfBoundAnswer
      !Int
  | GovernanceCmdPollInvalidChoice
  | GovernanceCmdDecoderError !DecoderError
  | GovernanceCmdVerifyPollError !GovernancePollError
  | GovernanceCmdWriteFileError !(FileError ())
  | -- Legacy - remove me after cardano-cli transitions to new era based structure
    GovernanceCmdMIRCertNotSupportedInConway
  | GovernanceCmdGenesisDelegationNotSupportedInConway
  | GovernanceDRepHashCheckError HashCheckError
  | GovernanceCmdHashMismatchError
      !(Hash DRepMetadata)
      -- ^ Expected hash
      !(Hash DRepMetadata)
      -- ^ Actual hash
  | GovernanceCmdFetchURLError !FetchURLError
  deriving Show

instance Error GovernanceCmdError where
  prettyError = \case
    StakeCredGovCmdError stakeAddressCmdError ->
      "Stake credential error: " <> prettyError stakeAddressCmdError
    VotingCredentialDecodeGovCmdEror decoderError ->
      "Could not decode voting credential: " <> renderDecoderError decoderError
    WriteFileError fileError ->
      prettyError fileError
    ReadFileError fileError ->
      prettyError fileError
    GovernanceCmdConstitutionError e ->
      "Constitution error " <> pshow e -- TODO Conway render this properly
    GovernanceCmdProposalError e ->
      "Proposal error " <> pshow e -- TODO Conway render this properly
    GovernanceCmdTextEnvReadError fileError ->
      "Cannot read text envelope: " <> prettyError fileError
    GovernanceCmdTextEnvCddlReadError fileError ->
      "Cannot read text cddl envelope: " <> prettyError fileError
    GovernanceCmdCddlError cddlError ->
      "Reading transaction CDDL file error: " <> prettyError cddlError
    GovernanceCmdKeyReadError fileError ->
      "Cannot read key: " <> prettyError fileError
    GovernanceCmdCostModelReadError fileError ->
      "Cannot read cost model: " <> prettyError fileError
    GovernanceCmdTextEnvWriteError fileError ->
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
    GovernanceCmdCostModelsJsonDecodeErr fp msg ->
      "Error decoding cost model: " <> pretty msg <> " at: " <> pretty fp
    GovernanceCmdEmptyCostModel fp ->
      "The decoded cost model was empty at: " <> pretty fp
    GovernanceCmdUnexpectedKeyType expectedTypes ->
      mconcat
        [ "Unexpected poll key type; expected one of: "
        , mconcat $ List.intersperse ", " (pshow <$> expectedTypes)
        ]
    GovernanceCmdPollOutOfBoundAnswer maxIdx ->
      "Poll answer out of bounds. Choices are between 0 and " <> pshow maxIdx
    GovernanceCmdPollInvalidChoice ->
      "Invalid choice. Please choose from the available answers."
    GovernanceCmdDecoderError decoderError ->
      "Unable to decode metadata: " <> renderDecoderError decoderError
    GovernanceCmdVerifyPollError pollError ->
      pretty $ renderGovernancePollError pollError
    GovernanceCmdWriteFileError fileError ->
      "Cannot write file: " <> prettyError fileError
    GovernanceCmdMIRCertNotSupportedInConway ->
      "MIR certificates are not supported in Conway era onwards."
    GovernanceCmdGenesisDelegationNotSupportedInConway ->
      "Genesis delegation is not supported in Conway era onwards."
    GovernanceDRepHashCheckError hashCheckError ->
      "Error while checking DRep metadata hash: " <> pretty (displayException hashCheckError)
    GovernanceCmdHashMismatchError (DRepMetadataHash expectedHash) (DRepMetadataHash actualHash) ->
      "Hashes do not match!"
        <> "\nExpected:"
          <+> pretty (show expectedHash)
        <> "\n  Actual:"
          <+> pretty (show actualHash)
    GovernanceCmdFetchURLError fetchErr ->
      pretty (displayException fetchErr)
   where
    renderDecoderError = pretty . TL.toLazyText . B.build
