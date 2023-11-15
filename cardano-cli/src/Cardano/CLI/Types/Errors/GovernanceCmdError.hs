{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Types.Errors.GovernanceCmdError where

import           Cardano.Api
import           Cardano.Api.Shelley

import           Cardano.Binary (DecoderError)
import           Cardano.CLI.Read
import           Cardano.CLI.Types.Errors.GovernanceHashError (GovernanceHashError)
import           Cardano.CLI.Types.Errors.StakeAddressCmdError

import qualified Data.List as List
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TL
import qualified Formatting.Buildable as B

data GovernanceCmdError
  = -- Voting related
    StakeCredGovCmdError StakeAddressCmdError
  | VotingCredentialDecodeGovCmdEror DecoderError
  | WriteFileError (FileError ())
  | ReadFileError (FileError InputDecodeError)
    -- Governance action related
  | GovernanceCmdConstitutionError ConstitutionError
  | GovernanceCmdHashError !GovernanceHashError
  | GovernanceCmdProposalError ProposalError
  | GovernanceCmdTextEnvReadError !(FileError TextEnvelopeError)
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
  | GovernanceCmdUnexpectedKeyType
      ![TextEnvelopeType]
      -- ^ Expected key types
  | GovernanceCmdPollOutOfBoundAnswer
      !Int
      -- ^ Maximum answer index
  | GovernanceCmdPollInvalidChoice
  | GovernanceCmdDecoderError !DecoderError
  | GovernanceCmdVerifyPollError !GovernancePollError
  | GovernanceCmdWriteFileError !(FileError ())
  | GovernanceCmdDRepMetadataValidationError !DRepMetadataValidationError
  -- Legacy - remove me after cardano-cli transitions to new era based structure
  | GovernanceCmdMIRCertNotSupportedInConway
  | GovernanceCmdGenesisDelegationNotSupportedInConway
  deriving Show

instance Error GovernanceCmdError where
  displayError = \case
    StakeCredGovCmdError stakeAddressCmdError ->
      "Stake credential error: " <> displayError stakeAddressCmdError
    VotingCredentialDecodeGovCmdEror decoderError ->
      "Could not decode voting credential: " <> renderDecoderError decoderError
    WriteFileError fileError ->
      displayError fileError
    ReadFileError fileError ->
      displayError fileError
    GovernanceCmdConstitutionError e ->
      "Constitution error " <> show e -- TODO Conway render this properly
    GovernanceCmdHashError e ->
      "Hash error " <> displayError e
    GovernanceCmdProposalError e ->
      "Proposal error " <> show e -- TODO Conway render this properly
    GovernanceCmdTextEnvReadError fileError ->
      "Cannot read text envelope: " <> displayError fileError
    GovernanceCmdCddlError cddlError ->
      "Reading transaction CDDL file error: " <> displayError cddlError
    GovernanceCmdKeyReadError fileError ->
      "Cannot read key: " <> displayError fileError
    GovernanceCmdCostModelReadError fileError ->
      "Cannot read cost model: " <> displayError fileError
    GovernanceCmdTextEnvWriteError fileError ->
      displayError fileError
    GovernanceCmdEmptyUpdateProposalError ->
      "Empty update proposals are not allowed."
    GovernanceCmdMIRCertificateKeyRewardMistmach fp nStakeVerKeys nRewards ->
      "Error creating the MIR certificate at: " <> fp
      <> " The number of staking keys: " <> show nStakeVerKeys
      <> " and the number of reward amounts: " <> show nRewards
      <> " are not equivalent."
    GovernanceCmdCostModelsJsonDecodeErr fp msg ->
      "Error decoding cost model: " <> Text.unpack msg <> " at: " <> fp
    GovernanceCmdEmptyCostModel fp ->
      "The decoded cost model was empty at: " <> fp
    GovernanceCmdUnexpectedKeyType expectedTypes ->
      "Unexpected poll key type; expected one of: "
      <> List.intercalate ", " (show <$> expectedTypes)
    GovernanceCmdPollOutOfBoundAnswer maxIdx ->
      "Poll answer out of bounds. Choices are between 0 and " <> show maxIdx
    GovernanceCmdPollInvalidChoice ->
      "Invalid choice. Please choose from the available answers."
    GovernanceCmdDecoderError decoderError ->
      "Unable to decode metadata: " <> renderDecoderError decoderError
    GovernanceCmdVerifyPollError pollError ->
      Text.unpack (renderGovernancePollError pollError)
    GovernanceCmdWriteFileError fileError ->
      "Cannot write file: " <> displayError fileError
    GovernanceCmdDRepMetadataValidationError e ->
      "DRep metadata validation error: " <> displayError e
    GovernanceCmdMIRCertNotSupportedInConway ->
      "MIR certificates are not supported in Conway era onwards."
    GovernanceCmdGenesisDelegationNotSupportedInConway ->
      "Genesis delegation is not supported in Conway era onwards."
    where
      renderDecoderError = TL.unpack . TL.toLazyText . B.build
