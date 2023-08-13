{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Types.Errors.GovernanceCmdError
  ( GovernanceCmdError(..)
  ) where

import           Cardano.Api
import           Cardano.Api.Shelley

import           Cardano.Binary (DecoderError)
import           Cardano.CLI.Read (CddlError)
import           Cardano.CLI.Types.Errors.StakeAddressCmdError
import           Cardano.Prelude (intercalate, toS)

import           Data.Text (Text)
import           Data.Text.Encoding.Error
import qualified Data.Text.Lazy.Builder as TL
import qualified Formatting.Buildable as B

data GovernanceCmdError
  = -- Voting related
    StakeCredGovCmdError
      StakeAddressCmdError
  | VotingCredentialDecodeGovCmdEror
      DecoderError
  | WriteFileError
      (FileError ())
  | ReadFileError
      (FileError InputDecodeError)
    -- Governance action related
  | NonUtf8EncodedConstitution
      UnicodeException

  | GovernanceCmdTextEnvReadError
      !(FileError TextEnvelopeError)
  | GovernanceCmdCddlError
      !CddlError
  | GovernanceCmdKeyReadError
      !(FileError InputDecodeError)
  | GovernanceCmdCostModelReadError
      !(FileError ())
  | GovernanceCmdTextEnvWriteError
      !(FileError ())
  | GovernanceCmdEmptyUpdateProposalError
  | GovernanceCmdMIRCertificateKeyRewardMistmach
      !FilePath
      !Int
      -- ^ Number of stake verification keys
      !Int
      -- ^ Number of reward amounts
  | GovernanceCmdCostModelsJsonDecodeErr
      !FilePath !Text
  | GovernanceCmdEmptyCostModel
      !FilePath
  | GovernanceCmdUnexpectedKeyType
      ![TextEnvelopeType]
      -- ^ Expected key types
  | GovernanceCmdPollOutOfBoundAnswer
      !Int
      -- ^ Maximum answer index
  | GovernanceCmdPollInvalidChoice
  | GovernanceCmdDecoderError
      !DecoderError
  | GovernanceCmdVerifyPollError
      !GovernancePollError
  | GovernanceCmdWriteFileError
      !(FileError ())
  -- Legacy - remove me after cardano-cli transitions to new era based structure
  | GovernanceCmdMIRCertNotSupportedInConway
  | GovernanceCmdGenesisDelegationNotSupportedInConway
  deriving Show

instance Error GovernanceCmdError where
  displayError = \case
    StakeCredGovCmdError stakeAddressCmdError ->
      "Stake credential error: " <> toS (renderStakeAddressCmdError stakeAddressCmdError)
    VotingCredentialDecodeGovCmdEror decoderError ->
      "Could not decode voting credential: " <> renderDecoderError decoderError
    WriteFileError fileError ->
      displayError fileError
    ReadFileError fileError ->
      displayError fileError
    NonUtf8EncodedConstitution unicodeException ->
      "Constitution encoded in a format different than UTF-8: " <> show unicodeException
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
      "Error decoding cost model: " <> toS msg <> " at: " <> fp
    GovernanceCmdEmptyCostModel fp ->
      "The decoded cost model was empty at: " <> fp
    GovernanceCmdUnexpectedKeyType expectedTypes ->
      "Unexpected poll key type; expected one of: "
      <> intercalate ", " (show <$> expectedTypes)
    GovernanceCmdPollOutOfBoundAnswer maxIdx ->
      "Poll answer out of bounds. Choices are between 0 and " <> show maxIdx
    GovernanceCmdPollInvalidChoice ->
      "Invalid choice. Please choose from the available answers."
    GovernanceCmdDecoderError decoderError ->
      "Unable to decode metadata: " <> renderDecoderError decoderError
    GovernanceCmdVerifyPollError pollError ->
      toS (renderGovernancePollError pollError)
    GovernanceCmdWriteFileError fileError ->
      "Cannot write file: " <> displayError fileError
    GovernanceCmdMIRCertNotSupportedInConway ->
      "MIR certificates are not supported in Conway era onwards."
    GovernanceCmdGenesisDelegationNotSupportedInConway ->
      "Genesis delegation is not supported in Conway era onwards."
    where
      renderDecoderError = toS . TL.toLazyText . B.build
