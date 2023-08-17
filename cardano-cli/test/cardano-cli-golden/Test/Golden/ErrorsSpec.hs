{-# LANGUAGE RankNTypes #-}

module Test.Golden.ErrorsSpec
  ( test_GovernanceCmdError
  , test_EraBasedDelegationError
  , test_EraBasedRegistrationError
  , test_EraBasedVoteReadError
  , test_GovernanceComitteeError
  , test_GovernanceActionsError
  ) where

import           Cardano.Api
import           Cardano.Api.Shelley

import           Cardano.Binary
import           Cardano.CLI.Commands.Governance
import           Cardano.CLI.EraBased.Errors.StakeAddress
import           Cardano.CLI.EraBased.Run.Certificate
import           Cardano.CLI.EraBased.Run.Governance.Actions
import           Cardano.CLI.EraBased.Run.Governance.Committee
import           Cardano.CLI.EraBased.Vote
import           Cardano.CLI.Read

import           Data.Text.Encoding.Error
import           GHC.Stack (HasCallStack)

import qualified Test.Hedgehog.Golden.ErrorMessage as ErrorMessage
import           Test.Tasty

test_GovernanceCmdError :: TestTree
test_GovernanceCmdError =
  testErrorMessagesRendering "Cardano.CLI.Commands.Governance" "GovernanceCmdError"
    [ ("StakeCredGovCmdError"
      , StakeCredGovCmdError
        . ShelleyStakeAddressCmdReadKeyFileError
        $ FileError "path/file.txt" InputInvalidError)
    , ("VotingCredentialDecodeGovCmdEror"
      , VotingCredentialDecodeGovCmdEror $ DecoderErrorEmptyList "emptylist")
    , ("WriteFileError"
      , WriteFileError $ FileError "path/file.txt" ())
    , ("ReadFileError"
      , ReadFileError $ FileError "path/file.txt" InputInvalidError)
    , ("NonUtf8EncodedConstitution"
      , NonUtf8EncodedConstitution $ DecodeError "seq" Nothing)
    , ("GovernanceCmdTextEnvReadError"
      , GovernanceCmdTextEnvReadError
        . FileError  "path/file.txt"
        $ TextEnvelopeAesonDecodeError "cannot decode json")
    , ("GovernanceCmdCddlError"
      , GovernanceCmdCddlError
        $ CddlErrorTextEnv
          (FileError "path/file.txt" . TextEnvelopeDecodeError $ DecoderErrorCustom "todecode" "decodeerr")
          (FileError "path/file.txt" TextEnvelopeCddlUnknownKeyWitness))
    , ("GovernanceCmdKeyReadError"
      , GovernanceCmdKeyReadError $ FileError "path/file.txt" InputInvalidError)
    , ("GovernanceCmdCostModelReadError"
      , GovernanceCmdCostModelReadError $ FileError "path/file.txt" ())
    , ("GovernanceCmdTextEnvWriteError"
      , GovernanceCmdTextEnvWriteError $ FileError "path/file.txt" ())
    , ("GovernanceCmdEmptyUpdateProposalError"
      , GovernanceCmdEmptyUpdateProposalError)
    , ("GovernanceCmdMIRCertificateKeyRewardMistmach"
       ,GovernanceCmdMIRCertificateKeyRewardMistmach "path/file.txt" 1 2)
    , ("GovernanceCmdCostModelsJsonDecodeErr"
      , GovernanceCmdCostModelsJsonDecodeErr "path/file.txt" "jsonerr")
    , ("GovernanceCmdEmptyCostModel"
      , GovernanceCmdEmptyCostModel "path/file.txt")
    , ("GovernanceCmdUnexpectedKeyType"
      , GovernanceCmdUnexpectedKeyType (TextEnvelopeType <$> ["env1", "env2"]))
    , ("GovernanceCmdPollOutOfBoundAnswer"
      , GovernanceCmdPollOutOfBoundAnswer 4)
    , ("GovernanceCmdPollInvalidChoice"
      , GovernanceCmdPollInvalidChoice)
    , ("GovernanceCmdDecoderError"
      , GovernanceCmdDecoderError $ DecoderErrorEmptyList "empty")
    , ("GovernanceCmdVerifyPollError"
      , GovernanceCmdVerifyPollError ErrGovernancePollNoAnswer)
    , ("GovernanceCmdWriteFileError"
      , GovernanceCmdWriteFileError $ FileError "path/file.txt" ())
    , ("GovernanceCmdMIRCertNotSupportedInConway"
      , GovernanceCmdMIRCertNotSupportedInConway)
    , ("GovernanceCmdGenesisDelegationNotSupportedInConway"
      , GovernanceCmdGenesisDelegationNotSupportedInConway)
    ]

test_EraBasedDelegationError :: TestTree
test_EraBasedDelegationError =
  testErrorMessagesRendering "Cardano.CLI.EraBased.Run.Certificate" "EraBasedDelegationError"
    [ ("EraBasedDelegReadError"
      , EraBasedDelegReadError $ FileError "path/file.txt" InputInvalidError)
    , ("EraBasedCredentialError1"
      , EraBasedCredentialError
        $ ShelleyStakeAddressCmdReadKeyFileError $ FileError "path/file.txt" InputInvalidError)
    , ("EraBasedCredentialError2"
      , EraBasedCredentialError
        $ ShelleyStakeAddressCmdReadScriptFileError
        $ FileError "path/file.txt"
        $ ScriptDecodeSimpleScriptError
        $ JsonDecodeError "json decode error")
    , ("EraBasedCredentialError3"
      , EraBasedCredentialError
        $ ShelleyStakeAddressCmdReadKeyFileError $ FileError "path/file.txt" InputInvalidError)
    , ("EraBasedCredentialError4"
      , EraBasedCredentialError
        $ ShelleyStakeAddressCmdWriteFileError $ FileError "path/file.txt" ())
    , ("EraBasedCredentialError5"
      , EraBasedCredentialError
        $ StakeRegistrationError StakeAddressRegistrationDepositRequired)
    , ("EraBasedCredentialError6"
      , EraBasedCredentialError
        $ StakeDelegationError
        $ VoteDelegationNotSupported
        $ AnyShelleyToBabbageEra ShelleyToBabbageEraShelley)
    , ("EraBasedCertificateWriteFileError"
      , EraBasedCertificateWriteFileError $ FileError "path/file.txt" ())
    , ("EraBasedDRepReadError"
      , EraBasedDRepReadError $ FileError "path/file.txt" InputInvalidError)
    ]

test_EraBasedRegistrationError :: TestTree
test_EraBasedRegistrationError =
  testErrorMessagesRendering "Cardano.CLI.EraBased.Run.Certificate" "EraBasedRegistrationError"
    [ ("EraBasedRegistReadError"
      , EraBasedRegistReadError $ FileError "path/file.txt" InputInvalidError)
    , ("EraBasedRegistWriteFileError"
      , EraBasedRegistWriteFileError $ FileError "path/file.txt" ())
    , ("EraBasedRegistStakeCredReadError1"
      , EraBasedRegistStakeCredReadError
        $ ShelleyStakeAddressCmdReadKeyFileError $ FileError "path/file.txt" InputInvalidError)
    , ("EraBasedRegistStakeCredReadError2"
      , EraBasedRegistStakeCredReadError
        $ ShelleyStakeAddressCmdReadScriptFileError
        $ FileError "path/file.txt"
        $ ScriptDecodeSimpleScriptError
        $ JsonDecodeError "json decode error")
    , ("EraBasedRegistStakeCredReadError3"
      , EraBasedRegistStakeCredReadError
        $ ShelleyStakeAddressCmdReadKeyFileError $ FileError "path/file.txt" InputInvalidError)
    , ("EraBasedRegistStakeCredReadError4"
      , EraBasedRegistStakeCredReadError
        $ ShelleyStakeAddressCmdWriteFileError $ FileError "path/file.txt" ())
    , ("EraBasedRegistStakeCredReadError5"
      , EraBasedRegistStakeCredReadError
        $ StakeRegistrationError StakeAddressRegistrationDepositRequired)
    , ("EraBasedRegistStakeCredReadError6"
      , EraBasedRegistStakeCredReadError
        $ StakeDelegationError
        $ VoteDelegationNotSupported
        $ AnyShelleyToBabbageEra ShelleyToBabbageEraShelley)
    , ("EraBasedRegistStakeError"
      , EraBasedRegistStakeError StakeAddressRegistrationDepositRequired)
    ]

test_EraBasedVoteReadError :: TestTree
test_EraBasedVoteReadError =
  testErrorMessagesRendering "Cardano.CLI.EraBased.Vote" "EraBasedVoteError"
    [ ("EraBasedVoteReadError"
      , EraBasedVoteReadError $ FileError "path/file.txt" InputInvalidError)
    , ("EraBasedVotingCredentialDecodeError"
      , EraBasedVotingCredentialDecodeError
        $ DecoderErrorCustom "<todecode>" "<decodeeerror>")
    , ("EraBasedVoteWriteError"
      , EraBasedVoteWriteError $ FileError "path/file.txt" ())
    ]

test_GovernanceComitteeError :: TestTree
test_GovernanceComitteeError =
  testErrorMessagesRendering "Cardano.CLI.EraBased.Run.Governance.Committee" "GovernanceCommitteeError"
    [ ("GovernanceCommitteeCmdWriteFileError"
      , GovernanceCommitteeCmdWriteFileError $ FileError "path/file.txt" ())
    , ("GovernanceCommitteeCmdTextEnvReadFileError"
      , GovernanceCommitteeCmdTextEnvReadFileError
        $ FileError  "path/file.txt"
        $ TextEnvelopeAesonDecodeError "cannot decode json")
    ]

test_GovernanceActionsError :: TestTree
test_GovernanceActionsError =
  testErrorMessagesRendering "Cardano.CLI.EraBased.Run.Governance.Actions" "GovernanceActionsError"
    [ ("GovernanceActionsCmdWriteFileError"
      , GovernanceActionsCmdWriteFileError $ FileError "path/file.txt" ())
    , ("GovernanceActionsCmdReadFileError"
      , GovernanceActionsCmdReadFileError $ FileError "path/file.txt" InputInvalidError)
    , ("GovernanceActionsCmdNonUtf8EncodedConstitution"
      , GovernanceActionsCmdNonUtf8EncodedConstitution $ DecodeError "seq" Nothing)
    ]


goldenFilesPath :: FilePath
goldenFilesPath = "test/cardano-cli-golden/files/golden/errors"

testErrorMessagesRendering :: forall a. ()
  => HasCallStack
  => Error a
  => String -- ^ module name
  -> String -- ^ type name
  -> [(String, a)]  -- ^ list of constructor names and values
  -> TestTree
testErrorMessagesRendering = ErrorMessage.testAllErrorMessages_ goldenFilesPath

