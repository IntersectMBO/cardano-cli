{-# LANGUAGE RankNTypes #-}

module Test.Golden.ErrorsSpec
  ( test_DelegationError
  , test_GovernanceActionsError
  , test_GovernanceCmdError
  , test_GovernanceComitteeError
  , test_RegistrationError
  , test_VoteReadError
  ) where

import           Cardano.Api
import           Cardano.Api.Shelley

import           Cardano.Binary
import           Cardano.CLI.EraBased.Run.Governance.Actions
import           Cardano.CLI.EraBased.Run.Governance.Committee
import           Cardano.CLI.Read
import           Cardano.CLI.Types.Errors.DelegationError
import           Cardano.CLI.Types.Errors.GovernanceCmdError
import           Cardano.CLI.Types.Errors.GovernanceVoteCmdError
import           Cardano.CLI.Types.Errors.RegistrationError
import           Cardano.CLI.Types.Errors.ShelleyStakeAddressCmdError
import           Cardano.CLI.Types.Errors.StakeAddressRegistrationError
import           Cardano.CLI.Types.Errors.StakeCredentialError

import           Data.Text.Encoding.Error
import           GHC.Stack (HasCallStack)

import qualified Test.Hedgehog.Golden.ErrorMessage as ErrorMessage
import           Test.Tasty

test_GovernanceCmdError :: TestTree
test_GovernanceCmdError =
  testErrorMessagesRendering "Cardano.CLI.Types.Errors.GovernanceCmdError" "GovernanceCmdError"
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
      , GovernanceCmdConstitutionError
          $ ConstitutionNotUnicodeError
          $ DecodeError "seq" Nothing)
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

test_DelegationError :: TestTree
test_DelegationError =
  testErrorMessagesRendering "Cardano.CLI.Types.Errors.CmdError" "DelegationError"
    [ ("DelegationReadError"
      , DelegationReadError
      $ FileError "path/file.txt" InputInvalidError)
    , ("DelegationStakeCredentialError1"
      , DelegationStakeCredentialError
        $ StakeCredentialInputDecodeError
        $ FileError "path/file.txt" InputInvalidError)
    , ("DelegationStakeCredentialError2"
      , DelegationStakeCredentialError
        $ StakeCredentialScriptDecodeError
        $ FileError "path/file.txt"
        $ ScriptDecodeSimpleScriptError
        $ JsonDecodeError "json decode error")
    , ("DelegationStakeCredentialError3"
      , DelegationStakeCredentialError
        $ StakeCredentialInputDecodeError
        $ FileError "path/file.txt" InputInvalidError)
    , ("DelegationCertificateWriteFileError"
      , DelegationCertificateWriteFileError
        $ FileError "path/file.txt" ())
    , ("DelegationDRepReadError"
      , DelegationDRepReadError $ FileError "path/file.txt" InputInvalidError)
    ]

test_RegistrationError :: TestTree
test_RegistrationError =
  testErrorMessagesRendering "Cardano.CLI.Types.Errors.CmdError" "RegistrationError"
    [ ("RegistrationReadError"
      , RegistrationReadError $ FileError "path/file.txt" InputInvalidError)
    , ("RegistrationWriteFileError"
      , RegistrationWriteFileError $ FileError "path/file.txt" ())
    , ("RegistrationStakeCredReadError1"
      , RegistrationStakeCredentialError
        $ StakeCredentialInputDecodeError
        $ FileError "path/file.txt" InputInvalidError)
    , ("RegistrationStakeCredReadError2"
      , RegistrationStakeCredentialError
        $ StakeCredentialScriptDecodeError
        $ FileError "path/file.txt"
        $ ScriptDecodeSimpleScriptError
        $ JsonDecodeError "json decode error")
    , ("RegistrationStakeCredReadError3"
      , RegistrationStakeCredentialError
        $ StakeCredentialInputDecodeError
        $ FileError "path/file.txt" InputInvalidError)
    , ("RegistrationStakeError"
      , RegistrationStakeError StakeAddressRegistrationDepositRequired)
    ]

test_VoteReadError :: TestTree
test_VoteReadError =
  testErrorMessagesRendering "Cardano.CLI.Types.Errors.GovernanceVoteCmdError" "GovernanceVoteCmdError"
    [ ("GovernanceVoteCmdCredentialDecodeError"
      , GovernanceVoteCmdCredentialDecodeError
        $ DecoderErrorCustom "<todecode>" "<decodeeerror>")
    , ("GovernanceVoteCmdReadError"
      , GovernanceVoteCmdReadError $ FileError "path/file.txt" InputInvalidError)
    , ("GovernanceVoteCmdWriteError"
      , GovernanceVoteCmdWriteError $ FileError "path/file.txt" ())
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
    , ("GovernanceActionsCmdConstitutionError"
      , GovernanceActionsCmdConstitutionError
          $ ConstitutionNotUnicodeError
          $ DecodeError "seq" Nothing)
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

