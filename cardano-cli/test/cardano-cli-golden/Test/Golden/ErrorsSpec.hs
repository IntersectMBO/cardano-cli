{-# LANGUAGE RankNTypes #-}

module Test.Golden.ErrorsSpec
  ( test_DelegationError
  , test_GovernanceActionsError
  , test_GovernanceCmdError
  , test_RegistrationError
  , test_CostModelsError
  )
where

import Cardano.Api

import Cardano.CLI.EraBased.Governance.Actions.Run
import Cardano.CLI.Read
import Cardano.CLI.Type.Error.DelegationError
import Cardano.CLI.Type.Error.GovernanceCmdError
import Cardano.CLI.Type.Error.RegistrationError
import Cardano.CLI.Type.Error.StakeAddressRegistrationError
import Cardano.CLI.Type.Error.StakeCredentialError

import GHC.Stack (HasCallStack)

import Test.Hedgehog.Golden.ErrorMessage qualified as ErrorMessage
import Test.Tasty

test_GovernanceCmdError :: TestTree
test_GovernanceCmdError =
  testErrorMessagesRendering
    "Cardano.CLI.Type.Error.GovernanceCmdError"
    "GovernanceCmdError"
    [
      ( "WriteFileError"
      , WriteFileError $ FileError "path/file.txt" ()
      )
    ,
      ( "GovernanceCmdEmptyUpdateProposalError"
      , GovernanceCmdEmptyUpdateProposalError
      )
    ,
      ( "GovernanceCmdMIRCertificateKeyRewardMistmach"
      , GovernanceCmdMIRCertificateKeyRewardMistmach "path/file.txt" 1 2
      )
    ,
      ( "GovernanceCmdEmptyCostModel"
      , GovernanceCmdEmptyCostModel "path/file.txt"
      )
    ,
      ( "GovernanceCmdPollOutOfBoundAnswer"
      , GovernanceCmdPollOutOfBoundAnswer 4
      )
    ,
      ( "GovernanceCmdPollInvalidChoice"
      , GovernanceCmdPollInvalidChoice
      )
    ]

test_DelegationError :: TestTree
test_DelegationError =
  testErrorMessagesRendering
    "Cardano.CLI.Type.Error.CmdError"
    "DelegationError"
    [
      ( "DelegationReadError"
      , DelegationReadError $
          FileError "path/file.txt" InputInvalidError
      )
    ,
      ( "DelegationStakeCredentialError1"
      , DelegationStakeCredentialError $
          StakeCredentialInputDecodeError $
            FileError "path/file.txt" InputInvalidError
      )
    ,
      ( "DelegationStakeCredentialError2"
      , DelegationStakeCredentialError $
          StakeCredentialScriptDecodeError $
            FileError "path/file.txt" $
              ScriptDecodeSimpleScriptError $
                JsonDecodeError "json decode error"
      )
    ,
      ( "DelegationStakeCredentialError3"
      , DelegationStakeCredentialError $
          StakeCredentialInputDecodeError $
            FileError "path/file.txt" InputInvalidError
      )
    ,
      ( "DelegationCertificateWriteFileError"
      , DelegationCertificateWriteFileError $
          FileError "path/file.txt" ()
      )
    ,
      ( "DelegationDRepReadError"
      , DelegationDRepReadError $ FileError "path/file.txt" InputInvalidError
      )
    ]

test_RegistrationError :: TestTree
test_RegistrationError =
  testErrorMessagesRendering
    "Cardano.CLI.Type.Error.CmdError"
    "RegistrationError"
    [
      ( "RegistrationReadError"
      , RegistrationReadError $ FileError "path/file.txt" InputInvalidError
      )
    ,
      ( "RegistrationWriteFileError"
      , RegistrationWriteFileError $ FileError "path/file.txt" ()
      )
    ,
      ( "RegistrationStakeCredReadError1"
      , RegistrationStakeCredentialError $
          StakeCredentialInputDecodeError $
            FileError "path/file.txt" InputInvalidError
      )
    ,
      ( "RegistrationStakeCredReadError2"
      , RegistrationStakeCredentialError $
          StakeCredentialScriptDecodeError $
            FileError "path/file.txt" $
              ScriptDecodeSimpleScriptError $
                JsonDecodeError "json decode error"
      )
    ,
      ( "RegistrationStakeCredReadError3"
      , RegistrationStakeCredentialError $
          StakeCredentialInputDecodeError $
            FileError "path/file.txt" InputInvalidError
      )
    ,
      ( "RegistrationStakeError"
      , RegistrationStakeError StakeAddressRegistrationDepositRequired
      )
    ]

test_GovernanceActionsError :: TestTree
test_GovernanceActionsError =
  testErrorMessagesRendering
    "Cardano.CLI.EraBased.Governance.Actions.Run"
    "GovernanceActionsError"
    [
      ( "GovernanceActionsCmdWriteFileError"
      , GovernanceActionsCmdWriteFileError $ FileError "path/file.txt" ()
      )
    ,
      ( "GovernanceActionsCmdCostModelsError"
      , GovernanceActionsCmdCostModelsError $
          CostModelsErrorReadFile $
            FileError "some/file.txt" ()
      )
    ]

test_CostModelsError :: TestTree
test_CostModelsError =
  testErrorMessagesRendering
    "Cardano.CLI.Read"
    "CostModelsError"
    [
      ( "CostModelsErrorReadFile"
      , CostModelsErrorReadFile $ FileError "some/file.txt" ()
      )
    ,
      ( "CostModelsErrorJSONDecode"
      , CostModelsErrorJSONDecode "some/file.txt" "some error"
      )
    ,
      ( "CostModelsErrorEmpty"
      , CostModelsErrorEmpty "some/file.txt"
      )
    ]

goldenFilesPath :: FilePath
goldenFilesPath = "test/cardano-cli-golden/files/golden/errors"

testErrorMessagesRendering
  :: forall a
   . ()
  => HasCallStack
  => Error a
  => String
  -- ^ module name
  -> String
  -- ^ type name
  -> [(String, a)]
  -- ^ list of constructor names and values
  -> TestTree
testErrorMessagesRendering = ErrorMessage.testAllErrorMessages_ goldenFilesPath
