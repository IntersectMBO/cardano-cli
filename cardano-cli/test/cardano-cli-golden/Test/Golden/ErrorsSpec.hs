{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}


module Test.Golden.ErrorsSpec
  ( test_GovernanceCmdError
  ) where

import           Cardano.Api
import           Cardano.Api.Shelley

import           Cardano.Binary
import           Cardano.CLI.Commands.Governance
import           Cardano.CLI.Legacy.Run.StakeAddress
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
