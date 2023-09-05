{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Types.Errors.ShelleyGenesisCmdError
  ( ShelleyGenesisCmdError(..)
  ) where

import           Cardano.Api

import           Cardano.CLI.Byron.Genesis as Byron
import           Cardano.CLI.Orphans ()
import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Errors.ShelleyAddressCmdError
import           Cardano.CLI.Types.Errors.ShelleyNodeCmdError
import           Cardano.CLI.Types.Errors.ShelleyStakeAddressCmdError
import           Cardano.CLI.Types.Errors.StakePoolCmdError

import           Control.Exception (IOException)
import           Data.Text (Text)
import qualified Data.Text as Text

data ShelleyGenesisCmdError
  = ShelleyGenesisCmdAesonDecodeError !FilePath !Text
  | ShelleyGenesisCmdGenesisFileReadError !(FileError IOException)
  | ShelleyGenesisCmdGenesisFileDecodeError !FilePath !Text
  | ShelleyGenesisCmdGenesisFileError !(FileError ())
  | ShelleyGenesisCmdFileError !(FileError ())
  | ShelleyGenesisCmdMismatchedGenesisKeyFiles [Int] [Int] [Int]
  | ShelleyGenesisCmdFilesNoIndex [FilePath]
  | ShelleyGenesisCmdFilesDupIndex [FilePath]
  | ShelleyGenesisCmdTextEnvReadFileError !(FileError TextEnvelopeError)
  | ShelleyGenesisCmdUnexpectedAddressVerificationKey !(VerificationKeyFile In) !Text !SomeAddressVerificationKey
  | ShelleyGenesisCmdTooFewPoolsForBulkCreds !Word !Word !Word
  | ShelleyGenesisCmdAddressCmdError !ShelleyAddressCmdError
  | ShelleyGenesisCmdNodeCmdError !ShelleyNodeCmdError
  | ShelleyGenesisCmdStakeAddressCmdError !ShelleyStakeAddressCmdError
  | ShelleyGenesisCmdStakePoolCmdError !StakePoolCmdError
  | ShelleyGenesisCmdCostModelsError !FilePath
  | ShelleyGenesisCmdByronError !ByronGenesisError
  | ShelleyGenesisStakePoolRelayFileError !FilePath !IOException
  | ShelleyGenesisStakePoolRelayJsonDecodeError !FilePath !String
  deriving Show

instance Error ShelleyGenesisCmdError where
  displayError =
    \case
      ShelleyGenesisCmdAesonDecodeError fp decErr ->
        "Error while decoding Shelley genesis at: " <> fp <> " Error: " <> Text.unpack decErr
      ShelleyGenesisCmdGenesisFileError fe -> displayError fe
      ShelleyGenesisCmdFileError fe -> displayError fe
      ShelleyGenesisCmdMismatchedGenesisKeyFiles gfiles dfiles vfiles ->
        "Mismatch between the files found:\n"
          <> "Genesis key file indexes:      " <> show gfiles <> "\n"
          <> "Delegate key file indexes:     " <> show dfiles <> "\n"
          <> "Delegate VRF key file indexes: " <> show vfiles
      ShelleyGenesisCmdFilesNoIndex files ->
        "The genesis keys files are expected to have a numeric index but these do not:\n"
          <> unlines files
      ShelleyGenesisCmdFilesDupIndex files ->
        "The genesis keys files are expected to have a unique numeric index but these do not:\n"
          <> unlines files
      ShelleyGenesisCmdTextEnvReadFileError fileErr -> displayError fileErr
      ShelleyGenesisCmdUnexpectedAddressVerificationKey (File file) expect got -> mconcat
        [ "Unexpected address verification key type in file ", file
        , ", expected: ", Text.unpack expect, ", got: ", Text.unpack (renderSomeAddressVerificationKey got)
        ]
      ShelleyGenesisCmdTooFewPoolsForBulkCreds pools files perPool -> mconcat
        [ "Number of pools requested for generation (", show pools
        , ") is insufficient to fill ", show files
        , " bulk files, with ", show perPool, " pools per file."
        ]
      ShelleyGenesisCmdAddressCmdError e ->
        Text.unpack $ renderShelleyAddressCmdError e
      ShelleyGenesisCmdNodeCmdError e ->
        Text.unpack $ renderShelleyNodeCmdError e
      ShelleyGenesisCmdStakePoolCmdError e ->
        Text.unpack $ renderStakePoolCmdError e
      ShelleyGenesisCmdStakeAddressCmdError e ->
        displayError e
      ShelleyGenesisCmdCostModelsError fp ->
        "Cost model is invalid: " <> fp
      ShelleyGenesisCmdGenesisFileDecodeError fp e ->
       "Error while decoding Shelley genesis at: " <> fp <>
       " Error: " <>  Text.unpack e
      ShelleyGenesisCmdGenesisFileReadError e -> displayError e
      ShelleyGenesisCmdByronError e -> show e
      ShelleyGenesisStakePoolRelayFileError fp e ->
        "Error occurred while reading the stake pool relay specification file: " <> fp <>
        " Error: " <> show e
      ShelleyGenesisStakePoolRelayJsonDecodeError fp e ->
        "Error occurred while decoding the stake pool relay specification file: " <> fp <>
        " Error: " <>  e
