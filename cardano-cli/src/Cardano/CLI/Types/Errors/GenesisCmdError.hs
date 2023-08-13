{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Types.Errors.GenesisCmdError
  ( GenesisCmdError(..)
  ) where

import           Cardano.Api

import           Cardano.CLI.Byron.Genesis
import           Cardano.CLI.Orphans ()
import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Errors.AddressCmdError
import           Cardano.CLI.Types.Errors.NodeCmdError
import           Cardano.CLI.Types.Errors.PoolCmdError
import           Cardano.CLI.Types.Errors.StakeAddressCmdError

import           Control.Exception (IOException)
import           Data.Text (Text)
import qualified Data.Text as Text

data GenesisCmdError
  = GenesisCmdAesonDecodeError
      !FilePath !Text
  | GenesisCmdGenesisFileReadError
      !(FileError IOException)
  | GenesisCmdGenesisFileDecodeError
      !FilePath !Text
  | GenesisCmdGenesisFileError
      !(FileError ())
  | GenesisCmdFileError
      !(FileError ())
  | GenesisCmdMismatchedGenesisKeyFilesError
      [Int] [Int] [Int]
  | GenesisCmdFilesNoIndexError
      [FilePath]
  | GenesisCmdFilesDupIndexError
      [FilePath]
  | GenesisCmdTextEnvReadFileError
      !(FileError TextEnvelopeError)
  | GenesisCmdUnexpectedAddressVerificationKeyError
      !(VerificationKeyFile In) !Text !SomeAddressVerificationKey
  | GenesisCmdTooFewPoolsForBulkCredsError
      !Word !Word !Word
  | GenesisCmdAddressCmdError
      !AddressCmdError
  | GenesisCmdNodeCmdError
      !NodeCmdError
  | GenesisCmdPoolCmdError
      !PoolCmdError
  | GenesisCmdStakeAddressCmdError
      !StakeAddressCmdError
  | GenesisCmdCostModelsError
      !FilePath
  | GenesisCmdByronError
      !ByronGenesisError
  | GenesisStakePoolRelayFileError
      !FilePath !IOException
  | GenesisStakePoolRelayJsonDecodeError
      !FilePath !String
  deriving Show

instance Error GenesisCmdError where
  displayError = \case
    GenesisCmdAesonDecodeError fp decErr ->
      "Error while decoding Shelley genesis at: " <> fp <> " Error: " <> Text.unpack decErr
    GenesisCmdGenesisFileError fe -> displayError fe
    GenesisCmdFileError fe -> displayError fe
    GenesisCmdMismatchedGenesisKeyFilesError gfiles dfiles vfiles ->
      "Mismatch between the files found:\n"
        <> "Genesis key file indexes:      " <> show gfiles <> "\n"
        <> "Delegate key file indexes:     " <> show dfiles <> "\n"
        <> "Delegate VRF key file indexes: " <> show vfiles
    GenesisCmdFilesNoIndexError files ->
      "The genesis keys files are expected to have a numeric index but these do not:\n"
        <> unlines files
    GenesisCmdFilesDupIndexError files ->
      "The genesis keys files are expected to have a unique numeric index but these do not:\n"
        <> unlines files
    GenesisCmdTextEnvReadFileError fileErr ->
      displayError fileErr
    GenesisCmdUnexpectedAddressVerificationKeyError (File file) expect got ->
      mconcat
      [ "Unexpected address verification key type in file ", file
      , ", expected: ", Text.unpack expect, ", got: ", Text.unpack (renderSomeAddressVerificationKey got)
      ]
    GenesisCmdTooFewPoolsForBulkCredsError pools files perPool ->
      mconcat
      [ "Number of pools requested for generation (", show pools
      , ") is insufficient to fill ", show files
      , " bulk files, with ", show perPool, " pools per file."
      ]
    GenesisCmdAddressCmdError e ->
      Text.unpack $ renderAddressCmdError e
    GenesisCmdNodeCmdError e ->
      Text.unpack $ renderNodeCmdError e
    GenesisCmdPoolCmdError e ->
      Text.unpack $ renderPoolCmdError e
    GenesisCmdStakeAddressCmdError e ->
      Text.unpack $ renderStakeAddressCmdError e
    GenesisCmdCostModelsError fp ->
      "Cost model is invalid: " <> fp
    GenesisCmdGenesisFileDecodeError fp e ->
      "Error while decoding Shelley genesis at: " <> fp <>
      " Error: " <>  Text.unpack e
    GenesisCmdGenesisFileReadError e ->
      displayError e
    GenesisCmdByronError e -> show e
    GenesisStakePoolRelayFileError fp e ->
      "Error occurred while reading the stake pool relay specification file: " <> fp <>
      " Error: " <> show e
    GenesisStakePoolRelayJsonDecodeError fp e ->
      "Error occurred while decoding the stake pool relay specification file: " <> fp <>
      " Error: " <>  e
