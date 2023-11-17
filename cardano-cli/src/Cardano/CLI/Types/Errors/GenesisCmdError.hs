{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Types.Errors.GenesisCmdError
  ( GenesisCmdError(..)
  ) where

import           Cardano.Api
import           Cardano.Api.Pretty

import           Cardano.CLI.Byron.Genesis as Byron
import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Errors.AddressCmdError
import           Cardano.CLI.Types.Errors.NodeCmdError
import           Cardano.CLI.Types.Errors.StakeAddressCmdError
import           Cardano.CLI.Types.Errors.StakePoolCmdError

import           Control.Exception (IOException)
import           Data.Text (Text)
import           Prettyprinter

data GenesisCmdError
  = GenesisCmdAesonDecodeError !FilePath !Text
  | GenesisCmdGenesisFileReadError !(FileError IOException)
  | GenesisCmdGenesisFileDecodeError !FilePath !Text
  | GenesisCmdGenesisFileError !(FileError ())
  | GenesisCmdFileError !(FileError ())
  | GenesisCmdMismatchedGenesisKeyFiles [Int] [Int] [Int]
  | GenesisCmdFilesNoIndex [FilePath]
  | GenesisCmdFilesDupIndex [FilePath]
  | GenesisCmdTextEnvReadFileError !(FileError TextEnvelopeError)
  | GenesisCmdUnexpectedAddressVerificationKey !(VerificationKeyFile In) !Text !SomeAddressVerificationKey
  | GenesisCmdTooFewPoolsForBulkCreds !Word !Word !Word
  | GenesisCmdAddressCmdError !AddressCmdError
  | GenesisCmdNodeCmdError !NodeCmdError
  | GenesisCmdStakeAddressCmdError !StakeAddressCmdError
  | GenesisCmdStakePoolCmdError !StakePoolCmdError
  | GenesisCmdCostModelsError !FilePath
  | GenesisCmdByronError !ByronGenesisError
  | GenesisCmdStakePoolRelayFileError !FilePath !IOException
  | GenesisCmdStakePoolRelayJsonDecodeError !FilePath !String
  deriving Show

instance Error GenesisCmdError where
  prettyError = \case
    GenesisCmdAesonDecodeError fp decErr ->
      "Error while decoding Shelley genesis at: " <> pretty fp <> " Error: " <> pretty decErr
    GenesisCmdGenesisFileError fe ->
      prettyError fe
    GenesisCmdFileError fe ->
      prettyError fe
    GenesisCmdMismatchedGenesisKeyFiles gfiles dfiles vfiles ->
      "Mismatch between the files found:\n"
        <> "Genesis key file indexes:      " <> pshow gfiles <> "\n"
        <> "Delegate key file indexes:     " <> pshow dfiles <> "\n"
        <> "Delegate VRF key file indexes: " <> pshow vfiles
    GenesisCmdFilesNoIndex files ->
      "The genesis keys files are expected to have a numeric index but these do not:\n"
        <> vsep (fmap pretty files)
    GenesisCmdFilesDupIndex files ->
      "The genesis keys files are expected to have a unique numeric index but these do not:\n"
        <> vsep (fmap pretty files)
    GenesisCmdTextEnvReadFileError fileErr ->
      prettyError fileErr
    GenesisCmdUnexpectedAddressVerificationKey (File file) expect got ->
      mconcat
        [ "Unexpected address verification key type in file ", pretty file
        , ", expected: ", pretty expect, ", got: ", pretty (renderSomeAddressVerificationKey got)
        ]
    GenesisCmdTooFewPoolsForBulkCreds pools files perPool ->
      mconcat
        [ "Number of pools requested for generation (", pshow pools
        , ") is insufficient to fill ", pshow files
        , " bulk files, with ", pshow perPool, " pools per file."
        ]
    GenesisCmdAddressCmdError e ->
      renderAddressCmdError e
    GenesisCmdNodeCmdError e ->
      renderNodeCmdError e
    GenesisCmdStakePoolCmdError e ->
      renderStakePoolCmdError e
    GenesisCmdStakeAddressCmdError e ->
      prettyError e
    GenesisCmdCostModelsError fp ->
      "Cost model is invalid: " <> pretty fp
    GenesisCmdGenesisFileDecodeError fp e ->
      "Error while decoding Shelley genesis at: " <> pretty fp <>
      " Error: " <>  pretty e
    GenesisCmdGenesisFileReadError e ->
      prettyError e
    GenesisCmdByronError e -> pshow e
    GenesisCmdStakePoolRelayFileError fp e ->
      "Error occurred while reading the stake pool relay specification file: " <> pretty fp <>
      " Error: " <> pshow e
    GenesisCmdStakePoolRelayJsonDecodeError fp e ->
      "Error occurred while decoding the stake pool relay specification file: " <> pretty fp <>
      " Error: " <> pretty e
