{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Types.Errors.GenesisCmdError
  ( GenesisCmdError (..)
  )
where

import           Cardano.Api

import           Cardano.CLI.Byron.Genesis as Byron
import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Errors.AddressCmdError
import           Cardano.CLI.Types.Errors.NodeCmdError
import           Cardano.CLI.Types.Errors.StakeAddressCmdError
import           Cardano.CLI.Types.Errors.StakePoolCmdError

import           Control.Exception (IOException)
import           Data.Text (Text)

data GenesisCmdError
  = GenesisCmdAddressCmdError !AddressCmdError
  | GenesisCmdByronError !ByronGenesisError
  | GenesisCmdCostModelsError !FilePath
  | -- | First @Integer@ is the delegate supply, second @Integer@ is the total supply
    GenesisCmdDelegatedSupplyExceedsTotalSupply !Integer !Integer
  | GenesisCmdFileError !(FileError ())
  | GenesisCmdFileDecodeError !FilePath !Text
  | GenesisCmdFilesDupIndex [FilePath]
  | GenesisCmdFilesNoIndex [FilePath]
  | GenesisCmdGenesisFileDecodeError !FilePath !Text
  | GenesisCmdGenesisFileError !(FileError ())
  | GenesisCmdMismatchedGenesisKeyFiles [Int] [Int] [Int]
  | GenesisCmdNodeCmdError !NodeCmdError
  | GenesisCmdStakeAddressCmdError !StakeAddressCmdError
  | GenesisCmdStakePoolCmdError !StakePoolCmdError
  | GenesisCmdStakePoolRelayFileError !FilePath !IOException
  | GenesisCmdStakePoolRelayJsonDecodeError !FilePath !String
  | GenesisCmdTextEnvReadFileError !(FileError TextEnvelopeError)
  | GenesisCmdTooFewPoolsForBulkCreds !Word !Word !Word
  | -- | First @Int@ is the number of SPOs, second @Int@ is number of relays
    GenesisCmdTooManyRelaysError !FilePath !Int !Int
  | GenesisCmdUnexpectedAddressVerificationKey
      !(VerificationKeyFile In)
      !Text
      !SomeAddressVerificationKey
  | -- | @GenesisCmdWrongNodeConfigFile path error@ indicates
    -- that the node configuration at @path@ is badly formed. @error@
    -- gives details about the error
    GenesisCmdWrongNodeConfigFile !FilePath !Text
  | -- | @GenesisCmdWrongGenesisHash path key seen expected@ indicates
    -- that the node configuration at @path@ has the wrong value @seen@ for @key@.
    -- The value should be @expected@ instead.
    GenesisCmdWrongGenesisHash !FilePath !Text !Text !Text
  deriving Show

instance Error GenesisCmdError where
  prettyError = \case
    GenesisCmdGenesisFileError fe ->
      prettyError fe
    GenesisCmdFileError fe ->
      prettyError fe
    GenesisCmdMismatchedGenesisKeyFiles gfiles dfiles vfiles ->
      "Mismatch between the files found:\n"
        <> "Genesis key file indexes:      "
        <> pshow gfiles
        <> "\n"
        <> "Delegate key file indexes:     "
        <> pshow dfiles
        <> "\n"
        <> "Delegate VRF key file indexes: "
        <> pshow vfiles
    GenesisCmdFilesNoIndex files ->
      "The genesis keys files are expected to have a numeric index but these do not:\n"
        <> vsep (fmap pretty files)
    GenesisCmdFilesDupIndex files ->
      "The genesis keys files are expected to have a unique numeric index but these do not:\n"
        <> vsep (fmap pretty files)
    GenesisCmdFileDecodeError path errorTxt ->
      "Cannot decode file:" <+> pretty path <+> "\nError:" <+> pretty errorTxt
    GenesisCmdTextEnvReadFileError fileErr ->
      prettyError fileErr
    GenesisCmdUnexpectedAddressVerificationKey (File file) expect got ->
      mconcat
        [ "Unexpected address verification key type in file "
        , pretty file
        , ", expected: "
        , pretty expect
        , ", got: "
        , pretty (renderSomeAddressVerificationKey got)
        ]
    GenesisCmdTooFewPoolsForBulkCreds pools files perPool ->
      mconcat
        [ "Number of pools requested for generation ("
        , pshow pools
        , ") is insufficient to fill "
        , pshow files
        , " bulk files, with "
        , pshow perPool
        , " pools per file."
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
      "Error while decoding Shelley genesis at: "
        <> pretty fp
        <> " Error: "
        <> pretty e
    GenesisCmdByronError e -> pshow e
    GenesisCmdTooManyRelaysError fp nbSPOs nbRelays ->
      pretty fp
        <> " specifies "
        <> pretty nbRelays
        <> " relays, but only "
        <> pretty nbSPOs
        <> " SPOs have been specified."
        <> " Please specify a number of relays that is lesser or equal to the number of SPOs."
    GenesisCmdStakePoolRelayFileError fp e ->
      "Error occurred while reading the stake pool relay specification file: "
        <> pretty fp
        <> " Error: "
        <> pshow e
    GenesisCmdStakePoolRelayJsonDecodeError fp e ->
      "Error occurred while decoding the stake pool relay specification file: "
        <> pretty fp
        <> " Error: "
        <> pretty e
    GenesisCmdDelegatedSupplyExceedsTotalSupply delegated total ->
      "Provided delegated supply is "
        <> pretty delegated
        <> ", which is greater than the specified total supply: "
        <> pretty total
        <> "."
        <> "This is incorrect: the delegated supply should be less or equal to the total supply."
        <> " Note that the total supply can either come from --total-supply or from the default template. Please fix what you use."
    GenesisCmdWrongNodeConfigFile path err ->
      "Node configuration file at "
        <> pretty path
        <> " is badly formed: "
        <> pretty err
    GenesisCmdWrongGenesisHash path key seen expected ->
      "Hash associated to key \""
        <> pretty key
        <> "\" in file "
        <> pretty path
        <> " is wrong. The value in the file is "
        <> pretty seen
        <> " whereas "
        <> pretty expected
        <> " is expected."
