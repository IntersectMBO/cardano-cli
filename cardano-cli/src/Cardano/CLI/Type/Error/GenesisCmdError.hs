{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Type.Error.GenesisCmdError
  ( GenesisCmdError (..)
  )
where

import Cardano.Api

import Cardano.CLI.Byron.Genesis as Byron
import Cardano.CLI.Type.Error.NodeCmdError
import Cardano.CLI.Type.Error.StakePoolCmdError

import Control.Exception
import Data.Typeable

data GenesisCmdError
  = GenesisCmdByronError !ByronGenesisError
  | -- | First @Integer@ is the delegate supply, second @Integer@ is the total supply
    GenesisCmdDelegatedSupplyExceedsTotalSupply !Integer !Integer
  | GenesisCmdFileError !(FileError ())
  | GenesisCmdFileDecodeError !FilePath !Text
  | GenesisCmdFilesDupIndex [FilePath]
  | GenesisCmdFilesNoIndex [FilePath]
  | GenesisCmdGenesisFileDecodeError !TypeRep !FilePath !Text
  | GenesisCmdGenesisFileError !(FileError ())
  | GenesisCmdMismatchedGenesisKeyFiles [Int] [Int] [Int]
  | GenesisCmdNodeCmdError !NodeCmdError
  | GenesisCmdStakePoolCmdError !StakePoolCmdError
  | GenesisCmdStakePoolRelayFileError !FilePath !IOException
  | GenesisCmdStakePoolRelayJsonDecodeError !FilePath !String
  | GenesisCmdTextEnvReadFileError !(FileError TextEnvelopeError)
  | GenesisCmdTooFewPoolsForBulkCreds !Word !Word !Word
  | -- | First @Int@ is the number of SPOs, second @Int@ is number of relays
    GenesisCmdTooManyRelaysError !FilePath !Int !Int
  deriving Show

instance Exception GenesisCmdError where
  displayException = displayError

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
    GenesisCmdNodeCmdError e ->
      renderNodeCmdError e
    GenesisCmdStakePoolCmdError e ->
      prettyError e
    GenesisCmdGenesisFileDecodeError ty fp e ->
      "Error while decoding "
        <> pretty ty
        <> " at: "
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
