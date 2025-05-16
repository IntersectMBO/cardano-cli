{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.CLI.Byron.Vote
  ( ByronVoteError (..)
  , readByronVote
  , renderByronVoteError
  , runVoteCreation
  , submitByronVote
  )
where

import Cardano.Api.Byron
import Cardano.Api.Consensus (condense, txId)

import Cardano.Binary qualified as Binary
import Cardano.CLI.Byron.Genesis (ByronGenesisError)
import Cardano.CLI.Byron.Key (ByronKeyFailure, readByronSigningKey)
import Cardano.CLI.Byron.Tx (ByronTxError, nodeSubmitTx)
import Cardano.CLI.Byron.UpdateProposal
  ( ByronUpdateProposalError
  , readByronUpdateProposal
  )
import Cardano.CLI.Compatible.Exception
import Cardano.CLI.Helper (HelpersError, ensureNewFileLBS)
import Cardano.CLI.Type.Common

import Control.Tracer (stdoutTracer, traceWith)
import Data.Bifunctor (first)
import Data.ByteString qualified as BS
import Data.Text (Text)

data ByronVoteError
  = ByronVoteDecodingError !FilePath
  | ByronVoteGenesisReadError !ByronGenesisError
  | ByronVoteKeyReadFailure !ByronKeyFailure
  | ByronVoteReadFileFailure !FilePath !Text
  | ByronVoteTxSubmissionError !ByronTxError
  | ByronVoteUpdateProposalFailure !ByronUpdateProposalError
  | ByronVoteUpdateProposalDecodingError !Binary.DecoderError
  | ByronVoteUpdateHelperError !HelpersError
  deriving Show

instance Error ByronVoteError where
  prettyError = renderByronVoteError

renderByronVoteError :: ByronVoteError -> Doc ann
renderByronVoteError = \case
  ByronVoteDecodingError fp ->
    "Error decoding Byron vote at " <> pretty fp
  ByronVoteGenesisReadError genErr ->
    "Error reading the genesis file:" <> pshow genErr
  ByronVoteReadFileFailure fp err ->
    "Error reading Byron vote at " <> pretty fp <> " Error: " <> pretty err
  ByronVoteTxSubmissionError txErr ->
    "Error submitting the transaction: " <> pshow txErr
  ByronVoteUpdateProposalDecodingError err ->
    "Error decoding Byron update proposal: " <> pshow err
  ByronVoteUpdateProposalFailure err ->
    "Error reading the update proposal: " <> pshow err
  ByronVoteUpdateHelperError err ->
    "Error creating the vote: " <> pshow err
  ByronVoteKeyReadFailure err ->
    "Error reading the signing key: " <> pshow err

runVoteCreation
  :: NetworkId
  -> SigningKeyFile In
  -> FilePath
  -> Bool
  -> FilePath
  -> CIO e ()
runVoteCreation nw sKey upPropFp voteBool outputFp = do
  sK <- fromExceptTCli $ readByronSigningKey NonLegacyByronKeyFormat sKey
  proposal <- readByronUpdateProposal upPropFp
  let vote = makeByronVote nw sK proposal voteBool
  fromExceptTCli . ensureNewFileLBS outputFp $
    serialiseToRawBytes vote

submitByronVote
  :: SocketPath
  -> NetworkId
  -> FilePath
  -> CIO e ()
submitByronVote nodeSocketPath network voteFp = do
  vote <- fromExceptTCli $ readByronVote voteFp
  let genTx = toByronLedgertoByronVote vote
  traceWith stdoutTracer ("Vote TxId: " ++ condense (txId genTx))
  fromExceptTCli $ nodeSubmitTx nodeSocketPath network genTx

readByronVote :: FilePath -> ExceptT ByronVoteError IO ByronVote
readByronVote fp = do
  voteBs <- liftIO $ BS.readFile fp
  let voteResult = deserialiseFromRawBytes AsByronVote voteBs
  hoistEither $ first (const (ByronVoteDecodingError fp)) voteResult
