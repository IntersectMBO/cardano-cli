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

import Cardano.Api
  ( Doc
  , SocketPath
  , deserialiseFromRawBytes
  , liftIO
  , pretty
  , serialiseToRawBytes
  )
import Cardano.Api.Byron
import Cardano.Api.Consensus (condense, txId)

import Cardano.CLI.Byron.Key (readByronSigningKey)
import Cardano.CLI.Byron.Tx (nodeSubmitTx)
import Cardano.CLI.Byron.UpdateProposal
  ( readByronUpdateProposal
  )
import Cardano.CLI.Compatible.Exception
import Cardano.CLI.Helper (ensureNewFileLBS)
import Cardano.CLI.Type.Common

import Control.Tracer (stdoutTracer, traceWith)
import Data.ByteString qualified as BS

data ByronVoteError
  = ByronVoteDecodingError !FilePath
  deriving Show

instance Error ByronVoteError where
  prettyError = renderByronVoteError

renderByronVoteError :: ByronVoteError -> Doc ann
renderByronVoteError = \case
  ByronVoteDecodingError fp ->
    "Error decoding Byron vote at " <> pretty fp

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
  vote <- readByronVote voteFp
  let genTx = toByronLedgertoByronVote vote
  traceWith stdoutTracer ("Vote TxId: " ++ condense (txId genTx))
  fromExceptTCli $ nodeSubmitTx nodeSocketPath network genTx

readByronVote :: FilePath -> CIO e ByronVote
readByronVote fp = do
  voteBs <- liftIO $ BS.readFile fp
  let voteResult = deserialiseFromRawBytes AsByronVote voteBs
  fromEitherCli voteResult
