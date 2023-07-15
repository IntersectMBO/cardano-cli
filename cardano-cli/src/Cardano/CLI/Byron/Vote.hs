{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Cardano.CLI.Byron.Vote
  ( ByronVoteError(..)
  , readByronVote
  , renderByronVoteError
  , runVoteCreation
  , submitByronVote
  ) where

import           Cardano.Api.Byron

import qualified Cardano.Binary as Binary
import           Cardano.CLI.Byron.Genesis (ByronGenesisError)
import           Cardano.CLI.Byron.Key (ByronKeyFailure, readByronSigningKey)
import           Cardano.CLI.Byron.Tx (ByronTxError, nodeSubmitTx)
import           Cardano.CLI.Byron.UpdateProposal (ByronUpdateProposalError,
                   readByronUpdateProposal)
import           Cardano.CLI.Commands.Legacy (ByronKeyFormat (..))
import           Cardano.CLI.Helpers (HelpersError, ensureNewFileLBS)
import           Cardano.CLI.Types
import           Ouroboros.Consensus.Ledger.SupportsMempool (txId)
import           Ouroboros.Consensus.Util.Condense (condense)

import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, hoistEither)
import           Control.Tracer (stdoutTracer, traceWith)
import           Data.Bifunctor (first)
import qualified Data.ByteString as BS
import           Data.Text (Text)
import qualified Data.Text as Text


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

renderByronVoteError :: ByronVoteError -> Text
renderByronVoteError bVerr =
  case bVerr of
    ByronVoteDecodingError fp -> "Error decoding Byron vote at " <>  Text.pack fp
    ByronVoteGenesisReadError genErr -> "Error reading the genesis file:" <> Text.pack (show genErr)
    ByronVoteReadFileFailure fp err -> "Error reading Byron vote at " <> Text.pack fp <> " Error: " <> err
    ByronVoteTxSubmissionError txErr -> "Error submitting the transaction: " <> Text.pack (show txErr)
    ByronVoteUpdateProposalDecodingError err -> "Error decoding Byron update proposal: " <> Text.pack (show err)
    ByronVoteUpdateProposalFailure err -> "Error reading the update proposal: " <> Text.pack (show err)
    ByronVoteUpdateHelperError err ->"Error creating the vote: " <> Text.pack (show err)
    ByronVoteKeyReadFailure err -> "Error reading the signing key: " <> Text.pack (show err)


runVoteCreation
  :: NetworkId
  -> SigningKeyFile In
  -> FilePath
  -> Bool
  -> FilePath
  -> ExceptT ByronVoteError IO ()
runVoteCreation nw sKey upPropFp voteBool outputFp = do
  sK <- firstExceptT ByronVoteKeyReadFailure $ readByronSigningKey NonLegacyByronKeyFormat sKey
  proposal <- firstExceptT ByronVoteUpdateProposalFailure $ readByronUpdateProposal upPropFp
  let vote = makeByronVote nw sK proposal voteBool
  firstExceptT ByronVoteUpdateHelperError . ensureNewFileLBS outputFp
    $ serialiseToRawBytes vote

submitByronVote
  :: SocketPath
  -> NetworkId
  -> FilePath
  -> ExceptT ByronVoteError IO ()
submitByronVote nodeSocketPath network voteFp = do
  vote <- readByronVote voteFp
  let genTx = toByronLedgertoByronVote vote
  traceWith stdoutTracer ("Vote TxId: " ++ condense (txId genTx))
  firstExceptT ByronVoteTxSubmissionError $ nodeSubmitTx nodeSocketPath network genTx

readByronVote :: FilePath -> ExceptT ByronVoteError IO ByronVote
readByronVote fp = do
  voteBs <- liftIO $ BS.readFile fp
  let voteResult = deserialiseFromRawBytes AsByronVote voteBs
  hoistEither $ first (const (ByronVoteDecodingError fp)) voteResult
