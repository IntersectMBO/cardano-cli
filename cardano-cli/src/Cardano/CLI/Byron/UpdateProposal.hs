{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Cardano.CLI.Byron.UpdateProposal
  ( ByronUpdateProposalError(..)
  , runProposalCreation
  , readByronUpdateProposal
  , renderByronUpdateProposalError
  , submitByronUpdateProposal
  ) where

import           Cardano.Api (NetworkId, SerialiseAsRawBytes (..), SocketPath, textShow)
import           Cardano.Api.Byron (AsType (AsByronUpdateProposal), ByronProtocolParametersUpdate,
                   ByronUpdateProposal, makeByronUpdateProposal, toByronLedgerUpdateProposal)

import           Cardano.Chain.Update (InstallerHash (..), ProtocolVersion (..),
                   SoftwareVersion (..), SystemTag (..))
import           Cardano.CLI.Byron.Genesis (ByronGenesisError)
import           Cardano.CLI.Byron.Key (ByronKeyFailure, readByronSigningKey)
import           Cardano.CLI.Byron.Tx (ByronTxError, nodeSubmitTx)
import           Cardano.CLI.Helpers (HelpersError, ensureNewFileLBS, renderHelpersError)
import           Cardano.CLI.Types.Common
import           Ouroboros.Consensus.Ledger.SupportsMempool (txId)
import           Ouroboros.Consensus.Util.Condense (condense)

import           Control.Exception (Exception (..))
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT, hoistEither)
import           Control.Tracer (stdoutTracer, traceWith)
import           Data.Bifunctor (Bifunctor (..))
import qualified Data.ByteString as BS
import           Data.Text (Text)
import qualified Data.Text as Text

data ByronUpdateProposalError
  = ByronReadUpdateProposalFileFailure !FilePath !Text
  | ByronUpdateProposalWriteError !HelpersError
  | ByronUpdateProposalGenesisReadError !FilePath !ByronGenesisError
  | ByronUpdateProposalTxError !ByronTxError
  | ReadSigningKeyFailure !FilePath !ByronKeyFailure
  | UpdateProposalDecodingError !FilePath
  deriving Show

renderByronUpdateProposalError :: ByronUpdateProposalError -> Text
renderByronUpdateProposalError err =
  case err of
    ByronReadUpdateProposalFileFailure fp rErr ->
      "Error reading update proposal at " <> textShow fp <> " Error: " <> textShow rErr
    ByronUpdateProposalWriteError hErr ->
      "Error writing update proposal: " <> renderHelpersError hErr
    ByronUpdateProposalGenesisReadError fp rErr ->
      "Error reading update proposal at: " <> textShow fp <> " Error: " <> textShow rErr
    ByronUpdateProposalTxError txErr ->
      "Error submitting update proposal: " <> textShow txErr
    ReadSigningKeyFailure fp rErr ->
      "Error reading signing key at: " <> textShow fp <> " Error: " <> textShow rErr
    UpdateProposalDecodingError fp ->
      "Error decoding update proposal at: " <> textShow fp

runProposalCreation
  :: NetworkId
  -> SigningKeyFile In
  -> ProtocolVersion
  -> SoftwareVersion
  -> SystemTag
  -> InstallerHash
  -> FilePath
  -> ByronProtocolParametersUpdate
  -> ExceptT ByronUpdateProposalError IO ()
runProposalCreation nw sKey@(File sKeyfp) pVer sVer
                    sysTag insHash outputFp params = do
  sK <- firstExceptT (ReadSigningKeyFailure sKeyfp) $ readByronSigningKey NonLegacyByronKeyFormat sKey
  let proposal = makeByronUpdateProposal nw pVer sVer sysTag insHash sK params
  firstExceptT ByronUpdateProposalWriteError $
    ensureNewFileLBS outputFp $ serialiseToRawBytes proposal

readByronUpdateProposal :: FilePath -> ExceptT ByronUpdateProposalError IO ByronUpdateProposal
readByronUpdateProposal fp = do
  proposalBs <- handleIOExceptT (ByronReadUpdateProposalFileFailure fp . Text.pack . displayException)
                  $ BS.readFile fp
  let proposalResult = deserialiseFromRawBytes AsByronUpdateProposal proposalBs
  hoistEither $ first (const (UpdateProposalDecodingError fp)) proposalResult

submitByronUpdateProposal
  :: SocketPath
  -> NetworkId
  -> FilePath
  -> ExceptT ByronUpdateProposalError IO ()
submitByronUpdateProposal nodeSocketPath network proposalFp = do
  proposal <- readByronUpdateProposal proposalFp
  let genTx = toByronLedgerUpdateProposal proposal
  traceWith stdoutTracer $ "Update proposal TxId: " ++ condense (txId genTx)
  firstExceptT ByronUpdateProposalTxError $ nodeSubmitTx nodeSocketPath network genTx
