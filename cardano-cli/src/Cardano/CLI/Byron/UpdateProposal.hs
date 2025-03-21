{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Byron.UpdateProposal
  ( ByronUpdateProposalError (..)
  , runProposalCreation
  , readByronUpdateProposal
  , renderByronUpdateProposalError
  , submitByronUpdateProposal
  )
where

import Cardano.Api
import Cardano.Api.Byron
  ( AsType (AsByronUpdateProposal)
  , ByronProtocolParametersUpdate
  , ByronUpdateProposal
  , makeByronUpdateProposal
  , toByronLedgerUpdateProposal
  )
import Cardano.Api.Byron qualified as Byron
import Cardano.Api.Consensus (condense, txId)

import Cardano.CLI.Byron.Genesis (ByronGenesisError)
import Cardano.CLI.Byron.Key (ByronKeyFailure, readByronSigningKey)
import Cardano.CLI.Byron.Tx (ByronTxError, nodeSubmitTx)
import Cardano.CLI.Helper (HelpersError, ensureNewFileLBS, renderHelpersError)
import Cardano.CLI.Type.Common

import Control.Exception (Exception (..))
import Control.Tracer (stdoutTracer, traceWith)
import Data.Bifunctor (Bifunctor (..))
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text qualified as Text

data ByronUpdateProposalError
  = ByronReadUpdateProposalFileFailure !FilePath !Text
  | ByronUpdateProposalWriteError !HelpersError
  | ByronUpdateProposalGenesisReadError !FilePath !ByronGenesisError
  | ByronUpdateProposalTxError !ByronTxError
  | ReadSigningKeyFailure !FilePath !ByronKeyFailure
  | UpdateProposalDecodingError !FilePath
  deriving Show

renderByronUpdateProposalError :: ByronUpdateProposalError -> Doc ann
renderByronUpdateProposalError = \case
  ByronReadUpdateProposalFileFailure fp rErr ->
    "Error reading update proposal at " <> pshow fp <> " Error: " <> pshow rErr
  ByronUpdateProposalWriteError hErr ->
    "Error writing update proposal: " <> renderHelpersError hErr
  ByronUpdateProposalGenesisReadError fp rErr ->
    "Error reading update proposal at: " <> pshow fp <> " Error: " <> pshow rErr
  ByronUpdateProposalTxError txErr ->
    "Error submitting update proposal: " <> pshow txErr
  ReadSigningKeyFailure fp rErr ->
    "Error reading signing key at: " <> pshow fp <> " Error: " <> pshow rErr
  UpdateProposalDecodingError fp ->
    "Error decoding update proposal at: " <> pshow fp

runProposalCreation
  :: NetworkId
  -> SigningKeyFile In
  -> Byron.ProtocolVersion
  -> Byron.SoftwareVersion
  -> Byron.SystemTag
  -> Byron.InstallerHash
  -> FilePath
  -> ByronProtocolParametersUpdate
  -> ExceptT ByronUpdateProposalError IO ()
runProposalCreation
  nw
  sKey@(File sKeyfp)
  pVer
  sVer
  sysTag
  insHash
  outputFp
  params = do
    sK <- firstExceptT (ReadSigningKeyFailure sKeyfp) $ readByronSigningKey NonLegacyByronKeyFormat sKey
    let proposal = makeByronUpdateProposal nw pVer sVer sysTag insHash sK params
    firstExceptT ByronUpdateProposalWriteError $
      ensureNewFileLBS outputFp $
        serialiseToRawBytes proposal

readByronUpdateProposal :: FilePath -> ExceptT ByronUpdateProposalError IO ByronUpdateProposal
readByronUpdateProposal fp = do
  proposalBs <-
    handleIOExceptT (ByronReadUpdateProposalFileFailure fp . Text.pack . displayException) $
      BS.readFile fp
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
