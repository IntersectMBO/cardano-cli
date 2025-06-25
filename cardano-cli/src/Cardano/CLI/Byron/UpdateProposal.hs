{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

{- HLINT ignore "Use newtype instead of data" -}

module Cardano.CLI.Byron.UpdateProposal
  ( runProposalCreation
  , readByronUpdateProposal
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

import Cardano.CLI.Byron.Key (readByronSigningKey)
import Cardano.CLI.Byron.Tx (nodeSubmitTx)
import Cardano.CLI.Compatible.Exception
import Cardano.CLI.Helper (ensureNewFileLBS)
import Cardano.CLI.Orphan ()
import Cardano.CLI.Read
import Cardano.CLI.Type.Common

import Control.Tracer (stdoutTracer, traceWith)

runProposalCreation
  :: NetworkId
  -> SigningKeyFile In
  -> Byron.ProtocolVersion
  -> Byron.SoftwareVersion
  -> Byron.SystemTag
  -> Byron.InstallerHash
  -> FilePath
  -> ByronProtocolParametersUpdate
  -> CIO e ()
runProposalCreation
  nw
  sKey
  pVer
  sVer
  sysTag
  insHash
  outputFp
  params = do
    sK <- fromExceptTCli $ readByronSigningKey NonLegacyByronKeyFormat sKey
    let proposal = makeByronUpdateProposal nw pVer sVer sysTag insHash sK params
    fromExceptTCli $
      ensureNewFileLBS outputFp $
        serialiseToRawBytes proposal

readByronUpdateProposal :: FilePath -> CIO e ByronUpdateProposal
readByronUpdateProposal fp = do
  proposalBs <-
    readFileCli fp
  let proposalResult = deserialiseFromRawBytes AsByronUpdateProposal proposalBs
  fromEitherCli proposalResult

submitByronUpdateProposal
  :: SocketPath
  -> NetworkId
  -> FilePath
  -> CIO e ()
submitByronUpdateProposal nodeSocketPath network proposalFp = do
  proposal <- readByronUpdateProposal proposalFp
  let genTx = toByronLedgerUpdateProposal proposal
  traceWith stdoutTracer $ "Update proposal TxId: " ++ condense (txId genTx)
  fromExceptTCli $ nodeSubmitTx nodeSocketPath network genTx
