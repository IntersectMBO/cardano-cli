{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Cardano.CLI.Legacy.Run.Transaction
  ( runLegacyTransactionCmds
  , readFileTx
  , toTxOutInAnyEra
  ) where

import           Cardano.Api hiding (txOuts)

import           Cardano.CLI.EraBased.Run.Transaction
import           Cardano.CLI.Legacy.Commands.Transaction
import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Errors.TxCmdError
import           Cardano.CLI.Types.Governance

import           Control.Monad.Trans.Except

{- HLINT ignore "Use let" -}

runLegacyTransactionCmds :: LegacyTransactionCmds -> ExceptT TxCmdError IO ()
runLegacyTransactionCmds = \case
  TxBuild
      mNodeSocketPath era consensusModeParams nid mScriptValidity mOverrideWits txins readOnlyRefIns
      reqSigners txinsc mReturnColl mTotCollateral txouts changeAddr mValue mLowBound
      mUpperBound certs wdrls metadataSchema scriptFiles metadataFiles mUpProp mconwayVote
      mNewConstitution outputOptions -> do
    runLegacyTxBuildCmd
        mNodeSocketPath era consensusModeParams nid mScriptValidity mOverrideWits txins readOnlyRefIns
        reqSigners txinsc mReturnColl mTotCollateral txouts changeAddr mValue mLowBound
        mUpperBound certs wdrls metadataSchema scriptFiles metadataFiles mUpProp mconwayVote
        mNewConstitution outputOptions

  TxBuildRaw
      era mScriptValidity txins readOnlyRefIns txinsc mReturnColl
      mTotColl reqSigners txouts mValue mLowBound mUpperBound fee certs wdrls
      metadataSchema scriptFiles metadataFiles mProtocolParamsFile mUpProp out -> do
    runLegacyTxBuildRawCmd
        era mScriptValidity txins readOnlyRefIns txinsc mReturnColl
        mTotColl reqSigners txouts mValue mLowBound mUpperBound fee certs wdrls
        metadataSchema scriptFiles metadataFiles mProtocolParamsFile mUpProp out

  TxSign txinfile skfiles network txoutfile ->
    runLegacyTxSign txinfile skfiles network txoutfile

  TxSubmit mNodeSocketPath anyConsensusModeParams network txFp ->
    runLegacyTxSubmit mNodeSocketPath anyConsensusModeParams network txFp

  TxCalculateMinFee txbody nw pParamsFile nInputs nOutputs nShelleyKeyWitnesses nByronKeyWitnesses ->
    runLegacyTxCalculateMinFee txbody nw pParamsFile nInputs nOutputs nShelleyKeyWitnesses nByronKeyWitnesses

  TxCalculateMinRequiredUTxO era pParamsFile txOuts ->
    runLegacyTxCalculateMinRequiredUTxO era pParamsFile txOuts

  TxHashScriptData scriptDataOrFile ->
    runLegacyTxHashScriptData scriptDataOrFile

  TxGetTxId txinfile ->
    runLegacyTxGetTxId txinfile

  TxView txinfile ->
    runLegacyTxView txinfile

  TxMintedPolicyId sFile ->
    runLegacyTxCreatePolicyId sFile

  TxCreateWitness txBodyfile witSignData mbNw outFile ->
    runLegacyTxCreateWitness txBodyfile witSignData mbNw outFile

  TxAssembleTxBodyWitness txBodyFile witnessFile outFile ->
    runLegacyTxSignWitness txBodyFile witnessFile outFile

-- ----------------------------------------------------------------------------
-- Building transactions
--

runLegacyTxBuildCmd :: ()
  => SocketPath
  -> AnyCardanoEra
  -> AnyConsensusModeParams
  -> NetworkId
  -> Maybe ScriptValidity
  -> Maybe Word -- ^ Override the required number of tx witnesses
  -> [(TxIn, Maybe (ScriptWitnessFiles WitCtxTxIn))] -- ^ Transaction inputs with optional spending scripts
  -> [TxIn] -- ^ Read only reference inputs
  -> [RequiredSigner] -- ^ Required signers
  -> [TxIn] -- ^ Transaction inputs for collateral, only key witnesses, no scripts.
  -> Maybe TxOutAnyEra -- ^ Return collateral
  -> Maybe Lovelace -- ^ Total collateral
  -> [TxOutAnyEra]
  -> TxOutChangeAddress
  -> Maybe (Value, [ScriptWitnessFiles WitCtxMint])
  -> Maybe SlotNo -- ^ Validity lower bound
  -> Maybe SlotNo -- ^ Validity upper bound
  -> [(CertificateFile, Maybe (ScriptWitnessFiles WitCtxStake))]
  -> [(StakeAddress, Lovelace, Maybe (ScriptWitnessFiles WitCtxStake))] -- ^ Withdrawals with potential script witness
  -> TxMetadataJsonSchema
  -> [ScriptFile]
  -> [MetadataFile]
  -> Maybe UpdateProposalFile
  -> [VoteFile In]
  -> [NewConstitutionFile In] -- TODO: Conway era - we should replace this with a sumtype that handles all governance actions
  -> TxBuildOutputOptions
  -> ExceptT TxCmdError IO ()
runLegacyTxBuildCmd = runTxBuildCmd

runLegacyTxBuildRawCmd :: ()
  => AnyCardanoEra
  -> Maybe ScriptValidity
  -> [(TxIn, Maybe (ScriptWitnessFiles WitCtxTxIn))]
  -> [TxIn] -- ^ Read only reference inputs
  -> [TxIn] -- ^ Transaction inputs for collateral, only key witnesses, no scripts.
  -> Maybe TxOutAnyEra
  -> Maybe Lovelace -- ^ Total collateral
  -> [RequiredSigner]
  -> [TxOutAnyEra]
  -> Maybe (Value, [ScriptWitnessFiles WitCtxMint]) -- ^ Multi-Asset value with script witness
  -> Maybe SlotNo -- ^ Validity lower bound
  -> Maybe SlotNo -- ^ Validity upper bound
  -> Maybe Lovelace -- ^ Tx fee
  -> [(CertificateFile, Maybe (ScriptWitnessFiles WitCtxStake))]
  -> [(StakeAddress, Lovelace, Maybe (ScriptWitnessFiles WitCtxStake))]
  -> TxMetadataJsonSchema
  -> [ScriptFile]
  -> [MetadataFile]
  -> Maybe ProtocolParamsFile
  -> Maybe UpdateProposalFile
  -> TxBodyFile Out
  -> ExceptT TxCmdError IO ()
runLegacyTxBuildRawCmd = runTxBuildRawCmd

runLegacyTxSign :: ()
  => InputTxBodyOrTxFile
  -> [WitnessSigningData]
  -> Maybe NetworkId
  -> TxFile Out
  -> ExceptT TxCmdError IO ()
runLegacyTxSign = runTxSign

runLegacyTxSubmit
  :: SocketPath
  -> AnyConsensusModeParams
  -> NetworkId
  -> FilePath
  -> ExceptT TxCmdError IO ()
runLegacyTxSubmit = runTxSubmit

runLegacyTxCalculateMinFee :: ()
  => TxBodyFile In
  -> NetworkId
  -> ProtocolParamsFile
  -> TxInCount
  -> TxOutCount
  -> TxShelleyWitnessCount
  -> TxByronWitnessCount
  -> ExceptT TxCmdError IO ()
runLegacyTxCalculateMinFee = runTxCalculateMinFee

runLegacyTxCalculateMinRequiredUTxO :: ()
  => AnyCardanoEra
  -> ProtocolParamsFile
  -> TxOutAnyEra
  -> ExceptT TxCmdError IO ()
runLegacyTxCalculateMinRequiredUTxO = runTxCalculateMinRequiredUTxO

runLegacyTxCreatePolicyId :: ()
  => ScriptFile
  -> ExceptT TxCmdError IO ()
runLegacyTxCreatePolicyId = runTxCreatePolicyId

runLegacyTxHashScriptData :: ()
  => ScriptDataOrFile
  -> ExceptT TxCmdError IO ()
runLegacyTxHashScriptData = runTxHashScriptData

runLegacyTxGetTxId :: ()
  => InputTxBodyOrTxFile
  -> ExceptT TxCmdError IO ()
runLegacyTxGetTxId = runTxGetTxId

runLegacyTxView :: ()
  => InputTxBodyOrTxFile
  -> ExceptT TxCmdError IO ()
runLegacyTxView = runTxView

runLegacyTxCreateWitness :: ()
  => TxBodyFile In
  -> WitnessSigningData
  -> Maybe NetworkId
  -> File () Out
  -> ExceptT TxCmdError IO ()
runLegacyTxCreateWitness = runTxCreateWitness

runLegacyTxSignWitness :: ()
  => TxBodyFile In
  -> [WitnessFile]
  -> File () Out
  -> ExceptT TxCmdError IO ()
runLegacyTxSignWitness = runTxSignWitness
