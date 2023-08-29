{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

{- HLINT ignore "Unused LANGUAGE pragma" -}
{- HLINT ignore "Use let" -}

module Cardano.CLI.Legacy.Run.Transaction
  ( runLegacyTransactionCmds
  ) where

import           Cardano.Api

import           Cardano.CLI.EraBased.Run.Transaction
import           Cardano.CLI.Legacy.Commands.Transaction
import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Errors.ShelleyTxCmdError
import           Cardano.CLI.Types.Governance

import           Control.Monad.Trans.Except

runLegacyTransactionCmds :: LegacyTransactionCmds -> ExceptT ShelleyTxCmdError IO ()
runLegacyTransactionCmds = \case
  TxBuild mNodeSocketPath era consensusModeParams nid mScriptValidity mOverrideWits txins readOnlyRefIns
          reqSigners txinsc mReturnColl mTotCollateral txouts changeAddr mValue mLowBound
          mUpperBound certs wdrls metadataSchema scriptFiles metadataFiles mUpProp mconwayVote
          mNewConstitution outputOptions ->
    runLegacyTxBuildCmd mNodeSocketPath era consensusModeParams nid mScriptValidity mOverrideWits txins readOnlyRefIns
          reqSigners txinsc mReturnColl mTotCollateral txouts changeAddr mValue mLowBound
          mUpperBound certs wdrls metadataSchema scriptFiles metadataFiles mUpProp mconwayVote
          mNewConstitution outputOptions

  TxBuildRaw era mScriptValidity txins readOnlyRefIns txinsc mReturnColl
              mTotColl reqSigners txouts mValue mLowBound mUpperBound fee certs wdrls
              metadataSchema scriptFiles metadataFiles mProtocolParamsFile mUpProp out ->
    runLegacyTxBuildRawCmd era mScriptValidity txins readOnlyRefIns txinsc mReturnColl
              mTotColl reqSigners txouts mValue mLowBound mUpperBound fee certs wdrls
              metadataSchema scriptFiles metadataFiles mProtocolParamsFile mUpProp out

  TxSign txinfile skfiles network txoutfile ->
    runLegacyTxSignCmd txinfile skfiles network txoutfile

  TxSubmit mNodeSocketPath anyConsensusModeParams network txFp ->
    runLegacyTxSubmitCmd mNodeSocketPath anyConsensusModeParams network txFp

  TxCalculateMinFee txbody nw pParamsFile nInputs nOutputs nShelleyKeyWitnesses nByronKeyWitnesses ->
    runLegacyTxCalculateMinFeeCmd txbody nw pParamsFile nInputs nOutputs nShelleyKeyWitnesses nByronKeyWitnesses

  TxCalculateMinRequiredUTxO era pParamsFile txOuts ->
    runLegacyTxCalculateMinRequiredUTxOCmd era pParamsFile txOuts

  TxHashScriptData scriptDataOrFile ->
    runLegacyTxHashScriptDataCmd scriptDataOrFile

  TxGetTxId txinfile ->
    runLegacyTxGetTxIdCmd txinfile

  TxView txinfile ->
    runLegacyTxViewCmd txinfile

  TxMintedPolicyId sFile ->
    runLegacyTxCreatePolicyIdCmd sFile

  TxCreateWitness txBodyfile witSignData mbNw outFile ->
    runLegacyTxCreateWitnessCmd txBodyfile witSignData mbNw outFile

  TxAssembleTxBodyWitness txBodyFile witnessFile outFile ->
    runLegacyTxSignWitnessCmd txBodyFile witnessFile outFile

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
  -> ExceptT ShelleyTxCmdError IO ()
runLegacyTxBuildCmd socketPath (AnyCardanoEra cEra) =
  runTxBuildCmd cEra socketPath

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
  -> ExceptT ShelleyTxCmdError IO ()
runLegacyTxBuildRawCmd (AnyCardanoEra era) =
  runTxBuildRawCmd era

runLegacyTxSignCmd :: ()
  => InputTxBodyOrTxFile
  -> [WitnessSigningData]
  -> Maybe NetworkId
  -> TxFile Out
  -> ExceptT ShelleyTxCmdError IO ()
runLegacyTxSignCmd = runTxSignCmd

runLegacyTxSubmitCmd :: ()
  => SocketPath
  -> AnyConsensusModeParams
  -> NetworkId
  -> FilePath
  -> ExceptT ShelleyTxCmdError IO ()
runLegacyTxSubmitCmd =
  runTxSubmitCmd

runLegacyTxCalculateMinFeeCmd :: ()
  => TxBodyFile In
  -> NetworkId
  -> ProtocolParamsFile
  -> TxInCount
  -> TxOutCount
  -> TxShelleyWitnessCount
  -> TxByronWitnessCount
  -> ExceptT ShelleyTxCmdError IO ()
runLegacyTxCalculateMinFeeCmd =
  runTxCalculateMinFeeCmd

runLegacyTxCalculateMinRequiredUTxOCmd :: ()
  => AnyCardanoEra
  -> ProtocolParamsFile
  -> TxOutAnyEra
  -> ExceptT ShelleyTxCmdError IO ()
runLegacyTxCalculateMinRequiredUTxOCmd (AnyCardanoEra era) =
  runTxCalculateMinRequiredUTxOCmd era

runLegacyTxCreatePolicyIdCmd :: ()
  => ScriptFile
  -> ExceptT ShelleyTxCmdError IO ()
runLegacyTxCreatePolicyIdCmd =
  runTxCreatePolicyIdCmd

runLegacyTxHashScriptDataCmd :: ()
  => ScriptDataOrFile
  -> ExceptT ShelleyTxCmdError IO ()
runLegacyTxHashScriptDataCmd =
  runTxHashScriptDataCmd

runLegacyTxGetTxIdCmd :: ()
  => InputTxBodyOrTxFile
  -> ExceptT ShelleyTxCmdError IO ()
runLegacyTxGetTxIdCmd =
  runTxGetTxIdCmd

runLegacyTxViewCmd :: ()
  => InputTxBodyOrTxFile
  -> ExceptT ShelleyTxCmdError IO ()
runLegacyTxViewCmd =
  runTxViewCmd

runLegacyTxCreateWitnessCmd :: ()
  => TxBodyFile In
  -> WitnessSigningData
  -> Maybe NetworkId
  -> File () Out
  -> ExceptT ShelleyTxCmdError IO ()
runLegacyTxCreateWitnessCmd =
  runTxCreateWitnessCmd

runLegacyTxSignWitnessCmd :: ()
  => TxBodyFile In
  -> [WitnessFile]
  -> File () Out
  -> ExceptT ShelleyTxCmdError IO ()
runLegacyTxSignWitnessCmd =
  runTxSignWitnessCmd
