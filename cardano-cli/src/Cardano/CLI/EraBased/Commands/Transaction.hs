{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.Commands.Transaction
  ( TransactionCmds (..)
  , TransactionBuildRawCmdArgs(..)
  , TransactionBuildCmdArgs(..)
  , TransactionSignCmdArgs(..)
  , TransactionWitnessCmdArgs(..)
  , TransactionSignWitnessCmdArgs(..)
  , TransactionSubmitCmdArgs(..)
  , TransactionPolicyIdCmdArgs(..)
  , TransactionCalculateMinFeeCmdArgs(..)
  , TransactionCalculateMinValueCmdArgs(..)
  , TransactionHashScriptDataCmdArgs(..)
  , TransactionTxIdCmdArgs(..)
  , TransactionViewCmdArgs(..)

  , renderTransactionCmds
  ) where

import           Cardano.Api.Shelley

import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Governance

import           Data.Text (Text)

data TransactionCmds era
  = TransactionBuildRawCmd            !(TransactionBuildRawCmdArgs era)
  | TransactionBuildCmd               !(TransactionBuildCmdArgs era)
  | TransactionSignCmd                !TransactionSignCmdArgs
  | TransactionWitnessCmd             !TransactionWitnessCmdArgs
  | TransactionSignWitnessCmd         !TransactionSignWitnessCmdArgs
  | TransactionSubmitCmd              !TransactionSubmitCmdArgs
  | TransactionPolicyIdCmd            !TransactionPolicyIdCmdArgs
  | TransactionCalculateMinFeeCmd     !TransactionCalculateMinFeeCmdArgs
  | TransactionCalculateMinValueCmd   !(TransactionCalculateMinValueCmdArgs era)
  | TransactionHashScriptDataCmd      !TransactionHashScriptDataCmdArgs
  | TransactionTxIdCmd                !TransactionTxIdCmdArgs
  | TransactionViewCmd                !TransactionViewCmdArgs

data TransactionBuildRawCmdArgs era = TransactionBuildRawCmdArgs
  (CardanoEra era)
  (Maybe ScriptValidity)
  -- ^ Mark script as expected to pass or fail validation
  [(TxIn, Maybe (ScriptWitnessFiles WitCtxTxIn))]
  -- ^ Transaction inputs with optional spending scripts
  [TxIn]
  -- ^ Read only reference inputs
  [TxIn]
  -- ^ Transaction inputs for collateral, only key witnesses, no scripts.
  (Maybe TxOutAnyEra)
  -- ^ Return collateral
  (Maybe Lovelace)
  -- ^ Total collateral
  [RequiredSigner]
  -- ^ Required signers
  [TxOutAnyEra]
  (Maybe (Value, [ScriptWitnessFiles WitCtxMint]))
  -- ^ Multi-Asset value with script witness
  (Maybe SlotNo)
  -- ^ Transaction validity lower bound
  (Maybe SlotNo)
  -- ^ Transaction validity upper bound
  (Maybe Lovelace)
  -- ^ Transaction fee
  [(CertificateFile, Maybe (ScriptWitnessFiles WitCtxStake))]
  -- ^ Certificates with potential script witness
  [(StakeAddress, Lovelace, Maybe (ScriptWitnessFiles WitCtxStake))]
  TxMetadataJsonSchema
  [ScriptFile]
  -- ^ Auxiliary scripts
  [MetadataFile]
  (Maybe ProtocolParamsFile)
  (Maybe UpdateProposalFile)
  [VoteFile In]
  [ProposalFile In]
  (TxBodyFile Out)

-- | Like 'TransactionBuildRaw' but without the fee, and with a change output.
data TransactionBuildCmdArgs era = TransactionBuildCmdArgs
  (CardanoEra era)
  SocketPath
  AnyConsensusModeParams
  NetworkId
  (Maybe ScriptValidity) -- ^ Mark script as expected to pass or fail validation
  (Maybe Word)
  -- ^ Override the required number of tx witnesses
  [(TxIn, Maybe (ScriptWitnessFiles WitCtxTxIn))]
  -- ^ Transaction inputs with optional spending scripts
  [TxIn]
  -- ^ Read only reference inputs
  [RequiredSigner]
  -- ^ Required signers
  [TxIn]
  -- ^ Transaction inputs for collateral, only key witnesses, no scripts.
  (Maybe TxOutAnyEra)
  -- ^ Return collateral
  (Maybe Lovelace)
  -- ^ Total collateral
  [TxOutAnyEra]
  -- ^ Normal outputs
  TxOutChangeAddress
  -- ^ A change output
  (Maybe (Value, [ScriptWitnessFiles WitCtxMint]))
  -- ^ Multi-Asset value with script witness
  (Maybe SlotNo)
  -- ^ Transaction validity lower bound
  (Maybe SlotNo)
  -- ^ Transaction validity upper bound
  [(CertificateFile, Maybe (ScriptWitnessFiles WitCtxStake))]
  -- ^ Certificates with potential script witness
  [(StakeAddress, Lovelace, Maybe (ScriptWitnessFiles WitCtxStake))]
  -- ^ Withdrawals with potential script witness
  TxMetadataJsonSchema
  [ScriptFile]
  -- ^ Auxiliary scripts
  [MetadataFile]
  (Maybe UpdateProposalFile)
  [VoteFile In]
  [ProposalFile In]
  TxBuildOutputOptions

data TransactionSignCmdArgs = TransactionSignCmdArgs
  InputTxBodyOrTxFile
  [WitnessSigningData]
  (Maybe NetworkId)
  (TxFile Out)

data TransactionWitnessCmdArgs = TransactionWitnessCmdArgs
  (TxBodyFile In)
  WitnessSigningData
  (Maybe NetworkId)
  (File () Out)

data TransactionSignWitnessCmdArgs = TransactionSignWitnessCmdArgs
  (TxBodyFile In)
  [WitnessFile]
  (File () Out)

data TransactionSubmitCmdArgs = TransactionSubmitCmdArgs
  SocketPath
  AnyConsensusModeParams
  NetworkId
  FilePath

newtype TransactionPolicyIdCmdArgs = TransactionPolicyIdCmdArgs
  ScriptFile

data TransactionCalculateMinFeeCmdArgs = TransactionCalculateMinFeeCmdArgs
  (TxBodyFile In)
  NetworkId
  ProtocolParamsFile
  TxInCount
  TxOutCount
  TxShelleyWitnessCount
  TxByronWitnessCount

data TransactionCalculateMinValueCmdArgs era = TransactionCalculateMinValueCmdArgs
  (CardanoEra era)
  ProtocolParamsFile
  TxOutAnyEra

newtype TransactionHashScriptDataCmdArgs = TransactionHashScriptDataCmdArgs
  ScriptDataOrFile

newtype TransactionTxIdCmdArgs = TransactionTxIdCmdArgs
  InputTxBodyOrTxFile

data TransactionViewCmdArgs = TransactionViewCmdArgs
  TxViewOutputFormat
  (Maybe (File () Out))
  InputTxBodyOrTxFile

renderTransactionCmds :: TransactionCmds era -> Text
renderTransactionCmds = \case
  TransactionBuildCmd                     {} -> "transaction build"
  TransactionBuildRawCmd                  {} -> "transaction build-raw"
  TransactionSignCmd                      {} -> "transaction sign"
  TransactionWitnessCmd                   {} -> "transaction witness"
  TransactionSignWitnessCmd               {} -> "transaction sign-witness"
  TransactionSubmitCmd                    {} -> "transaction submit"
  TransactionPolicyIdCmd                  {} -> "transaction policyid"
  TransactionCalculateMinFeeCmd           {} -> "transaction calculate-min-fee"
  TransactionCalculateMinValueCmd         {} -> "transaction calculate-min-value"
  TransactionHashScriptDataCmd            {} -> "transaction hash-script-data"
  TransactionTxIdCmd                      {} -> "transaction txid"
  TransactionViewCmd                      {} -> "transaction view"
