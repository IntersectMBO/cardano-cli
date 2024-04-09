{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.Commands.Transaction
  ( TransactionCmds (..)
  , TransactionBuildRawCmdArgs (..)
  , TransactionBuildCmdArgs (..)
  , TransactionBuildEstimateCmdArgs (..)
  , TransactionEchoCmdArgs (..)
  , TransactionSignCmdArgs (..)
  , TransactionWitnessCmdArgs (..)
  , TransactionSignWitnessCmdArgs (..)
  , TransactionSubmitCmdArgs (..)
  , TransactionPolicyIdCmdArgs (..)
  , TransactionCalculateMinFeeCmdArgs (..)
  , TransactionCalculateMinValueCmdArgs (..)
  , TransactionHashScriptDataCmdArgs (..)
  , TransactionTxIdCmdArgs (..)
  , TransactionViewCmdArgs (..)
  , renderTransactionCmds
  )
where

import           Cardano.Api.Ledger (Coin)
import           Cardano.Api.Shelley

import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Governance

import           Data.Text (Text)

data TransactionCmds era
  = TransactionBuildRawCmd !(TransactionBuildRawCmdArgs era)
  | TransactionBuildCmd !(TransactionBuildCmdArgs era)
  | TransactionBuildEstimateCmd !(TransactionBuildEstimateCmdArgs era)
  | TransactionEchoCmd !TransactionEchoCmdArgs
  | TransactionSignCmd !TransactionSignCmdArgs
  | TransactionWitnessCmd !TransactionWitnessCmdArgs
  | TransactionSignWitnessCmd !TransactionSignWitnessCmdArgs
  | TransactionSubmitCmd !TransactionSubmitCmdArgs
  | TransactionPolicyIdCmd !TransactionPolicyIdCmdArgs
  | TransactionCalculateMinFeeCmd !TransactionCalculateMinFeeCmdArgs
  | TransactionCalculateMinValueCmd !(TransactionCalculateMinValueCmdArgs era)
  | TransactionHashScriptDataCmd !TransactionHashScriptDataCmdArgs
  | TransactionTxIdCmd !TransactionTxIdCmdArgs
  | TransactionViewCmd !TransactionViewCmdArgs

data TransactionBuildRawCmdArgs era = TransactionBuildRawCmdArgs
  { eon :: !(ShelleyBasedEra era)
  , mScriptValidity :: !(Maybe ScriptValidity)
  -- ^ Mark script as expected to pass or fail validation
  , txIns :: ![(TxIn, Maybe (ScriptWitnessFiles WitCtxTxIn))]
  -- ^ Transaction inputs with optional spending scripts
  , readOnlyRefIns :: ![TxIn]
  -- ^ Read only reference inputs
  , txInsCollateral :: ![TxIn]
  -- ^ Transaction inputs for collateral, only key witnesses, no scripts.
  , mReturnCollateral :: !(Maybe TxOutShelleyBasedEra)
  -- ^ Return collateral
  , mTotalCollateral :: !(Maybe Coin)
  -- ^ Total collateral
  , requiredSigners :: ![RequiredSigner]
  -- ^ Required signers
  , txouts :: ![TxOutAnyEra]
  , mValue :: !(Maybe (Value, [ScriptWitnessFiles WitCtxMint]))
  -- ^ Multi-Asset value with script witness
  , mValidityLowerBound :: !(Maybe SlotNo)
  -- ^ Transaction validity lower bound
  , mValidityUpperBound :: !(TxValidityUpperBound era)
  -- ^ Transaction validity upper bound
  , fee :: !Coin
  -- ^ Transaction fee
  , certificates :: ![(CertificateFile, Maybe (ScriptWitnessFiles WitCtxStake))]
  -- ^ Certificates with potential script witness
  , withdrawals :: ![(StakeAddress, Coin, Maybe (ScriptWitnessFiles WitCtxStake))]
  , metadataSchema :: !TxMetadataJsonSchema
  , scriptFiles :: ![ScriptFile]
  -- ^ Auxiliary scripts
  , metadataFiles :: ![MetadataFile]
  , mProtocolParamsFile :: !(Maybe ProtocolParamsFile)
  , mUpdateProprosalFile :: !(Maybe (Featured ShelleyToBabbageEra era (Maybe UpdateProposalFile)))
  , voteFiles :: ![(VoteFile In, Maybe (ScriptWitnessFiles WitCtxStake))]
  , proposalFiles :: ![(ProposalFile In, Maybe (ScriptWitnessFiles WitCtxStake))]
  , currentTreasuryValueAndDonation :: !(Maybe (TxCurrentTreasuryValue, TxTreasuryDonation))
  , txBodyOutFile :: !(TxBodyFile Out)
  }
  deriving Show

-- | Like 'TransactionBuildRaw' but without the fee, and with a change output.
data TransactionBuildCmdArgs era = TransactionBuildCmdArgs
  { eon :: !(ShelleyBasedEra era)
  , nodeSocketPath :: !SocketPath
  , consensusModeParams :: !ConsensusModeParams
  , networkId :: !NetworkId
  , mScriptValidity :: !(Maybe ScriptValidity)
  -- ^ Mark script as expected to pass or fail validation
  , mOverrideWitnesses :: !(Maybe Word)
  -- ^ Override the required number of tx witnesses
  , txins :: ![(TxIn, Maybe (ScriptWitnessFiles WitCtxTxIn))]
  -- ^ Transaction inputs with optional spending scripts
  , readOnlyReferenceInputs :: ![TxIn]
  -- ^ Read only reference inputs
  , requiredSigners :: ![RequiredSigner]
  -- ^ Required signers
  , txinsc :: ![TxIn]
  -- ^ Transaction inputs for collateral, only key witnesses, no scripts.
  , mReturnCollateral :: !(Maybe TxOutShelleyBasedEra)
  -- ^ Return collateral
  , mTotalCollateral :: !(Maybe Coin)
  -- ^ Total collateral
  , txouts :: ![TxOutAnyEra]
  -- ^ Normal outputs
  , changeAddresses :: !TxOutChangeAddress
  -- ^ A change output
  , mValue :: !(Maybe (Value, [ScriptWitnessFiles WitCtxMint]))
  -- ^ Multi-Asset value with script witness
  , mValidityLowerBound :: !(Maybe SlotNo)
  -- ^ Transaction validity lower bound
  , mValidityUpperBound :: !(TxValidityUpperBound era)
  -- ^ Transaction validity upper bound
  , certificates :: ![(CertificateFile, Maybe (ScriptWitnessFiles WitCtxStake))]
  -- ^ Certificates with potential script witness
  , withdrawals :: ![(StakeAddress, Coin, Maybe (ScriptWitnessFiles WitCtxStake))]
  -- ^ Withdrawals with potential script witness
  , metadataSchema :: !TxMetadataJsonSchema
  , scriptFiles :: ![ScriptFile]
  -- ^ Auxiliary scripts
  , metadataFiles :: ![MetadataFile]
  , mUpdateProposalFile :: !(Maybe (Featured ShelleyToBabbageEra era (Maybe UpdateProposalFile)))
  , voteFiles :: ![(VoteFile In, Maybe (ScriptWitnessFiles WitCtxStake))]
  , proposalFiles :: ![(ProposalFile In, Maybe (ScriptWitnessFiles WitCtxStake))]
  , treasuryDonation :: !(Maybe TxTreasuryDonation)
  , buildOutputOptions :: !TxBuildOutputOptions
  }
  deriving Show

-- | Like 'TransactionBuildCmd' but does not require explicit access to a running node
data TransactionBuildEstimateCmdArgs era = TransactionBuildEstimateCmdArgs
  { eon :: !(MaryEraOnwards era)
  , mScriptValidity :: !(Maybe ScriptValidity)
  -- ^ Mark script as expected to pass or fail validation
  , shelleyWitnesses :: !Int
  -- ^ Number of shelley witnesses to be added
  , mByronWitnesses :: !(Maybe Int)
  , protocolParamsFile :: !ProtocolParamsFile
  , totalUTxOValue :: !Value
  , txins :: ![(TxIn, Maybe (ScriptWitnessFiles WitCtxTxIn))]
  -- ^ Transaction inputs with optional spending scripts
  , readOnlyReferenceInputs :: ![TxIn]
  -- ^ Read only reference inputs
  , requiredSigners :: ![RequiredSigner]
  -- ^ Required signers
  , txinsc :: ![TxIn]
  -- ^ Transaction inputs for collateral, only key witnesses, no scripts.
  , mReturnCollateral :: !(Maybe TxOutShelleyBasedEra)
  -- ^ Return collateral
  , txouts :: ![TxOutAnyEra]
  -- ^ Normal outputs
  , changeAddress :: !TxOutChangeAddress
  -- ^ A change output
  , mValue :: !(Maybe (Value, [ScriptWitnessFiles WitCtxMint]))
  -- ^ Multi-Asset value with script witness
  , mValidityLowerBound :: !(Maybe SlotNo)
  -- ^ Transaction validity lower bound
  , mValidityUpperBound :: !(TxValidityUpperBound era)
  -- ^ Transaction validity upper bound
  , certificates :: ![(CertificateFile, Maybe (ScriptWitnessFiles WitCtxStake))]
  -- ^ Certificates with potential script witness
  , withdrawals :: ![(StakeAddress, Coin, Maybe (ScriptWitnessFiles WitCtxStake))]
  -- ^ Withdrawals with potential script witness
  , plutusCollateral :: !(Maybe Coin)
  -- ^ Total collateral
  , totalReferenceScriptSize :: !(Maybe ReferenceScriptSize)
  -- ^ Size of all reference scripts in bytes
  , metadataSchema :: !TxMetadataJsonSchema
  , scriptFiles :: ![ScriptFile]
  -- ^ Auxiliary scripts
  , metadataFiles :: ![MetadataFile]
  , mUpdateProposalFile :: !(Maybe (Featured ShelleyToBabbageEra era (Maybe UpdateProposalFile)))
  , voteFiles :: ![(VoteFile In, Maybe (ScriptWitnessFiles WitCtxStake))]
  , proposalFiles :: ![(ProposalFile In, Maybe (ScriptWitnessFiles WitCtxStake))]
  , currentTreasuryValueAndDonation :: !(Maybe (TxCurrentTreasuryValue, TxTreasuryDonation))
  , txBodyOutFile :: !(TxBodyFile Out)
  }

data TransactionEchoCmdArgs = TransactionEchoCmdArgs
  { txOrTxBodyFile :: !InputTxBodyOrTxFile
  , outTxFile :: !(TxFile Out)
  }
  deriving Show

data TransactionSignCmdArgs = TransactionSignCmdArgs
  { txOrTxBodyFile :: !InputTxBodyOrTxFile
  , witnessSigningData :: ![WitnessSigningData]
  , mNetworkId :: !(Maybe NetworkId)
  , outTxFile :: !(TxFile Out)
  }
  deriving Show

data TransactionWitnessCmdArgs = TransactionWitnessCmdArgs
  { txBodyFile :: !(TxBodyFile In)
  , witnessSigningData :: !WitnessSigningData
  , mNetworkId :: !(Maybe NetworkId)
  , outFile :: !(File () Out)
  }
  deriving Show

data TransactionSignWitnessCmdArgs = TransactionSignWitnessCmdArgs
  { txBodyFile :: !(TxBodyFile In)
  , witnessFiles :: ![WitnessFile]
  , outFile :: !(File () Out)
  }
  deriving Show

data TransactionSubmitCmdArgs = TransactionSubmitCmdArgs
  { nodeSocketPath :: !SocketPath
  , consensusModeParams :: !ConsensusModeParams
  , networkId :: !NetworkId
  , txFile :: !FilePath
  }
  deriving Show

newtype TransactionPolicyIdCmdArgs = TransactionPolicyIdCmdArgs
  { scriptFile :: ScriptFile
  }
  deriving Show

data TransactionCalculateMinFeeCmdArgs = TransactionCalculateMinFeeCmdArgs
  { txBodyFile :: !(TxBodyFile In)
  , protocolParamsFile :: !ProtocolParamsFile
  , txShelleyWitnessCount :: !TxShelleyWitnessCount
  , txByronWitnessCount :: !TxByronWitnessCount
  , referenceScriptSize :: !ReferenceScriptSize
  -- ^ The total size in bytes of the transaction reference scripts.
  , outputFormat :: !(Maybe OutputFormatJsonOrText)
  , outFile :: !(Maybe (File () Out))
  }
  deriving Show

data TransactionCalculateMinValueCmdArgs era = TransactionCalculateMinValueCmdArgs
  { eon :: !(ShelleyBasedEra era)
  , protocolParamsFile :: !ProtocolParamsFile
  , txOut :: !TxOutShelleyBasedEra
  }
  deriving Show

newtype TransactionHashScriptDataCmdArgs = TransactionHashScriptDataCmdArgs
  { scriptDataOrFile :: ScriptDataOrFile
  }
  deriving Show

newtype TransactionTxIdCmdArgs = TransactionTxIdCmdArgs
  { inputTxBodyOrTxFile :: InputTxBodyOrTxFile
  }
  deriving Show

data TransactionViewCmdArgs = TransactionViewCmdArgs
  { outputFormat :: !ViewOutputFormat
  , mOutFile :: !(Maybe (File () Out))
  , inputTxBodyOrTxFile :: !InputTxBodyOrTxFile
  }
  deriving Show

renderTransactionCmds :: TransactionCmds era -> Text
renderTransactionCmds = \case
  TransactionBuildCmd{} -> "transaction build"
  TransactionBuildEstimateCmd{} -> "transaction build-estimate"
  TransactionBuildRawCmd{} -> "transaction build-raw"
  TransactionEchoCmd{} -> "transaction echo"
  TransactionSignCmd{} -> "transaction sign"
  TransactionWitnessCmd{} -> "transaction witness"
  TransactionSignWitnessCmd{} -> "transaction sign-witness"
  TransactionSubmitCmd{} -> "transaction submit"
  TransactionPolicyIdCmd{} -> "transaction policyid"
  TransactionCalculateMinFeeCmd{} -> "transaction calculate-min-fee"
  TransactionCalculateMinValueCmd{} -> "transaction calculate-min-value"
  TransactionHashScriptDataCmd{} -> "transaction hash-script-data"
  TransactionTxIdCmd{} -> "transaction txid"
  TransactionViewCmd{} -> "transaction view"
