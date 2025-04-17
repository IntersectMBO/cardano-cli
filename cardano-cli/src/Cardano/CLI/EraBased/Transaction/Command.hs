{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.Transaction.Command
  ( TransactionCmds (..)
  , TransactionBuildRawCmdArgs (..)
  , TxCborFormat (..)
  , TransactionBuildCmdArgs (..)
  , TransactionBuildEstimateCmdArgs (..)
  , TransactionSignCmdArgs (..)
  , TransactionWitnessCmdArgs (..)
  , TransactionSignWitnessCmdArgs (..)
  , TransactionSubmitCmdArgs (..)
  , TransactionPolicyIdCmdArgs (..)
  , TransactionCalculateMinFeeCmdArgs (..)
  , TransactionCalculateMinValueCmdArgs (..)
  , TransactionCalculatePlutusScriptCostCmdArgs (..)
  , TransactionHashScriptDataCmdArgs (..)
  , TransactionTxIdCmdArgs (..)
  , TransactionViewCmdArgs (..)
  , renderTransactionCmds
  )
where

import Cardano.Api.Experimental qualified as Exp
import Cardano.Api.Ledger (Coin)
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Shelley

import Cardano.CLI.EraBased.Script.Certificate.Type (CliCertificateScriptRequirements)
import Cardano.CLI.EraBased.Script.Mint.Type
import Cardano.CLI.EraBased.Script.Proposal.Type (CliProposalScriptRequirements)
import Cardano.CLI.EraBased.Script.Spend.Type (CliSpendScriptRequirements)
import Cardano.CLI.EraBased.Script.Vote.Type
import Cardano.CLI.EraBased.Script.Withdrawal.Type
import Cardano.CLI.Orphan ()
import Cardano.CLI.Type.Common
import Cardano.CLI.Type.Governance

import Data.Text (Text)

import Vary (Vary)

data TransactionCmds era
  = TransactionBuildRawCmd !(TransactionBuildRawCmdArgs era)
  | TransactionBuildCmd !(TransactionBuildCmdArgs era)
  | TransactionBuildEstimateCmd !(TransactionBuildEstimateCmdArgs era)
  | TransactionSignCmd !TransactionSignCmdArgs
  | TransactionWitnessCmd !TransactionWitnessCmdArgs
  | TransactionSignWitnessCmd !TransactionSignWitnessCmdArgs
  | TransactionSubmitCmd !TransactionSubmitCmdArgs
  | TransactionPolicyIdCmd !TransactionPolicyIdCmdArgs
  | TransactionCalculateMinFeeCmd !TransactionCalculateMinFeeCmdArgs
  | TransactionCalculateMinValueCmd !(TransactionCalculateMinValueCmdArgs era)
  | TransactionCalculatePlutusScriptCostCmd !TransactionCalculatePlutusScriptCostCmdArgs
  | TransactionHashScriptDataCmd !TransactionHashScriptDataCmdArgs
  | TransactionTxIdCmd !TransactionTxIdCmdArgs

data TransactionBuildRawCmdArgs era = TransactionBuildRawCmdArgs
  { eon :: !(ShelleyBasedEra era)
  , mScriptValidity :: !(Maybe ScriptValidity)
  -- ^ Mark script as expected to pass or fail validation
  , txIns :: ![(TxIn, Maybe CliSpendScriptRequirements)]
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
  , mMintedAssets :: !(Maybe (L.MultiAsset, [CliMintScriptRequirements]))
  -- ^ Multi-Asset minted value with script witness
  , mValidityLowerBound :: !(Maybe SlotNo)
  -- ^ Transaction validity lower bound
  , mValidityUpperBound :: !(TxValidityUpperBound era)
  -- ^ Transaction validity upper bound
  , fee :: !Coin
  -- ^ Transaction fee
  , certificates :: ![(CertificateFile, Maybe CliCertificateScriptRequirements)]
  -- ^ Certificates with potential script witness
  , withdrawals :: ![(StakeAddress, Coin, Maybe CliWithdrawalScriptRequirements)]
  , metadataSchema :: !TxMetadataJsonSchema
  , scriptFiles :: ![ScriptFile]
  -- ^ Auxiliary scripts
  , metadataFiles :: ![MetadataFile]
  , mProtocolParamsFile :: !(Maybe ProtocolParamsFile)
  , mUpdateProprosalFile :: !(Maybe (Featured ShelleyToBabbageEra era (Maybe UpdateProposalFile)))
  , voteFiles :: ![(VoteFile In, Maybe CliVoteScriptRequirements)]
  , proposalFiles :: ![(ProposalFile In, Maybe CliProposalScriptRequirements)]
  , currentTreasuryValueAndDonation :: !(Maybe (TxCurrentTreasuryValue, TxTreasuryDonation))
  , isCborOutCanonical :: !TxCborFormat
  , txBodyOutFile :: !(TxBodyFile Out)
  }
  deriving Show

-- | Whether output transaction is in CBOR canonical format according to RFC7049 section 3.9.
--
-- 1. https://datatracker.ietf.org/doc/html/rfc7049#section-3.9
-- 2. https://github.com/cardano-foundation/CIPs/blob/master/CIP-0021/README.md#canonical-cbor-serialization-format
data TxCborFormat
  = TxCborCanonical
  | TxCborNotCanonical
  deriving (Eq, Show)

-- | Like 'TransactionBuildRaw' but without the fee, and with a change output.
data TransactionBuildCmdArgs era = TransactionBuildCmdArgs
  { currentEra :: !(Exp.Era era)
  , nodeConnInfo :: !LocalNodeConnectInfo
  , mScriptValidity :: !(Maybe ScriptValidity)
  -- ^ Mark script as expected to pass or fail validation
  , mOverrideWitnesses :: !(Maybe Word)
  -- ^ Override the required number of tx witnesses
  , txins :: ![(TxIn, Maybe CliSpendScriptRequirements)]
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
  , mMintedAssets :: !(Maybe (L.MultiAsset, [CliMintScriptRequirements]))
  -- ^ Multi-Asset minted value with script witness
  , mValidityLowerBound :: !(Maybe SlotNo)
  -- ^ Transaction validity lower bound
  , mValidityUpperBound :: !(TxValidityUpperBound era)
  -- ^ Transaction validity upper bound
  , certificates :: ![(CertificateFile, Maybe CliCertificateScriptRequirements)]
  -- ^ Certificates with potential script witness
  , withdrawals :: ![(StakeAddress, Coin, Maybe CliWithdrawalScriptRequirements)]
  -- ^ Withdrawals with potential script witness
  , metadataSchema :: !TxMetadataJsonSchema
  , scriptFiles :: ![ScriptFile]
  -- ^ Auxiliary scripts
  , metadataFiles :: ![MetadataFile]
  , mUpdateProposalFile :: !(Maybe (Featured ShelleyToBabbageEra era (Maybe UpdateProposalFile)))
  , voteFiles :: ![(VoteFile In, Maybe CliVoteScriptRequirements)]
  , proposalFiles :: ![(ProposalFile In, Maybe CliProposalScriptRequirements)]
  , treasuryDonation :: !(Maybe TxTreasuryDonation)
  , isCborOutCanonical :: !TxCborFormat
  , buildOutputOptions :: !TxBuildOutputOptions
  }
  deriving Show

-- | Like 'TransactionBuildCmd' but does not require explicit access to a running node
data TransactionBuildEstimateCmdArgs era = TransactionBuildEstimateCmdArgs
  { currentEra :: !(Exp.Era era)
  , mScriptValidity :: !(Maybe ScriptValidity)
  -- ^ Mark script as expected to pass or fail validation
  , shelleyWitnesses :: !Int
  -- ^ Number of shelley witnesses to be added
  , mByronWitnesses :: !(Maybe Int)
  , protocolParamsFile :: !ProtocolParamsFile
  , totalUTxOValue :: !Value
  , txins :: ![(TxIn, Maybe CliSpendScriptRequirements)]
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
  , mMintedAssets :: !(Maybe (L.MultiAsset, [CliMintScriptRequirements]))
  -- ^ Multi-Asset value with script witness
  , mValidityLowerBound :: !(Maybe SlotNo)
  -- ^ Transaction validity lower bound
  , mValidityUpperBound :: !(TxValidityUpperBound era)
  -- ^ Transaction validity upper bound
  , certificates :: ![(CertificateFile, Maybe CliCertificateScriptRequirements)]
  -- ^ Certificates with potential script witness
  , withdrawals :: ![(StakeAddress, Coin, Maybe CliWithdrawalScriptRequirements)]
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
  , voteFiles :: ![(VoteFile In, Maybe CliVoteScriptRequirements)]
  , proposalFiles :: ![(ProposalFile In, Maybe CliProposalScriptRequirements)]
  , currentTreasuryValueAndDonation :: !(Maybe (TxCurrentTreasuryValue, TxTreasuryDonation))
  , isCborOutCanonical :: !TxCborFormat
  , txBodyOutFile :: !(TxBodyFile Out)
  }

data TransactionSignCmdArgs = TransactionSignCmdArgs
  { txOrTxBodyFile :: !InputTxBodyOrTxFile
  , witnessSigningData :: ![WitnessSigningData]
  , mNetworkId :: !(Maybe NetworkId)
  , isCborOutCanonical :: !TxCborFormat
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
  , isCborOutCanonical :: !TxCborFormat
  , outFile :: !(File () Out)
  }
  deriving Show

data TransactionSubmitCmdArgs = TransactionSubmitCmdArgs
  { nodeConnInfo :: !LocalNodeConnectInfo
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
  , outputFormat :: !(Maybe (Vary [FormatJson, FormatText]))
  , outFile :: !(Maybe (File () Out))
  }
  deriving Show

data TransactionCalculateMinValueCmdArgs era = TransactionCalculateMinValueCmdArgs
  { eon :: !(ShelleyBasedEra era)
  , protocolParamsFile :: !ProtocolParamsFile
  , txOut :: !TxOutShelleyBasedEra
  }
  deriving Show

data TransactionCalculatePlutusScriptCostCmdArgs = TransactionCalculatePlutusScriptCostCmdArgs
  { nodeConnInfo :: !LocalNodeConnectInfo
  , txFileIn :: FilePath
  , outputFile :: !(Maybe (File () Out))
  }

newtype TransactionHashScriptDataCmdArgs = TransactionHashScriptDataCmdArgs
  { scriptDataOrFile :: ScriptDataOrFile
  }
  deriving Show

data TransactionTxIdCmdArgs = TransactionTxIdCmdArgs
  { inputTxBodyOrTxFile :: InputTxBodyOrTxFile
  , outputFormat :: !(Vary [FormatJson, FormatText])
  }
  deriving Show

data TransactionViewCmdArgs = TransactionViewCmdArgs
  deriving Show

renderTransactionCmds :: TransactionCmds era -> Text
renderTransactionCmds = \case
  TransactionBuildCmd{} -> "transaction build"
  TransactionBuildEstimateCmd{} -> "transaction build-estimate"
  TransactionBuildRawCmd{} -> "transaction build-raw"
  TransactionSignCmd{} -> "transaction sign"
  TransactionWitnessCmd{} -> "transaction witness"
  TransactionSignWitnessCmd{} -> "transaction sign-witness"
  TransactionSubmitCmd{} -> "transaction submit"
  TransactionPolicyIdCmd{} -> "transaction policyid"
  TransactionCalculateMinFeeCmd{} -> "transaction calculate-min-fee"
  TransactionCalculateMinValueCmd{} -> "transaction calculate-min-value"
  TransactionCalculatePlutusScriptCostCmd{} -> "transaction calculate-plutus-script-cost"
  TransactionHashScriptDataCmd{} -> "transaction hash-script-data"
  TransactionTxIdCmd{} -> "transaction txid"
