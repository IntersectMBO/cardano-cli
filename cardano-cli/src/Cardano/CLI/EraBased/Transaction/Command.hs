{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.Transaction.Command
  ( MustExtendSafeZone (..)
  , NodeContextInfoSource (..)
  , SystemStartOrGenesisFileSource (..)
  , TransactionBuildCmdArgs (..)
  , TransactionBuildEstimateCmdArgs (..)
  , TransactionBuildRawCmdArgs (..)
  , TransactionCalculateMinFeeCmdArgs (..)
  , TransactionCalculateMinValueCmdArgs (..)
  , TransactionCalculatePlutusScriptCostCmdArgs (..)
  , TransactionCmds (..)
  , TransactionContext (..)
  , TransactionHashScriptDataCmdArgs (..)
  , TransactionPolicyIdCmdArgs (..)
  , TransactionSignCmdArgs (..)
  , TransactionSignWitnessCmdArgs (..)
  , TransactionSubmitCmdArgs (..)
  , TransactionTxIdCmdArgs (..)
  , TransactionViewCmdArgs (..)
  , TransactionWitnessCmdArgs (..)
  , TxCborFormat (..)
  , IncludeCurrentTreasuryValue (..)
  , renderTransactionCmds
  )
where

import Cardano.Api
import Cardano.Api.Experimental qualified as Exp
import Cardano.Api.Ledger qualified as L

import Cardano.CLI.EraBased.Script.Type
import Cardano.CLI.Orphan ()
import Cardano.CLI.Type.Common
import Cardano.CLI.Type.Governance

import Data.Text (Text)
import Data.Universe (Some)
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
  | TransactionCalculatePlutusScriptCostCmd !(TransactionCalculatePlutusScriptCostCmdArgs era)
  | TransactionHashScriptDataCmd !TransactionHashScriptDataCmdArgs
  | TransactionTxIdCmd !TransactionTxIdCmdArgs

data TransactionBuildRawCmdArgs era = TransactionBuildRawCmdArgs
  { eon :: !(Exp.Era era)
  , mScriptValidity :: !(Maybe ScriptValidity)
  -- ^ Mark script as expected to pass or fail validation
  , txIns :: ![(TxIn, Maybe (ScriptRequirements Exp.TxInItem))]
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
  , mMintedAssets :: !(Maybe (L.MultiAsset, [ScriptRequirements Exp.MintItem]))
  -- ^ Multi-Asset minted value with script witness
  , mValidityLowerBound :: !(Maybe SlotNo)
  -- ^ Transaction validity lower bound
  , mValidityUpperBound :: !(TxValidityUpperBound era)
  -- ^ Transaction validity upper bound
  , fee :: !Coin
  -- ^ Transaction fee
  , certificates :: ![(CertificateFile, Maybe (ScriptRequirements Exp.CertItem))]
  -- ^ Certificates with potential script witness
  , withdrawals :: ![(StakeAddress, Coin, Maybe (ScriptRequirements Exp.WithdrawalItem))]
  , metadataSchema :: !TxMetadataJsonSchema
  , scriptFiles :: ![ScriptFile]
  -- ^ Auxiliary scripts
  , metadataFiles :: ![MetadataFile]
  , mProtocolParamsFile :: !(Maybe ProtocolParamsFile)
  , mUpdateProprosalFile :: !(Maybe (Featured ShelleyToBabbageEra era (Maybe UpdateProposalFile)))
  , voteFiles :: ![(VoteFile In, Maybe (ScriptRequirements Exp.VoterItem))]
  , proposalFiles :: ![(ProposalFile In, Maybe (ScriptRequirements Exp.ProposalItem))]
  , mCurrentTreasuryValue :: !(Maybe TxCurrentTreasuryValue)
  , mTreasuryDonation :: !(Maybe TxTreasuryDonation)
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

-- | Whether to include the current treasury value in the transaction body.
--
-- If included, the current treasury value will be obtained from the node.
--
-- The current treasury value serves as a precondition to executing Plutus
-- scripts that access the value of the treasury.
--
-- See: https://intersectmbo.github.io/formal-ledger-specifications/site/Ledger.Conway.Specification.Transaction.html#sec:transactions
--
-- If a transaction contains any votes, proposals, a treasury donation or
-- asserts the treasury amount, it is only allowed to contain Plutus V3 scripts.
--
-- See: https://intersectmbo.github.io/formal-ledger-specifications/site/Ledger.Conway.Specification.Utxow.html#sec:witnessing-functions
data IncludeCurrentTreasuryValue = IncludeCurrentTreasuryValue | ExcludeCurrentTreasuryValue
  deriving (Eq, Show)

-- | Like 'TransactionBuildRaw' but without the fee, and with a change output.
data TransactionBuildCmdArgs era = TransactionBuildCmdArgs
  { currentEra :: !(Exp.Era era)
  , nodeConnInfo :: !LocalNodeConnectInfo
  , mScriptValidity :: !(Maybe ScriptValidity)
  -- ^ Mark script as expected to pass or fail validation
  , mOverrideWitnesses :: !(Maybe Word)
  -- ^ Override the required number of tx witnesses
  , txins :: ![(TxIn, Maybe (ScriptRequirements Exp.TxInItem))]
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
  , mMintedAssets :: !(Maybe (L.MultiAsset, [ScriptRequirements Exp.MintItem]))
  -- ^ Multi-Asset minted value with script witness
  , mValidityLowerBound :: !(Maybe SlotNo)
  -- ^ Transaction validity lower bound
  , mValidityUpperBound :: !(TxValidityUpperBound era)
  -- ^ Transaction validity upper bound
  , certificates :: ![(CertificateFile, Maybe (ScriptRequirements Exp.CertItem))]
  -- ^ Certificates with potential script witness
  , withdrawals :: ![(StakeAddress, Coin, Maybe (ScriptRequirements Exp.WithdrawalItem))]
  -- ^ Withdrawals with potential script witness
  , metadataSchema :: !TxMetadataJsonSchema
  , scriptFiles :: ![ScriptFile]
  -- ^ Auxiliary scripts
  , metadataFiles :: ![MetadataFile]
  , mUpdateProposalFile :: !(Maybe (Featured ShelleyToBabbageEra era (Maybe UpdateProposalFile)))
  , voteFiles :: ![(VoteFile In, Maybe (ScriptRequirements Exp.VoterItem))]
  , proposalFiles :: ![(ProposalFile In, Maybe (ScriptRequirements Exp.ProposalItem))]
  , includeCurrentTreasuryValue :: !IncludeCurrentTreasuryValue
  , mTreasuryDonation :: !(Maybe TxTreasuryDonation)
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
  , txins :: ![(TxIn, Maybe (ScriptRequirements Exp.TxInItem))]
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
  , mMintedAssets :: !(Maybe (L.MultiAsset, [ScriptRequirements Exp.MintItem]))
  -- ^ Multi-Asset value with script witness
  , mValidityLowerBound :: !(Maybe SlotNo)
  -- ^ Transaction validity lower bound
  , mValidityUpperBound :: !(TxValidityUpperBound era)
  -- ^ Transaction validity upper bound
  , certificates :: ![(CertificateFile, Maybe (ScriptRequirements Exp.CertItem))]
  -- ^ Certificates with potential script witness
  , withdrawals :: ![(StakeAddress, Coin, Maybe (ScriptRequirements Exp.WithdrawalItem))]
  -- ^ Withdrawals with potential script witness
  , plutusCollateral :: !(Maybe Coin)
  -- ^ Total collateral
  , totalReferenceScriptSize :: !(Maybe ReferenceScriptSize)
  -- ^ Size of all reference scripts in bytes
  , metadataSchema :: !TxMetadataJsonSchema
  , scriptFiles :: ![ScriptFile]
  -- ^ Auxiliary scripts
  , metadataFiles :: ![MetadataFile]
  , voteFiles :: ![(VoteFile In, Maybe (ScriptRequirements Exp.VoterItem))]
  , proposalFiles :: ![(ProposalFile In, Maybe (ScriptRequirements Exp.ProposalItem))]
  , currentTreasuryValue :: !(Maybe TxCurrentTreasuryValue)
  , treasuryDonation :: !(Maybe TxTreasuryDonation)
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
  , outputFormat :: !(Vary [FormatJson, FormatText, FormatYaml])
  , outFile :: !(Maybe (File () Out))
  }
  deriving Show

data TransactionCalculateMinValueCmdArgs era = TransactionCalculateMinValueCmdArgs
  { era :: !(Exp.Era era)
  , protocolParamsFile :: !ProtocolParamsFile
  , txOut :: !TxOutShelleyBasedEra
  }
  deriving Show

data TransactionCalculatePlutusScriptCostCmdArgs era = TransactionCalculatePlutusScriptCostCmdArgs
  { nodeContextInfoSource :: !(NodeContextInfoSource era)
  , txFileIn :: FilePath
  , outputFile :: !(Maybe (File () Out))
  }

-- | Either information about the context in which the transaction command
-- is run, or information required to obtain it (information to connect to the node).
data NodeContextInfoSource era
  = NodeConnectionInfo !LocalNodeConnectInfo
  | ProvidedTransactionContextInfo !(TransactionContext era)

-- | Transaction context, required to evaluate the execution
-- costs of the plutus scripts in the transaction.
data TransactionContext era = TransactionContext
  { systemStartSource :: SystemStartOrGenesisFileSource
  , mustExtendSafeZone :: MustExtendSafeZone
  , eraHistoryFile :: File EraHistory In
  , utxoFile :: File (Some UTxO) In
  , protocolParamsFile :: ProtocolParamsFile
  }

-- | The system start time or the genesis file from which to get it
data SystemStartOrGenesisFileSource
  = SystemStartLiteral !SystemStart
  | SystemStartFromGenesisFile !GenesisFile

-- | Allow overriding the validity of the era history past the safe zone. The
-- safe zone is a period of time during which we are sure there won't be any
-- era transition (hard fork), and we are confident that the slot duration
-- will not change, thus the conversion from slot numbers to POSIX times
-- using the era history will be correct.
--
-- This safe zone is conservative. Even if we are past the safe zone, if
-- there hasn't been any era transition (hard fork) since we obtained it, we can
-- continue safely using the era history.
--
-- 'MustExtendSafeZone' essentially disables the safe zone check. This allows the user to
-- use the era history past the safe zone, at the user's discretion.
data MustExtendSafeZone
  = MustExtendSafeZone
  | DoNotExtendSafeZone

newtype TransactionHashScriptDataCmdArgs = TransactionHashScriptDataCmdArgs
  { scriptDataOrFile :: ScriptDataOrFile
  }
  deriving Show

data TransactionTxIdCmdArgs = TransactionTxIdCmdArgs
  { inputTxBodyOrTxFile :: InputTxBodyOrTxFile
  , outputFormat :: !(Vary [FormatJson, FormatText, FormatYaml])
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
