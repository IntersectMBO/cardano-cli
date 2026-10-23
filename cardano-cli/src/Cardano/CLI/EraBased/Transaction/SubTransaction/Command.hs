{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.Transaction.SubTransaction.Command
  ( SubTransactionCmds (..)
  , SubTransactionBuildRawCmdArgs (..)
  , SubTransactionSignCmdArgs (..)
  , SubTransactionWitnessCmdArgs (..)
  , SubTransactionAssembleCmdArgs (..)
  , SubTransactionTxIdCmdArgs (..)
  , Guard (..)
  , GuardScript (..)
  , TopLevelGuard (..)
  , renderSubTransactionCmds
  )
where

import Cardano.Api
import Cardano.Api.Experimental qualified as Exp
import Cardano.Api.Ledger qualified as L

import Cardano.CLI.EraBased.Script.Type
import Cardano.CLI.Type.Common
import Cardano.CLI.Type.Governance
import Cardano.CLI.Type.Key (VerificationKeyOrHashOrFile)
import Cardano.Ledger.Dijkstra.Scripts qualified as L

import Vary (Vary)

-- | Commands for Dijkstra sub-transactions: the pieces a top-level
-- transaction embeds with @--signed-sub-tx-file@. Sub-transactions only exist in
-- Dijkstra, so these carry no era index.
data SubTransactionCmds
  = SubTransactionBuildRawCmd !SubTransactionBuildRawCmdArgs
  | SubTransactionSignCmd !SubTransactionSignCmdArgs
  | SubTransactionWitnessCmd !SubTransactionWitnessCmdArgs
  | SubTransactionAssembleCmd !SubTransactionAssembleCmdArgs
  | SubTransactionTxIdCmd !SubTransactionTxIdCmdArgs

-- | Like 'TransactionBuildRawCmdArgs' for a sub-transaction body. A
-- sub-transaction has no fee, collateral, required signers or script
-- validity flag; guards replace required signers.
data SubTransactionBuildRawCmdArgs = SubTransactionBuildRawCmdArgs
  { txIns :: ![(TxIn, Maybe AnySpendScript)]
  -- ^ Transaction inputs with optional spending scripts
  , readOnlyRefIns :: ![TxIn]
  -- ^ Read only reference inputs
  , txouts :: ![TxOutAnyEra]
  , mMintedAssets :: !(Maybe (L.MultiAsset, [AnyMintScript]))
  -- ^ Multi-Asset minted value with script witness
  , mValidityLowerBound :: !(Maybe SlotNo)
  -- ^ Transaction validity lower bound
  , mValidityUpperBound :: !(Maybe SlotNo)
  -- ^ Transaction validity upper bound
  , certificates :: ![(CertificateFile, Maybe AnyNonAssetScript)]
  -- ^ Certificates with potential script witness
  , withdrawals :: ![(StakeAddress, Coin, Maybe AnyNonAssetScript)]
  , directDeposits :: ![(StakeAddress, Coin)]
  -- ^ Direct ADA deposits as recipient stake addresses and amounts in lovelace.
  , accountBalanceIntervals
      :: ![(StakeAddress, L.AccountBalanceInterval (Exp.LedgerEra Exp.DijkstraEra))]
  , metadataSchema :: !TxMetadataJsonSchema
  , scriptFiles :: ![ScriptFile]
  -- ^ Auxiliary scripts
  , metadataFiles :: ![MetadataFile]
  , mProtocolParamsFile :: !(Maybe ProtocolParamsFile)
  , voteFiles :: ![(VoteFile In, Maybe AnyNonAssetScript)]
  , proposalFiles :: ![(ProposalFile In, Maybe AnyNonAssetScript)]
  , mCurrentTreasuryValue :: !(Maybe TxCurrentTreasuryValue)
  , mTreasuryDonation :: !(Maybe TxTreasuryDonation)
  , guards :: ![Guard]
  -- ^ Credentials whose authorisation the sub-transaction requires
  , topLevelGuards :: ![TopLevelGuard]
  -- ^ Credentials required as guards in the enclosing transaction
  , isCborOutCanonical :: !TxCborFormat
  , outFile :: !(UnsignedSubTxFile Out)
  }
  deriving Show

-- | A script guard may carry its guarding redeemer and execution budget.
-- They are required for Plutus scripts and forbidden for native scripts.
data Guard
  = KeyGuard (VerificationKeyOrHashOrFile PaymentKey)
  | ScriptGuard GuardScript (Maybe (ScriptDataOrFile, ExecutionUnits))
  deriving Show

data GuardScript
  = GuardScriptHash ScriptHash
  | GuardScriptFile ScriptFile
  deriving Show

-- | Only script requirements can carry a datum; key requirements never do.
data TopLevelGuard
  = TopLevelKeyGuard (VerificationKeyOrHashOrFile PaymentKey)
  | TopLevelScriptGuard GuardScript (Maybe ScriptDataOrFile)
  deriving Show

data SubTransactionSignCmdArgs = SubTransactionSignCmdArgs
  { subTxFile :: !(UnsignedSubTxFile In)
  , witnessSigningData :: ![WitnessSigningData]
  , outFile :: !(SignedSubTxFile Out)
  }
  deriving Show

data SubTransactionWitnessCmdArgs = SubTransactionWitnessCmdArgs
  { subTxFile :: !(UnsignedSubTxFile In)
  , witnessSigningData :: !WitnessSigningData
  , outFile :: !(File () Out)
  }
  deriving Show

data SubTransactionAssembleCmdArgs = SubTransactionAssembleCmdArgs
  { subTxFile :: !(UnsignedSubTxFile In)
  , witnessFiles :: ![WitnessFile]
  , outFile :: !(SignedSubTxFile Out)
  }
  deriving Show

data SubTransactionTxIdCmdArgs = SubTransactionTxIdCmdArgs
  { inputSubTxFile :: !InputSubTxFile
  , outputFormat :: !(Vary [FormatJson, FormatText, FormatYaml])
  }
  deriving Show

renderSubTransactionCmds :: SubTransactionCmds -> Text
renderSubTransactionCmds = \case
  SubTransactionBuildRawCmd{} -> "sub-transaction build-raw"
  SubTransactionSignCmd{} -> "sub-transaction sign"
  SubTransactionWitnessCmd{} -> "sub-transaction witness"
  SubTransactionAssembleCmd{} -> "sub-transaction assemble"
  SubTransactionTxIdCmd{} -> "sub-transaction txid"
