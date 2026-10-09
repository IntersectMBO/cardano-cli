{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.Transaction.SubTransaction.Command
  ( SubTransactionCmds (..)
  , SubTransactionBuildRawCmdArgs (..)
  , SubTransactionSignCmdArgs (..)
  , SubTransactionTxIdCmdArgs (..)
  , renderSubTransactionCmds
  )
where

import Cardano.Api
import Cardano.Api.Ledger qualified as L

import Cardano.CLI.EraBased.Script.Type
import Cardano.CLI.Type.Common
import Cardano.CLI.Type.Governance
import Cardano.CLI.Type.Key (VerificationKeyOrHashOrFileOrScriptHash)

import Vary (Vary)

-- | Commands for Dijkstra sub-transactions: the pieces a top-level
-- transaction embeds with @--signed-sub-tx-file@. Sub-transactions only exist in
-- Dijkstra, so these carry no era index.
data SubTransactionCmds
  = SubTransactionBuildRawCmd !SubTransactionBuildRawCmdArgs
  | SubTransactionSignCmd !SubTransactionSignCmdArgs
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
  , metadataSchema :: !TxMetadataJsonSchema
  , scriptFiles :: ![ScriptFile]
  -- ^ Auxiliary scripts
  , metadataFiles :: ![MetadataFile]
  , mProtocolParamsFile :: !(Maybe ProtocolParamsFile)
  , voteFiles :: ![(VoteFile In, Maybe AnyNonAssetScript)]
  , proposalFiles :: ![(ProposalFile In, Maybe AnyNonAssetScript)]
  , mCurrentTreasuryValue :: !(Maybe TxCurrentTreasuryValue)
  , mTreasuryDonation :: !(Maybe TxTreasuryDonation)
  , guards :: ![VerificationKeyOrHashOrFileOrScriptHash PaymentKey]
  -- ^ Credentials whose authorisation the sub-transaction requires
  , outFile :: !(UnsignedSubTxFile Out)
  }
  deriving Show

data SubTransactionSignCmdArgs = SubTransactionSignCmdArgs
  { subTxFile :: !(UnsignedSubTxFile In)
  , witnessSigningData :: ![WitnessSigningData]
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
  SubTransactionBuildRawCmd{} -> "transaction sub-transaction build-raw"
  SubTransactionSignCmd{} -> "transaction sub-transaction sign"
  SubTransactionTxIdCmd{} -> "transaction sub-transaction txid"
