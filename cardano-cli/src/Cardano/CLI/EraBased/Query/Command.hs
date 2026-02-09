{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.Query.Command
  ( QueryCmds (..)
  , QueryCommons (..)
  , QueryCommitteeMembersStateCmdArgs (..)
  , QueryLeadershipScheduleCmdArgs (..)
  , QueryProtocolParametersCmdArgs (..)
  , QueryTipCmdArgs (..)
  , QueryStakePoolsCmdArgs (..)
  , QueryProposalsCmdArgs (..)
  , QueryStakeDistributionCmdArgs (..)
  , QueryStakeAddressInfoCmdArgs (..)
  , QueryUTxOCmdArgs (..)
  , QueryLedgerStateCmdArgs (..)
  , QueryProtocolStateCmdArgs (..)
  , QueryStakeSnapshotCmdArgs (..)
  , QueryKesPeriodInfoCmdArgs (..)
  , QueryPoolStateCmdArgs (..)
  , QueryTxMempoolCmdArgs (..)
  , QuerySlotNumberCmdArgs (..)
  , QueryRefScriptSizeCmdArgs (..)
  , QueryNoArgCmdArgs (..)
  , QueryDRepStateCmdArgs (..)
  , QueryDRepStakeDistributionCmdArgs (..)
  , QuerySPOStakeDistributionCmdArgs (..)
  , QueryTreasuryValueCmdArgs (..)
  , QueryLedgerPeerSnapshotCmdArgs (..)
  , QueryStakePoolDefaultVoteCmdArgs (..)
  , QueryEraHistoryCmdArgs (..)
  , renderQueryCmds
  , IncludeStake (..)
  )
where

import Cardano.Api
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Network as Consensus

import Cardano.CLI.Orphan ()
import Cardano.CLI.Type.Common
import Cardano.CLI.Type.Key

import Data.Set (Set)
import Data.Time.Clock
import GHC.Generics
import Vary

data QueryCmds era
  = QueryLeadershipScheduleCmd !QueryLeadershipScheduleCmdArgs
  | QueryProtocolParametersCmd !QueryProtocolParametersCmdArgs
  | QueryTipCmd !QueryTipCmdArgs
  | QueryStakePoolsCmd !QueryStakePoolsCmdArgs
  | QueryStakeDistributionCmd !QueryStakeDistributionCmdArgs
  | QueryStakeAddressInfoCmd !QueryStakeAddressInfoCmdArgs
  | QueryUTxOCmd !QueryUTxOCmdArgs
  | QueryLedgerStateCmd !QueryLedgerStateCmdArgs
  | QueryProtocolStateCmd !QueryProtocolStateCmdArgs
  | QueryStakeSnapshotCmd !QueryStakeSnapshotCmdArgs
  | QueryKesPeriodInfoCmd !QueryKesPeriodInfoCmdArgs
  | QueryPoolStateCmd !QueryPoolStateCmdArgs
  | QueryTxMempoolCmd !QueryTxMempoolCmdArgs
  | QuerySlotNumberCmd !QuerySlotNumberCmdArgs
  | QueryRefScriptSizeCmd !QueryRefScriptSizeCmdArgs
  | QueryConstitutionCmd !(QueryNoArgCmdArgs era)
  | QueryGovStateCmd !(QueryNoArgCmdArgs era)
  | QueryRatifyStateCmd !(QueryNoArgCmdArgs era)
  | QueryFuturePParamsCmd !(QueryNoArgCmdArgs era)
  | QueryDRepStateCmd !(QueryDRepStateCmdArgs era)
  | QueryDRepStakeDistributionCmd !(QueryDRepStakeDistributionCmdArgs era)
  | QuerySPOStakeDistributionCmd !(QuerySPOStakeDistributionCmdArgs era)
  | QueryCommitteeMembersStateCmd !(QueryCommitteeMembersStateCmdArgs era)
  | QueryTreasuryValueCmd !(QueryTreasuryValueCmdArgs era)
  | QueryProposalsCmd !(QueryProposalsCmdArgs era)
  | QueryLedgerPeerSnapshotCmd !QueryLedgerPeerSnapshotCmdArgs
  | QueryStakePoolDefaultVoteCmd !(QueryStakePoolDefaultVoteCmdArgs era)
  | QueryEraHistoryCmd !QueryEraHistoryCmdArgs
  deriving (Generic, Show)

-- | Fields that are common to most queries
data QueryCommons = QueryCommons
  { nodeConnInfo :: !LocalNodeConnectInfo
  , target :: !(Consensus.Target ChainPoint)
  }
  deriving (Generic, Show)

data QueryLeadershipScheduleCmdArgs = QueryLeadershipScheduleCmdArgs
  { commons :: !QueryCommons
  , genesisFp :: !GenesisFile
  , poolColdVerKeyFile :: !StakePoolKeyHashSource
  , vrkSkeyFp :: !(SigningKeyFile In)
  , whichSchedule :: !EpochLeadershipSchedule
  , outputFormat :: !(Vary [FormatJson, FormatText, FormatYaml])
  , mOutFile :: !(Maybe (File () Out))
  }
  deriving (Generic, Show)

data QueryProtocolParametersCmdArgs = QueryProtocolParametersCmdArgs
  { nodeConnInfo :: !LocalNodeConnectInfo
  , outputFormat :: !(Vary [FormatJson, FormatYaml])
  , mOutFile :: !(Maybe (File () Out))
  }
  deriving (Generic, Show)

data QueryTipCmdArgs = QueryTipCmdArgs
  { commons :: !QueryCommons
  , outputFormat :: !(Vary [FormatJson, FormatYaml])
  , mOutFile :: !(Maybe (File () Out))
  }
  deriving (Generic, Show)

data QueryStakePoolsCmdArgs = QueryStakePoolsCmdArgs
  { commons :: !QueryCommons
  , outputFormat :: !(Vary [FormatJson, FormatText, FormatYaml])
  , mOutFile :: !(Maybe (File () Out))
  }
  deriving (Generic, Show)

data QueryStakeDistributionCmdArgs = QueryStakeDistributionCmdArgs
  { commons :: !QueryCommons
  , outputFormat :: !(Vary [FormatJson, FormatText, FormatYaml])
  , mOutFile :: !(Maybe (File () Out))
  }
  deriving (Generic, Show)

data QueryStakeAddressInfoCmdArgs = QueryStakeAddressInfoCmdArgs
  { commons :: !QueryCommons
  , addr :: !StakeAddress
  , outputFormat :: !(Vary [FormatJson, FormatYaml])
  , mOutFile :: !(Maybe (File () Out))
  }
  deriving (Generic, Show)

data QueryUTxOCmdArgs = QueryUTxOCmdArgs
  { commons :: !QueryCommons
  , queryFilter :: !QueryUTxOFilter
  , outputFormat :: !(Vary [FormatCborBin, FormatCborHex, FormatJson, FormatText, FormatYaml])
  , mOutFile :: !(Maybe (File () Out))
  }
  deriving (Generic, Show)

data QueryLedgerStateCmdArgs = QueryLedgerStateCmdArgs
  { commons :: !QueryCommons
  , outputFormat :: !(Vary [FormatJson, FormatText, FormatYaml])
  , mOutFile :: !(Maybe (File () Out))
  }
  deriving (Generic, Show)

data QueryLedgerPeerSnapshotCmdArgs = QueryLedgerPeerSnapshotCmdArgs
  { commons :: !QueryCommons
  , outputFormat :: !(Vary [FormatJson, FormatYaml])
  , peerKind :: !(Vary [BigLedgerPeers, AllLedgerPeers])
  , mOutFile :: !(Maybe (File () Out))
  }
  deriving (Generic, Show)

data QueryProtocolStateCmdArgs = QueryProtocolStateCmdArgs
  { commons :: !QueryCommons
  , outputFormat :: !(Vary [FormatCborBin, FormatCborHex, FormatJson, FormatYaml])
  , mOutFile :: !(Maybe (File () Out))
  }
  deriving (Generic, Show)

data QueryStakeSnapshotCmdArgs = QueryStakeSnapshotCmdArgs
  { commons :: !QueryCommons
  , allOrOnlyPoolIds :: !(AllOrOnly (Hash StakePoolKey))
  , outputFormat :: !(Vary [FormatJson, FormatYaml])
  , mOutFile :: !(Maybe (File () Out))
  }
  deriving (Generic, Show)

data QueryKesPeriodInfoCmdArgs = QueryKesPeriodInfoCmdArgs
  { commons :: !QueryCommons
  , nodeOpCertFp :: !(File () In)
  -- ^ Node operational certificate
  , outputFormat :: !(Vary [FormatJson, FormatYaml])
  , mOutFile :: !(Maybe (File () Out))
  }
  deriving (Generic, Show)

data QueryPoolStateCmdArgs = QueryPoolStateCmdArgs
  { commons :: !QueryCommons
  , allOrOnlyPoolIds :: !(AllOrOnly (Hash StakePoolKey))
  , outputFormat :: !(Vary [FormatJson, FormatYaml])
  , mOutFile :: !(Maybe (File () Out))
  }
  deriving (Generic, Show)

data QueryTxMempoolCmdArgs = QueryTxMempoolCmdArgs
  { nodeConnInfo :: !LocalNodeConnectInfo
  , query :: !TxMempoolQuery
  , outputFormat :: !(Vary [FormatJson, FormatYaml])
  , mOutFile :: !(Maybe (File () Out))
  }
  deriving (Generic, Show)

data QuerySlotNumberCmdArgs = QuerySlotNumberCmdArgs
  { commons :: !QueryCommons
  , utcTime :: !UTCTime
  }
  deriving (Generic, Show)

data QueryRefScriptSizeCmdArgs = QueryRefScriptSizeCmdArgs
  { commons :: !QueryCommons
  , transactionInputs :: !(Set TxIn)
  , outputFormat :: !(Vary [FormatJson, FormatText, FormatYaml])
  , mOutFile :: !(Maybe (File () Out))
  }
  deriving (Generic, Show)

data QueryNoArgCmdArgs era = QueryNoArgCmdArgs
  { eon :: !(ConwayEraOnwards era)
  , commons :: !QueryCommons
  , outputFormat :: !(Vary [FormatJson, FormatYaml])
  , mOutFile :: !(Maybe (File () Out))
  }
  deriving Show

data QueryDRepStateCmdArgs era = QueryDRepStateCmdArgs
  { eon :: !(ConwayEraOnwards era)
  , commons :: !QueryCommons
  , drepHashSources :: !(AllOrOnly DRepHashSource)
  , includeStake :: !IncludeStake
  , outputFormat :: !(Vary [FormatJson, FormatYaml])
  , mOutFile :: !(Maybe (File () Out))
  }
  deriving Show

data QueryProposalsCmdArgs era = QueryProposalsCmdArgs
  { eon :: !(ConwayEraOnwards era)
  , commons :: !QueryCommons
  , govActionIds :: !(AllOrOnly L.GovActionId)
  , outputFormat :: !(Vary [FormatJson, FormatYaml])
  , mOutFile :: !(Maybe (File () Out))
  }
  deriving Show

data QueryDRepStakeDistributionCmdArgs era = QueryDRepStakeDistributionCmdArgs
  { eon :: !(ConwayEraOnwards era)
  , commons :: !QueryCommons
  , drepHashSources :: !(AllOrOnly DRepHashSource)
  , outputFormat :: !(Vary [FormatJson, FormatYaml])
  , mOutFile :: !(Maybe (File () Out))
  }
  deriving Show

data QuerySPOStakeDistributionCmdArgs era = QuerySPOStakeDistributionCmdArgs
  { eon :: !(ConwayEraOnwards era)
  , commons :: !QueryCommons
  , spoHashSources :: !(AllOrOnly SPOHashSource)
  , outputFormat :: !(Vary [FormatJson, FormatYaml])
  , mOutFile :: !(Maybe (File () Out))
  }
  deriving Show

data QueryCommitteeMembersStateCmdArgs era = QueryCommitteeMembersStateCmdArgs
  { eon :: !(ConwayEraOnwards era)
  , commons :: !QueryCommons
  , committeeColdKeys :: ![VerificationKeyOrHashOrFileOrScriptHash CommitteeColdKey]
  , committeeHotKeys :: ![VerificationKeyOrHashOrFileOrScriptHash CommitteeHotKey]
  , memberStatuses :: ![MemberStatus]
  , outputFormat :: !(Vary [FormatJson, FormatYaml])
  , mOutFile :: !(Maybe (File () Out))
  }
  deriving Show

data QueryTreasuryValueCmdArgs era = QueryTreasuryValueCmdArgs
  { eon :: !(ConwayEraOnwards era)
  , commons :: !QueryCommons
  , mOutFile :: !(Maybe (File () Out))
  }
  deriving Show

data QueryStakePoolDefaultVoteCmdArgs era = QueryStakePoolDefaultVoteCmdArgs
  { eon :: !(ConwayEraOnwards era)
  , commons :: !QueryCommons
  , spoHashSources :: !SPOHashSource
  , outputFormat :: !(Vary [FormatJson, FormatYaml])
  , mOutFile :: !(Maybe (File () Out))
  }
  deriving Show

data QueryEraHistoryCmdArgs = QueryEraHistoryCmdArgs
  { commons :: !QueryCommons
  , mOutFile :: !(Maybe (File () Out))
  }
  deriving (Generic, Show)

renderQueryCmds :: QueryCmds era -> Text
renderQueryCmds = \case
  QueryLeadershipScheduleCmd{} ->
    "query leadership-schedule"
  QueryProtocolParametersCmd{} ->
    "query protocol-parameters "
  QueryTipCmd{} ->
    "query tip"
  QueryStakePoolsCmd{} ->
    "query stake-pools"
  QueryStakeDistributionCmd{} ->
    "query stake-distribution"
  QueryStakeAddressInfoCmd{} ->
    "query stake-address-info"
  QueryUTxOCmd{} ->
    "query utxo"
  QueryLedgerStateCmd{} ->
    "query ledger-state"
  QueryLedgerPeerSnapshotCmd{} ->
    "query ledger-peer-snapshot"
  QueryProtocolStateCmd{} ->
    "query protocol-state"
  QueryStakeSnapshotCmd{} ->
    "query stake-snapshot"
  QueryKesPeriodInfoCmd{} ->
    "query kes-period-info"
  QueryPoolStateCmd{} ->
    "query pool-state"
  QueryTxMempoolCmd (QueryTxMempoolCmdArgs _ q _ _) ->
    "query tx-mempool" <> renderTxMempoolQuery q
  QuerySlotNumberCmd{} ->
    "query slot-number"
  QueryRefScriptSizeCmd{} ->
    "query ref-script-size"
  QueryProposalsCmd{} ->
    "query proposals"
  QueryConstitutionCmd{} ->
    "constitution"
  QueryGovStateCmd{} ->
    "gov-state"
  QueryRatifyStateCmd{} ->
    "ratify-state"
  QueryFuturePParamsCmd{} ->
    "future-pparams"
  QueryDRepStateCmd{} ->
    "drep-state"
  QueryDRepStakeDistributionCmd{} ->
    "drep-stake-distribution"
  QuerySPOStakeDistributionCmd{} ->
    "spo-stake-distribution"
  QueryCommitteeMembersStateCmd{} ->
    "committee-state"
  QueryTreasuryValueCmd{} ->
    "treasury"
  QueryStakePoolDefaultVoteCmd{} ->
    "query stake-pool-default-vote"
  QueryEraHistoryCmd{} ->
    "query era-history"

renderTxMempoolQuery :: TxMempoolQuery -> Text
renderTxMempoolQuery = \case
  TxMempoolQueryTxExists tx -> "tx-exists " <> serialiseToRawBytesHexText tx
  TxMempoolQueryNextTx -> "next-tx"
  TxMempoolQueryInfo -> "info"
