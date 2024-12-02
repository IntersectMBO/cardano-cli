{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.Commands.Query
  ( QueryCmds (..)
  , QueryCommons (..)
  , QueryCommitteeMembersStateCmdArgs (..)
  , QueryLeadershipScheduleCmdArgs (..)
  , QueryProtocolParametersCmdArgs (..)
  , QueryTipCmdArgs (..)
  , QueryStakePoolsCmdArgs (..)
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
  , renderQueryCmds
  , IncludeStake (..)
  )
where

import qualified Cardano.Api.Network as Consensus
import           Cardano.Api.Shelley hiding (QueryInShelleyBasedEra (..))

import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Key

import           Data.Set (Set)
import           Data.Text (Text)
import           Data.Time.Clock
import           GHC.Generics

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
  | QueryDRepStateCmd !(QueryDRepStateCmdArgs era)
  | QueryDRepStakeDistributionCmd !(QueryDRepStakeDistributionCmdArgs era)
  | QuerySPOStakeDistributionCmd !(QuerySPOStakeDistributionCmdArgs era)
  | QueryCommitteeMembersStateCmd !(QueryCommitteeMembersStateCmdArgs era)
  | QueryTreasuryValueCmd !(QueryTreasuryValueCmdArgs era)
  deriving (Generic, Show)

-- | Fields that are common to most queries
data QueryCommons = QueryCommons
  { nodeSocketPath :: !SocketPath
  , consensusModeParams :: !ConsensusModeParams
  , networkId :: !NetworkId
  , target :: !(Consensus.Target ChainPoint)
  }
  deriving (Generic, Show)

data QueryLeadershipScheduleCmdArgs = QueryLeadershipScheduleCmdArgs
  { commons :: !QueryCommons
  , genesisFp :: !GenesisFile
  , poolColdVerKeyFile :: !(VerificationKeyOrHashOrFile StakePoolKey)
  , vrkSkeyFp :: !(SigningKeyFile In)
  , whichSchedule :: !EpochLeadershipSchedule
  , format :: Maybe OutputFormatJsonOrText
  , mOutFile :: !(Maybe (File () Out))
  }
  deriving (Generic, Show)

data QueryProtocolParametersCmdArgs = QueryProtocolParametersCmdArgs
  { nodeSocketPath :: !SocketPath
  , consensusModeParams :: !ConsensusModeParams
  , networkId :: !NetworkId
  , mOutFile :: !(Maybe (File () Out))
  }
  deriving (Generic, Show)

data QueryTipCmdArgs = QueryTipCmdArgs
  { commons :: !QueryCommons
  , mOutFile :: !(Maybe (File () Out))
  }
  deriving (Generic, Show)

data QueryStakePoolsCmdArgs = QueryStakePoolsCmdArgs
  { commons :: !QueryCommons
  , format :: Maybe OutputFormatJsonOrText
  , mOutFile :: !(Maybe (File () Out))
  }
  deriving (Generic, Show)

data QueryStakeDistributionCmdArgs = QueryStakeDistributionCmdArgs
  { commons :: !QueryCommons
  , format :: Maybe OutputFormatJsonOrText
  , mOutFile :: !(Maybe (File () Out))
  }
  deriving (Generic, Show)

data QueryStakeAddressInfoCmdArgs = QueryStakeAddressInfoCmdArgs
  { commons :: !QueryCommons
  , addr :: !StakeAddress
  , mOutFile :: !(Maybe (File () Out))
  }
  deriving (Generic, Show)

data QueryUTxOCmdArgs = QueryUTxOCmdArgs
  { commons :: !QueryCommons
  , queryFilter :: !QueryUTxOFilter
  , format :: Maybe OutputFormatJsonOrText
  , mOutFile :: !(Maybe (File () Out))
  }
  deriving (Generic, Show)

data QueryLedgerStateCmdArgs = QueryLedgerStateCmdArgs
  { commons :: !QueryCommons
  , mOutFile :: !(Maybe (File () Out))
  }
  deriving (Generic, Show)

data QueryProtocolStateCmdArgs = QueryProtocolStateCmdArgs
  { commons :: !QueryCommons
  , mOutFile :: !(Maybe (File () Out))
  }
  deriving (Generic, Show)

data QueryStakeSnapshotCmdArgs = QueryStakeSnapshotCmdArgs
  { commons :: !QueryCommons
  , allOrOnlyPoolIds :: !(AllOrOnly (Hash StakePoolKey))
  , mOutFile :: !(Maybe (File () Out))
  }
  deriving (Generic, Show)

data QueryKesPeriodInfoCmdArgs = QueryKesPeriodInfoCmdArgs
  { commons :: !QueryCommons
  , nodeOpCertFp :: !(File () In)
  -- ^ Node operational certificate
  , mOutFile :: !(Maybe (File () Out))
  }
  deriving (Generic, Show)

data QueryPoolStateCmdArgs = QueryPoolStateCmdArgs
  { commons :: !QueryCommons
  , allOrOnlyPoolIds :: !(AllOrOnly (Hash StakePoolKey))
  , mOutFile :: !(Maybe (File () Out))
  }
  deriving (Generic, Show)

data QueryTxMempoolCmdArgs = QueryTxMempoolCmdArgs
  { nodeSocketPath :: !SocketPath
  , consensusModeParams :: !ConsensusModeParams
  , networkId :: !NetworkId
  , query :: !TxMempoolQuery
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
  , format :: Maybe OutputFormatJsonOrText
  , mOutFile :: !(Maybe (File () Out))
  }
  deriving (Generic, Show)

data QueryNoArgCmdArgs era = QueryNoArgCmdArgs
  { eon :: !(ConwayEraOnwards era)
  , commons :: !QueryCommons
  , mOutFile :: !(Maybe (File () Out))
  }
  deriving Show

data QueryDRepStateCmdArgs era = QueryDRepStateCmdArgs
  { eon :: !(ConwayEraOnwards era)
  , commons :: !QueryCommons
  , drepHashSources :: !(AllOrOnly DRepHashSource)
  , includeStake :: !IncludeStake
  , mOutFile :: !(Maybe (File () Out))
  }
  deriving Show

data QueryDRepStakeDistributionCmdArgs era = QueryDRepStakeDistributionCmdArgs
  { eon :: !(ConwayEraOnwards era)
  , commons :: !QueryCommons
  , drepHashSources :: !(AllOrOnly DRepHashSource)
  , mOutFile :: !(Maybe (File () Out))
  }
  deriving Show

data QuerySPOStakeDistributionCmdArgs era = QuerySPOStakeDistributionCmdArgs
  { eon :: !(ConwayEraOnwards era)
  , commons :: !QueryCommons
  , spoHashSources :: !(AllOrOnly SPOHashSource)
  , mOutFile :: !(Maybe (File () Out))
  }
  deriving Show

data QueryCommitteeMembersStateCmdArgs era = QueryCommitteeMembersStateCmdArgs
  { eon :: !(ConwayEraOnwards era)
  , commons :: !QueryCommons
  , committeeColdKeys :: ![VerificationKeyOrHashOrFileOrScriptHash CommitteeColdKey]
  , committeeHotKeys :: ![VerificationKeyOrHashOrFileOrScriptHash CommitteeHotKey]
  , memberStatuses :: ![MemberStatus]
  , mOutFile :: !(Maybe (File () Out))
  }
  deriving Show

data QueryTreasuryValueCmdArgs era = QueryTreasuryValueCmdArgs
  { eon :: !(ConwayEraOnwards era)
  , commons :: !QueryCommons
  , mOutFile :: !(Maybe (File () Out))
  }
  deriving Show

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
  QueryProtocolStateCmd{} ->
    "query protocol-state"
  QueryStakeSnapshotCmd{} ->
    "query stake-snapshot"
  QueryKesPeriodInfoCmd{} ->
    "query kes-period-info"
  QueryPoolStateCmd{} ->
    "query pool-state"
  QueryTxMempoolCmd (QueryTxMempoolCmdArgs _ _ _ q _) ->
    "query tx-mempool" <> renderTxMempoolQuery q
  QuerySlotNumberCmd{} ->
    "query slot-number"
  QueryRefScriptSizeCmd{} ->
    "query ref-script-size"
  QueryConstitutionCmd{} ->
    "constitution"
  QueryGovStateCmd{} ->
    "gov-state"
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

renderTxMempoolQuery :: TxMempoolQuery -> Text
renderTxMempoolQuery = \case
  TxMempoolQueryTxExists tx -> "tx-exists " <> serialiseToRawBytesHexText tx
  TxMempoolQueryNextTx -> "next-tx"
  TxMempoolQueryInfo -> "info"
