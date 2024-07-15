{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Legacy.Commands.Query
  ( LegacyQueryCmds (..)
  , LegacyQueryLeadershipScheduleCmdArgs (..)
  , LegacyQueryProtocolParametersCmdArgs (..)
  , LegacyQueryConstitutionHashCmdArgs (..)
  , LegacyQueryTipCmdArgs (..)
  , LegacyQueryStakePoolsCmdArgs (..)
  , LegacyQueryStakeDistributionCmdArgs (..)
  , LegacyQueryStakeAddressInfoCmdArgs (..)
  , LegacyQueryUTxOCmdArgs (..)
  , LegacyQueryLedgerStateCmdArgs (..)
  , LegacyQueryProtocolStateCmdArgs (..)
  , LegacyQueryStakeSnapshotCmdArgs (..)
  , LegacyQueryKesPeriodInfoCmdArgs (..)
  , LegacyQueryPoolStateCmdArgs (..)
  , LegacyQueryTxMempoolCmdArgs (..)
  , LegacyQuerySlotNumberCmdArgs (..)
  , renderLegacyQueryCmds
  )
where

import           Cardano.Api.Shelley hiding (QueryInShelleyBasedEra (..))

import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Key

import           Data.Text (Text)
import           Data.Time.Clock
import           GHC.Generics

data LegacyQueryCmds
  = QueryLeadershipScheduleCmd !LegacyQueryLeadershipScheduleCmdArgs
  | QueryProtocolParametersCmd !LegacyQueryProtocolParametersCmdArgs
  | QueryTipCmd !LegacyQueryTipCmdArgs
  | QueryStakePoolsCmd !LegacyQueryStakePoolsCmdArgs
  | QueryStakeDistributionCmd !LegacyQueryStakeDistributionCmdArgs
  | QueryStakeAddressInfoCmd !LegacyQueryStakeAddressInfoCmdArgs
  | QueryUTxOCmd !LegacyQueryUTxOCmdArgs
  | QueryLedgerStateCmd !LegacyQueryLedgerStateCmdArgs
  | QueryProtocolStateCmd !LegacyQueryProtocolStateCmdArgs
  | QueryStakeSnapshotCmd !LegacyQueryStakeSnapshotCmdArgs
  | QueryKesPeriodInfoCmd !LegacyQueryKesPeriodInfoCmdArgs
  | QueryPoolStateCmd !LegacyQueryPoolStateCmdArgs
  | QueryTxMempoolCmd !LegacyQueryTxMempoolCmdArgs
  | QuerySlotNumberCmd !LegacyQuerySlotNumberCmdArgs
  deriving (Generic, Show)

data LegacyQueryLeadershipScheduleCmdArgs = LegacyQueryLeadershipScheduleCmdArgs
  { nodeSocketPath :: !SocketPath
  , consensusModeParams :: !ConsensusModeParams
  , networkId :: !NetworkId
  , genesisFp :: !GenesisFile
  , poolColdVerKeyFile :: !(VerificationKeyOrHashOrFile StakePoolKey)
  , vrkSkeyFp :: !(SigningKeyFile In)
  , whichSchedule :: !EpochLeadershipSchedule
  , format :: Maybe OutputFormatJsonOrText
  , mOutFile :: !(Maybe (File () Out))
  }
  deriving (Generic, Show)

data LegacyQueryProtocolParametersCmdArgs = LegacyQueryProtocolParametersCmdArgs
  { nodeSocketPath :: !SocketPath
  , consensusModeParams :: !ConsensusModeParams
  , networkId :: !NetworkId
  , mOutFile :: !(Maybe (File () Out))
  }
  deriving (Generic, Show)

data LegacyQueryConstitutionHashCmdArgs = LegacyQueryConstitutionHashCmdArgs
  { nodeSocketPath :: !SocketPath
  , consensusModeParams :: !ConsensusModeParams
  , networkId :: !NetworkId
  , mOutFile :: !(Maybe (File () Out))
  }
  deriving (Generic, Show)

data LegacyQueryTipCmdArgs = LegacyQueryTipCmdArgs
  { nodeSocketPath :: !SocketPath
  , consensusModeParams :: !ConsensusModeParams
  , networkId :: !NetworkId
  , mOutFile :: !(Maybe (File () Out))
  }
  deriving (Generic, Show)

data LegacyQueryStakePoolsCmdArgs = LegacyQueryStakePoolsCmdArgs
  { nodeSocketPath :: !SocketPath
  , consensusModeParams :: !ConsensusModeParams
  , networkId :: !NetworkId
  , format :: Maybe OutputFormatJsonOrText
  , mOutFile :: !(Maybe (File () Out))
  }
  deriving (Generic, Show)

data LegacyQueryStakeDistributionCmdArgs = LegacyQueryStakeDistributionCmdArgs
  { nodeSocketPath :: !SocketPath
  , consensusModeParams :: !ConsensusModeParams
  , networkId :: !NetworkId
  , format :: Maybe OutputFormatJsonOrText
  , mOutFile :: !(Maybe (File () Out))
  }
  deriving (Generic, Show)

data LegacyQueryStakeAddressInfoCmdArgs = LegacyQueryStakeAddressInfoCmdArgs
  { nodeSocketPath :: !SocketPath
  , consensusModeParams :: !ConsensusModeParams
  , addr :: !StakeAddress
  , networkId :: !NetworkId
  , mOutFile :: !(Maybe (File () Out))
  }
  deriving (Generic, Show)

data LegacyQueryUTxOCmdArgs = LegacyQueryUTxOCmdArgs
  { nodeSocketPath :: !SocketPath
  , consensusModeParams :: !ConsensusModeParams
  , queryFilter :: !QueryUTxOFilter
  , networkId :: !NetworkId
  , format :: Maybe OutputFormatJsonOrText
  , mOutFile :: !(Maybe (File () Out))
  }
  deriving (Generic, Show)

data LegacyQueryLedgerStateCmdArgs = LegacyQueryLedgerStateCmdArgs
  { nodeSocketPath :: !SocketPath
  , consensusModeParams :: !ConsensusModeParams
  , networkId :: !NetworkId
  , mOutFile :: !(Maybe (File () Out))
  }
  deriving (Generic, Show)

data LegacyQueryProtocolStateCmdArgs = LegacyQueryProtocolStateCmdArgs
  { nodeSocketPath :: !SocketPath
  , consensusModeParams :: !ConsensusModeParams
  , networkId :: !NetworkId
  , mOutFile :: !(Maybe (File () Out))
  }
  deriving (Generic, Show)

data LegacyQueryStakeSnapshotCmdArgs = LegacyQueryStakeSnapshotCmdArgs
  { nodeSocketPath :: !SocketPath
  , consensusModeParams :: !ConsensusModeParams
  , networkId :: !NetworkId
  , allOrOnlyPoolIds :: !(AllOrOnly (Hash StakePoolKey))
  , mOutFile :: !(Maybe (File () Out))
  }
  deriving (Generic, Show)

data LegacyQueryKesPeriodInfoCmdArgs = LegacyQueryKesPeriodInfoCmdArgs
  { nodeSocketPath :: !SocketPath
  , consensusModeParams :: !ConsensusModeParams
  , networkId :: !NetworkId
  , nodeOpCertFp :: !(File () In)
  -- ^ Node operational certificate
  , mOutFile :: !(Maybe (File () Out))
  }
  deriving (Generic, Show)

data LegacyQueryPoolStateCmdArgs = LegacyQueryPoolStateCmdArgs
  { nodeSocketPath :: !SocketPath
  , consensusModeParams :: !ConsensusModeParams
  , networkId :: !NetworkId
  , allOrOnlyPoolIds :: !(AllOrOnly (Hash StakePoolKey))
  , mOutFile :: !(Maybe (File () Out))
  }
  deriving (Generic, Show)

data LegacyQueryTxMempoolCmdArgs = LegacyQueryTxMempoolCmdArgs
  { nodeSocketPath :: !SocketPath
  , consensusModeParams :: !ConsensusModeParams
  , networkId :: !NetworkId
  , query :: !TxMempoolQuery
  , mOutFile :: !(Maybe (File () Out))
  }
  deriving (Generic, Show)

data LegacyQuerySlotNumberCmdArgs = LegacyQuerySlotNumberCmdArgs
  { nodeSocketPath :: !SocketPath
  , consensusModeParams :: !ConsensusModeParams
  , networkId :: !NetworkId
  , utcTime :: !UTCTime
  }
  deriving (Generic, Show)

renderLegacyQueryCmds :: LegacyQueryCmds -> Text
renderLegacyQueryCmds = \case
  QueryLeadershipScheduleCmd{} -> "query leadership-schedule"
  QueryProtocolParametersCmd{} -> "query protocol-parameters "
  QueryTipCmd{} -> "query tip"
  QueryStakePoolsCmd{} -> "query stake-pools"
  QueryStakeDistributionCmd{} -> "query stake-distribution"
  QueryStakeAddressInfoCmd{} -> "query stake-address-info"
  QueryUTxOCmd{} -> "query utxo"
  QueryLedgerStateCmd{} -> "query ledger-state"
  QueryProtocolStateCmd{} -> "query protocol-state"
  QueryStakeSnapshotCmd{} -> "query stake-snapshot"
  QueryKesPeriodInfoCmd{} -> "query kes-period-info"
  QueryPoolStateCmd{} -> "query pool-state"
  QueryTxMempoolCmd (LegacyQueryTxMempoolCmdArgs _ _ _ txMempoolQuery _) -> "query tx-mempool" <> renderTxMempoolQuery txMempoolQuery
  QuerySlotNumberCmd{} -> "query slot-number"
 where
  renderTxMempoolQuery = \case
    TxMempoolQueryTxExists tx -> "tx-exists " <> serialiseToRawBytesHexText tx
    TxMempoolQueryNextTx -> "next-tx"
    TxMempoolQueryInfo -> "info"
