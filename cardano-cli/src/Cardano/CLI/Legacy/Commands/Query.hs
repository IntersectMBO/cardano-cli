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
  ) where

import           Cardano.Api.Shelley hiding (QueryInShelleyBasedEra (..))

import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Key

import           Data.Text (Text)
import           Data.Time.Clock
import           GHC.Generics

data LegacyQueryCmds
  = QueryLeadershipScheduleCmd  !LegacyQueryLeadershipScheduleCmdArgs
  | QueryProtocolParametersCmd  !LegacyQueryProtocolParametersCmdArgs
  | QueryConstitutionHashCmd    !LegacyQueryConstitutionHashCmdArgs
  | QueryTipCmd                 !LegacyQueryTipCmdArgs
  | QueryStakePoolsCmd          !LegacyQueryStakePoolsCmdArgs
  | QueryStakeDistributionCmd   !LegacyQueryStakeDistributionCmdArgs
  | QueryStakeAddressInfoCmd    !LegacyQueryStakeAddressInfoCmdArgs
  | QueryUTxOCmd                !LegacyQueryUTxOCmdArgs
  | QueryLedgerStateCmd         !LegacyQueryLedgerStateCmdArgs
  | QueryProtocolStateCmd       !LegacyQueryProtocolStateCmdArgs
  | QueryStakeSnapshotCmd       !LegacyQueryStakeSnapshotCmdArgs
  | QueryKesPeriodInfoCmd       !LegacyQueryKesPeriodInfoCmdArgs
  | QueryPoolStateCmd           !LegacyQueryPoolStateCmdArgs
  | QueryTxMempoolCmd           !LegacyQueryTxMempoolCmdArgs
  | QuerySlotNumberCmd          !LegacyQuerySlotNumberCmdArgs
  deriving (Generic, Show)

data LegacyQueryLeadershipScheduleCmdArgs = LegacyQueryLeadershipScheduleCmdArgs
  { nodeSocketPath      :: !SocketPath
  , consensusModeParams :: !AnyConsensusModeParams
  , network             :: !NetworkId
  , genesisFp           :: !GenesisFile
  , poolId              :: !(VerificationKeyOrHashOrFile StakePoolKey)
  , vrkSkeyFp           :: !(SigningKeyFile In)
  , whichSchedule       :: !EpochLeadershipSchedule
  , mOutFile            :: !(Maybe (File () Out))
  } deriving (Generic, Show)

data LegacyQueryProtocolParametersCmdArgs = LegacyQueryProtocolParametersCmdArgs
  { nodeSocketPath      :: !SocketPath
  , consensusModeParams :: !AnyConsensusModeParams
  , network             :: !NetworkId
  , mOutFile            :: !(Maybe (File () Out))
  } deriving (Generic, Show)

data LegacyQueryConstitutionHashCmdArgs = LegacyQueryConstitutionHashCmdArgs
  { nodeSocketPath      :: !SocketPath
  , consensusModeParams :: !AnyConsensusModeParams
  , network             :: !NetworkId
  , mOutFile            :: !(Maybe (File () Out))
  } deriving (Generic, Show)

data LegacyQueryTipCmdArgs = LegacyQueryTipCmdArgs
  { nodeSocketPath      :: !SocketPath
  , consensusModeParams :: !AnyConsensusModeParams
  , network             :: !NetworkId
  , mOutFile            :: !(Maybe (File () Out))
  } deriving (Generic, Show)

data LegacyQueryStakePoolsCmdArgs = LegacyQueryStakePoolsCmdArgs
  { nodeSocketPath      :: !SocketPath
  , consensusModeParams :: !AnyConsensusModeParams
  , network             :: !NetworkId
  , mOutFile            :: !(Maybe (File () Out))
  } deriving (Generic, Show)

data LegacyQueryStakeDistributionCmdArgs = LegacyQueryStakeDistributionCmdArgs
  { nodeSocketPath      :: !SocketPath
  , consensusModeParams :: !AnyConsensusModeParams
  , network             :: !NetworkId
  , mOutFile            :: !(Maybe (File () Out))
  } deriving (Generic, Show)

data LegacyQueryStakeAddressInfoCmdArgs = LegacyQueryStakeAddressInfoCmdArgs
  { mNodeSocketPath     :: !SocketPath
  , consensusModeParams :: !AnyConsensusModeParams
  , addr                :: !StakeAddress
  , network             :: !NetworkId
  , mOutFile            :: !(Maybe (File () Out))
  } deriving (Generic, Show)

data LegacyQueryUTxOCmdArgs = LegacyQueryUTxOCmdArgs
  { nodeSocketPath      :: !SocketPath
  , consensusModeParams :: !AnyConsensusModeParams
  , queryFilter         :: !QueryUTxOFilter
  , networkId           :: !NetworkId
  , mOutFile            :: !(Maybe (File () Out))
  } deriving (Generic, Show)

data LegacyQueryLedgerStateCmdArgs = LegacyQueryLedgerStateCmdArgs
  { nodeSocketPath      :: !SocketPath
  , consensusModeParams :: !AnyConsensusModeParams
  , network             :: !NetworkId
  , mOutFile            :: !(Maybe (File () Out))
  } deriving (Generic, Show)

data LegacyQueryProtocolStateCmdArgs = LegacyQueryProtocolStateCmdArgs
  { nodeSocketPath      :: !SocketPath
  , consensusModeParams :: !AnyConsensusModeParams
  , network             :: !NetworkId
  , mOutFile            :: !(Maybe (File () Out))
  } deriving (Generic, Show)

data LegacyQueryStakeSnapshotCmdArgs = LegacyQueryStakeSnapshotCmdArgs
  { nodeSocketPath      :: !SocketPath
  , consensusModeParams :: !AnyConsensusModeParams
  , network             :: !NetworkId
  , allOrOnlyPoolIds    :: !(AllOrOnly [Hash StakePoolKey])
  , mOutFile            :: !(Maybe (File () Out))
  } deriving (Generic, Show)

data LegacyQueryKesPeriodInfoCmdArgs = LegacyQueryKesPeriodInfoCmdArgs
  { nodeSocketPath      :: !SocketPath
  , consensusModeParams :: !AnyConsensusModeParams
  , network             :: !NetworkId
  , nodeOpCert          :: !(File () In) -- ^ Node operational certificate
  , mOutFile            :: !(Maybe (File () Out))
  } deriving (Generic, Show)

data LegacyQueryPoolStateCmdArgs = LegacyQueryPoolStateCmdArgs
  { nodeSocketPath      :: !SocketPath
  , consensusModeParams :: !AnyConsensusModeParams
  , network             :: !NetworkId
  , poolid              :: ![Hash StakePoolKey]
  } deriving (Generic, Show)

data LegacyQueryTxMempoolCmdArgs = LegacyQueryTxMempoolCmdArgs
  { nodeSocketPath      :: !SocketPath
  , consensusModeParams :: !AnyConsensusModeParams
  , network             :: !NetworkId
  , op                  :: !TxMempoolQuery
  , mOutFile            :: !(Maybe (File () Out))
  }
  deriving (Generic, Show)

data LegacyQuerySlotNumberCmdArgs = LegacyQuerySlotNumberCmdArgs
  { nodeSocketPath      :: !SocketPath
  , consensusModeParams :: !AnyConsensusModeParams
  , network             :: !NetworkId
  , utcTime             :: !UTCTime
  } deriving (Generic, Show)

renderLegacyQueryCmds :: LegacyQueryCmds -> Text
renderLegacyQueryCmds = \case
  QueryLeadershipScheduleCmd {} -> "query leadership-schedule"
  QueryProtocolParametersCmd {} -> "query protocol-parameters "
  QueryConstitutionHashCmd {} -> "query constitution-hash "
  QueryTipCmd {} -> "query tip"
  QueryStakePoolsCmd {} -> "query stake-pools"
  QueryStakeDistributionCmd {} -> "query stake-distribution"
  QueryStakeAddressInfoCmd {} -> "query stake-address-info"
  QueryUTxOCmd {} -> "query utxo"
  QueryLedgerStateCmd {} -> "query ledger-state"
  QueryProtocolStateCmd {} -> "query protocol-state"
  QueryStakeSnapshotCmd {} -> "query stake-snapshot"
  QueryKesPeriodInfoCmd {} -> "query kes-period-info"
  QueryPoolStateCmd {} -> "query pool-state"
  QueryTxMempoolCmd (LegacyQueryTxMempoolCmdArgs _ _ _ query _) -> "query tx-mempool" <> renderTxMempoolQuery query
  QuerySlotNumberCmd {} -> "query slot-number"
  where
    renderTxMempoolQuery query =
      case query of
        TxMempoolQueryTxExists tx -> "tx-exists " <> serialiseToRawBytesHexText tx
        TxMempoolQueryNextTx -> "next-tx"
        TxMempoolQueryInfo -> "info"
