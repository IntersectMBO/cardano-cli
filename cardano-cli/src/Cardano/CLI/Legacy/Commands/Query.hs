{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
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
  !SocketPath
  !AnyConsensusModeParams
  !NetworkId
  !GenesisFile
  !(VerificationKeyOrHashOrFile StakePoolKey)
  !(SigningKeyFile In)
  !EpochLeadershipSchedule
  !(Maybe (File () Out))
  deriving (Generic, Show)

data LegacyQueryProtocolParametersCmdArgs = LegacyQueryProtocolParametersCmdArgs
  !SocketPath
  !AnyConsensusModeParams
  !NetworkId
  !(Maybe (File () Out))
  deriving (Generic, Show)

data LegacyQueryConstitutionHashCmdArgs = LegacyQueryConstitutionHashCmdArgs
  !SocketPath
  !AnyConsensusModeParams
  !NetworkId
  !(Maybe (File () Out))
  deriving (Generic, Show)

data LegacyQueryTipCmdArgs = LegacyQueryTipCmdArgs
  !SocketPath
  !AnyConsensusModeParams
  !NetworkId
  !(Maybe (File () Out))
  deriving (Generic, Show)

data LegacyQueryStakePoolsCmdArgs = LegacyQueryStakePoolsCmdArgs
  !SocketPath
  !AnyConsensusModeParams
  !NetworkId
  !(Maybe (File () Out))
  deriving (Generic, Show)

data LegacyQueryStakeDistributionCmdArgs = LegacyQueryStakeDistributionCmdArgs
  !SocketPath
  !AnyConsensusModeParams
  !NetworkId
  !(Maybe (File () Out))
  deriving (Generic, Show)

data LegacyQueryStakeAddressInfoCmdArgs = LegacyQueryStakeAddressInfoCmdArgs
  !SocketPath
  !AnyConsensusModeParams
  !StakeAddress
  !NetworkId
  !(Maybe (File () Out))
  deriving (Generic, Show)

data LegacyQueryUTxOCmdArgs = LegacyQueryUTxOCmdArgs
  !SocketPath
  !AnyConsensusModeParams
  !QueryUTxOFilter
  !NetworkId
  !(Maybe (File () Out))
  deriving (Generic, Show)

data LegacyQueryLedgerStateCmdArgs = LegacyQueryLedgerStateCmdArgs
  !SocketPath
  !AnyConsensusModeParams
  !NetworkId
  !(Maybe (File () Out))
  deriving (Generic, Show)

data LegacyQueryProtocolStateCmdArgs = LegacyQueryProtocolStateCmdArgs
  !SocketPath
  !AnyConsensusModeParams
  !NetworkId
  !(Maybe (File () Out))
  deriving (Generic, Show)

data LegacyQueryStakeSnapshotCmdArgs = LegacyQueryStakeSnapshotCmdArgs
  !SocketPath
  !AnyConsensusModeParams
  !NetworkId
  !(AllOrOnly [Hash StakePoolKey])
  !(Maybe (File () Out))
  deriving (Generic, Show)

data LegacyQueryKesPeriodInfoCmdArgs = LegacyQueryKesPeriodInfoCmdArgs
  !SocketPath
  !AnyConsensusModeParams
  !NetworkId
  !(File () In)
  -- ^ Node operational certificate
  !(Maybe (File () Out))
  deriving (Generic, Show)

data LegacyQueryPoolStateCmdArgs = LegacyQueryPoolStateCmdArgs
  !SocketPath
  !AnyConsensusModeParams
  !NetworkId
  ![Hash StakePoolKey]
  deriving (Generic, Show)

data LegacyQueryTxMempoolCmdArgs = LegacyQueryTxMempoolCmdArgs
  !SocketPath
  !AnyConsensusModeParams
  !NetworkId
  !TxMempoolQuery
  !(Maybe (File () Out))
  deriving (Generic, Show)

data LegacyQuerySlotNumberCmdArgs = LegacyQuerySlotNumberCmdArgs
  !SocketPath
  !AnyConsensusModeParams
  !NetworkId
  !UTCTime
  deriving (Generic, Show)

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
