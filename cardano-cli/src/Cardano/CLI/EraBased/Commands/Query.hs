{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.Commands.Query
  ( QueryCmds (..)
  , QueryLeadershipScheduleCmdArgs(..)
  , QueryProtocolParametersCmdArgs(..)
  , QueryConstitutionHashCmdArgs(..)
  , QueryTipCmdArgs(..)
  , QueryStakePoolsCmdArgs(..)
  , QueryStakeDistributionCmdArgs(..)
  , QueryStakeAddressInfoCmdArgs(..)
  , QueryUTxOCmdArgs(..)
  , QueryDebugLedgerStateCmdArgs(..)
  , QueryProtocolStateCmdArgs(..)
  , QueryStakeSnapshotCmdArgs(..)
  , QueryKesPeriodInfoCmdArgs(..)
  , QueryPoolStateCmdArgs(..)
  , QueryTxMempoolCmdArgs(..)
  , QuerySlotNumberCmdArgs(..)
  , renderQueryCmds
  ) where

import           Cardano.Api.Shelley hiding (QueryInShelleyBasedEra (..))

import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Key

import           Data.Text (Text)
import           Data.Time.Clock
import           GHC.Generics

data QueryCmds era
  = QueryLeadershipScheduleCmd  !QueryLeadershipScheduleCmdArgs
  | QueryProtocolParametersCmd  !QueryProtocolParametersCmdArgs
  | QueryConstitutionHashCmd    !QueryConstitutionHashCmdArgs
  | QueryTipCmd                 !QueryTipCmdArgs
  | QueryStakePoolsCmd          !QueryStakePoolsCmdArgs
  | QueryStakeDistributionCmd   !QueryStakeDistributionCmdArgs
  | QueryStakeAddressInfoCmd    !QueryStakeAddressInfoCmdArgs
  | QueryUTxOCmd                !QueryUTxOCmdArgs
  | QueryDebugLedgerStateCmd    !QueryDebugLedgerStateCmdArgs
  | QueryProtocolStateCmd       !QueryProtocolStateCmdArgs
  | QueryStakeSnapshotCmd       !QueryStakeSnapshotCmdArgs
  | QueryKesPeriodInfoCmd       !QueryKesPeriodInfoCmdArgs
  | QueryPoolStateCmd           !QueryPoolStateCmdArgs
  | QueryTxMempoolCmd           !QueryTxMempoolCmdArgs
  | QuerySlotNumberCmd          !QuerySlotNumberCmdArgs
  deriving (Generic, Show)

data QueryLeadershipScheduleCmdArgs = QueryLeadershipScheduleCmdArgs
  !SocketPath
  !AnyConsensusModeParams
  !NetworkId
  !GenesisFile
  !(VerificationKeyOrHashOrFile StakePoolKey)
  !(SigningKeyFile In)
  !EpochLeadershipSchedule
  !(Maybe (File () Out))
  deriving (Generic, Show)

data QueryProtocolParametersCmdArgs = QueryProtocolParametersCmdArgs
  !SocketPath
  !AnyConsensusModeParams
  !NetworkId
  !(Maybe (File () Out))
  deriving (Generic, Show)

data QueryConstitutionHashCmdArgs = QueryConstitutionHashCmdArgs
  !SocketPath
  !AnyConsensusModeParams
  !NetworkId
  !(Maybe (File () Out))
  deriving (Generic, Show)

data QueryTipCmdArgs = QueryTipCmdArgs
  !SocketPath
  !AnyConsensusModeParams
  !NetworkId
  !(Maybe (File () Out))
  deriving (Generic, Show)

data QueryStakePoolsCmdArgs = QueryStakePoolsCmdArgs
  !SocketPath
  !AnyConsensusModeParams
  !NetworkId
  !(Maybe (File () Out))
  deriving (Generic, Show)

data QueryStakeDistributionCmdArgs = QueryStakeDistributionCmdArgs
  !SocketPath
  !AnyConsensusModeParams
  !NetworkId
  !(Maybe (File () Out))
  deriving (Generic, Show)

data QueryStakeAddressInfoCmdArgs = QueryStakeAddressInfoCmdArgs
  !SocketPath
  !AnyConsensusModeParams
  !StakeAddress
  !NetworkId
  !(Maybe (File () Out))
  deriving (Generic, Show)

data QueryUTxOCmdArgs = QueryUTxOCmdArgs
  !SocketPath
  !AnyConsensusModeParams
  !QueryUTxOFilter
  !NetworkId
  !(Maybe (File () Out))
  deriving (Generic, Show)

data QueryDebugLedgerStateCmdArgs = QueryDebugLedgerStateCmdArgs
  !SocketPath
  !AnyConsensusModeParams
  !NetworkId
  !(Maybe (File () Out))
  deriving (Generic, Show)

data QueryProtocolStateCmdArgs = QueryProtocolStateCmdArgs
  !SocketPath
  !AnyConsensusModeParams
  !NetworkId
  !(Maybe (File () Out))
  deriving (Generic, Show)

data QueryStakeSnapshotCmdArgs = QueryStakeSnapshotCmdArgs
  !SocketPath
  !AnyConsensusModeParams
  !NetworkId
  !(AllOrOnly [Hash StakePoolKey])
  !(Maybe (File () Out))
  deriving (Generic, Show)

data QueryKesPeriodInfoCmdArgs = QueryKesPeriodInfoCmdArgs
  !SocketPath
  !AnyConsensusModeParams
  !NetworkId
  !(File () In)
  -- ^ Node operational certificate
  !(Maybe (File () Out))
  deriving (Generic, Show)

data QueryPoolStateCmdArgs = QueryPoolStateCmdArgs
  !SocketPath
  !AnyConsensusModeParams
  !NetworkId
  ![Hash StakePoolKey]
  deriving (Generic, Show)

data QueryTxMempoolCmdArgs = QueryTxMempoolCmdArgs
  !SocketPath
  !AnyConsensusModeParams
  !NetworkId
  !TxMempoolQuery
  !(Maybe (File () Out))
  deriving (Generic, Show)

data QuerySlotNumberCmdArgs = QuerySlotNumberCmdArgs
  !SocketPath
  !AnyConsensusModeParams
  !NetworkId
  !UTCTime
  deriving (Generic, Show)

renderQueryCmds :: QueryCmds era -> Text
renderQueryCmds = \case
  QueryLeadershipScheduleCmd {} ->
    "query leadership-schedule"
  QueryProtocolParametersCmd {} ->
    "query protocol-parameters "
  QueryConstitutionHashCmd {} ->
    "query constitution-hash "
  QueryTipCmd {} ->
    "query tip"
  QueryStakePoolsCmd {} ->
    "query stake-pools"
  QueryStakeDistributionCmd {} ->
    "query stake-distribution"
  QueryStakeAddressInfoCmd {} ->
    "query stake-address-info"
  QueryUTxOCmd {} ->
    "query utxo"
  QueryDebugLedgerStateCmd {} ->
    "query ledger-state"
  QueryProtocolStateCmd {} ->
    "query protocol-state"
  QueryStakeSnapshotCmd {} ->
    "query stake-snapshot"
  QueryKesPeriodInfoCmd {} ->
    "query kes-period-info"
  QueryPoolStateCmd {} ->
    "query pool-state"
  QueryTxMempoolCmd (QueryTxMempoolCmdArgs _ _ _ query _) ->
    "query tx-mempool" <> renderTxMempoolQuery query
  QuerySlotNumberCmd {} ->
    "query slot-number"

renderTxMempoolQuery :: TxMempoolQuery -> Text
renderTxMempoolQuery = \case
  TxMempoolQueryTxExists tx -> "tx-exists " <> serialiseToRawBytesHexText tx
  TxMempoolQueryNextTx -> "next-tx"
  TxMempoolQueryInfo -> "info"
