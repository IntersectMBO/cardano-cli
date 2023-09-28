{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.Commands.Query
  ( QueryCmds (..)
  , renderQueryCmds
  ) where

import           Cardano.Api.Shelley hiding (QueryInShelleyBasedEra (..))

import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Key

import           Data.Text (Text)
import           Data.Time.Clock

data QueryCmds era
  = QueryLeadershipScheduleCmd
      SocketPath
      AnyConsensusModeParams
      NetworkId
      GenesisFile
      (VerificationKeyOrHashOrFile StakePoolKey)
      (SigningKeyFile In)
      EpochLeadershipSchedule
      (Maybe (File () Out))
  | QueryProtocolParametersCmd
      SocketPath
      AnyConsensusModeParams
      NetworkId
      (Maybe (File () Out))
  | QueryConstitutionHashCmd
      SocketPath
      AnyConsensusModeParams
      NetworkId
      (Maybe (File () Out))
  | QueryTipCmd
      SocketPath
      AnyConsensusModeParams
      NetworkId
      (Maybe (File () Out))
  | QueryStakePoolsCmd
      SocketPath
      AnyConsensusModeParams
      NetworkId
      (Maybe (File () Out))
  | QueryStakeDistributionCmd
      SocketPath
      AnyConsensusModeParams
      NetworkId
      (Maybe (File () Out))
  | QueryStakeAddressInfoCmd
      SocketPath
      AnyConsensusModeParams
      StakeAddress
      NetworkId
      (Maybe (File () Out))
  | QueryUTxOCmd
      SocketPath
      AnyConsensusModeParams
      QueryUTxOFilter
      NetworkId
      (Maybe (File () Out))
  | QueryDebugLedgerStateCmd
      SocketPath
      AnyConsensusModeParams
      NetworkId
      (Maybe (File () Out))
  | QueryProtocolStateCmd
      SocketPath
      AnyConsensusModeParams
      NetworkId
      (Maybe (File () Out))
  | QueryStakeSnapshotCmd
      SocketPath
      AnyConsensusModeParams
      NetworkId
      (AllOrOnly [Hash StakePoolKey])
      (Maybe (File () Out))
  | QueryKesPeriodInfoCmd
      SocketPath
      AnyConsensusModeParams
      NetworkId
      (File () In)
      -- ^ Node operational certificate
      (Maybe (File () Out))
  | QueryPoolStateCmd
      SocketPath
      AnyConsensusModeParams
      NetworkId
      [Hash StakePoolKey]
  | QueryTxMempoolCmd
      SocketPath
      AnyConsensusModeParams
      NetworkId
      TxMempoolQuery
      (Maybe (File () Out))
  | QuerySlotNumberCmd
      SocketPath
      AnyConsensusModeParams
      NetworkId
      UTCTime
  deriving Show

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
  QueryTxMempoolCmd _ _ _ query _ ->
    "query tx-mempool" <> renderTxMempoolQuery query
  QuerySlotNumberCmd {} ->
    "query slot-number"

renderTxMempoolQuery :: TxMempoolQuery -> Text
renderTxMempoolQuery = \case
  TxMempoolQueryTxExists tx -> "tx-exists " <> serialiseToRawBytesHexText tx
  TxMempoolQueryNextTx -> "next-tx"
  TxMempoolQueryInfo -> "info"
