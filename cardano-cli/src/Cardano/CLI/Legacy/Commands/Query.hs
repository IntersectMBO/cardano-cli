{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Legacy.Commands.Query
  ( LegacyQueryCmds (..)
  , renderLegacyQueryCmds
  ) where

import Cardano.Api.Shelley hiding (QueryInShelleyBasedEra (..))

import Cardano.CLI.Types.Common
import Cardano.CLI.Types.Key

import Data.Text (Text)
import Data.Time.Clock

data LegacyQueryCmds
  = QueryLeadershipSchedule
      SocketPath
      AnyConsensusModeParams
      NetworkId
      GenesisFile
      (VerificationKeyOrHashOrFile StakePoolKey)
      (SigningKeyFile In)
      EpochLeadershipSchedule
      (Maybe (File () Out))
  | QueryProtocolParameters'
      SocketPath
      AnyConsensusModeParams
      NetworkId
      (Maybe (File () Out))
  | QueryConstitutionHash
      SocketPath
      AnyConsensusModeParams
      NetworkId
      (Maybe (File () Out))
  | QueryTip
      SocketPath
      AnyConsensusModeParams
      NetworkId
      (Maybe (File () Out))
  | QueryStakePools'
      SocketPath
      AnyConsensusModeParams
      NetworkId
      (Maybe (File () Out))
  | QueryStakeDistribution'
      SocketPath
      AnyConsensusModeParams
      NetworkId
      (Maybe (File () Out))
  | QueryStakeAddressInfo
      SocketPath
      AnyConsensusModeParams
      StakeAddress
      NetworkId
      (Maybe (File () Out))
  | QueryUTxO'
      SocketPath
      AnyConsensusModeParams
      QueryUTxOFilter
      NetworkId
      (Maybe (File () Out))
  | QueryDebugLedgerState'
      SocketPath
      AnyConsensusModeParams
      NetworkId
      (Maybe (File () Out))
  | QueryProtocolState'
      SocketPath
      AnyConsensusModeParams
      NetworkId
      (Maybe (File () Out))
  | QueryStakeSnapshot'
      SocketPath
      AnyConsensusModeParams
      NetworkId
      (AllOrOnly [Hash StakePoolKey])
      (Maybe (File () Out))
  | QueryKesPeriodInfo
      SocketPath
      AnyConsensusModeParams
      NetworkId
      (File () In)
      -- ^ Node operational certificate
      (Maybe (File () Out))
  | QueryPoolState'
      SocketPath
      AnyConsensusModeParams
      NetworkId
      [Hash StakePoolKey]
  | QueryTxMempool
      SocketPath
      AnyConsensusModeParams
      NetworkId
      TxMempoolQuery
      (Maybe (File () Out))
  | QuerySlotNumber
      SocketPath
      AnyConsensusModeParams
      NetworkId
      UTCTime
  deriving (Show)

renderLegacyQueryCmds :: LegacyQueryCmds -> Text
renderLegacyQueryCmds = \case
  QueryLeadershipSchedule {} -> "query leadership-schedule"
  QueryProtocolParameters' {} -> "query protocol-parameters "
  QueryConstitutionHash {} -> "query constitution-hash "
  QueryTip {} -> "query tip"
  QueryStakePools' {} -> "query stake-pools"
  QueryStakeDistribution' {} -> "query stake-distribution"
  QueryStakeAddressInfo {} -> "query stake-address-info"
  QueryUTxO' {} -> "query utxo"
  QueryDebugLedgerState' {} -> "query ledger-state"
  QueryProtocolState' {} -> "query protocol-state"
  QueryStakeSnapshot' {} -> "query stake-snapshot"
  QueryKesPeriodInfo {} -> "query kes-period-info"
  QueryPoolState' {} -> "query pool-state"
  QueryTxMempool _ _ _ query _ -> "query tx-mempool" <> renderTxMempoolQuery query
  QuerySlotNumber {} -> "query slot-number"
 where
  renderTxMempoolQuery query =
    case query of
      TxMempoolQueryTxExists tx -> "tx-exists " <> serialiseToRawBytesHexText tx
      TxMempoolQueryNextTx -> "next-tx"
      TxMempoolQueryInfo -> "info"
