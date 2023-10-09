{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
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
  , QueryLedgerStateCmdArgs(..)
  , QueryProtocolStateCmdArgs(..)
  , QueryStakeSnapshotCmdArgs(..)
  , QueryKesPeriodInfoCmdArgs(..)
  , QueryPoolStateCmdArgs(..)
  , QueryTxMempoolCmdArgs(..)
  , QuerySlotNumberCmdArgs(..)
  , QueryNoArgCmdArgs(..)
  , QueryDRepStateCmdArgs(..)
  , QueryDRepStakeDistributionCmdArgs(..)
  , renderQueryCmds
  ) where

import           Cardano.Api.Shelley hiding (QueryInShelleyBasedEra (..))

import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Key

import           Data.Text (Text)
import           Data.Time.Clock
import           GHC.Generics

data QueryCmds era
  = QueryLeadershipScheduleCmd      !QueryLeadershipScheduleCmdArgs
  | QueryProtocolParametersCmd      !QueryProtocolParametersCmdArgs
  | QueryConstitutionHashCmd        !QueryConstitutionHashCmdArgs
  | QueryTipCmd                     !QueryTipCmdArgs
  | QueryStakePoolsCmd              !QueryStakePoolsCmdArgs
  | QueryStakeDistributionCmd       !QueryStakeDistributionCmdArgs
  | QueryStakeAddressInfoCmd        !QueryStakeAddressInfoCmdArgs
  | QueryUTxOCmd                    !QueryUTxOCmdArgs
  | QueryLedgerStateCmd             !QueryLedgerStateCmdArgs
  | QueryProtocolStateCmd           !QueryProtocolStateCmdArgs
  | QueryStakeSnapshotCmd           !QueryStakeSnapshotCmdArgs
  | QueryKesPeriodInfoCmd           !QueryKesPeriodInfoCmdArgs
  | QueryPoolStateCmd               !QueryPoolStateCmdArgs
  | QueryTxMempoolCmd               !QueryTxMempoolCmdArgs
  | QuerySlotNumberCmd              !QuerySlotNumberCmdArgs
  | QueryConstitutionCmd            !(QueryNoArgCmdArgs era)
  | QueryGovStateCmd                !(QueryNoArgCmdArgs era)
  | QueryDRepStateCmd               !(QueryDRepStateCmdArgs era)
  | QueryDRepStakeDistributionCmd   !(QueryDRepStakeDistributionCmdArgs era)
  | QueryCommitteeStateCmd          !(QueryNoArgCmdArgs era)
  deriving (Generic, Show)

data QueryLeadershipScheduleCmdArgs = QueryLeadershipScheduleCmdArgs
  { nodeSocketPath      :: !SocketPath
  , consensusModeParams :: !AnyConsensusModeParams
  , networkId           :: !NetworkId
  , genesisFp           :: !GenesisFile
  , poolColdVerKeyFile  :: !(VerificationKeyOrHashOrFile StakePoolKey)
  , vrkSkeyFp           :: !(SigningKeyFile In)
  , whichSchedule       :: !EpochLeadershipSchedule
  , mOutFile            :: !(Maybe (File () Out))
  } deriving (Generic, Show)

data QueryProtocolParametersCmdArgs = QueryProtocolParametersCmdArgs
  { nodeSocketPath      :: !SocketPath
  , consensusModeParams :: !AnyConsensusModeParams
  , networkId           :: !NetworkId
  , mOutFile            :: !(Maybe (File () Out))
  } deriving (Generic, Show)

data QueryConstitutionHashCmdArgs = QueryConstitutionHashCmdArgs
  { nodeSocketPath      :: !SocketPath
  , consensusModeParams :: !AnyConsensusModeParams
  , networkId           :: !NetworkId
  , mOutFile            :: !(Maybe (File () Out))
  } deriving (Generic, Show)

data QueryTipCmdArgs = QueryTipCmdArgs
  { nodeSocketPath      :: !SocketPath
  , consensusModeParams :: !AnyConsensusModeParams
  , networkId           :: !NetworkId
  , mOutFile            :: !(Maybe (File () Out))
  } deriving (Generic, Show)

data QueryStakePoolsCmdArgs = QueryStakePoolsCmdArgs
  { nodeSocketPath      :: !SocketPath
  , consensusModeParams :: !AnyConsensusModeParams
  , networkId           :: !NetworkId
  , mOutFile            :: !(Maybe (File () Out))
  } deriving (Generic, Show)

data QueryStakeDistributionCmdArgs = QueryStakeDistributionCmdArgs
  { nodeSocketPath      :: !SocketPath
  , consensusModeParams :: !AnyConsensusModeParams
  , networkId           :: !NetworkId
  , mOutFile            :: !(Maybe (File () Out))
  } deriving (Generic, Show)

data QueryStakeAddressInfoCmdArgs = QueryStakeAddressInfoCmdArgs
  { nodeSocketPath      :: !SocketPath
  , consensusModeParams :: !AnyConsensusModeParams
  , addr                :: !StakeAddress
  , networkId           :: !NetworkId
  , mOutFile            :: !(Maybe (File () Out))
  } deriving (Generic, Show)

data QueryUTxOCmdArgs = QueryUTxOCmdArgs
  { nodeSocketPath      :: !SocketPath
  , consensusModeParams :: !AnyConsensusModeParams
  , queryFilter         :: !QueryUTxOFilter
  , networkId           :: !NetworkId
  , mOutFile            :: !(Maybe (File () Out))
  } deriving (Generic, Show)

data QueryLedgerStateCmdArgs = QueryLedgerStateCmdArgs
  { nodeSocketPath      :: !SocketPath
  , consensusModeParams :: !AnyConsensusModeParams
  , networkId           :: !NetworkId
  , mOutFile            :: !(Maybe (File () Out))
  } deriving (Generic, Show)

data QueryProtocolStateCmdArgs = QueryProtocolStateCmdArgs
  { nodeSocketPath      :: !SocketPath
  , consensusModeParams :: !AnyConsensusModeParams
  , networkId           :: !NetworkId
  , mOutFile            :: !(Maybe (File () Out))
  } deriving (Generic, Show)

data QueryStakeSnapshotCmdArgs = QueryStakeSnapshotCmdArgs
  { nodeSocketPath      :: !SocketPath
  , consensusModeParams :: !AnyConsensusModeParams
  , networkId           :: !NetworkId
  , allOrOnlyPoolIds    :: !(AllOrOnly [Hash StakePoolKey])
  , mOutFile            :: !(Maybe (File () Out))
  } deriving (Generic, Show)

data QueryKesPeriodInfoCmdArgs = QueryKesPeriodInfoCmdArgs
  { nodeSocketPath      :: !SocketPath
  , consensusModeParams :: !AnyConsensusModeParams
  , networkId           :: !NetworkId
  , nodeOpCertFp        :: !(File () In) -- ^ Node operational certificate
  , mOutFile            :: !(Maybe (File () Out))
  } deriving (Generic, Show)

data QueryPoolStateCmdArgs = QueryPoolStateCmdArgs
  { nodeSocketPath      :: !SocketPath
  , consensusModeParams :: !AnyConsensusModeParams
  , networkId           :: !NetworkId
  , poolIds             :: ![Hash StakePoolKey]
  } deriving (Generic, Show)

data QueryTxMempoolCmdArgs = QueryTxMempoolCmdArgs
  { nodeSocketPath      :: !SocketPath
  , consensusModeParams :: !AnyConsensusModeParams
  , networkId           :: !NetworkId
  , query               :: !TxMempoolQuery
  , mOutFile            :: !(Maybe (File () Out))
  }
  deriving (Generic, Show)

data QuerySlotNumberCmdArgs = QuerySlotNumberCmdArgs
  { nodeSocketPath      :: !SocketPath
  , consensusModeParams :: !AnyConsensusModeParams
  , networkId           :: !NetworkId
  , utcTime             :: !UTCTime
  } deriving (Generic, Show)

data QueryNoArgCmdArgs era = QueryNoArgCmdArgs
  { eon                 :: !(ConwayEraOnwards era)
  , nodeSocketPath      :: !SocketPath
  , consensusModeParams :: !AnyConsensusModeParams
  , networkId           :: !NetworkId
  , mOutFile            :: !(Maybe (File () Out))
  } deriving Show

data QueryDRepStateCmdArgs era = QueryDRepStateCmdArgs
  { eon                 :: !(ConwayEraOnwards era)
  , nodeSocketPath      :: !SocketPath
  , consensusModeParams :: !AnyConsensusModeParams
  , networkId           :: !NetworkId
  , drepKeys            :: ![VerificationKeyOrHashOrFile DRepKey]
  , mOutFile            :: !(Maybe (File () Out))
  } deriving Show

data QueryDRepStakeDistributionCmdArgs era = QueryDRepStakeDistributionCmdArgs
  { eon                 :: !(ConwayEraOnwards era)
  , nodeSocketPath      :: !SocketPath
  , consensusModeParams :: !AnyConsensusModeParams
  , networkId           :: !NetworkId
  , drepKeys            :: ![VerificationKeyOrHashOrFile DRepKey]
  , mOutFile            :: !(Maybe (File () Out))
  } deriving Show

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
  QueryLedgerStateCmd {} ->
    "query ledger-state"
  QueryProtocolStateCmd {} ->
    "query protocol-state"
  QueryStakeSnapshotCmd {} ->
    "query stake-snapshot"
  QueryKesPeriodInfoCmd {} ->
    "query kes-period-info"
  QueryPoolStateCmd {} ->
    "query pool-state"
  QueryTxMempoolCmd (QueryTxMempoolCmdArgs _ _ _ q _) ->
    "query tx-mempool" <> renderTxMempoolQuery q
  QuerySlotNumberCmd {} ->
    "query slot-number"
  QueryConstitutionCmd {} ->
    "constitution"
  QueryGovStateCmd {} ->
    "gov-state"
  QueryDRepStateCmd {} ->
    "drep-state"
  QueryDRepStakeDistributionCmd {} ->
    "drep-stake-distribution"
  QueryCommitteeStateCmd {} ->
    "committee-state"

renderTxMempoolQuery :: TxMempoolQuery -> Text
renderTxMempoolQuery = \case
  TxMempoolQueryTxExists tx -> "tx-exists " <> serialiseToRawBytesHexText tx
  TxMempoolQueryNextTx -> "next-tx"
  TxMempoolQueryInfo -> "info"
