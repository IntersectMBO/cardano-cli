{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.Commands.Query
  ( QueryCmds (..)
  , QueryCommitteeMembersStateCmdArgs(..)
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
  , IncludeStake (..)
  ) where

import           Cardano.Api.Shelley hiding (QueryInShelleyBasedEra (..))

import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Key
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Type as Consensus

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
  | QueryCommitteeMembersStateCmd   !(QueryCommitteeMembersStateCmdArgs era)
  deriving (Generic, Show)

data QueryLeadershipScheduleCmdArgs = QueryLeadershipScheduleCmdArgs
  { nodeSocketPath      :: !SocketPath
  , consensusModeParams :: !ConsensusModeParams
  , networkId           :: !NetworkId
  , genesisFp           :: !GenesisFile
  , poolColdVerKeyFile  :: !(VerificationKeyOrHashOrFile StakePoolKey)
  , vrkSkeyFp           :: !(SigningKeyFile In)
  , whichSchedule       :: !EpochLeadershipSchedule
  , target              :: !(Consensus.Target ChainPoint)
  , mOutFile            :: !(Maybe (File () Out))
  } deriving (Generic, Show)

data QueryProtocolParametersCmdArgs = QueryProtocolParametersCmdArgs
  { nodeSocketPath      :: !SocketPath
  , consensusModeParams :: !ConsensusModeParams
  , networkId           :: !NetworkId
  , mOutFile            :: !(Maybe (File () Out))
  } deriving (Generic, Show)

data QueryConstitutionHashCmdArgs = QueryConstitutionHashCmdArgs
  { nodeSocketPath      :: !SocketPath
  , consensusModeParams :: !ConsensusModeParams
  , networkId           :: !NetworkId
  , target              :: !(Consensus.Target ChainPoint)
  , mOutFile            :: !(Maybe (File () Out))
  } deriving (Generic, Show)

data QueryTipCmdArgs = QueryTipCmdArgs
  { nodeSocketPath      :: !SocketPath
  , consensusModeParams :: !ConsensusModeParams
  , networkId           :: !NetworkId
  , target              :: !(Consensus.Target ChainPoint)
  , mOutFile            :: !(Maybe (File () Out))
  } deriving (Generic, Show)

data QueryStakePoolsCmdArgs = QueryStakePoolsCmdArgs
  { nodeSocketPath      :: !SocketPath
  , consensusModeParams :: !ConsensusModeParams
  , networkId           :: !NetworkId
  , target              :: !(Consensus.Target ChainPoint)
  , mOutFile            :: !(Maybe (File () Out))
  } deriving (Generic, Show)

data QueryStakeDistributionCmdArgs = QueryStakeDistributionCmdArgs
  { nodeSocketPath      :: !SocketPath
  , consensusModeParams :: !ConsensusModeParams
  , networkId           :: !NetworkId
  , target              :: !(Consensus.Target ChainPoint)
  , mOutFile            :: !(Maybe (File () Out))
  } deriving (Generic, Show)

data QueryStakeAddressInfoCmdArgs = QueryStakeAddressInfoCmdArgs
  { nodeSocketPath      :: !SocketPath
  , consensusModeParams :: !ConsensusModeParams
  , addr                :: !StakeAddress
  , networkId           :: !NetworkId
  , target              :: !(Consensus.Target ChainPoint)
  , mOutFile            :: !(Maybe (File () Out))
  } deriving (Generic, Show)

data QueryUTxOCmdArgs = QueryUTxOCmdArgs
  { nodeSocketPath      :: !SocketPath
  , consensusModeParams :: !ConsensusModeParams
  , queryFilter         :: !QueryUTxOFilter
  , networkId           :: !NetworkId
  , target              :: !(Consensus.Target ChainPoint)
  , format              :: Maybe QueryOutputFormat
  , mOutFile            :: !(Maybe (File () Out))
  } deriving (Generic, Show)

data QueryLedgerStateCmdArgs = QueryLedgerStateCmdArgs
  { nodeSocketPath      :: !SocketPath
  , consensusModeParams :: !ConsensusModeParams
  , networkId           :: !NetworkId
  , target              :: !(Consensus.Target ChainPoint)
  , mOutFile            :: !(Maybe (File () Out))
  } deriving (Generic, Show)

data QueryProtocolStateCmdArgs = QueryProtocolStateCmdArgs
  { nodeSocketPath      :: !SocketPath
  , consensusModeParams :: !ConsensusModeParams
  , networkId           :: !NetworkId
  , target              :: !(Consensus.Target ChainPoint)
  , mOutFile            :: !(Maybe (File () Out))
  } deriving (Generic, Show)

data QueryStakeSnapshotCmdArgs = QueryStakeSnapshotCmdArgs
  { nodeSocketPath      :: !SocketPath
  , consensusModeParams :: !ConsensusModeParams
  , networkId           :: !NetworkId
  , allOrOnlyPoolIds    :: !(AllOrOnly (Hash StakePoolKey))
  , target              :: !(Consensus.Target ChainPoint)
  , mOutFile            :: !(Maybe (File () Out))
  } deriving (Generic, Show)

data QueryKesPeriodInfoCmdArgs = QueryKesPeriodInfoCmdArgs
  { nodeSocketPath      :: !SocketPath
  , consensusModeParams :: !ConsensusModeParams
  , networkId           :: !NetworkId
  , nodeOpCertFp        :: !(File () In) -- ^ Node operational certificate
  , target              :: !(Consensus.Target ChainPoint)
  , mOutFile            :: !(Maybe (File () Out))
  } deriving (Generic, Show)

data QueryPoolStateCmdArgs = QueryPoolStateCmdArgs
  { nodeSocketPath      :: !SocketPath
  , consensusModeParams :: !ConsensusModeParams
  , networkId           :: !NetworkId
  , allOrOnlyPoolIds    :: !(AllOrOnly (Hash StakePoolKey))
  , target              :: !(Consensus.Target ChainPoint)
  } deriving (Generic, Show)

data QueryTxMempoolCmdArgs = QueryTxMempoolCmdArgs
  { nodeSocketPath      :: !SocketPath
  , consensusModeParams :: !ConsensusModeParams
  , networkId           :: !NetworkId
  , query               :: !TxMempoolQuery
  , mOutFile            :: !(Maybe (File () Out))
  }
  deriving (Generic, Show)

data QuerySlotNumberCmdArgs = QuerySlotNumberCmdArgs
  { nodeSocketPath      :: !SocketPath
  , consensusModeParams :: !ConsensusModeParams
  , networkId           :: !NetworkId
  , target              :: !(Consensus.Target ChainPoint)
  , utcTime             :: !UTCTime
  } deriving (Generic, Show)

data QueryNoArgCmdArgs era = QueryNoArgCmdArgs
  { eon                 :: !(ConwayEraOnwards era)
  , nodeSocketPath      :: !SocketPath
  , consensusModeParams :: !ConsensusModeParams
  , networkId           :: !NetworkId
  , target              :: !(Consensus.Target ChainPoint)
  , mOutFile            :: !(Maybe (File () Out))
  } deriving Show

data QueryDRepStateCmdArgs era = QueryDRepStateCmdArgs
  { eon                 :: !(ConwayEraOnwards era)
  , nodeSocketPath      :: !SocketPath
  , consensusModeParams :: !ConsensusModeParams
  , networkId           :: !NetworkId
  , drepKeys            :: !(AllOrOnly (VerificationKeyOrHashOrFile DRepKey))
  , includeStake        :: !IncludeStake
  , target              :: !(Consensus.Target ChainPoint)
  , mOutFile            :: !(Maybe (File () Out))
  } deriving Show

-- | Whether to include the stake, as queried by drep-stake-distribution, in
-- the output of drep-state. This is (computationally) expensive, but sometimes
-- convenient.
data IncludeStake = WithStake | NoStake deriving Show

data QueryDRepStakeDistributionCmdArgs era = QueryDRepStakeDistributionCmdArgs
  { eon                 :: !(ConwayEraOnwards era)
  , nodeSocketPath      :: !SocketPath
  , consensusModeParams :: !ConsensusModeParams
  , networkId           :: !NetworkId
  , drepKeys            :: !(AllOrOnly (VerificationKeyOrHashOrFile DRepKey))
  , target              :: !(Consensus.Target ChainPoint)
  , mOutFile            :: !(Maybe (File () Out))
  } deriving Show

data QueryCommitteeMembersStateCmdArgs era = QueryCommitteeMembersStateCmdArgs
  { eon                     :: !(ConwayEraOnwards era)
  , nodeSocketPath          :: !SocketPath
  , consensusModeParams     :: !ConsensusModeParams
  , networkId               :: !NetworkId
  , committeeColdKeys       :: ![VerificationKeyOrHashOrFile CommitteeColdKey]
  , committeeHotKeys        :: ![VerificationKeyOrHashOrFile CommitteeHotKey]
  , memberStatuses          :: ![MemberStatus]
  , target                  :: !(Consensus.Target ChainPoint)
  , mOutFile                :: !(Maybe (File () Out))
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
  QueryCommitteeMembersStateCmd {} ->
    "committee-state"

renderTxMempoolQuery :: TxMempoolQuery -> Text
renderTxMempoolQuery = \case
  TxMempoolQueryTxExists tx -> "tx-exists " <> serialiseToRawBytesHexText tx
  TxMempoolQueryNextTx -> "next-tx"
  TxMempoolQueryInfo -> "info"
