{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Cardano.CLI.Legacy.Run.Query
  ( runLegacyQueryCmds
  ) where

import           Cardano.Api hiding (QueryInShelleyBasedEra (..))
import           Cardano.Api.Shelley hiding (QueryInShelleyBasedEra (..))

import qualified Cardano.CLI.EraBased.Run.Query as EraBased
import           Cardano.CLI.Legacy.Commands.Query
import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Errors.ShelleyQueryCmdError
import           Cardano.CLI.Types.Key (VerificationKeyOrHashOrFile)

import           Control.Monad.Trans.Except (ExceptT)
import           Data.Time.Clock

runLegacyQueryCmds :: LegacyQueryCmds -> ExceptT ShelleyQueryCmdError IO ()
runLegacyQueryCmds = \case
  QueryLeadershipSchedule mNodeSocketPath consensusModeParams network shelleyGenFp poolid vrkSkeyFp whichSchedule outputAs ->
    runLegacyQueryLeadershipScheduleCmd mNodeSocketPath consensusModeParams network shelleyGenFp poolid vrkSkeyFp whichSchedule outputAs
  QueryProtocolParameters' mNodeSocketPath consensusModeParams network mOutFile ->
    runLegacyQueryProtocolParametersCmd mNodeSocketPath consensusModeParams network mOutFile
  QueryConstitutionHash mNodeSocketPath consensusModeParams network mOutFile ->
    runLegacyQueryConstitutionHashCmd mNodeSocketPath consensusModeParams network mOutFile
  QueryTip mNodeSocketPath consensusModeParams network mOutFile ->
    runLegacyQueryTipCmd mNodeSocketPath consensusModeParams network mOutFile
  QueryStakePools' mNodeSocketPath consensusModeParams network mOutFile ->
    runLegacyQueryStakePoolsCmd mNodeSocketPath consensusModeParams network mOutFile
  QueryStakeDistribution' mNodeSocketPath consensusModeParams network mOutFile ->
    runLegacyQueryStakeDistributionCmd mNodeSocketPath consensusModeParams network mOutFile
  QueryStakeAddressInfo mNodeSocketPath consensusModeParams addr network mOutFile ->
    runLegacyQueryStakeAddressInfoCmd mNodeSocketPath consensusModeParams addr network mOutFile
  QueryDebugLedgerState' mNodeSocketPath consensusModeParams network mOutFile ->
    runLegacyQueryLedgerStateCmd mNodeSocketPath consensusModeParams network mOutFile
  QueryStakeSnapshot' mNodeSocketPath consensusModeParams network allOrOnlyPoolIds mOutFile ->
    runLegacyQueryStakeSnapshotCmd mNodeSocketPath consensusModeParams network allOrOnlyPoolIds mOutFile
  QueryProtocolState' mNodeSocketPath consensusModeParams network mOutFile ->
    runLegacyQueryProtocolStateCmd mNodeSocketPath consensusModeParams network mOutFile
  QueryUTxO' mNodeSocketPath consensusModeParams qFilter networkId mOutFile ->
    runLegacyQueryUTxOCmd mNodeSocketPath consensusModeParams qFilter networkId mOutFile
  QueryKesPeriodInfo mNodeSocketPath consensusModeParams network nodeOpCert mOutFile ->
    runLegacyQueryKesPeriodInfoCmd mNodeSocketPath consensusModeParams network nodeOpCert mOutFile
  QueryPoolState' mNodeSocketPath consensusModeParams network poolid ->
    runLegacyQueryPoolStateCmd mNodeSocketPath consensusModeParams network poolid
  QueryTxMempool mNodeSocketPath consensusModeParams network op mOutFile ->
    runLegacyQueryTxMempoolCmd mNodeSocketPath consensusModeParams network op mOutFile
  QuerySlotNumber mNodeSocketPath consensusModeParams network utcTime ->
    runLegacyQuerySlotNumberCmd mNodeSocketPath consensusModeParams network utcTime

runLegacyQueryConstitutionHashCmd :: ()
  => SocketPath
  -> AnyConsensusModeParams
  -> NetworkId
  -> Maybe (File () Out)
  -> ExceptT ShelleyQueryCmdError IO ()
runLegacyQueryConstitutionHashCmd = EraBased.runLegacyQueryConstitutionHashCmd

runLegacyQueryProtocolParametersCmd :: ()
  => SocketPath
  -> AnyConsensusModeParams
  -> NetworkId
  -> Maybe (File () Out)
  -> ExceptT ShelleyQueryCmdError IO ()
runLegacyQueryProtocolParametersCmd = EraBased.runLegacyQueryProtocolParametersCmd

runLegacyQueryTipCmd :: ()
  => SocketPath
  -> AnyConsensusModeParams
  -> NetworkId
  -> Maybe (File () Out)
  -> ExceptT ShelleyQueryCmdError IO ()
runLegacyQueryTipCmd = EraBased.runLegacyQueryTipCmd

-- | Query the UTxO, filtered by a given set of addresses, from a Shelley node
-- via the local state query protocol.
runLegacyQueryUTxOCmd :: ()
  => SocketPath
  -> AnyConsensusModeParams
  -> QueryUTxOFilter
  -> NetworkId
  -> Maybe (File () Out)
  -> ExceptT ShelleyQueryCmdError IO ()
runLegacyQueryUTxOCmd = EraBased.runLegacyQueryUTxOCmd

runLegacyQueryKesPeriodInfoCmd :: ()
  => SocketPath
  -> AnyConsensusModeParams
  -> NetworkId
  -> File () In
  -> Maybe (File () Out)
  -> ExceptT ShelleyQueryCmdError IO ()
runLegacyQueryKesPeriodInfoCmd = EraBased.runLegacyQueryKesPeriodInfoCmd

-- | Query the current and future parameters for a stake pool, including the retirement date.
-- Any of these may be empty (in which case a null will be displayed).
--
runLegacyQueryPoolStateCmd :: ()
  => SocketPath
  -> AnyConsensusModeParams
  -> NetworkId
  -> [Hash StakePoolKey]
  -> ExceptT ShelleyQueryCmdError IO ()
runLegacyQueryPoolStateCmd = EraBased.runLegacyQueryPoolStateCmd

-- | Query the local mempool state
runLegacyQueryTxMempoolCmd :: ()
  => SocketPath
  -> AnyConsensusModeParams
  -> NetworkId
  -> TxMempoolQuery
  -> Maybe (File () Out)
  -> ExceptT ShelleyQueryCmdError IO ()
runLegacyQueryTxMempoolCmd = EraBased.runLegacyQueryTxMempoolCmd

runLegacyQuerySlotNumberCmd :: ()
  => SocketPath
  -> AnyConsensusModeParams
  -> NetworkId
  -> UTCTime
  -> ExceptT ShelleyQueryCmdError IO ()
runLegacyQuerySlotNumberCmd = EraBased.runLegacyQuerySlotNumberCmd

-- | Obtain stake snapshot information for a pool, plus information about the total active stake.
-- This information can be used for leader slot calculation, for example, and has been requested by SPOs.
-- Obtaining the information directly is significantly more time and memory efficient than using a full ledger state dump.
runLegacyQueryStakeSnapshotCmd :: ()
  => SocketPath
  -> AnyConsensusModeParams
  -> NetworkId
  -> AllOrOnly [Hash StakePoolKey]
  -> Maybe (File () Out)
  -> ExceptT ShelleyQueryCmdError IO ()
runLegacyQueryStakeSnapshotCmd = EraBased.runLegacyQueryStakeSnapshotCmd

runLegacyQueryLedgerStateCmd :: ()
  => SocketPath
  -> AnyConsensusModeParams
  -> NetworkId
  -> Maybe (File () Out)
  -> ExceptT ShelleyQueryCmdError IO ()
runLegacyQueryLedgerStateCmd = EraBased.runLegacyQueryLedgerStateCmd

runLegacyQueryProtocolStateCmd :: ()
  => SocketPath
  -> AnyConsensusModeParams
  -> NetworkId
  -> Maybe (File () Out)
  -> ExceptT ShelleyQueryCmdError IO ()
runLegacyQueryProtocolStateCmd = EraBased.runLegacyQueryProtocolStateCmd

-- | Query the current delegations and reward accounts, filtered by a given
-- set of addresses, from a Shelley node via the local state query protocol.

runLegacyQueryStakeAddressInfoCmd :: ()
  => SocketPath
  -> AnyConsensusModeParams
  -> StakeAddress
  -> NetworkId
  -> Maybe (File () Out)
  -> ExceptT ShelleyQueryCmdError IO ()
runLegacyQueryStakeAddressInfoCmd = EraBased.runLegacyQueryStakeAddressInfoCmd

runLegacyQueryStakePoolsCmd :: ()
  => SocketPath
  -> AnyConsensusModeParams
  -> NetworkId
  -> Maybe (File () Out)
  -> ExceptT ShelleyQueryCmdError IO ()
runLegacyQueryStakePoolsCmd = EraBased.runLegacyQueryStakePoolsCmd

runLegacyQueryStakeDistributionCmd :: ()
  => SocketPath
  -> AnyConsensusModeParams
  -> NetworkId
  -> Maybe (File () Out)
  -> ExceptT ShelleyQueryCmdError IO ()
runLegacyQueryStakeDistributionCmd = EraBased.runLegacyQueryStakeDistributionCmd

runLegacyQueryLeadershipScheduleCmd :: ()
  => SocketPath
  -> AnyConsensusModeParams
  -> NetworkId
  -> GenesisFile -- ^ Shelley genesis
  -> VerificationKeyOrHashOrFile StakePoolKey
  -> SigningKeyFile In -- ^ VRF signing key
  -> EpochLeadershipSchedule
  -> Maybe (File () Out)
  -> ExceptT ShelleyQueryCmdError IO ()
runLegacyQueryLeadershipScheduleCmd = EraBased.runLegacyQueryLeadershipScheduleCmd
