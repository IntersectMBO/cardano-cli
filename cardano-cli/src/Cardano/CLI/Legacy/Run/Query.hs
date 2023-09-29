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
import qualified Cardano.CLI.Legacy.Commands.Query as Cmd
import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Errors.QueryCmdError
import           Cardano.CLI.Types.Key (VerificationKeyOrHashOrFile)

import           Control.Monad.Trans.Except
import           Data.Time.Clock

runLegacyQueryCmds :: Cmd.LegacyQueryCmds -> ExceptT QueryCmdError IO ()
runLegacyQueryCmds = \case
  Cmd.QueryLeadershipScheduleCmd
      (Cmd.LegacyQueryLeadershipScheduleCmdArgs mNodeSocketPath consensusModeParams network shelleyGenFp poolid vrkSkeyFp whichSchedule outputAs) ->
    runLegacyQueryLeadershipScheduleCmd mNodeSocketPath consensusModeParams network shelleyGenFp poolid vrkSkeyFp whichSchedule outputAs
  Cmd.QueryProtocolParametersCmd
      (Cmd.LegacyQueryProtocolParametersCmdArgs mNodeSocketPath consensusModeParams network mOutFile) ->
    runLegacyQueryProtocolParametersCmd mNodeSocketPath consensusModeParams network mOutFile
  Cmd.QueryConstitutionHashCmd
      (Cmd.LegacyQueryConstitutionHashCmdArgs mNodeSocketPath consensusModeParams network mOutFile) ->
    runLegacyQueryConstitutionHashCmd mNodeSocketPath consensusModeParams network mOutFile
  Cmd.QueryTipCmd
      (Cmd.LegacyQueryTipCmdArgs mNodeSocketPath consensusModeParams network mOutFile) ->
    runLegacyQueryTipCmd mNodeSocketPath consensusModeParams network mOutFile
  Cmd.QueryStakePoolsCmd
      (Cmd.LegacyQueryStakePoolsCmdArgs mNodeSocketPath consensusModeParams network mOutFile) ->
    runLegacyQueryStakePoolsCmd mNodeSocketPath consensusModeParams network mOutFile
  Cmd.QueryStakeDistributionCmd
      (Cmd.LegacyQueryStakeDistributionCmdArgs mNodeSocketPath consensusModeParams network mOutFile) ->
    runLegacyQueryStakeDistributionCmd mNodeSocketPath consensusModeParams network mOutFile
  Cmd.QueryStakeAddressInfoCmd
      (Cmd.LegacyQueryStakeAddressInfoCmdArgs mNodeSocketPath consensusModeParams addr network mOutFile) ->
    runLegacyQueryStakeAddressInfoCmd mNodeSocketPath consensusModeParams addr network mOutFile
  Cmd.QueryLedgerStateCmd
      (Cmd.LegacyQueryLedgerStateCmdArgs mNodeSocketPath consensusModeParams network mOutFile) ->
    runLegacyQueryLedgerStateCmd mNodeSocketPath consensusModeParams network mOutFile
  Cmd.QueryStakeSnapshotCmd
      (Cmd.LegacyQueryStakeSnapshotCmdArgs mNodeSocketPath consensusModeParams network allOrOnlyPoolIds mOutFile) ->
    runLegacyQueryStakeSnapshotCmd mNodeSocketPath consensusModeParams network allOrOnlyPoolIds mOutFile
  Cmd.QueryProtocolStateCmd
      (Cmd.LegacyQueryProtocolStateCmdArgs mNodeSocketPath consensusModeParams network mOutFile) ->
    runLegacyQueryProtocolStateCmd mNodeSocketPath consensusModeParams network mOutFile
  Cmd.QueryUTxOCmd
      (Cmd.LegacyQueryUTxOCmdArgs mNodeSocketPath consensusModeParams qFilter networkId mOutFile) ->
    runLegacyQueryUTxOCmd mNodeSocketPath consensusModeParams qFilter networkId mOutFile
  Cmd.QueryKesPeriodInfoCmd
      (Cmd.LegacyQueryKesPeriodInfoCmdArgs mNodeSocketPath consensusModeParams network nodeOpCert mOutFile) ->
    runLegacyQueryKesPeriodInfoCmd mNodeSocketPath consensusModeParams network nodeOpCert mOutFile
  Cmd.QueryPoolStateCmd
      (Cmd.LegacyQueryPoolStateCmdArgs mNodeSocketPath consensusModeParams network poolid) ->
    runLegacyQueryPoolStateCmd mNodeSocketPath consensusModeParams network poolid
  Cmd.QueryTxMempoolCmd
      (Cmd.LegacyQueryTxMempoolCmdArgs mNodeSocketPath consensusModeParams network op mOutFile) ->
    runLegacyQueryTxMempoolCmd mNodeSocketPath consensusModeParams network op mOutFile
  Cmd.QuerySlotNumberCmd
      (Cmd.LegacyQuerySlotNumberCmdArgs mNodeSocketPath consensusModeParams network utcTime) ->
    runLegacyQuerySlotNumberCmd mNodeSocketPath consensusModeParams network utcTime

runLegacyQueryConstitutionHashCmd :: ()
  => SocketPath
  -> AnyConsensusModeParams
  -> NetworkId
  -> Maybe (File () Out)
  -> ExceptT QueryCmdError IO ()
runLegacyQueryConstitutionHashCmd = EraBased.runQueryConstitutionHashCmd

runLegacyQueryProtocolParametersCmd :: ()
  => SocketPath
  -> AnyConsensusModeParams
  -> NetworkId
  -> Maybe (File () Out)
  -> ExceptT QueryCmdError IO ()
runLegacyQueryProtocolParametersCmd = EraBased.runQueryProtocolParametersCmd

runLegacyQueryTipCmd :: ()
  => SocketPath
  -> AnyConsensusModeParams
  -> NetworkId
  -> Maybe (File () Out)
  -> ExceptT QueryCmdError IO ()
runLegacyQueryTipCmd = EraBased.runQueryTipCmd

-- | Query the UTxO, filtered by a given set of addresses, from a Shelley node
-- via the local state query protocol.
runLegacyQueryUTxOCmd :: ()
  => SocketPath
  -> AnyConsensusModeParams
  -> QueryUTxOFilter
  -> NetworkId
  -> Maybe (File () Out)
  -> ExceptT QueryCmdError IO ()
runLegacyQueryUTxOCmd = EraBased.runQueryUTxOCmd

runLegacyQueryKesPeriodInfoCmd :: ()
  => SocketPath
  -> AnyConsensusModeParams
  -> NetworkId
  -> File () In
  -> Maybe (File () Out)
  -> ExceptT QueryCmdError IO ()
runLegacyQueryKesPeriodInfoCmd = EraBased.runQueryKesPeriodInfoCmd

-- | Query the current and future parameters for a stake pool, including the retirement date.
-- Any of these may be empty (in which case a null will be displayed).
--
runLegacyQueryPoolStateCmd :: ()
  => SocketPath
  -> AnyConsensusModeParams
  -> NetworkId
  -> [Hash StakePoolKey]
  -> ExceptT QueryCmdError IO ()
runLegacyQueryPoolStateCmd = EraBased.runQueryPoolStateCmd

-- | Query the local mempool state
runLegacyQueryTxMempoolCmd :: ()
  => SocketPath
  -> AnyConsensusModeParams
  -> NetworkId
  -> TxMempoolQuery
  -> Maybe (File () Out)
  -> ExceptT QueryCmdError IO ()
runLegacyQueryTxMempoolCmd = EraBased.runQueryTxMempoolCmd

runLegacyQuerySlotNumberCmd :: ()
  => SocketPath
  -> AnyConsensusModeParams
  -> NetworkId
  -> UTCTime
  -> ExceptT QueryCmdError IO ()
runLegacyQuerySlotNumberCmd = EraBased.runQuerySlotNumberCmd

-- | Obtain stake snapshot information for a pool, plus information about the total active stake.
-- This information can be used for leader slot calculation, for example, and has been requested by SPOs.
-- Obtaining the information directly is significantly more time and memory efficient than using a full ledger state dump.
runLegacyQueryStakeSnapshotCmd :: ()
  => SocketPath
  -> AnyConsensusModeParams
  -> NetworkId
  -> AllOrOnly [Hash StakePoolKey]
  -> Maybe (File () Out)
  -> ExceptT QueryCmdError IO ()
runLegacyQueryStakeSnapshotCmd = EraBased.runQueryStakeSnapshotCmd

runLegacyQueryLedgerStateCmd :: ()
  => SocketPath
  -> AnyConsensusModeParams
  -> NetworkId
  -> Maybe (File () Out)
  -> ExceptT QueryCmdError IO ()
runLegacyQueryLedgerStateCmd = EraBased.runQueryLedgerStateCmd

runLegacyQueryProtocolStateCmd :: ()
  => SocketPath
  -> AnyConsensusModeParams
  -> NetworkId
  -> Maybe (File () Out)
  -> ExceptT QueryCmdError IO ()
runLegacyQueryProtocolStateCmd = EraBased.runQueryProtocolStateCmd

-- | Query the current delegations and reward accounts, filtered by a given
-- set of addresses, from a Shelley node via the local state query protocol.

runLegacyQueryStakeAddressInfoCmd :: ()
  => SocketPath
  -> AnyConsensusModeParams
  -> StakeAddress
  -> NetworkId
  -> Maybe (File () Out)
  -> ExceptT QueryCmdError IO ()
runLegacyQueryStakeAddressInfoCmd = EraBased.runQueryStakeAddressInfoCmd

runLegacyQueryStakePoolsCmd :: ()
  => SocketPath
  -> AnyConsensusModeParams
  -> NetworkId
  -> Maybe (File () Out)
  -> ExceptT QueryCmdError IO ()
runLegacyQueryStakePoolsCmd = EraBased.runQueryStakePoolsCmd

runLegacyQueryStakeDistributionCmd :: ()
  => SocketPath
  -> AnyConsensusModeParams
  -> NetworkId
  -> Maybe (File () Out)
  -> ExceptT QueryCmdError IO ()
runLegacyQueryStakeDistributionCmd = EraBased.runQueryStakeDistributionCmd

runLegacyQueryLeadershipScheduleCmd :: ()
  => SocketPath
  -> AnyConsensusModeParams
  -> NetworkId
  -> GenesisFile -- ^ Shelley genesis
  -> VerificationKeyOrHashOrFile StakePoolKey
  -> SigningKeyFile In -- ^ VRF signing key
  -> EpochLeadershipSchedule
  -> Maybe (File () Out)
  -> ExceptT QueryCmdError IO ()
runLegacyQueryLeadershipScheduleCmd = EraBased.runQueryLeadershipScheduleCmd
