{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Cardano.CLI.Legacy.Run.Query
  ( runLegacyQueryCmds
  )
where

import qualified Cardano.CLI.EraBased.Commands.Query as EraBased
import qualified Cardano.CLI.EraBased.Run.Query as EraBased
import qualified Cardano.CLI.Legacy.Commands.Query as Cmd
import           Cardano.CLI.Types.Errors.QueryCmdError
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Type as Consensus

import           Control.Monad.Trans.Except

runLegacyQueryCmds :: Cmd.LegacyQueryCmds -> ExceptT QueryCmdError IO ()
runLegacyQueryCmds = \case
  Cmd.QueryLeadershipScheduleCmd args -> runLegacyQueryLeadershipScheduleCmd args
  Cmd.QueryProtocolParametersCmd args -> runLegacyQueryProtocolParametersCmd args
  Cmd.QueryTipCmd args -> runLegacyQueryTipCmd args
  Cmd.QueryStakePoolsCmd args -> runLegacyQueryStakePoolsCmd args
  Cmd.QueryStakeDistributionCmd args -> runLegacyQueryStakeDistributionCmd args
  Cmd.QueryStakeAddressInfoCmd args -> runLegacyQueryStakeAddressInfoCmd args
  Cmd.QueryLedgerStateCmd args -> runLegacyQueryLedgerStateCmd args
  Cmd.QueryStakeSnapshotCmd args -> runLegacyQueryStakeSnapshotCmd args
  Cmd.QueryProtocolStateCmd args -> runLegacyQueryProtocolStateCmd args
  Cmd.QueryUTxOCmd args -> runLegacyQueryUTxOCmd args
  Cmd.QueryKesPeriodInfoCmd args -> runLegacyQueryKesPeriodInfoCmd args
  Cmd.QueryPoolStateCmd args -> runLegacyQueryPoolStateCmd args
  Cmd.QueryTxMempoolCmd args -> runLegacyQueryTxMempoolCmd args
  Cmd.QuerySlotNumberCmd args -> runLegacyQuerySlotNumberCmd args

runLegacyQueryProtocolParametersCmd
  :: ()
  => Cmd.LegacyQueryProtocolParametersCmdArgs
  -> ExceptT QueryCmdError IO ()
runLegacyQueryProtocolParametersCmd Cmd.LegacyQueryProtocolParametersCmdArgs{..} =
  EraBased.runQueryProtocolParametersCmd EraBased.QueryProtocolParametersCmdArgs{..}

runLegacyQueryTipCmd
  :: ()
  => Cmd.LegacyQueryTipCmdArgs
  -> ExceptT QueryCmdError IO ()
runLegacyQueryTipCmd Cmd.LegacyQueryTipCmdArgs{..} =
  EraBased.runQueryTipCmd EraBased.QueryTipCmdArgs{target = Consensus.VolatileTip, ..}

-- | Query the UTxO, filtered by a given set of addresses, from a Shelley node
-- via the local state query protocol.
runLegacyQueryUTxOCmd
  :: ()
  => Cmd.LegacyQueryUTxOCmdArgs
  -> ExceptT QueryCmdError IO ()
runLegacyQueryUTxOCmd Cmd.LegacyQueryUTxOCmdArgs{..} =
  EraBased.runQueryUTxOCmd EraBased.QueryUTxOCmdArgs{target = Consensus.VolatileTip, ..}

runLegacyQueryKesPeriodInfoCmd
  :: ()
  => Cmd.LegacyQueryKesPeriodInfoCmdArgs
  -> ExceptT QueryCmdError IO ()
runLegacyQueryKesPeriodInfoCmd Cmd.LegacyQueryKesPeriodInfoCmdArgs{..} =
  EraBased.runQueryKesPeriodInfoCmd
    EraBased.QueryKesPeriodInfoCmdArgs{target = Consensus.VolatileTip, ..}

-- | Query the current and future parameters for a stake pool, including the retirement date.
-- Any of these may be empty (in which case a null will be displayed).
runLegacyQueryPoolStateCmd
  :: ()
  => Cmd.LegacyQueryPoolStateCmdArgs
  -> ExceptT QueryCmdError IO ()
runLegacyQueryPoolStateCmd Cmd.LegacyQueryPoolStateCmdArgs{..} =
  EraBased.runQueryPoolStateCmd EraBased.QueryPoolStateCmdArgs{target = Consensus.VolatileTip, ..}

-- | Query the local mempool state
runLegacyQueryTxMempoolCmd
  :: ()
  => Cmd.LegacyQueryTxMempoolCmdArgs
  -> ExceptT QueryCmdError IO ()
runLegacyQueryTxMempoolCmd Cmd.LegacyQueryTxMempoolCmdArgs{..} =
  EraBased.runQueryTxMempoolCmd EraBased.QueryTxMempoolCmdArgs{..}

runLegacyQuerySlotNumberCmd
  :: ()
  => Cmd.LegacyQuerySlotNumberCmdArgs
  -> ExceptT QueryCmdError IO ()
runLegacyQuerySlotNumberCmd Cmd.LegacyQuerySlotNumberCmdArgs{..} =
  EraBased.runQuerySlotNumberCmd EraBased.QuerySlotNumberCmdArgs{target = Consensus.VolatileTip, ..}

-- | Obtain stake snapshot information for a pool, plus information about the total active stake.
-- This information can be used for leader slot calculation, for example, and has been requested by SPOs.
-- Obtaining the information directly is significantly more time and memory efficient than using a full ledger state dump.
runLegacyQueryStakeSnapshotCmd
  :: ()
  => Cmd.LegacyQueryStakeSnapshotCmdArgs
  -> ExceptT QueryCmdError IO ()
runLegacyQueryStakeSnapshotCmd Cmd.LegacyQueryStakeSnapshotCmdArgs{..} =
  EraBased.runQueryStakeSnapshotCmd
    EraBased.QueryStakeSnapshotCmdArgs{target = Consensus.VolatileTip, ..}

runLegacyQueryLedgerStateCmd
  :: ()
  => Cmd.LegacyQueryLedgerStateCmdArgs
  -> ExceptT QueryCmdError IO ()
runLegacyQueryLedgerStateCmd Cmd.LegacyQueryLedgerStateCmdArgs{..} =
  EraBased.runQueryLedgerStateCmd
    EraBased.QueryLedgerStateCmdArgs{target = Consensus.VolatileTip, ..}

runLegacyQueryProtocolStateCmd
  :: ()
  => Cmd.LegacyQueryProtocolStateCmdArgs
  -> ExceptT QueryCmdError IO ()
runLegacyQueryProtocolStateCmd Cmd.LegacyQueryProtocolStateCmdArgs{..} =
  EraBased.runQueryProtocolStateCmd
    EraBased.QueryProtocolStateCmdArgs{target = Consensus.VolatileTip, ..}

-- | Query the current delegations and reward accounts, filtered by a given
-- set of addresses, from a Shelley node via the local state query protocol.
runLegacyQueryStakeAddressInfoCmd
  :: ()
  => Cmd.LegacyQueryStakeAddressInfoCmdArgs
  -> ExceptT QueryCmdError IO ()
runLegacyQueryStakeAddressInfoCmd Cmd.LegacyQueryStakeAddressInfoCmdArgs{..} =
  EraBased.runQueryStakeAddressInfoCmd
    EraBased.QueryStakeAddressInfoCmdArgs{target = Consensus.VolatileTip, ..}

runLegacyQueryStakePoolsCmd
  :: ()
  => Cmd.LegacyQueryStakePoolsCmdArgs
  -> ExceptT QueryCmdError IO ()
runLegacyQueryStakePoolsCmd Cmd.LegacyQueryStakePoolsCmdArgs{..} =
  EraBased.runQueryStakePoolsCmd EraBased.QueryStakePoolsCmdArgs{target = Consensus.VolatileTip, ..}

runLegacyQueryStakeDistributionCmd
  :: ()
  => Cmd.LegacyQueryStakeDistributionCmdArgs
  -> ExceptT QueryCmdError IO ()
runLegacyQueryStakeDistributionCmd Cmd.LegacyQueryStakeDistributionCmdArgs{..} =
  EraBased.runQueryStakeDistributionCmd
    EraBased.QueryStakeDistributionCmdArgs{target = Consensus.VolatileTip, ..}

runLegacyQueryLeadershipScheduleCmd
  :: ()
  => Cmd.LegacyQueryLeadershipScheduleCmdArgs
  -> ExceptT QueryCmdError IO ()
runLegacyQueryLeadershipScheduleCmd Cmd.LegacyQueryLeadershipScheduleCmdArgs{..} =
  EraBased.runQueryLeadershipScheduleCmd
    EraBased.QueryLeadershipScheduleCmdArgs{target = Consensus.VolatileTip, ..}
