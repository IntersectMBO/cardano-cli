{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.CLI.Legacy.Run.Query
  ( runLegacyQueryCmds
  ) where

import qualified Cardano.CLI.EraBased.Commands.Query as EraBased
import qualified Cardano.CLI.EraBased.Run.Query as EraBased
import qualified Cardano.CLI.Legacy.Commands.Query as Cmd
import           Cardano.CLI.Types.Errors.QueryCmdError

import           Control.Monad.Trans.Except

runLegacyQueryCmds :: Cmd.LegacyQueryCmds -> ExceptT QueryCmdError IO ()
runLegacyQueryCmds = \case
  Cmd.QueryLeadershipScheduleCmd  args -> runLegacyQueryLeadershipScheduleCmd args
  Cmd.QueryProtocolParametersCmd  args -> runLegacyQueryProtocolParametersCmd args
  Cmd.QueryConstitutionHashCmd    args -> runLegacyQueryConstitutionHashCmd args
  Cmd.QueryTipCmd                 args -> runLegacyQueryTipCmd args
  Cmd.QueryStakePoolsCmd          args -> runLegacyQueryStakePoolsCmd args
  Cmd.QueryStakeDistributionCmd   args -> runLegacyQueryStakeDistributionCmd args
  Cmd.QueryStakeAddressInfoCmd    args -> runLegacyQueryStakeAddressInfoCmd args
  Cmd.QueryLedgerStateCmd         args -> runLegacyQueryLedgerStateCmd args
  Cmd.QueryStakeSnapshotCmd       args -> runLegacyQueryStakeSnapshotCmd args
  Cmd.QueryProtocolStateCmd       args -> runLegacyQueryProtocolStateCmd args
  Cmd.QueryUTxOCmd                args -> runLegacyQueryUTxOCmd args
  Cmd.QueryKesPeriodInfoCmd       args -> runLegacyQueryKesPeriodInfoCmd args
  Cmd.QueryPoolStateCmd           args -> runLegacyQueryPoolStateCmd args
  Cmd.QueryTxMempoolCmd           args -> runLegacyQueryTxMempoolCmd args
  Cmd.QuerySlotNumberCmd          args -> runLegacyQuerySlotNumberCmd args

runLegacyQueryConstitutionHashCmd :: ()
  => Cmd.LegacyQueryConstitutionHashCmdArgs
  -> ExceptT QueryCmdError IO ()
runLegacyQueryConstitutionHashCmd Cmd.LegacyQueryConstitutionHashCmdArgs {..} =
  EraBased.runQueryConstitutionHashCmd EraBased.QueryConstitutionHashCmdArgs {..}

runLegacyQueryProtocolParametersCmd :: ()
  => Cmd.LegacyQueryProtocolParametersCmdArgs
  -> ExceptT QueryCmdError IO ()
runLegacyQueryProtocolParametersCmd Cmd.LegacyQueryProtocolParametersCmdArgs {..} =
  EraBased.runQueryProtocolParametersCmd EraBased.QueryProtocolParametersCmdArgs {..}

runLegacyQueryTipCmd :: ()
  => Cmd.LegacyQueryTipCmdArgs
  -> ExceptT QueryCmdError IO ()
runLegacyQueryTipCmd Cmd.LegacyQueryTipCmdArgs {..} =
  EraBased.runQueryTipCmd EraBased.QueryTipCmdArgs {..}

-- | Query the UTxO, filtered by a given set of addresses, from a Shelley node
-- via the local state query protocol.
runLegacyQueryUTxOCmd :: ()
  => Cmd.LegacyQueryUTxOCmdArgs
  -> ExceptT QueryCmdError IO ()
runLegacyQueryUTxOCmd Cmd.LegacyQueryUTxOCmdArgs {..} =
  EraBased.runQueryUTxOCmd EraBased.QueryUTxOCmdArgs {..}

runLegacyQueryKesPeriodInfoCmd :: ()
  => Cmd.LegacyQueryKesPeriodInfoCmdArgs
  -> ExceptT QueryCmdError IO ()
runLegacyQueryKesPeriodInfoCmd Cmd.LegacyQueryKesPeriodInfoCmdArgs {..} =
  EraBased.runQueryKesPeriodInfoCmd EraBased.QueryKesPeriodInfoCmdArgs {..}

-- | Query the current and future parameters for a stake pool, including the retirement date.
-- Any of these may be empty (in which case a null will be displayed).
--
runLegacyQueryPoolStateCmd :: ()
  => Cmd.LegacyQueryPoolStateCmdArgs
  -> ExceptT QueryCmdError IO ()
runLegacyQueryPoolStateCmd Cmd.LegacyQueryPoolStateCmdArgs {..} =
  EraBased.runQueryPoolStateCmd EraBased.QueryPoolStateCmdArgs {..}

-- | Query the local mempool state
runLegacyQueryTxMempoolCmd :: ()
  => Cmd.LegacyQueryTxMempoolCmdArgs
  -> ExceptT QueryCmdError IO ()
runLegacyQueryTxMempoolCmd Cmd.LegacyQueryTxMempoolCmdArgs {..} =
  EraBased.runQueryTxMempoolCmd EraBased.QueryTxMempoolCmdArgs {..}

runLegacyQuerySlotNumberCmd :: ()
  => Cmd.LegacyQuerySlotNumberCmdArgs
  -> ExceptT QueryCmdError IO ()
runLegacyQuerySlotNumberCmd Cmd.LegacyQuerySlotNumberCmdArgs {..} =
  EraBased.runQuerySlotNumberCmd EraBased.QuerySlotNumberCmdArgs {..}

-- | Obtain stake snapshot information for a pool, plus information about the total active stake.
-- This information can be used for leader slot calculation, for example, and has been requested by SPOs.
-- Obtaining the information directly is significantly more time and memory efficient than using a full ledger state dump.
runLegacyQueryStakeSnapshotCmd :: ()
  => Cmd.LegacyQueryStakeSnapshotCmdArgs
  -> ExceptT QueryCmdError IO ()
runLegacyQueryStakeSnapshotCmd Cmd.LegacyQueryStakeSnapshotCmdArgs {..} =
  EraBased.runQueryStakeSnapshotCmd EraBased.QueryStakeSnapshotCmdArgs {..}

runLegacyQueryLedgerStateCmd :: ()
  => Cmd.LegacyQueryLedgerStateCmdArgs
  -> ExceptT QueryCmdError IO ()
runLegacyQueryLedgerStateCmd Cmd.LegacyQueryLedgerStateCmdArgs {..} =
  EraBased.runQueryLedgerStateCmd EraBased.QueryLedgerStateCmdArgs {..}

runLegacyQueryProtocolStateCmd :: ()
  => Cmd.LegacyQueryProtocolStateCmdArgs
  -> ExceptT QueryCmdError IO ()
runLegacyQueryProtocolStateCmd Cmd.LegacyQueryProtocolStateCmdArgs {..} =
  EraBased.runQueryProtocolStateCmd EraBased.QueryProtocolStateCmdArgs {..}

-- | Query the current delegations and reward accounts, filtered by a given
-- set of addresses, from a Shelley node via the local state query protocol.

runLegacyQueryStakeAddressInfoCmd :: ()
  => Cmd.LegacyQueryStakeAddressInfoCmdArgs
  -> ExceptT QueryCmdError IO ()
runLegacyQueryStakeAddressInfoCmd Cmd.LegacyQueryStakeAddressInfoCmdArgs {..} =
  EraBased.runQueryStakeAddressInfoCmd EraBased.QueryStakeAddressInfoCmdArgs {..}

runLegacyQueryStakePoolsCmd :: ()
  => Cmd.LegacyQueryStakePoolsCmdArgs
  -> ExceptT QueryCmdError IO ()
runLegacyQueryStakePoolsCmd Cmd.LegacyQueryStakePoolsCmdArgs {..} =
  EraBased.runQueryStakePoolsCmd EraBased.QueryStakePoolsCmdArgs {..}

runLegacyQueryStakeDistributionCmd :: ()
  => Cmd.LegacyQueryStakeDistributionCmdArgs
  -> ExceptT QueryCmdError IO ()
runLegacyQueryStakeDistributionCmd Cmd.LegacyQueryStakeDistributionCmdArgs {..} =
  EraBased.runQueryStakeDistributionCmd EraBased.QueryStakeDistributionCmdArgs {..}

runLegacyQueryLeadershipScheduleCmd :: ()
  => Cmd.LegacyQueryLeadershipScheduleCmdArgs
  -> ExceptT QueryCmdError IO ()
runLegacyQueryLeadershipScheduleCmd Cmd.LegacyQueryLeadershipScheduleCmdArgs {..} =
  EraBased.runQueryLeadershipScheduleCmd EraBased.QueryLeadershipScheduleCmdArgs {..}
