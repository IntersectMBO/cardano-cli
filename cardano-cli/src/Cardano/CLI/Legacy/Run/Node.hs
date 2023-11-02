{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Legacy.Run.Node
  ( runLegacyNodeCmds
  ) where

import qualified Cardano.CLI.EraBased.Commands.Node as Cmd
import           Cardano.CLI.EraBased.Run.Node
import           Cardano.CLI.Legacy.Commands.Node
import           Cardano.CLI.Types.Errors.NodeCmdError

import           Control.Monad.Trans.Except (ExceptT)

{- HLINT ignore "Reduce duplication" -}

runLegacyNodeCmds :: ()
  => LegacyNodeCmds
  -> ExceptT NodeCmdError IO ()
runLegacyNodeCmds = \case
  LegacyNodeKeyGenColdCmd args  -> runLegacyNodeKeyGenColdCmd args
  LegacyNodeKeyGenKESCmd args   -> runLegacyNodeKeyGenKesCmd args
  LegacyNodeKeyGenVRFCmd args   -> runLegacyNodeKeyGenVrfCmd args
  LegacyNodeKeyHashVRFCmd args  -> runLegacyNodeKeyHashVrfCmd args
  LegacyNodeNewCounterCmd args  -> runLegacyNodeNewCounterCmd args
  LegacyNodeIssueOpCertCmd args -> runLegacyNodeIssueOpCertCmd args

runLegacyNodeKeyGenColdCmd :: ()
  => Cmd.NodeKeyGenColdCmdArgs
  -> ExceptT NodeCmdError IO ()
runLegacyNodeKeyGenColdCmd = runNodeKeyGenColdCmd

runLegacyNodeKeyGenKesCmd :: ()
  => Cmd.NodeKeyGenKESCmdArgs
  -> ExceptT NodeCmdError IO ()
runLegacyNodeKeyGenKesCmd = runNodeKeyGenKesCmd

runLegacyNodeKeyGenVrfCmd :: ()
  => Cmd.NodeKeyGenVRFCmdArgs
  -> ExceptT NodeCmdError IO ()
runLegacyNodeKeyGenVrfCmd = runNodeKeyGenVrfCmd

runLegacyNodeKeyHashVrfCmd :: ()
  => Cmd.NodeKeyHashVRFCmdArgs
  -> ExceptT NodeCmdError IO ()
runLegacyNodeKeyHashVrfCmd = runNodeKeyHashVrfCmd

runLegacyNodeNewCounterCmd :: ()
  => Cmd.NodeNewCounterCmdArgs
  -> ExceptT NodeCmdError IO ()
runLegacyNodeNewCounterCmd = runNodeNewCounterCmd

runLegacyNodeIssueOpCertCmd :: ()
  => Cmd.NodeIssueOpCertCmdArgs
  -> ExceptT NodeCmdError IO ()
runLegacyNodeIssueOpCertCmd = runNodeIssueOpCertCmd
