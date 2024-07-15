{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Legacy.Commands.Node
  ( LegacyNodeCmds (..)
  , renderLegacyNodeCmds
  )
where

import qualified Cardano.CLI.EraBased.Commands.Node as Cmd

import           Data.Text (Text)

data LegacyNodeCmds
  = LegacyNodeKeyGenColdCmd !Cmd.NodeKeyGenColdCmdArgs
  | LegacyNodeKeyGenKESCmd !Cmd.NodeKeyGenKESCmdArgs
  | LegacyNodeKeyGenVRFCmd !Cmd.NodeKeyGenVRFCmdArgs
  | LegacyNodeKeyHashVRFCmd !Cmd.NodeKeyHashVRFCmdArgs
  | LegacyNodeNewCounterCmd !Cmd.NodeNewCounterCmdArgs
  | LegacyNodeIssueOpCertCmd !Cmd.NodeIssueOpCertCmdArgs
  deriving Show

renderLegacyNodeCmds :: LegacyNodeCmds -> Text
renderLegacyNodeCmds = \case
  LegacyNodeKeyGenColdCmd{} -> "node key-gen"
  LegacyNodeKeyGenKESCmd{} -> "node key-gen-KES"
  LegacyNodeKeyGenVRFCmd{} -> "node key-gen-VRF"
  LegacyNodeKeyHashVRFCmd{} -> "node key-hash-VRF"
  LegacyNodeNewCounterCmd{} -> "node new-counter"
  LegacyNodeIssueOpCertCmd{} -> "node issue-op-cert"
