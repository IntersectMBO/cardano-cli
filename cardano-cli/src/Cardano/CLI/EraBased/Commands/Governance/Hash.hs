{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}


module Cardano.CLI.EraBased.Commands.Governance.Hash
  (
    GovernanceHashCmds (..),
    GovernanceHashCmdArgs (..),
    GovernanceHashSource (..),
    renderGovernanceHashCmds
  ) where

import           Cardano.Api

import           Cardano.CLI.Types.Common

import           Data.Text (Text)

newtype GovernanceHashCmds era = GovernanceHashCmd (GovernanceHashCmdArgs era)

data GovernanceHashCmdArgs era
  = GovernanceHashCmdArgs {
      eon    :: !(ConwayEraOnwards era),
      toHash :: !GovernanceHashSource
  } deriving Show

data GovernanceHashSource
  = GovernanceHashSourceBinaryFile (File ProposalText In)
  | GovernanceHashSourceTextFile (File ProposalText In)
  | GovernanceHashSourceText Text
  deriving Show

renderGovernanceHashCmds :: GovernanceHashCmds era -> Text
renderGovernanceHashCmds =
  \case GovernanceHashCmd {} -> "governance hash"
