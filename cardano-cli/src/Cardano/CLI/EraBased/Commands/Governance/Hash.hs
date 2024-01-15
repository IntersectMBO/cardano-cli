{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.Commands.Governance.Hash
  (
    GovernanceHashCmds (..),
    GovernanceHashAnchorDataCmdArgs (..),
    GovernanceHashScriptCmdArgs (..),
    GovernanceAnchorDataHashSource (..),
    renderGovernanceHashCmds
  ) where

import           Cardano.Api

import           Cardano.CLI.Types.Common

import           Data.Text (Text)

data GovernanceHashCmds era
  = GovernanceHashAnchorDataCmd !(GovernanceHashAnchorDataCmdArgs era)
  | GovernanceHashScriptCmd !(GovernanceHashScriptCmdArgs era)

data GovernanceHashAnchorDataCmdArgs era
  = GovernanceHashAnchorDataCmdArgs {
      eon     :: !(ConwayEraOnwards era)
    , toHash  :: !GovernanceAnchorDataHashSource
    , moutFile :: !(Maybe (File () Out)) -- ^ The output file to which the hash is written
  } deriving Show

data GovernanceAnchorDataHashSource
  = GovernanceAnchorDataHashSourceBinaryFile (File ProposalText In)
  | GovernanceAnchorDataHashSourceTextFile (File ProposalText In)
  | GovernanceAnchorDataHashSourceText Text
  deriving Show

data GovernanceHashScriptCmdArgs era
  = GovernanceHashScriptCmdArgs {
      eon     :: !(ConwayEraOnwards era)
    , toHash  ::  !ScriptFile
    , moutFile :: !(Maybe (File () Out)) -- ^ The output file to which the hash is written
  } deriving Show

renderGovernanceHashCmds :: GovernanceHashCmds era -> Text
renderGovernanceHashCmds = \case
  GovernanceHashAnchorDataCmd {} -> "governance hash anchor-data"
  GovernanceHashScriptCmd {} -> "governance hash script"
