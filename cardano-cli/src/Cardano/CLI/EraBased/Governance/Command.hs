{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.Governance.Command
  ( GovernanceCmds (..)
  , renderGovernanceCmds
  )
where

import Cardano.CLI.EraBased.Governance.Actions.Command
import Cardano.CLI.EraBased.Governance.Committee.Command
import Cardano.CLI.EraBased.Governance.DRep.Command
import Cardano.CLI.EraBased.Governance.Vote.Command

import Data.Text (Text)

data GovernanceCmds era
  = GovernanceActionCmds
      (GovernanceActionCmds era)
  | GovernanceCommitteeCmds
      (GovernanceCommitteeCmds era)
  | GovernanceDRepCmds
      (GovernanceDRepCmds era)
  | GovernanceVoteCmds
      (GovernanceVoteCmds era)

renderGovernanceCmds :: GovernanceCmds era -> Text
renderGovernanceCmds = \case
  GovernanceActionCmds cmds ->
    renderGovernanceActionCmds cmds
  GovernanceCommitteeCmds cmds ->
    renderGovernanceCommitteeCmds cmds
  GovernanceDRepCmds cmds ->
    renderGovernanceDRepCmds cmds
  GovernanceVoteCmds cmds ->
    renderGovernanceVoteCmds cmds
