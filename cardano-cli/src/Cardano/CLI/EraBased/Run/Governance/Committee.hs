module Cardano.CLI.EraBased.Run.Governance.Committee
  ( runGovernanceCommitteeCmds
  ) where

import           Cardano.CLI.EraBased.Commands.Governance.Committee

import           Control.Monad.Except (ExceptT)

runGovernanceCommitteeCmds :: GovernanceCommitteeCmds era -> ExceptT () IO ()
runGovernanceCommitteeCmds _cmds = pure ()
