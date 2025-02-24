{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Compatible.Governance.Run
  ( runCompatibleGovernanceCmds
  )
where

import Cardano.Api

import Cardano.CLI.Compatible.Governance.Command
import Cardano.CLI.EraBased.Governance.Run
import Cardano.CLI.Type.Error.CmdError

runCompatibleGovernanceCmds :: CompatibleGovernanceCmds era -> ExceptT CmdError IO ()
runCompatibleGovernanceCmds = \case
  CreateCompatibleProtocolUpdateCmd cmd -> runGovernanceCmds cmd
