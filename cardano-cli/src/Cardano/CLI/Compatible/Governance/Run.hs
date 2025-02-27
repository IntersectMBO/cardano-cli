{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.CLI.Compatible.Governance.Run
  ( runCompatibleGovernanceCmds
  )
where

import Cardano.CLI.Compatible.Exception
import Cardano.CLI.Compatible.Governance.Command
import Cardano.CLI.EraBased.Governance.Run

runCompatibleGovernanceCmds :: CompatibleGovernanceCmds era -> CIO e ()
runCompatibleGovernanceCmds = \case
  CreateCompatibleProtocolUpdateCmd cmd -> fromExceptTCli $ runGovernanceCmds cmd
