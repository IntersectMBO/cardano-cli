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

import Data.Typeable (Typeable)

runCompatibleGovernanceCmds :: Typeable era => CompatibleGovernanceCmds era -> CIO e ()
runCompatibleGovernanceCmds = \case
  CreateCompatibleProtocolUpdateCmd cmd -> runGovernanceCmds cmd
