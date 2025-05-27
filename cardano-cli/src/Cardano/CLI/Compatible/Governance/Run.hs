{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.CLI.Compatible.Governance.Run
  ( runCompatibleGovernanceCmds
  )
where

import Cardano.CLI.Compatible.Exception
import Cardano.CLI.Compatible.Governance.Command
import Cardano.CLI.EraBased.Governance.Actions.Run
import Cardano.CLI.EraBased.Governance.Run

import Data.Typeable (Typeable)

runCompatibleGovernanceCmds :: Typeable era => CompatibleGovernanceCmds era -> CIO e ()
runCompatibleGovernanceCmds = \case
  CreateCompatibleProtocolUpdateCmd cmd -> runGovernanceCmds cmd
  CreateCompatibleProtocolParametersUpdateCmd cmd ->
    runGovernanceActionCmds cmd
  CreateCompatibleMirCertificateCmd cmd -> runGovernanceCmds cmd
  CreateCompatibleGenesisKeyDelegationCertificateCmd cmd ->
    runGovernanceCmds cmd
  LatestCompatibleGovernanceCmds cmd -> runGovernanceCmds cmd
