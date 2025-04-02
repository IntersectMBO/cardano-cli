{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.Compatible.Run
  ( runAnyCompatibleCommand
  , runCompatibleCommand
  )
where

import Cardano.CLI.Compatible.Command
import Cardano.CLI.Compatible.Exception
import Cardano.CLI.Compatible.Governance.Run
import Cardano.CLI.Compatible.StakeAddress.Run
import Cardano.CLI.Compatible.StakePool.Run
import Cardano.CLI.Compatible.Transaction.Run

import Data.Typeable (Typeable)

runAnyCompatibleCommand :: AnyCompatibleCommand -> CIO e ()
runAnyCompatibleCommand (AnyCompatibleCommand cmd) = runCompatibleCommand cmd

runCompatibleCommand :: Typeable era => CompatibleCommand era -> CIO e ()
runCompatibleCommand = \case
  CompatibleTransactionCmds txCmd ->
    runCompatibleTransactionCmd txCmd
  CompatibleGovernanceCmds govCmd ->
    runCompatibleGovernanceCmds govCmd
  CompatibleStakeAddressCmds stakeAddressCmd ->
    runCompatibleStakeAddressCmds stakeAddressCmd
  CompatibleStakePoolCmds stakePoolCmd ->
    runCompatibleStakePoolCmds stakePoolCmd
