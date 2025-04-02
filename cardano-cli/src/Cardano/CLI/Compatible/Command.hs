{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

{-
This module is concerned with providing backwards compatible cli commands for our internal
testing needs. The intention is to restrict as much as possible which functionality we maintain backwards
compatibility for.
-}

module Cardano.CLI.Compatible.Command
  ( AnyCompatibleCommand (..)
  , CompatibleCommand (..)
  , renderAnyCompatibleCommand
  )
where

import Cardano.CLI.Compatible.Governance.Command
import Cardano.CLI.Compatible.StakeAddress.Command
import Cardano.CLI.Compatible.StakePool.Command
import Cardano.CLI.Compatible.Transaction.Command

import Data.Text
import Data.Typeable (Typeable)

data AnyCompatibleCommand where
  AnyCompatibleCommand :: Typeable era => CompatibleCommand era -> AnyCompatibleCommand

renderAnyCompatibleCommand :: AnyCompatibleCommand -> Text
renderAnyCompatibleCommand = \case
  AnyCompatibleCommand cmd -> renderCompatibleCommand cmd

data CompatibleCommand era
  = CompatibleTransactionCmds (CompatibleTransactionCmds era)
  | CompatibleGovernanceCmds (CompatibleGovernanceCmds era)
  | CompatibleStakeAddressCmds (CompatibleStakeAddressCmds era)
  | CompatibleStakePoolCmds (CompatibleStakePoolCmds era)

renderCompatibleCommand :: CompatibleCommand era -> Text
renderCompatibleCommand = \case
  CompatibleTransactionCmds cmd -> renderCompatibleTransactionCmd cmd
  CompatibleGovernanceCmds cmd -> renderCompatibleGovernanceCmds cmd
  CompatibleStakeAddressCmds cmd -> renderCompatibleStakeAddressCmds cmd
  CompatibleStakePoolCmds cmd -> renderCompatibleStakePoolCmds cmd
