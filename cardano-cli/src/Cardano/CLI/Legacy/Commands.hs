{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Legacy.Commands
  ( LegacyCmds (..)
  , renderLegacyCommand
  )
where

import           Cardano.CLI.Legacy.Commands.Address
import           Cardano.CLI.Legacy.Commands.Genesis
import           Cardano.CLI.Legacy.Commands.Governance
import           Cardano.CLI.Legacy.Commands.Key
import           Cardano.CLI.Legacy.Commands.Node
import           Cardano.CLI.Legacy.Commands.Query
import           Cardano.CLI.Legacy.Commands.StakeAddress
import           Cardano.CLI.Legacy.Commands.StakePool
import           Cardano.CLI.Legacy.Commands.TextView
import           Cardano.CLI.Legacy.Commands.Transaction

import           Data.Text (Text)

data LegacyCmds
  = LegacyAddressCmds LegacyAddressCmds
  | LegacyGenesisCmds LegacyGenesisCmds
  | LegacyGovernanceCmds LegacyGovernanceCmds
  | LegacyKeyCmds LegacyKeyCmds
  | LegacyNodeCmds LegacyNodeCmds
  | LegacyQueryCmds LegacyQueryCmds
  | LegacyStakeAddressCmds LegacyStakeAddressCmds
  | LegacyStakePoolCmds LegacyStakePoolCmds
  | LegacyTextViewCmds LegacyTextViewCmds
  | LegacyTransactionCmds LegacyTransactionCmds

renderLegacyCommand :: LegacyCmds -> Text
renderLegacyCommand = \case
  LegacyAddressCmds cmd ->
    renderLegacyAddressCmds cmd
  LegacyStakeAddressCmds cmd ->
    renderLegacyStakeAddressCmds cmd
  LegacyKeyCmds cmd ->
    renderLegacyKeyCmds cmd
  LegacyTransactionCmds cmd ->
    renderLegacyTransactionCmds cmd
  LegacyNodeCmds cmd ->
    renderLegacyNodeCmds cmd
  LegacyStakePoolCmds cmd ->
    renderLegacyStakePoolCmds cmd
  LegacyQueryCmds cmd ->
    renderLegacyQueryCmds cmd
  LegacyGovernanceCmds cmd ->
    renderLegacyGovernanceCmds cmd
  LegacyGenesisCmds cmd ->
    renderLegacyGenesisCmds cmd
  LegacyTextViewCmds cmd ->
    renderLegacyTextViewCmds cmd
