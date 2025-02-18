{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Legacy.Commands
  ( LegacyCmds (..)
  , renderLegacyCommand
  )
where

import Cardano.CLI.Legacy.Commands.Genesis
import Cardano.CLI.Legacy.Commands.Governance

import Data.Text (Text)

data LegacyCmds
  = LegacyGenesisCmds LegacyGenesisCmds
  | LegacyGovernanceCmds LegacyGovernanceCmds

renderLegacyCommand :: LegacyCmds -> Text
renderLegacyCommand = \case
  LegacyGovernanceCmds cmd ->
    renderLegacyGovernanceCmds cmd
  LegacyGenesisCmds cmd ->
    renderLegacyGenesisCmds cmd
