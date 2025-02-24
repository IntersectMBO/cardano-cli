{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Legacy.Command
  ( LegacyCmds (..)
  , renderLegacyCommand
  )
where

import Cardano.CLI.Legacy.Genesis.Command
import Cardano.CLI.Legacy.Governance.Command

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
