{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Legacy.Command
  ( LegacyCmds (..)
  , renderLegacyCommand
  )
where

import Cardano.CLI.Legacy.Genesis.Command

import Data.Text (Text)

newtype LegacyCmds
  = LegacyGenesisCmds LegacyGenesisCmds

renderLegacyCommand :: LegacyCmds -> Text
renderLegacyCommand = \case
  LegacyGenesisCmds cmd ->
    renderLegacyGenesisCmds cmd
