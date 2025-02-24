{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Compatible.Governance.Command
  ( CompatibleGovernanceCmds (..)
  , renderCompatibleGovernanceCmds
  )
where

import Cardano.CLI.EraBased.Governance.Option

import Data.Text

-- TODO: After QA confirmms that the new compatibility commands meet their needs
-- we can remove all remaining legacy commands. We can also remove/move the exising
-- byron era commands under the new compatiblilty commands.
newtype CompatibleGovernanceCmds era
  = CreateCompatibleProtocolUpdateCmd (GovernanceCmds era)

renderCompatibleGovernanceCmds :: CompatibleGovernanceCmds era -> Text
renderCompatibleGovernanceCmds = \case
  CreateCompatibleProtocolUpdateCmd cmd -> renderGovernanceCmds cmd
