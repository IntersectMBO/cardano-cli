{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Compatible.Governance
  ( CompatibleGovernanceCmds (..)
  , pCompatibleGovernanceCmds
  , renderCompatibleGovernanceCmds
  , runCompatibleGovernanceCmds
  )
where

import Cardano.Api

import Cardano.CLI.EraBased.Options.Governance
import Cardano.CLI.EraBased.Run.Governance
import Cardano.CLI.Types.Errors.CmdError

import Data.Foldable
import Data.Maybe
import Data.Text
import Options.Applicative

pCompatibleGovernanceCmds :: ShelleyBasedEra era -> Parser (CompatibleGovernanceCmds era)
pCompatibleGovernanceCmds sbe =
  asum $ catMaybes [fmap CreateCompatibleProtocolUpdateCmd <$> pGovernanceCmds sbe]

-- TODO: After QA confirmms that the new compatibility commands meet their needs
-- we can remove all remaining legacy commands. We can also remove/move the exising
-- byron era commands under the new compatiblilty commands.
newtype CompatibleGovernanceCmds era
  = CreateCompatibleProtocolUpdateCmd (GovernanceCmds era)

runCompatibleGovernanceCmds :: CompatibleGovernanceCmds era -> ExceptT CmdError IO ()
runCompatibleGovernanceCmds = \case
  CreateCompatibleProtocolUpdateCmd cmd -> runGovernanceCmds cmd

renderCompatibleGovernanceCmds :: CompatibleGovernanceCmds era -> Text
renderCompatibleGovernanceCmds = \case
  CreateCompatibleProtocolUpdateCmd cmd -> renderGovernanceCmds cmd
