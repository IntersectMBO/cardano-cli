{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Legacy.Run
  ( runLegacyCmds
  )
where

import           Cardano.CLI.Legacy.Options
import           Cardano.CLI.Legacy.Run.Genesis
import           Cardano.CLI.Legacy.Run.Governance
import           Cardano.CLI.Types.Errors.CmdError

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT)

runLegacyCmds :: LegacyCmds -> ExceptT CmdError IO ()
runLegacyCmds = \case
  LegacyGenesisCmds cmd -> firstExceptT CmdGenesisError $ runLegacyGenesisCmds cmd
  LegacyGovernanceCmds cmd -> firstExceptT CmdGovernanceCmdError $ runLegacyGovernanceCmds cmd
