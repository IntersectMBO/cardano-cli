{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Legacy.Run
  ( runLegacyCmds
  )
where

import Cardano.CLI.Legacy.Genesis.Run
import Cardano.CLI.Legacy.Governance.Run
import Cardano.CLI.Legacy.Option
import Cardano.CLI.Type.Error.CmdError

import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Except.Extra (firstExceptT)

runLegacyCmds :: LegacyCmds -> ExceptT CmdError IO ()
runLegacyCmds = \case
  LegacyGenesisCmds cmd -> firstExceptT CmdGenesisError $ runLegacyGenesisCmds cmd
  LegacyGovernanceCmds cmd -> firstExceptT CmdGovernanceCmdError $ runLegacyGovernanceCmds cmd
