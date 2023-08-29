{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Legacy.Run
  ( runLegacyCmds
  ) where

import           Cardano.CLI.Legacy.Options
import           Cardano.CLI.Legacy.Run.Address
import           Cardano.CLI.Legacy.Run.Genesis
import           Cardano.CLI.Legacy.Run.Governance
import           Cardano.CLI.Legacy.Run.Key
import           Cardano.CLI.Legacy.Run.Node
import           Cardano.CLI.Legacy.Run.Pool
import           Cardano.CLI.Legacy.Run.Query
import           Cardano.CLI.Legacy.Run.StakeAddress
import           Cardano.CLI.Legacy.Run.TextView
import           Cardano.CLI.Legacy.Run.Transaction
import           Cardano.CLI.Types.Errors.CmdError

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT)

runLegacyCmds :: LegacyCmds -> ExceptT CmdError IO ()
runLegacyCmds = \case
  LegacyAddressCmds      cmd -> firstExceptT CmdAddressError $ runAddressCmds cmd
  LegacyGenesisCmds      cmd -> firstExceptT CmdGenesisError $ runGenesisCmds cmd
  LegacyGovernanceCmds   cmd -> firstExceptT CmdGovernanceCmdError $ runLegacyGovernanceCmds cmd
  LegacyKeyCmds          cmd -> firstExceptT CmdKeyError $ runKeyCmds cmd
  LegacyNodeCmds         cmd -> firstExceptT CmdNodeError $ runNodeCmds cmd
  LegacyPoolCmds         cmd -> firstExceptT CmdPoolError $ runPoolCmds cmd
  LegacyQueryCmds        cmd -> firstExceptT CmdQueryError $ runQueryCmds cmd
  LegacyStakeAddressCmds cmd -> firstExceptT CmdStakeAddressError $ runStakeAddressCmds cmd
  LegacyTextViewCmds     cmd -> firstExceptT CmdTextViewError $ runTextViewCmds cmd
  LegacyTransactionCmds  cmd -> firstExceptT CmdTransactionError $ runTransactionCmds cmd
