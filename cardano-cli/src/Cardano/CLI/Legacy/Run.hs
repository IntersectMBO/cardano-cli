{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Legacy.Run
  ( runLegacyCmds
  ) where

import           Cardano.CLI.EraBased.Run.Pool
import           Cardano.CLI.Legacy.Options
import           Cardano.CLI.Legacy.Run.Address
import           Cardano.CLI.Legacy.Run.Genesis
import           Cardano.CLI.Legacy.Run.Governance
import           Cardano.CLI.Legacy.Run.Key
import           Cardano.CLI.Legacy.Run.Node
import           Cardano.CLI.Legacy.Run.Query
import           Cardano.CLI.Legacy.Run.StakeAddress
import           Cardano.CLI.Legacy.Run.TextView
import           Cardano.CLI.Legacy.Run.Transaction
import           Cardano.CLI.Types.Errors.CmdError

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT)

runLegacyCmds :: LegacyCmds -> ExceptT CmdError IO ()
runLegacyCmds = \case
  LegacyAddressCmds      cmd -> firstExceptT CmdAddressError $ runLegacyAddressCmds cmd
  LegacyGenesisCmds      cmd -> firstExceptT CmdGenesisError $ runLegacyGenesisCmds cmd
  LegacyGovernanceCmds   cmd -> firstExceptT CmdGovernanceCmdError $ runLegacyGovernanceCmds cmd
  LegacyKeyCmds          cmd -> firstExceptT CmdKeyError $ runLegacyKeyCmds cmd
  LegacyNodeCmds         cmd -> firstExceptT CmdNodeError $ runLegacyNodeCmds cmd
  LegacyPoolCmds         cmd -> firstExceptT CmdPoolError $ runLegacyPoolCmds cmd
  LegacyQueryCmds        cmd -> firstExceptT CmdQueryError $ runLegacyQueryCmds cmd
  LegacyStakeAddressCmds cmd -> firstExceptT CmdStakeAddressError $ runLegacyStakeAddressCmds cmd
  LegacyTextViewCmds     cmd -> firstExceptT CmdTextViewError $ runLegacyTextViewCmds cmd
  LegacyTransactionCmds  cmd -> firstExceptT CmdTransactionError $ runLegacyTransactionCmds cmd
