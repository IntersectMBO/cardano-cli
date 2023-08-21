{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Legacy.Run
  ( LegacyClientCmdError
  , renderLegacyClientCmdError
  , runLegacyCmds
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
import           Cardano.CLI.Types.Errors.LegacyClientCmdError

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT)

runLegacyCmds :: LegacyCmds -> ExceptT LegacyClientCmdError IO ()
runLegacyCmds = \case
  LegacyAddressCmds      cmd -> firstExceptT LegacyCmdAddressError $ runAddressCmds cmd
  LegacyStakeAddressCmds cmd -> firstExceptT LegacyCmdStakeAddressError $ runStakeAddressCmds cmd
  LegacyKeyCmds          cmd -> firstExceptT LegacyCmdKeyError $ runKeyCmds cmd
  LegacyTransactionCmds  cmd -> firstExceptT LegacyCmdTransactionError $ runTransactionCmds cmd
  LegacyNodeCmds         cmd -> firstExceptT LegacyCmdNodeError $ runNodeCmds cmd
  LegacyPoolCmds         cmd -> firstExceptT LegacyCmdPoolError $ runPoolCmds cmd
  LegacyQueryCmds        cmd -> firstExceptT LegacyCmdQueryError $ runQueryCmds cmd
  LegacyGovernanceCmds   cmd -> firstExceptT LegacyCmdGovernanceError $ runGovernanceCmds cmd
  LegacyGenesisCmds      cmd -> firstExceptT LegacyCmdGenesisError $ runGenesisCmds cmd
  LegacyTextViewCmds     cmd -> firstExceptT LegacyCmdTextViewError $ runTextViewCmds cmd
