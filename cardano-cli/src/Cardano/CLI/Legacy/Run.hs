{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Legacy.Run
  ( LegacyClientCmdError
  , renderLegacyClientCmdError
  , runLegacyCmds
  ) where

import           Cardano.Api

import           Cardano.CLI.Commands.Governance
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

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT)
import           Data.Text (Text)
import qualified Data.Text as Text

data LegacyClientCmdError
  = LegacyCmdAddressError !AddressCmdError
  | LegacyCmdGenesisError !GenesisCmdError
  | LegacyCmdGovernanceError !GovernanceCmdError
  | LegacyCmdNodeError !NodeCmdError
  | LegacyCmdPoolError !PoolCmdError
  | LegacyCmdStakeAddressError !StakeAddressCmdError
  | LegacyCmdTextViewError !TextViewFileError
  | LegacyCmdTransactionError !TxCmdError
  | LegacyCmdQueryError !QueryCmdError
  | LegacyCmdKeyError !KeyCmdError

renderLegacyClientCmdError :: LegacyCmds -> LegacyClientCmdError -> Text
renderLegacyClientCmdError cmd err =
  case err of
    LegacyCmdAddressError addrCmdErr ->
       renderError cmd renderAddressCmdError addrCmdErr
    LegacyCmdGenesisError genesisCmdErr ->
       renderError cmd (Text.pack . displayError) genesisCmdErr
    LegacyCmdGovernanceError govCmdErr ->
       renderError cmd (Text.pack . displayError) govCmdErr
    LegacyCmdNodeError nodeCmdErr ->
       renderError cmd renderNodeCmdError nodeCmdErr
    LegacyCmdPoolError poolCmdErr ->
       renderError cmd renderPoolCmdError poolCmdErr
    LegacyCmdStakeAddressError stakeAddrCmdErr ->
       renderError cmd renderStakeAddressCmdError stakeAddrCmdErr
    LegacyCmdTextViewError txtViewErr ->
       renderError cmd renderTextViewFileError txtViewErr
    LegacyCmdTransactionError txErr ->
       renderError cmd renderTxCmdError txErr
    LegacyCmdQueryError queryErr ->
       renderError cmd renderQueryCmdError queryErr
    LegacyCmdKeyError keyErr ->
       renderError cmd renderKeyCmdError keyErr
  where
    renderError :: LegacyCmds -> (a -> Text) -> a -> Text
    renderError shelleyCmd renderer shelCliCmdErr =
      mconcat
        [ "Command failed: "
        , renderLegacyCommand shelleyCmd
        , "  Error: "
        , renderer shelCliCmdErr
        ]


--
-- CLI shelley command dispatch
--

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
