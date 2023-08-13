{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Legacy.Run
  ( LegacyClientCmdError
  , renderLegacyClientCmdError
  , runLegacyCmds
  ) where

import           Cardano.Api

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
import           Cardano.CLI.Types.Errors.AddressCmdError
import           Cardano.CLI.Types.Errors.KeyCmdError
import           Cardano.CLI.Types.Errors.LegacyClientCmdError
import           Cardano.CLI.Types.Errors.NodeCmdError
import           Cardano.CLI.Types.Errors.PoolCmdError
import           Cardano.CLI.Types.Errors.QueryCmdError
import           Cardano.CLI.Types.Errors.StakeAddressCmdError
import           Cardano.CLI.Types.Errors.TextViewFileError
import           Cardano.CLI.Types.Errors.TxCmdError

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT)
import           Data.Text (Text)
import qualified Data.Text as Text

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
  LegacyAddressCmds      cmd -> firstExceptT LegacyCmdAddressError      $ runLegacyAddressCmds      cmd
  LegacyStakeAddressCmds cmd -> firstExceptT LegacyCmdStakeAddressError $ runLegacyStakeAddressCmds cmd
  LegacyKeyCmds          cmd -> firstExceptT LegacyCmdKeyError          $ runLegacyKeyCmds          cmd
  LegacyTransactionCmds  cmd -> firstExceptT LegacyCmdTransactionError  $ runLegacyTransactionCmds  cmd
  LegacyNodeCmds         cmd -> firstExceptT LegacyCmdNodeError         $ runLegacyNodeCmds         cmd
  LegacyPoolCmds         cmd -> firstExceptT LegacyCmdPoolError         $ runLegacyPoolCmds         cmd
  LegacyQueryCmds        cmd -> firstExceptT LegacyCmdQueryError        $ runLegacyQueryCmds        cmd
  LegacyGovernanceCmds   cmd -> firstExceptT LegacyCmdGovernanceError   $ runLegacyGovernanceCmds   cmd
  LegacyGenesisCmds      cmd -> firstExceptT LegacyCmdGenesisError      $ runLegacyGenesisCmds      cmd
  LegacyTextViewCmds     cmd -> firstExceptT LegacyCmdTextViewError     $ runLegacyTextViewCmds     cmd
