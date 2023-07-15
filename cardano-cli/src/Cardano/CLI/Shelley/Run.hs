{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Shelley.Run
  ( LegacyClientCmdError
  , renderLegacyClientCmdError
  , runLegacyClientCommand
  ) where

import           Cardano.Api

import           Cardano.CLI.Conway.Commands
import           Cardano.CLI.Shelley.Parsers
import           Cardano.CLI.Shelley.Run.Address
import           Cardano.CLI.Shelley.Run.Genesis
import           Cardano.CLI.Shelley.Run.Governance
import           Cardano.CLI.Shelley.Run.Key
import           Cardano.CLI.Shelley.Run.Node
import           Cardano.CLI.Shelley.Run.Pool
import           Cardano.CLI.Shelley.Run.Query
import           Cardano.CLI.Shelley.Run.StakeAddress
import           Cardano.CLI.Shelley.Run.TextView
import           Cardano.CLI.Shelley.Run.Transaction

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT)
import           Data.Text (Text)
import qualified Data.Text as Text

data LegacyClientCmdError
  = LegacyCmdAddressError !ShelleyAddressCmdError
  | LegacyCmdGenesisError !ShelleyGenesisCmdError
  | LegacyCmdGovernanceError !GovernanceCmdError
  | LegacyCmdNodeError !ShelleyNodeCmdError
  | LegacyCmdPoolError !ShelleyPoolCmdError
  | LegacyCmdStakeAddressError !ShelleyStakeAddressCmdError
  | LegacyCmdTextViewError !ShelleyTextViewFileError
  | LegacyCmdTransactionError !ShelleyTxCmdError
  | LegacyCmdQueryError !ShelleyQueryCmdError
  | LegacyCmdKeyError !ShelleyKeyCmdError

renderLegacyClientCmdError :: LegacyCommand -> LegacyClientCmdError -> Text
renderLegacyClientCmdError cmd err =
  case err of
    LegacyCmdAddressError addrCmdErr ->
       renderError cmd renderShelleyAddressCmdError addrCmdErr
    LegacyCmdGenesisError genesisCmdErr ->
       renderError cmd (Text.pack . displayError) genesisCmdErr
    LegacyCmdGovernanceError govCmdErr -> Text.pack $ show govCmdErr
       -- TODO: Conway era
       -- renderError cmd renderShelleyGovernanceError govCmdErr
    LegacyCmdNodeError nodeCmdErr ->
       renderError cmd renderShelleyNodeCmdError nodeCmdErr
    LegacyCmdPoolError poolCmdErr ->
       renderError cmd renderShelleyPoolCmdError poolCmdErr
    LegacyCmdStakeAddressError stakeAddrCmdErr ->
       renderError cmd renderShelleyStakeAddressCmdError stakeAddrCmdErr
    LegacyCmdTextViewError txtViewErr ->
       renderError cmd renderShelleyTextViewFileError txtViewErr
    LegacyCmdTransactionError txErr ->
       renderError cmd renderShelleyTxCmdError txErr
    LegacyCmdQueryError queryErr ->
       renderError cmd renderShelleyQueryCmdError queryErr
    LegacyCmdKeyError keyErr ->
       renderError cmd renderShelleyKeyCmdError keyErr
  where
    renderError :: LegacyCommand -> (a -> Text) -> a -> Text
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

runLegacyClientCommand :: LegacyCommand -> ExceptT LegacyClientCmdError IO ()
runLegacyClientCommand = \case
  AddressCmd      cmd -> firstExceptT LegacyCmdAddressError $ runAddressCmd cmd
  StakeAddressCmd cmd -> firstExceptT LegacyCmdStakeAddressError $ runStakeAddressCmd cmd
  KeyCmd          cmd -> firstExceptT LegacyCmdKeyError $ runKeyCmd cmd
  TransactionCmd  cmd -> firstExceptT LegacyCmdTransactionError $ runTransactionCmd  cmd
  NodeCmd         cmd -> firstExceptT LegacyCmdNodeError $ runNodeCmd cmd
  PoolCmd         cmd -> firstExceptT LegacyCmdPoolError $ runPoolCmd cmd
  QueryCmd        cmd -> firstExceptT LegacyCmdQueryError $ runQueryCmd cmd
  GovernanceCmd'  cmd -> firstExceptT LegacyCmdGovernanceError $ runGovernanceCmd cmd
  GenesisCmd      cmd -> firstExceptT LegacyCmdGenesisError $ runGenesisCmd cmd
  TextViewCmd     cmd -> firstExceptT LegacyCmdTextViewError $ runTextViewCmd cmd
