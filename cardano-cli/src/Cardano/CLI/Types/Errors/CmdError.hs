{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Types.Errors.CmdError
  ( CmdError (..)
  , renderCmdError
  ) where

import Cardano.Api

import Cardano.CLI.Types.Errors.AddressCmdError
import Cardano.CLI.Types.Errors.DelegationError
import Cardano.CLI.Types.Errors.GenesisCmdError
import Cardano.CLI.Types.Errors.GovernanceActionsError
import Cardano.CLI.Types.Errors.GovernanceCmdError
import Cardano.CLI.Types.Errors.GovernanceCommitteeError
import Cardano.CLI.Types.Errors.GovernanceQueryError
import Cardano.CLI.Types.Errors.GovernanceVoteCmdError
import Cardano.CLI.Types.Errors.KeyCmdError
import Cardano.CLI.Types.Errors.NodeCmdError
import Cardano.CLI.Types.Errors.QueryCmdError
import Cardano.CLI.Types.Errors.RegistrationError
import Cardano.CLI.Types.Errors.StakeAddressCmdError
import Cardano.CLI.Types.Errors.StakePoolCmdError
import Cardano.CLI.Types.Errors.TextViewFileError
import Cardano.CLI.Types.Errors.TxCmdError

import Data.Text (Text)
import qualified Data.Text as Text

data CmdError
  = CmdAddressError !AddressCmdError
  | CmdEraDelegationError !DelegationError
  | CmdGenesisError !GenesisCmdError
  | CmdGovernanceActionError !GovernanceActionsError
  | CmdGovernanceCmdError !GovernanceCmdError
  | CmdGovernanceCommitteeError !GovernanceCommitteeError
  | CmdGovernanceQueryError !GovernanceQueryError
  | CmdGovernanceVoteError !GovernanceVoteCmdError
  | CmdKeyError !KeyCmdError
  | CmdNodeError !NodeCmdError
  | CmdQueryError !QueryCmdError
  | CmdRegistrationError !RegistrationError
  | CmdStakeAddressError !StakeAddressCmdError
  | CmdStakePoolError !StakePoolCmdError
  | CmdTextViewError !TextViewFileError
  | CmdTransactionError !TxCmdError

renderCmdError :: Text -> CmdError -> Text
renderCmdError cmdText = \case
  CmdAddressError e -> renderError renderAddressCmdError e
  CmdEraDelegationError e -> renderError (Text.pack . displayError) e
  CmdGenesisError e -> renderError (Text.pack . displayError) e
  CmdGovernanceActionError e -> renderError (Text.pack . displayError) e
  CmdGovernanceCmdError e -> renderError (Text.pack . displayError) e
  CmdGovernanceCommitteeError e -> renderError (Text.pack . displayError) e
  CmdGovernanceQueryError e -> renderError (Text.pack . displayError) e
  CmdGovernanceVoteError e -> renderError (Text.pack . displayError) e
  CmdKeyError e -> renderError renderKeyCmdError e
  CmdNodeError e -> renderError renderNodeCmdError e
  CmdQueryError e -> renderError renderQueryCmdError e
  CmdRegistrationError e -> renderError (Text.pack . displayError) e
  CmdStakeAddressError e -> renderError (Text.pack . displayError) e
  CmdStakePoolError e -> renderError renderStakePoolCmdError e
  CmdTextViewError e -> renderError renderTextViewFileError e
  CmdTransactionError e -> renderError renderTxCmdError e
 where
  renderError :: (a -> Text) -> a -> Text
  renderError renderer shelCliCmdErr =
    mconcat
      [ "Command failed: "
      , cmdText
      , "  Error: "
      , renderer shelCliCmdErr
      ]
