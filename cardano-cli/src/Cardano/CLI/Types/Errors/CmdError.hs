{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Types.Errors.CmdError
  ( CmdError(..)
  , renderCmdError
  ) where

import           Cardano.Api

import           Cardano.CLI.Types.Errors.AddressCmdError
import           Cardano.CLI.Types.Errors.DelegationError
import           Cardano.CLI.Types.Errors.GenesisCmdError
import           Cardano.CLI.Types.Errors.GovernanceActionsError
import           Cardano.CLI.Types.Errors.GovernanceCmdError
import           Cardano.CLI.Types.Errors.GovernanceCommitteeError
import           Cardano.CLI.Types.Errors.GovernanceQueryError
import           Cardano.CLI.Types.Errors.GovernanceVoteCmdError
import           Cardano.CLI.Types.Errors.KeyCmdError
import           Cardano.CLI.Types.Errors.NodeCmdError
import           Cardano.CLI.Types.Errors.QueryCmdError
import           Cardano.CLI.Types.Errors.RegistrationError
import           Cardano.CLI.Types.Errors.StakeAddressCmdError
import           Cardano.CLI.Types.Errors.StakePoolCmdError
import           Cardano.CLI.Types.Errors.TextViewFileError
import           Cardano.CLI.Types.Errors.TxCmdError

import           Data.Text (Text)

data CmdError
  = CmdAddressError               !AddressCmdError
  | CmdEraDelegationError         !DelegationError
  | CmdGenesisError               !GenesisCmdError
  | CmdGovernanceActionError      !GovernanceActionsError
  | CmdGovernanceCmdError         !GovernanceCmdError
  | CmdGovernanceCommitteeError   !GovernanceCommitteeError
  | CmdGovernanceQueryError       !GovernanceQueryError
  | CmdGovernanceVoteError        !GovernanceVoteCmdError
  | CmdKeyError                   !KeyCmdError
  | CmdNodeError                  !NodeCmdError
  | CmdQueryError                 !QueryCmdError
  | CmdRegistrationError          !RegistrationError
  | CmdStakeAddressError          !StakeAddressCmdError
  | CmdStakePoolError             !StakePoolCmdError
  | CmdTextViewError              !TextViewFileError
  | CmdTransactionError           !TxCmdError

renderCmdError :: Text -> CmdError -> Doc ann
renderCmdError cmdText = \case
  CmdAddressError               e -> renderError renderAddressCmdError e
  CmdEraDelegationError         e -> renderError prettyError e
  CmdGenesisError               e -> renderError prettyError e
  CmdGovernanceActionError      e -> renderError prettyError e
  CmdGovernanceCmdError         e -> renderError prettyError e
  CmdGovernanceCommitteeError   e -> renderError prettyError e
  CmdGovernanceQueryError       e -> renderError prettyError e
  CmdGovernanceVoteError        e -> renderError prettyError e
  CmdKeyError                   e -> renderError renderKeyCmdError e
  CmdNodeError                  e -> renderError renderNodeCmdError e
  CmdQueryError                 e -> renderError renderQueryCmdError e
  CmdRegistrationError          e -> renderError prettyError e
  CmdStakeAddressError          e -> renderError prettyError e
  CmdStakePoolError             e -> renderError renderStakePoolCmdError e
  CmdTextViewError              e -> renderError renderTextViewFileError e
  CmdTransactionError           e -> renderError renderTxCmdError e
  where
    renderError :: (a -> Doc ann) -> a -> Doc ann
    renderError renderer shelCliCmdErr =
      mconcat
        [ "Command failed: "
        , pretty cmdText
        , "  Error: "
        , renderer shelCliCmdErr
        ]
