{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Type.Error.CmdError
  ( CmdError (..)
  , renderCmdError
  )
where

import Cardano.Api

import Cardano.CLI.Render
import Cardano.CLI.Type.Error.AddressCmdError
import Cardano.CLI.Type.Error.DelegationError
import Cardano.CLI.Type.Error.GenesisCmdError
import Cardano.CLI.Type.Error.GovernanceActionsError
import Cardano.CLI.Type.Error.GovernanceCmdError
import Cardano.CLI.Type.Error.GovernanceCommitteeError
import Cardano.CLI.Type.Error.GovernanceQueryError
import Cardano.CLI.Type.Error.GovernanceVoteCmdError
import Cardano.CLI.Type.Error.KeyCmdError
import Cardano.CLI.Type.Error.NodeCmdError
import Cardano.CLI.Type.Error.QueryCmdError
import Cardano.CLI.Type.Error.RegistrationError
import Cardano.CLI.Type.Error.StakeAddressCmdError
import Cardano.CLI.Type.Error.StakePoolCmdError
import Cardano.CLI.Type.Error.TextViewFileError
import Cardano.CLI.Type.Error.TxCmdError

import Data.Text (Text)

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

renderCmdError :: Text -> CmdError -> Doc ann
renderCmdError cmdText = \case
  CmdAddressError e -> renderError renderAddressCmdError e
  CmdEraDelegationError e -> renderError prettyError e
  CmdGenesisError e -> renderError prettyError e
  CmdGovernanceActionError e -> renderError prettyError e
  CmdGovernanceCmdError e -> renderError prettyError e
  CmdGovernanceCommitteeError e -> renderError prettyError e
  CmdGovernanceQueryError e -> renderError prettyError e
  CmdGovernanceVoteError e -> renderError prettyError e
  CmdKeyError e -> renderError renderKeyCmdError e
  CmdNodeError e -> renderError renderNodeCmdError e
  CmdQueryError e -> renderError renderQueryCmdError e
  CmdRegistrationError e -> renderError prettyError e
  CmdStakeAddressError e -> renderError prettyError e
  CmdStakePoolError e -> renderError renderStakePoolCmdError e
  CmdTextViewError e -> renderError renderTextViewFileError e
  CmdTransactionError e -> renderError renderTxCmdError e
 where
  renderError :: (a -> Doc ann) -> a -> Doc ann
  renderError = renderAnyCmdError cmdText
