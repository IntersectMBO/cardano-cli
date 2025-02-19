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
  deriving Show

instance Error CmdError where
  prettyError = \case
    CmdAddressError e -> renderAddressCmdError e
    CmdEraDelegationError e -> prettyError e
    CmdGenesisError e -> prettyError e
    CmdGovernanceActionError e -> prettyError e
    CmdGovernanceCmdError e -> prettyError e
    CmdGovernanceCommitteeError e -> prettyError e
    CmdGovernanceQueryError e -> prettyError e
    CmdGovernanceVoteError e -> prettyError e
    CmdKeyError e -> renderKeyCmdError e
    CmdNodeError e -> renderNodeCmdError e
    CmdQueryError e -> renderQueryCmdError e
    CmdRegistrationError e -> prettyError e
    CmdStakeAddressError e -> prettyError e
    CmdStakePoolError e -> prettyError e
    CmdTextViewError e -> renderTextViewFileError e
    CmdTransactionError e -> renderTxCmdError e

renderCmdError :: Text -> CmdError -> Doc ann
renderCmdError cmdText = renderAnyCmdError cmdText prettyError
