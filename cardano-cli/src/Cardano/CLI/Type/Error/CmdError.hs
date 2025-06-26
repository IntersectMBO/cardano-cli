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
import Cardano.CLI.Type.Error.GovernanceQueryError
import Cardano.CLI.Type.Error.KeyCmdError
import Cardano.CLI.Type.Error.QueryCmdError
import Cardano.CLI.Type.Error.RegistrationError
import Cardano.CLI.Type.Error.StakePoolCmdError
import Cardano.CLI.Type.Error.TextViewFileError
import Cardano.CLI.Type.Error.TxCmdError
import Cardano.Prelude (SomeException)

import Data.Text (Text)

data CmdError
  = CmdAddressError !AddressCmdError
  | CmdEraDelegationError !DelegationError
  | CmdGenesisError !GenesisCmdError
  | CmdGovernanceActionError !GovernanceActionsError
  | CmdGovernanceCmdError !GovernanceCmdError
  | CmdGovernanceQueryError !GovernanceQueryError
  | CmdKeyError !KeyCmdError
  | CmdQueryError !QueryCmdError
  | CmdRegistrationError !RegistrationError
  | CmdStakePoolError !StakePoolCmdError
  | CmdTextViewError !TextViewFileError
  | CmdTransactionError !TxCmdError
  | CmdBackwardCompatibleError !SomeException
  deriving Show

instance Error CmdError where
  prettyError = \case
    CmdAddressError e -> renderAddressCmdError e
    CmdEraDelegationError e -> prettyError e
    CmdGenesisError e -> prettyError e
    CmdGovernanceActionError e -> prettyError e
    CmdGovernanceCmdError e -> prettyError e
    CmdGovernanceQueryError e -> prettyError e
    CmdKeyError e -> renderKeyCmdError e
    CmdQueryError e -> renderQueryCmdError e
    CmdRegistrationError e -> prettyError e
    CmdStakePoolError e -> prettyError e
    CmdTextViewError e -> renderTextViewFileError e
    CmdTransactionError e -> renderTxCmdError e
    CmdBackwardCompatibleError e ->
      prettyException e

renderCmdError :: Text -> CmdError -> Doc ann
renderCmdError cmdText = renderAnyCmdError cmdText prettyError
