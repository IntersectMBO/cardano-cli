
module Cardano.CLI.Types.Errors.LegacyClientCmdError
  ( LegacyClientCmdError(..)
  ) where

import           Cardano.CLI.Types.Errors.AddressCmdError
import           Cardano.CLI.Types.Errors.GenesisCmdError
import           Cardano.CLI.Types.Errors.GovernanceCmdError
import           Cardano.CLI.Types.Errors.KeyCmdError
import           Cardano.CLI.Types.Errors.NodeCmdError
import           Cardano.CLI.Types.Errors.PoolCmdError
import           Cardano.CLI.Types.Errors.QueryCmdError
import           Cardano.CLI.Types.Errors.StakeAddressCmdError
import           Cardano.CLI.Types.Errors.TextViewFileError
import           Cardano.CLI.Types.Errors.TxCmdError

data LegacyClientCmdError
  = LegacyCmdAddressError
      !AddressCmdError
  | LegacyCmdGenesisError
      !GenesisCmdError
  | LegacyCmdGovernanceError
      !GovernanceCmdError
  | LegacyCmdNodeError
      !NodeCmdError
  | LegacyCmdPoolError
      !PoolCmdError
  | LegacyCmdStakeAddressError
      !StakeAddressCmdError
  | LegacyCmdTextViewError
      !TextViewFileError
  | LegacyCmdTransactionError
      !TxCmdError
  | LegacyCmdQueryError
      !QueryCmdError
  | LegacyCmdKeyError
      !KeyCmdError
