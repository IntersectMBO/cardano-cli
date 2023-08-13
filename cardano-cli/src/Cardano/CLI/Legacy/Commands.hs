module Cardano.CLI.Legacy.Commands
  ( LegacyCmds (..)
  , renderLegacyCommand
  ) where

import           Cardano.CLI.Legacy.Commands.Address
import           Cardano.CLI.Legacy.Commands.Genesis
import           Cardano.CLI.Legacy.Commands.Governance
import           Cardano.CLI.Legacy.Commands.Key
import           Cardano.CLI.Legacy.Commands.Node
import           Cardano.CLI.Legacy.Commands.Pool
import           Cardano.CLI.Legacy.Commands.Query
import           Cardano.CLI.Legacy.Commands.StakeAddress
import           Cardano.CLI.Legacy.Commands.TextView
import           Cardano.CLI.Legacy.Commands.Transaction

import           Data.Text (Text)

data LegacyCmds
  = LegacyAddressCmds       LegacyAddressCmds
  | LegacyStakeAddressCmds  LegacyStakeAddressCmds
  | LegacyKeyCmds           LegacyKeyCmds
  | LegacyTransactionCmds   LegacyTransactionCmds
  | LegacyNodeCmds          LegacyNodeCmds
  | LegacyPoolCmds          LegacyPoolCmds
  | LegacyQueryCmds         LegacyQueryCmds
  | LegacyGovernanceCmds    LegacyGovernanceCmds
  | LegacyGenesisCmds       LegacyGenesisCmds
  | LegacyTextViewCmds      LegacyTextViewCmds

renderLegacyCommand :: LegacyCmds -> Text
renderLegacyCommand sc =
  case sc of
    LegacyAddressCmds cmd -> renderLegacyAddressCmds cmd
    LegacyStakeAddressCmds cmd -> renderLegacyStakeAddressCmds cmd
    LegacyKeyCmds cmd -> renderLegacyKeyCmds cmd
    LegacyTransactionCmds cmd -> renderLegacyTransactionCmds cmd
    LegacyNodeCmds cmd -> renderLegacyNodeCmds cmd
    LegacyPoolCmds cmd -> renderLegacyPoolCmds cmd
    LegacyQueryCmds cmd -> renderLegacyQueryCmds cmd
    LegacyGovernanceCmds cmd -> renderLegacyGovernanceCmds cmd
    LegacyGenesisCmds cmd -> renderLegacyGenesisCmds cmd
    LegacyTextViewCmds cmd -> renderLegacyTextViewCmds cmd
