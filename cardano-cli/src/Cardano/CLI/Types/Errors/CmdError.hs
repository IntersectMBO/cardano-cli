{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Types.Errors.CmdError
  ( CmdError(..)
  , renderCmdError
  ) where

import           Cardano.Api

import           Cardano.CLI.Types.Errors.EraBasedDelegationError
import           Cardano.CLI.Types.Errors.EraBasedRegistrationError
import           Cardano.CLI.Types.Errors.GovernanceActionsError
import           Cardano.CLI.Types.Errors.GovernanceCmdError
import           Cardano.CLI.Types.Errors.GovernanceCommitteeError
import           Cardano.CLI.Types.Errors.GovernanceQueryError
import           Cardano.CLI.Types.Errors.GovernanceVoteCmdError
import           Cardano.CLI.Types.Errors.ShelleyAddressCmdError
import           Cardano.CLI.Types.Errors.ShelleyGenesisCmdError
import           Cardano.CLI.Types.Errors.ShelleyKeyCmdError
import           Cardano.CLI.Types.Errors.ShelleyNodeCmdError
import           Cardano.CLI.Types.Errors.ShelleyPoolCmdError
import           Cardano.CLI.Types.Errors.ShelleyQueryCmdError
import           Cardano.CLI.Types.Errors.ShelleyStakeAddressCmdError
import           Cardano.CLI.Types.Errors.ShelleyTextViewFileError
import           Cardano.CLI.Types.Errors.ShelleyTxCmdError

import           Data.Text (Text)
import qualified Data.Text as Text

data CmdError
  = CmdAddressError               !ShelleyAddressCmdError
  | CmdEraBasedRegistrationError  !EraBasedRegistrationError
  | CmdEraDelegationError         !EraBasedDelegationError
  | CmdGenesisError               !ShelleyGenesisCmdError
  | CmdGovernanceActionError      !GovernanceActionsError
  | CmdGovernanceCmdError         !GovernanceCmdError
  | CmdGovernanceCommitteeError   !GovernanceCommitteeError
  | CmdGovernanceVoteError        !GovernanceVoteCmdError
  | CmdGovernanceQueryError       !GovernanceQueryError
  | CmdKeyError                   !ShelleyKeyCmdError
  | CmdNodeError                  !ShelleyNodeCmdError
  | CmdPoolError                  !ShelleyPoolCmdError
  | CmdQueryError                 !ShelleyQueryCmdError
  | CmdStakeAddressError          !ShelleyStakeAddressCmdError
  | CmdTextViewError              !ShelleyTextViewFileError
  | CmdTransactionError           !ShelleyTxCmdError

renderCmdError :: Text -> CmdError -> Text
renderCmdError cmdText = \case
  CmdAddressError               e -> renderError renderShelleyAddressCmdError e
  CmdEraBasedRegistrationError  e -> renderError (Text.pack . displayError) e
  CmdEraDelegationError         e -> renderError (Text.pack . displayError) e
  CmdGenesisError               e -> renderError (Text.pack . displayError) e
  CmdGovernanceActionError      e -> renderError (Text.pack . displayError) e
  CmdGovernanceCmdError         e -> renderError (Text.pack . displayError) e
  CmdGovernanceCommitteeError   e -> renderError (Text.pack . displayError) e
  CmdGovernanceVoteError        e -> renderError (Text.pack . displayError) e
  CmdGovernanceQueryError       e -> renderError (Text.pack . displayError) e
  CmdKeyError                   e -> renderError renderShelleyKeyCmdError e
  CmdNodeError                  e -> renderError renderShelleyNodeCmdError e
  CmdPoolError                  e -> renderError renderShelleyPoolCmdError e
  CmdQueryError                 e -> renderError renderShelleyQueryCmdError e
  CmdStakeAddressError          e -> renderError (Text.pack . displayError) e
  CmdTextViewError              e -> renderError renderShelleyTextViewFileError e
  CmdTransactionError           e -> renderError renderShelleyTxCmdError e
  where
    renderError :: (a -> Text) -> a -> Text
    renderError renderer shelCliCmdErr =
      mconcat
        [ "Command failed: "
        , cmdText
        , "  Error: "
        , renderer shelCliCmdErr
        ]
