{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Types.Errors.LegacyClientCmdError
  ( LegacyClientCmdError(..)
  , renderLegacyClientCmdError
  ) where

import           Cardano.Api

import           Cardano.CLI.Types.Errors.GovernanceCmdError
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

renderLegacyClientCmdError :: Text -> LegacyClientCmdError -> Text
renderLegacyClientCmdError cmdText = \case
  LegacyCmdAddressError addrCmdErr ->
      renderError renderShelleyAddressCmdError addrCmdErr
  LegacyCmdGenesisError genesisCmdErr ->
      renderError (Text.pack . displayError) genesisCmdErr
  LegacyCmdGovernanceError govCmdErr ->
      renderError (Text.pack . displayError) govCmdErr
  LegacyCmdNodeError nodeCmdErr ->
      renderError renderShelleyNodeCmdError nodeCmdErr
  LegacyCmdPoolError poolCmdErr ->
      renderError renderShelleyPoolCmdError poolCmdErr
  LegacyCmdStakeAddressError stakeAddrCmdErr ->
      renderError (Text.pack . displayError) stakeAddrCmdErr
  LegacyCmdTextViewError txtViewErr ->
      renderError renderShelleyTextViewFileError txtViewErr
  LegacyCmdTransactionError txErr ->
      renderError renderShelleyTxCmdError txErr
  LegacyCmdQueryError queryErr ->
      renderError renderShelleyQueryCmdError queryErr
  LegacyCmdKeyError keyErr ->
      renderError renderShelleyKeyCmdError keyErr
  where
    renderError :: (a -> Text) -> a -> Text
    renderError renderer shelCliCmdErr =
      mconcat
        [ "Command failed: "
        , cmdText
        , "  Error: "
        , renderer shelCliCmdErr
        ]
