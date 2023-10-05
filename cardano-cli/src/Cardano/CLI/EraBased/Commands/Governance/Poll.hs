{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.Commands.Governance.Poll
  ( GovernancePollCmds(..) , renderGovernancePollCmds) where

import           Cardano.Api
import           Cardano.Api.Shelley

import           Data.Text (Text)

data GovernancePollCmds era
  = GovernanceCreatePoll -- ^ Create a SPO poll
      (BabbageEraOnwards era) {- TODO smelc, use BabbageEraOnly here instead -}
      Text -- ^ Prompt
      [Text] -- ^ Choices
      (Maybe Word) -- ^ Nonce
      (File GovernancePoll Out)
  | GovernanceAnswerPoll  -- ^ Answer a SPO poll
      (BabbageEraOnwards era) {- TODO smelc, use BabbageEraOnly here instead -}
      (File GovernancePoll In) -- ^ Poll file
      (Maybe Word) -- ^ Answer index
      (Maybe (File () Out)) -- ^ Tx file
  | GovernanceVerifyPoll  -- ^ Verify answer to a given SPO poll
      (BabbageEraOnwards era) {- TODO smelc, use BabbageEraOnly here instead -}
      (File GovernancePoll In) -- Poll file
      (File (Tx ()) In) -- Tx file
      (Maybe (File () Out)) -- Tx file


renderGovernancePollCmds :: ()
  => GovernancePollCmds era
  -> Text
renderGovernancePollCmds = \case
  GovernanceCreatePoll {} -> "governance create-poll"
  GovernanceAnswerPoll {} -> "governance answer-poll"
  GovernanceVerifyPoll {} -> "governance verify-poll"
