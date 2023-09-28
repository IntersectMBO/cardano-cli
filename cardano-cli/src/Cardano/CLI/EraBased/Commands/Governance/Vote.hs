{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.Commands.Governance.Vote
  ( GovernanceVoteCmds (..)
  , renderGovernanceVoteCmds
  ) where

import Cardano.CLI.Types.Governance

import Data.Text (Text)

newtype GovernanceVoteCmds era
  = GovernanceVoteCreateCmd
      AnyVote

renderGovernanceVoteCmds
  :: ()
  => GovernanceVoteCmds era
  -> Text
renderGovernanceVoteCmds = \case
  GovernanceVoteCreateCmd {} ->
    "governance vote create"
