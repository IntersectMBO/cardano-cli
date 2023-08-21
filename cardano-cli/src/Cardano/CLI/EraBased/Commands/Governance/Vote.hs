{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.Commands.Governance.Vote
  ( GovernanceVoteCmds(..)
  , renderGovernanceVoteCmds
  ) where

import           Cardano.Api

import           Cardano.CLI.Types.Governance

import           Data.Text (Text)

data GovernanceVoteCmds era
  = GovernanceVoteCreateCmd
      AnyVote
      (File () Out)

renderGovernanceVoteCmds :: ()
  => GovernanceVoteCmds era
  -> Text
renderGovernanceVoteCmds = \case
  GovernanceVoteCreateCmd {} ->
    "governance vote create"
