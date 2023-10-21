{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.Commands.Governance.Vote
  ( GovernanceVoteCmds(..)
  , GovernanceVoteViewCmdArgs(..)
  , GovernanceVoteCreateCmdArgs(..)
  , renderGovernanceVoteCmds
  ) where

import           Cardano.Api.Shelley

import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Governance

import           Data.Text (Text)
import           Data.Word

data GovernanceVoteCmds era
  = GovernanceVoteCreateCmd
      (GovernanceVoteCreateCmdArgs era)
  | GovernanceVoteViewCmd
      (GovernanceVoteViewCmdArgs era)

data GovernanceVoteCreateCmdArgs era where
  GovernanceVoteCreateCmdArgs
    :: ConwayEraOnwards era
    -> Vote
    -> (TxId, Word32)
    -> AnyVotingStakeVerificationKeyOrHashOrFile
    -> VoteFile Out
    -> Maybe (VoteUrl, VoteHashSource)
    -> GovernanceVoteCreateCmdArgs era

data GovernanceVoteViewCmdArgs era
  = GovernanceVoteViewCmdArgs
    { governanceVoteViewCmdEra :: ConwayEraOnwards era
    , governanceVoteViewCmdYamlOutput :: Bool
    , governanceVoteViewCmdVoteFile :: VoteFile In
    , governanceVoteViewCmdOutputFile :: Maybe (File () Out)
    }

renderGovernanceVoteCmds :: ()
  => GovernanceVoteCmds era
  -> Text
renderGovernanceVoteCmds = \case
  GovernanceVoteCreateCmd {} ->
    "governance vote create"
  GovernanceVoteViewCmd {} ->
    "governance vote view"
