{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.Governance.Vote.Command
  ( GovernanceVoteCmds (..)
  , GovernanceVoteViewCmdArgs (..)
  , GovernanceVoteCreateCmdArgs (..)
  , renderGovernanceVoteCmds
  )
where

import Cardano.Api.Ledger qualified as L
import Cardano.Api.Shelley

import Cardano.CLI.Type.Common
import Cardano.CLI.Type.Governance
import Cardano.CLI.Vary (Vary)

import Data.Text (Text)

data GovernanceVoteCmds era
  = GovernanceVoteCreateCmd
      (GovernanceVoteCreateCmdArgs era)
  | GovernanceVoteViewCmd
      (GovernanceVoteViewCmdArgs era)

data GovernanceVoteCreateCmdArgs era
  = GovernanceVoteCreateCmdArgs
  { eon :: ConwayEraOnwards era
  , voteChoice :: Vote
  , governanceActionId :: L.GovActionId
  , votingStakeCredentialSource :: AnyVotingStakeVerificationKeyOrHashOrFile
  , mAnchor
      :: !( Maybe
              ( PotentiallyCheckedAnchor
                  VoteUrl
                  (VoteUrl, L.SafeHash L.AnchorData)
              )
          )
  , outFile :: VoteFile Out
  }

data GovernanceVoteViewCmdArgs era
  = GovernanceVoteViewCmdArgs
  { eon :: ConwayEraOnwards era
  , outFormat :: !(Vary [FormatJson, FormatYaml])
  , voteFile :: VoteFile In
  , mOutFile :: Maybe (File () Out)
  }

renderGovernanceVoteCmds
  :: ()
  => GovernanceVoteCmds era
  -> Text
renderGovernanceVoteCmds = \case
  GovernanceVoteCreateCmd{} ->
    "governance vote create"
  GovernanceVoteViewCmd{} ->
    "governance vote view"
