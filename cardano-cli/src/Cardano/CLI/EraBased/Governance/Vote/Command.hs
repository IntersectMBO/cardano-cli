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

import Cardano.Api.Experimental qualified as Exp
import Cardano.Api.Governance
import Cardano.Api.Ledger qualified as L

import Cardano.CLI.Type.Common
import Cardano.CLI.Type.Governance

import Data.Text (Text)
import Vary (Vary)

data GovernanceVoteCmds era
  = GovernanceVoteCreateCmd
      (GovernanceVoteCreateCmdArgs era)
  | GovernanceVoteViewCmd
      (GovernanceVoteViewCmdArgs era)

data GovernanceVoteCreateCmdArgs era
  = GovernanceVoteCreateCmdArgs
  { era :: Exp.Era era
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
  { era :: Exp.Era era
  , voteFile :: VoteFile In
  , outputFormat :: !(Vary [FormatJson, FormatYaml])
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
