{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
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
import qualified Cardano.Ledger.BaseTypes as Ledger
import qualified Cardano.Ledger.Crypto as Crypto
import qualified Cardano.Ledger.SafeHash as Ledger

import           Data.Text (Text)
import           Data.Word

data GovernanceVoteCmds era
  = GovernanceVoteCreateCmd
      (GovernanceVoteCreateCmdArgs era)
  | GovernanceVoteViewCmd
      (GovernanceVoteViewCmdArgs era)

data GovernanceVoteCreateCmdArgs era
  = GovernanceVoteCreateCmdArgs
      { eon                         :: ConwayEraOnwards era
      , voteChoice                  :: Vote
      , governanceAction            :: (TxId, Word32)
      , votingStakeCredentialSource :: AnyVotingStakeVerificationKeyOrHashOrFile
      , mAnchor                     :: Maybe (VoteUrl, Ledger.SafeHash Crypto.StandardCrypto Ledger.AnchorData)
      , outFile                     :: VoteFile Out
      }

data GovernanceVoteViewCmdArgs era
  = GovernanceVoteViewCmdArgs
      { eon         :: ConwayEraOnwards era
      , outFormat  :: !ViewOutputFormat
      , voteFile    :: VoteFile In
      , mOutFile    :: Maybe (File () Out)
      }

renderGovernanceVoteCmds :: ()
  => GovernanceVoteCmds era
  -> Text
renderGovernanceVoteCmds = \case
  GovernanceVoteCreateCmd {} ->
    "governance vote create"
  GovernanceVoteViewCmd {} ->
    "governance vote view"
