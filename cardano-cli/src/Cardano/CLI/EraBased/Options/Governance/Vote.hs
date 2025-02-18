{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Cardano.CLI.EraBased.Options.Governance.Vote
  ( pGovernanceVoteCmds
  )
where

import Cardano.Api

import Cardano.CLI.EraBased.Commands.Governance.Vote
  ( GovernanceVoteCmds (..)
  , GovernanceVoteCreateCmdArgs (GovernanceVoteCreateCmdArgs)
  , GovernanceVoteViewCmdArgs (GovernanceVoteViewCmdArgs)
  )
import Cardano.CLI.EraBased.Options.Common
import Cardano.CLI.Parser
import Cardano.CLI.Types.Governance

import Control.Applicative (optional)
import Data.Foldable
import Options.Applicative (Parser)
import Options.Applicative qualified as Opt

pGovernanceVoteCmds
  :: ()
  => ShelleyBasedEra era
  -> Maybe (Parser (GovernanceVoteCmds era))
pGovernanceVoteCmds era =
  subInfoParser
    "vote"
    (Opt.progDesc "Vote commands.")
    [ pGovernanceVoteCreateCmd era
    , pGovernanceVoteViewCmd era
    ]

pGovernanceVoteCreateCmd
  :: ()
  => ShelleyBasedEra era
  -> Maybe (Parser (GovernanceVoteCmds era))
pGovernanceVoteCreateCmd era = do
  w <- forShelleyBasedEraMaybeEon era
  pure
    $ subParser "create"
    $ Opt.info
      ( GovernanceVoteCreateCmd
          <$> pGovernanceVoteCreateCmdArgs w
      )
    $ Opt.progDesc "Vote creation."

pGovernanceVoteCreateCmdArgs
  :: ()
  => ConwayEraOnwards era -> Parser (GovernanceVoteCreateCmdArgs era)
pGovernanceVoteCreateCmdArgs cOnwards =
  GovernanceVoteCreateCmdArgs cOnwards
    <$> pVoteChoice
    <*> pGovernanceActionId
    <*> pAnyVotingStakeVerificationKeyOrHashOrFile
    <*> optional
      ( pPotentiallyCheckedAnchorData
          pMustCheckVoteUrl
          pVoteAnchor
      )
    <*> pFileOutDirection "out-file" "Output filepath of the vote."

pAnyVotingStakeVerificationKeyOrHashOrFile :: Parser AnyVotingStakeVerificationKeyOrHashOrFile
pAnyVotingStakeVerificationKeyOrHashOrFile =
  asum
    [ AnyDRepVerificationKeyOrHashOrFileOrScriptHash <$> pDRepVerificationKeyOrHashOrFileOrScriptHash
    , AnyStakePoolVerificationKeyOrHashOrFile <$> pStakePoolVerificationKeyOrHashOrFile Nothing
    , AnyCommitteeHotVerificationKeyOrHashOrFileOrScriptHash
        <$> pCommitteeHotVerificationKeyOrHashOrVerificationFileOrScriptHash
    ]

pGovernanceVoteViewCmd
  :: ()
  => ShelleyBasedEra era
  -> Maybe (Parser (GovernanceVoteCmds era))
pGovernanceVoteViewCmd era = do
  w <- forShelleyBasedEraMaybeEon era
  pure
    $ subParser "view"
    $ Opt.info
      (GovernanceVoteViewCmd <$> pGovernanceVoteViewCmdArgs w)
    $ Opt.progDesc "Vote viewing."

pGovernanceVoteViewCmdArgs :: ConwayEraOnwards era -> Parser (GovernanceVoteViewCmdArgs era)
pGovernanceVoteViewCmdArgs cOnwards =
  GovernanceVoteViewCmdArgs cOnwards
    <$> pGovernanceVoteViewOutputFormat
    <*> pFileInDirection "vote-file" "Input filepath of the vote."
    <*> pMaybeOutputFile
