{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Cardano.CLI.EraBased.Options.Governance.Vote
  ( pGovernanceVoteCmds
  ) where

import           Cardano.Api

import           Cardano.CLI.EraBased.Commands.Governance.Vote
import           Cardano.CLI.EraBased.Options.Common
import           Cardano.CLI.Types.Governance

import           Control.Applicative (optional)
import           Data.Foldable
import           Options.Applicative (Parser)
import qualified Options.Applicative as Opt

pGovernanceVoteCmds :: ()
  => CardanoEra era
  -> Maybe (Parser (GovernanceVoteCmds era))
pGovernanceVoteCmds era =
  subInfoParser "vote"
    (Opt.progDesc "Vote commands.")
    [ pGovernanceVoteCreateCmd era,
      pGovernanceVoteViewCmd era
    ]

pGovernanceVoteCreateCmd :: ()
  => CardanoEra era
  -> Maybe (Parser (GovernanceVoteCmds era))
pGovernanceVoteCreateCmd era = do
  w <- forEraMaybeEon era
  pure
    $ subParser "create"
    $ Opt.info
        ( GovernanceVoteCreateCmd
            <$> pAnyVote w
        )
    $ Opt.progDesc "Vote creation."

pAnyVote :: ConwayEraOnwards era -> Parser AnyVote
pAnyVote cOnwards =
  ConwayOnwardsVote cOnwards
      <$> pVoteChoice
      <*> pGovernanceActionId
      <*> pAnyVotingStakeVerificationKeyOrHashOrFile
      <*> pFileOutDirection "out-file" "Output filepath of the vote."
      <*> optional pVoteAnchor

pAnyVotingStakeVerificationKeyOrHashOrFile :: Parser AnyVotingStakeVerificationKeyOrHashOrFile
pAnyVotingStakeVerificationKeyOrHashOrFile =
  asum [ AnyDRepVerificationKeyOrHashOrFile <$> pDRepVerificationKeyOrHashOrFile
       , AnyStakePoolVerificationKeyOrHashOrFile <$> pStakePoolVerificationKeyOrHashOrFile Nothing
       , AnyCommitteeHotVerificationKeyOrHashOrFile <$> pCommitteeHotVerificationKeyOrHashOrVerificationFile
       ]

pGovernanceVoteViewCmd :: ()
  => CardanoEra era
  -> Maybe (Parser (GovernanceVoteCmds era))
pGovernanceVoteViewCmd era = do
  w <- forEraMaybeEon era
  pure
    $ subParser "view"
    $ Opt.info
        (GovernanceVoteViewCmd <$> pAnyVoteViewCmd w)
    $ Opt.progDesc "Vote viewing."

pAnyVoteViewCmd :: ConwayEraOnwards era -> Parser (AnyVoteViewCmd era)
pAnyVoteViewCmd cOnwards =
  AnyVoteViewCmd
    <$> pYamlOutput
    <*> pure cOnwards
    <*> pFileInDirection "vote-file" "Input filepath of the vote."
    <*> pMaybeOutputFile
  where
    pYamlOutput :: Parser Bool
    pYamlOutput =
      Opt.switch
        ( Opt.long "yaml"
            <> Opt.help "Output vote in YAML format (and not JSON)."
        )
