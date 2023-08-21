{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Cardano.CLI.EraBased.Options.Governance.Vote
  ( pGovernanceVoteCmds
  ) where

import           Cardano.Api

import           Cardano.CLI.EraBased.Commands.Governance.Vote
import           Cardano.CLI.EraBased.Options.Common
import           Cardano.CLI.Types.Governance

import           Data.Foldable
import           Options.Applicative (Parser)
import qualified Options.Applicative as Opt

pGovernanceVoteCmds :: ()
  => CardanoEra era
  -> Maybe (Parser (GovernanceVoteCmds era))
pGovernanceVoteCmds era =
  subInfoParser "vote"
    ( Opt.progDesc
        $ mconcat
          [ "Vote commands."
          ]
    )
    [ pGovernanceVoteCreateCmd era
    ]

pGovernanceVoteCreateCmd :: ()
  => CardanoEra era
  -> Maybe (Parser (GovernanceVoteCmds era))
pGovernanceVoteCreateCmd era = do
  w <- maybeFeatureInEra era
  pure
    $ subParser "create"
    $ Opt.info
        ( GovernanceVoteCreateCmd
            <$> pAnyVote w
            <*> pOutputFile
        )
    $ Opt.progDesc "Vote creation."

pAnyVote :: ConwayEraOnwards era -> Parser AnyVote
pAnyVote cOnwards =
  ConwayOnwardsVote cOnwards
    <$> pVoteChoice
    <*> pGoveranceActionIdentifier "TxIn of governance action (already on chain)."
    <*> pAnyVotingStakeVerificationKeyOrHashOrFile

pAnyVotingStakeVerificationKeyOrHashOrFile :: Parser AnyVotingStakeVerificationKeyOrHashOrFile
pAnyVotingStakeVerificationKeyOrHashOrFile =
  asum [ AnyDRepVerificationKeyOrHashOrFile <$> pDRepVerificationKeyOrHashOrFile
       , AnyStakePoolVerificationKeyOrHashOrFile <$> pStakePoolVerificationKeyOrHashOrFile
       ]

