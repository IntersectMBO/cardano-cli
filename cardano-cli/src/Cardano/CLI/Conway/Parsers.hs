module Cardano.CLI.Conway.Parsers where

import           Cardano.Api
import           Cardano.Api.Shelley

import           Cardano.CLI.Common.Parsers
import           Cardano.CLI.Conway.Types
import           Cardano.CLI.Shelley.Commands
import           Cardano.CLI.Shelley.Key (StakeIdentifier (..))

import           Data.Foldable
import           Options.Applicative hiding (help, str)
import qualified Options.Applicative as Opt



pCreateVote :: Parser VoteCmd
pCreateVote =
  fmap CreateVoteCmd $
    ConwayVote
      <$> pVoteChoice
      <*> pVoterType
      <*> pGoveranceActionIdentifier
      <*> pVotingCredential

 where
  pVoteChoice :: Parser VoteChoice
  pVoteChoice =
    asum
     [  flag' Yes $ long "yes"
     ,  flag' No $ long "no"
     ,  flag' Abst $ long "abstain"
     ]

  pVoterType :: Parser VoterType
  pVoterType =
    asum
     [  flag' CC $ mconcat [long "constitutional-committee-member", Opt.help "Member of the constiutional committee"]
     ,  flag' DR $ mconcat [long "drep", Opt.help "Delegate representative"]
     ,  flag' SP $ mconcat [long "spo", Opt.help "Stake pool operator"]
     ]

  pGoveranceActionIdentifier :: Parser TxIn
  pGoveranceActionIdentifier =
    Opt.option (readerFromParsecParser parseTxIn)
        (  Opt.long "tx-in"
        <> Opt.metavar "TX-IN"
        <> Opt.help "TxIn of governance action (already on chain)."
        )


  pVotingCredential :: Parser StakeIdentifier
  pVotingCredential = pStakeIdentifier


pVoteCommmands :: Parser VoteCmd
pVoteCommmands =
  asum
    [ subParser "create-vote"
        $ Opt.info pCreateVote
        $ Opt.progDesc "Create a vote for a proposed governance action."
    ]

pActionCommmands :: Parser VoteCmd
pActionCommmands =
  asum
    [ subParser "create-action"
        $ Opt.info pCreateAction
        $ Opt.progDesc "Create a vote for a proposed governance action."
    ]
pCreateAction :: Parser VoteCmd
pCreateAction = error "Change to GovActionCmd"

