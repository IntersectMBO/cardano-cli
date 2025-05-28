{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Cardano.CLI.EraBased.Governance.Vote.Option
  ( pGovernanceVoteCmds
  )
where

import Cardano.Api.Experimental qualified as Exp

import Cardano.CLI.EraBased.Common.Option
import Cardano.CLI.EraBased.Governance.Vote.Command
  ( GovernanceVoteCmds (..)
  , GovernanceVoteCreateCmdArgs (GovernanceVoteCreateCmdArgs)
  , GovernanceVoteViewCmdArgs (GovernanceVoteViewCmdArgs)
  )
import Cardano.CLI.Option.Flag (setDefault)
import Cardano.CLI.Parser
import Cardano.CLI.Type.Governance

import Control.Applicative (optional)
import Data.Foldable
import Data.Function ((&))
import Options.Applicative (Parser)
import Options.Applicative qualified as Opt

pGovernanceVoteCmds
  :: Exp.IsEra era => Maybe (Parser (GovernanceVoteCmds era))
pGovernanceVoteCmds =
  subInfoParser
    "vote"
    (Opt.progDesc "Vote commands.")
    [ pGovernanceVoteCreateCmd
    , pGovernanceVoteViewCmd
    ]

pGovernanceVoteCreateCmd
  :: Exp.IsEra era => Maybe (Parser (GovernanceVoteCmds era))
pGovernanceVoteCreateCmd = do
  pure
    $ Opt.hsubparser
    $ commandWithMetavar "create"
    $ Opt.info
      ( GovernanceVoteCreateCmd
          <$> pGovernanceVoteCreateCmdArgs
      )
    $ Opt.progDesc "Vote creation."

pGovernanceVoteCreateCmdArgs
  :: Exp.IsEra era => Parser (GovernanceVoteCreateCmdArgs era)
pGovernanceVoteCreateCmdArgs =
  GovernanceVoteCreateCmdArgs Exp.useEra
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
  :: Exp.IsEra era => Maybe (Parser (GovernanceVoteCmds era))
pGovernanceVoteViewCmd = do
  pure
    $ Opt.hsubparser
    $ commandWithMetavar "view"
    $ Opt.info
      (GovernanceVoteViewCmd <$> pGovernanceVoteViewCmdArgs)
    $ Opt.progDesc "Vote viewing."

pGovernanceVoteViewCmdArgs :: Exp.IsEra era => Parser (GovernanceVoteViewCmdArgs era)
pGovernanceVoteViewCmdArgs =
  GovernanceVoteViewCmdArgs Exp.useEra
    <$> pFileInDirection "vote-file" "Input filepath of the vote."
    <*> pFormatFlags
      "governance vote view output"
      [ flagFormatJson & setDefault
      , flagFormatYaml
      ]
    <*> pMaybeOutputFile
