module Cardano.CLI.EraBased.Governance.Poll.Option
  ( pGovernancePollCmds
  )
where

import Cardano.Api.Experimental qualified as Exp

import Cardano.CLI.EraBased.Common.Option
  ( pOutputFile
  , pPollAnswer
  , pPollAnswerIndex
  , pPollFile
  , pPollNonce
  , pPollQuestion
  , pPollTxFile
  )
import Cardano.CLI.EraBased.Governance.Poll.Command qualified as Cmd
import Cardano.CLI.Parser (commandWithMetavar)
import Cardano.Prelude (catMaybes)

import Options.Applicative
  ( Alternative (some)
  , Parser
  , asum
  , optional
  )
import Options.Applicative qualified as Opt

pGovernancePollCmds
  :: Exp.IsEra era
  => Maybe (Parser (Cmd.GovernancePollCmds era))
pGovernancePollCmds =
  case parsers of
    [] -> Nothing
    _ -> Just $ asum parsers
 where
  parsers =
    catMaybes
      [ pure
          . Opt.hsubparser
          . commandWithMetavar "create-poll"
          $ Opt.info
            pGovernanceCreatePoll
          $ Opt.progDesc "Create an SPO poll"
      , pure
          . Opt.hsubparser
          . commandWithMetavar "answer-poll"
          $ Opt.info
            pGovernanceAnswerPoll
          $ Opt.progDesc "Answer an SPO poll"
      , pure
          . Opt.hsubparser
          . commandWithMetavar "verify-poll"
          $ Opt.info
            pGovernanceVerifyPoll
          $ Opt.progDesc "Verify an answer to a given SPO poll"
      ]

pGovernanceCreatePoll
  :: Exp.IsEra era
  => Parser (Cmd.GovernancePollCmds era)
pGovernanceCreatePoll =
  fmap Cmd.GovernanceCreatePoll $
    Cmd.GovernanceCreatePollCmdArgs Exp.useEra
      <$> pPollQuestion
      <*> some pPollAnswer
      <*> optional pPollNonce
      <*> pOutputFile

pGovernanceAnswerPoll
  :: Exp.IsEra era
  => Parser (Cmd.GovernancePollCmds era)
pGovernanceAnswerPoll =
  fmap Cmd.GovernanceAnswerPoll $
    Cmd.GovernanceAnswerPollCmdArgs Exp.useEra
      <$> pPollFile
      <*> optional pPollAnswerIndex
      <*> optional pOutputFile

pGovernanceVerifyPoll
  :: Exp.IsEra era
  => Parser (Cmd.GovernancePollCmds era)
pGovernanceVerifyPoll =
  fmap Cmd.GovernanceVerifyPoll $
    Cmd.GovernanceVerifyPollCmdArgs Exp.useEra
      <$> pPollFile
      <*> pPollTxFile
      <*> optional pOutputFile
