module Cardano.CLI.EraBased.Options.Governance.Poll
  ( pGovernancePollCmds
  )
where

import Cardano.Api

import Cardano.CLI.EraBased.Commands.Governance.Poll qualified as Cmd
import Cardano.CLI.EraBased.Options.Common
import Cardano.CLI.Parser
import Cardano.Prelude (catMaybes, isInfixOf)

import Control.Monad (when)
import Data.Foldable
import Options.Applicative hiding (help, str)
import Options.Applicative qualified as Opt

pGovernancePollCmds
  :: ()
  => ShelleyBasedEra era
  -> Maybe (Parser (Cmd.GovernancePollCmds era))
pGovernancePollCmds era =
  case parsers of
    [] -> Nothing
    _ -> Just $ asum parsers
 where
  parsers =
    catMaybes
      [ subParser "create-poll"
          <$> ( Opt.info
                  <$> pGovernanceCreatePoll era
                  <*> pure (Opt.progDesc "Create an SPO poll")
              )
      , subParser "answer-poll"
          <$> ( Opt.info
                  <$> pGovernanceAnswerPoll era
                  <*> pure (Opt.progDesc "Answer an SPO poll")
              )
      , subParser "verify-poll"
          <$> ( Opt.info
                  <$> pGovernanceVerifyPoll era
                  <*> pure (Opt.progDesc "Verify an answer to a given SPO poll")
              )
      ]

pGovernanceCreatePoll :: ShelleyBasedEra era -> Maybe (Parser (Cmd.GovernancePollCmds era))
pGovernanceCreatePoll era = do
  w <- forShelleyBasedEraMaybeEon era
  when ("BabbageEraOnwardsConway" `isInfixOf` show w) Nothing
  pure $
    fmap Cmd.GovernanceCreatePoll $
      Cmd.GovernanceCreatePollCmdArgs w
        <$> pPollQuestion
        <*> some pPollAnswer
        <*> optional pPollNonce
        <*> pOutputFile

pGovernanceAnswerPoll :: ShelleyBasedEra era -> Maybe (Parser (Cmd.GovernancePollCmds era))
pGovernanceAnswerPoll era = do
  w <- forShelleyBasedEraMaybeEon era
  when ("BabbageEraOnwardsConway" `isInfixOf` show w) Nothing
  pure $
    fmap Cmd.GovernanceAnswerPoll $
      Cmd.GovernanceAnswerPollCmdArgs w
        <$> pPollFile
        <*> optional pPollAnswerIndex
        <*> optional pOutputFile

pGovernanceVerifyPoll :: ShelleyBasedEra era -> Maybe (Parser (Cmd.GovernancePollCmds era))
pGovernanceVerifyPoll era = do
  w <- forShelleyBasedEraMaybeEon era
  when ("BabbageEraOnwardsConway" `isInfixOf` show w) Nothing
  pure $
    fmap Cmd.GovernanceVerifyPoll $
      Cmd.GovernanceVerifyPollCmdArgs w
        <$> pPollFile
        <*> pPollTxFile
        <*> optional pOutputFile
