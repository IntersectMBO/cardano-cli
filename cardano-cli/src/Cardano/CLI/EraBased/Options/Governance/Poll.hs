module Cardano.CLI.EraBased.Options.Governance.Poll
  ( pGovernancePollCmds
  )
where

import           Cardano.Api

import qualified Cardano.CLI.EraBased.Commands.Governance.Poll as Cmd
import           Cardano.CLI.EraBased.Options.Common
import           Cardano.Prelude (catMaybes, isInfixOf)

import           Control.Monad (when)
import           Data.Foldable
import           Options.Applicative hiding (help, str)
import qualified Options.Applicative as Opt

pGovernancePollCmds
  :: ()
  => CardanoEra era
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

pGovernanceCreatePoll :: CardanoEra era -> Maybe (Parser (Cmd.GovernancePollCmds era))
pGovernanceCreatePoll era = do
  w <- forEraMaybeEon era
  when ("BabbageEraOnwardsConway" `isInfixOf` show w) Nothing
  pure $
    fmap Cmd.GovernanceCreatePoll $
      Cmd.GovernanceCreatePollCmdArgs w
        <$> pPollQuestion
        <*> some pPollAnswer
        <*> optional pPollNonce
        <*> pOutputFile

pGovernanceAnswerPoll :: CardanoEra era -> Maybe (Parser (Cmd.GovernancePollCmds era))
pGovernanceAnswerPoll era = do
  w <- forEraMaybeEon era
  when ("BabbageEraOnwardsConway" `isInfixOf` show w) Nothing
  pure $
    fmap Cmd.GovernanceAnswerPoll $
      Cmd.GovernanceAnswerPollCmdArgs w
        <$> pPollFile
        <*> optional pPollAnswerIndex
        <*> optional pOutputFile

pGovernanceVerifyPoll :: CardanoEra era -> Maybe (Parser (Cmd.GovernancePollCmds era))
pGovernanceVerifyPoll era = do
  w <- forEraMaybeEon era
  when ("BabbageEraOnwardsConway" `isInfixOf` show w) Nothing
  pure $
    fmap Cmd.GovernanceVerifyPoll $
      Cmd.GovernanceVerifyPollCmdArgs w
        <$> pPollFile
        <*> pPollTxFile
        <*> optional pOutputFile
