{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.EraBased.Run.Governance.Vote
  ( runGovernanceVoteCmds
  ) where

import           Cardano.Api.Shelley

import           Cardano.CLI.EraBased.Commands.Governance.Vote
import           Cardano.CLI.Types.Errors.CmdError
import           Cardano.CLI.Types.Errors.GovernanceVoteCmdError
import           Cardano.CLI.Types.Governance

import           Control.Monad.Except
import           Control.Monad.Trans.Except.Extra
import           Data.Function

runGovernanceVoteCmds :: ()
  => GovernanceVoteCmds era
  -> ExceptT CmdError IO ()
runGovernanceVoteCmds = \case
  GovernanceVoteCreateCmd anyVote outFp ->
    runGovernanceVoteCreateCmd anyVote outFp
      & firstExceptT CmdGovernanceVoteError

runGovernanceVoteCreateCmd
  :: AnyVote
  -> File () Out
  -> ExceptT GovernanceVoteCmdError IO ()
runGovernanceVoteCreateCmd (ConwayOnwardsVote cOnwards v) outFp = do
  let sbe = conwayEraOnwardsToShelleyBasedEra cOnwards -- TODO: Conway era - update vote creation related function to take ConwayEraOnwards
      voteProcedure = createVotingProcedure sbe v Nothing
  firstExceptT GovernanceVoteCmeWriteError . newExceptT
    $ shelleyBasedEraConstraints sbe $ writeFileTextEnvelope outFp Nothing voteProcedure
