{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant id" #-}

module Cardano.CLI.EraBased.Governance.Vote.Run
  ( runGovernanceVoteCmds
  )
where

import Cardano.Api
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Shelley

import Cardano.CLI.Compatible.Exception
import Cardano.CLI.EraBased.Governance.Vote.Command qualified as Cmd
import Cardano.CLI.EraBased.Script.Vote.Read
import Cardano.CLI.EraIndependent.Hash.Internal.Common (carryHashChecks)
import Cardano.CLI.Json.Encode qualified as Json
import Cardano.CLI.Read (getHashFromStakePoolKeyHashSource)
import Cardano.CLI.Type.Common
import Cardano.CLI.Type.Governance
import Cardano.CLI.Type.Key

import Data.Function
import Vary qualified

runGovernanceVoteCmds
  :: ()
  => Cmd.GovernanceVoteCmds era
  -> CIO e ()
runGovernanceVoteCmds = \case
  Cmd.GovernanceVoteCreateCmd args ->
    runGovernanceVoteCreateCmd args
  Cmd.GovernanceVoteViewCmd args ->
    runGovernanceVoteViewCmd args

runGovernanceVoteCreateCmd
  :: forall era e
   . ()
  => Cmd.GovernanceVoteCreateCmdArgs era
  -> CIO e ()
runGovernanceVoteCreateCmd
  Cmd.GovernanceVoteCreateCmdArgs
    { eon
    , voteChoice
    , governanceActionId
    , votingStakeCredentialSource
    , mAnchor
    , outFile
    } = do
    let sbe = convert eon -- TODO: Conway era - update vote creation related function to take ConwayEraOnwards
        mAnchor' =
          fmap
            ( \pca@PotentiallyCheckedAnchor{pcaAnchor = (VoteUrl url, voteHash)} ->
                pca{pcaAnchor = L.Anchor{L.anchorUrl = url, L.anchorDataHash = voteHash}}
            )
            mAnchor

    mapM_
      (fromExceptTCli . carryHashChecks)
      mAnchor'

    voteProcedure <- case mAnchor' of
      Nothing -> pure $ createVotingProcedure eon voteChoice Nothing
      Just voteAnchor ->
        shelleyBasedEraConstraints sbe $
          let VotingProcedure votingProcedureWithoutAnchor = createVotingProcedure eon voteChoice Nothing
              votingProcedureWithAnchor = VotingProcedure $ votingProcedureWithoutAnchor{L.vProcAnchor = L.SJust (pcaAnchor voteAnchor)}
           in return votingProcedureWithAnchor

    shelleyBasedEraConstraints sbe $ do
      voter <- fromExceptTCli $ case votingStakeCredentialSource of
        AnyDRepVerificationKeyOrHashOrFileOrScriptHash stake -> do
          L.DRepVoter <$> readVerificationKeyOrHashOrFileOrScriptHash unDRepKeyHash stake
        AnyStakePoolVerificationKeyOrHashOrFile stake -> do
          StakePoolKeyHash h <-
            liftIO $ getHashFromStakePoolKeyHashSource stake
          pure $ L.StakePoolVoter h
        AnyCommitteeHotVerificationKeyOrHashOrFileOrScriptHash stake ->
          L.CommitteeVoter <$> readVerificationKeyOrHashOrFileOrScriptHash unCommitteeHotKeyHash stake

      let votingProcedures = singletonVotingProcedures eon voter governanceActionId (unVotingProcedure voteProcedure)
      fromEitherIOCli $
        writeFileTextEnvelope outFile Nothing votingProcedures

runGovernanceVoteViewCmd
  :: forall era e
   . ()
  => Cmd.GovernanceVoteViewCmdArgs era
  -> CIO e ()
runGovernanceVoteViewCmd
  Cmd.GovernanceVoteViewCmdArgs
    { eon
    , voteFile
    , outputFormat
    , mOutFile
    } = do
    let sbe :: ShelleyBasedEra era = convert eon

    shelleyBasedEraConstraints sbe $ do
      voteProcedures <-
        fmap fst $
          fromExceptTCli $
            readVoteScriptWitness eon (voteFile, Nothing)

      let output =
            outputFormat
              & ( id
                    . Vary.on (\FormatJson -> Json.encodeJson)
                    . Vary.on (\FormatYaml -> Json.encodeYaml)
                    $ Vary.exhaustiveCase
                )
              $ unVotingProcedures voteProcedures

      fromExceptTCli $
        writeLazyByteStringOutput mOutFile output
