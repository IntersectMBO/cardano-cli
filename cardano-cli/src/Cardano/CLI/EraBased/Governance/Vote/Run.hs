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

import Cardano.CLI.EraBased.Governance.Vote.Command qualified as Cmd
import Cardano.CLI.EraBased.Script.Vote.Read
import Cardano.CLI.EraIndependent.Hash.Internal.Common (carryHashChecks)
import Cardano.CLI.Json.Encode qualified as Json
import Cardano.CLI.Read (getHashFromStakePoolKeyHashSource)
import Cardano.CLI.Type.Common
import Cardano.CLI.Type.Error.CmdError
import Cardano.CLI.Type.Error.GovernanceVoteCmdError
import Cardano.CLI.Type.Governance
import Cardano.CLI.Type.Key

import Data.Function
import Vary qualified

runGovernanceVoteCmds
  :: ()
  => Cmd.GovernanceVoteCmds era
  -> ExceptT CmdError IO ()
runGovernanceVoteCmds = \case
  Cmd.GovernanceVoteCreateCmd args ->
    runGovernanceVoteCreateCmd args
      & firstExceptT CmdGovernanceVoteError
  Cmd.GovernanceVoteViewCmd args ->
    runGovernanceVoteViewCmd args
      & firstExceptT CmdGovernanceVoteError

runGovernanceVoteCreateCmd
  :: forall era
   . ()
  => Cmd.GovernanceVoteCreateCmdArgs era
  -> ExceptT GovernanceVoteCmdError IO ()
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
      (withExceptT GovernanceVoteCmdResignationCertHashCheckError . carryHashChecks)
      mAnchor'

    voteProcedure <- case mAnchor' of
      Nothing -> pure $ createVotingProcedure eon voteChoice Nothing
      Just voteAnchor ->
        shelleyBasedEraConstraints sbe $
          let VotingProcedure votingProcedureWithoutAnchor = createVotingProcedure eon voteChoice Nothing
              votingProcedureWithAnchor = VotingProcedure $ votingProcedureWithoutAnchor{L.vProcAnchor = L.SJust (pcaAnchor voteAnchor)}
           in return votingProcedureWithAnchor

    shelleyBasedEraConstraints sbe $ do
      voter <- firstExceptT GovernanceVoteCmdReadVerificationKeyError $ case votingStakeCredentialSource of
        AnyDRepVerificationKeyOrHashOrFileOrScriptHash stake -> do
          drepCred <- readVerificationKeyOrHashOrFileOrScriptHash AsDRepKey unDRepKeyHash stake
          pure $ L.DRepVoter drepCred
        AnyStakePoolVerificationKeyOrHashOrFile stake -> do
          StakePoolKeyHash h <-
            liftIO $ getHashFromStakePoolKeyHashSource stake
          pure $ L.StakePoolVoter h
        AnyCommitteeHotVerificationKeyOrHashOrFileOrScriptHash stake -> do
          hotCred <- readVerificationKeyOrHashOrFileOrScriptHash AsCommitteeHotKey unCommitteeHotKeyHash stake
          pure $ L.CommitteeVoter hotCred

      let votingProcedures = singletonVotingProcedures eon voter governanceActionId (unVotingProcedure voteProcedure)
      firstExceptT GovernanceVoteCmdWriteError . newExceptT $
        writeFileTextEnvelope outFile Nothing votingProcedures

runGovernanceVoteViewCmd
  :: forall era
   . ()
  => Cmd.GovernanceVoteViewCmdArgs era
  -> ExceptT GovernanceVoteCmdError IO ()
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
          firstExceptT GovernanceVoteCmdReadVoteFileError $
            readVoteScriptWitness eon (voteFile, Nothing)

      let output =
            outputFormat
              & ( id
                    . Vary.on (\FormatJson -> Json.encodeJson)
                    . Vary.on (\FormatYaml -> Json.encodeYaml)
                    $ Vary.exhaustiveCase
                )
              $ unVotingProcedures voteProcedures

      firstExceptT GovernanceVoteCmdWriteError
        . newExceptT
        $ writeLazyByteStringOutput mOutFile output
