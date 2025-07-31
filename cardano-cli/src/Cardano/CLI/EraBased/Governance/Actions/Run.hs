{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{- HLINT ignore "Redundant id" -}

module Cardano.CLI.EraBased.Governance.Actions.Run
  ( runGovernanceActionCmds
  , GovernanceActionsError (..)
  , addCostModelsToEraBasedProtocolParametersUpdate
  , carryHashChecks
  )
where

import Cardano.Api as Api
import Cardano.Api.Experimental (obtainCommonConstraints)
import Cardano.Api.Experimental qualified as Exp
import Cardano.Api.Ledger (StrictMaybe (..))
import Cardano.Api.Ledger qualified as L

import Cardano.CLI.Compatible.Exception
import Cardano.CLI.Compatible.Governance.Types (CostModelsFile (..))
import Cardano.CLI.Compatible.Json.Friendly
import Cardano.CLI.EraBased.Governance.Actions.Command
import Cardano.CLI.EraBased.Governance.Actions.Command qualified as Cmd
import Cardano.CLI.EraBased.Script.Proposal.Read
import Cardano.CLI.EraBased.Script.Proposal.Type
import Cardano.CLI.EraIndependent.Hash.Internal.Common (getByteStringFromURL, httpsAndIpfsSchemes)
import Cardano.CLI.Read
import Cardano.CLI.Type.Common
import Cardano.CLI.Type.Error.GovernanceActionsError
import Cardano.CLI.Type.Error.HashCmdError (FetchURLError)
import Cardano.CLI.Type.Key
import Cardano.Ledger.Hashes qualified as L

import Control.Monad
import GHC.Exts (IsList (..))

runGovernanceActionCmds
  :: ()
  => GovernanceActionCmds era
  -> CIO e ()
runGovernanceActionCmds = \case
  GovernanceActionCreateConstitutionCmd args ->
    runGovernanceActionCreateConstitutionCmd args
  GovernanceActionProtocolParametersUpdateCmd args ->
    runGovernanceActionCreateProtocolParametersUpdateCmd args
  GovernanceActionTreasuryWithdrawalCmd args ->
    runGovernanceActionTreasuryWithdrawalCmd args
  GovernanceActionUpdateCommitteeCmd args ->
    runGovernanceActionUpdateCommitteeCmd args
  GovernanceActionCreateNoConfidenceCmd args ->
    runGovernanceActionCreateNoConfidenceCmd args
  GovernanceActionHardforkInitCmd args ->
    runGovernanceActionHardforkInitCmd args
  GovernanceActionInfoCmd args ->
    runGovernanceActionInfoCmd args
  GovernanceActionViewCmd args ->
    runGovernanceActionViewCmd args

runGovernanceActionViewCmd
  :: forall era e
   . GovernanceActionViewCmdArgs era
  -> CIO e ()
runGovernanceActionViewCmd
  Cmd.GovernanceActionViewCmdArgs
    { Cmd.actionFile
    , Cmd.outputFormat
    , Cmd.mOutFile
    , Cmd.era
    } = Exp.obtainCommonConstraints era $ do
    proposal :: (Proposal era, Maybe (ProposalScriptWitness era)) <-
      readProposal (actionFile, Nothing)

    void $ friendlyProposal outputFormat mOutFile $ fst proposal

runGovernanceActionInfoCmd
  :: forall era e
   . ()
  => GovernanceActionInfoCmdArgs era
  -> CIO e ()
runGovernanceActionInfoCmd
  Cmd.GovernanceActionInfoCmdArgs
    { Cmd.era
    , Cmd.networkId
    , Cmd.deposit
    , Cmd.returnStakeAddress
    , Cmd.proposalUrl
    , Cmd.proposalHash
    , Cmd.checkProposalHash
    , Cmd.outFile
    } = do
    depositStakeCredential <-
      getStakeCredentialFromIdentifier returnStakeAddress

    let proposalAnchor =
          L.Anchor
            { L.anchorUrl = unProposalUrl proposalUrl
            , L.anchorDataHash = proposalHash
            }

    fromExceptTCli $ carryHashChecks checkProposalHash proposalAnchor ProposalCheck

    let sbe = convert era
        govAction = InfoAct
        proposalProcedure = createProposalProcedure sbe networkId deposit depositStakeCredential govAction proposalAnchor

    obtainCommonConstraints era $
      fromEitherIOCli $
        writeFileTextEnvelope outFile (Just "Info proposal") proposalProcedure

fetchURLErrorToGovernanceActionError
  :: AnchorDataTypeCheck -> ExceptT FetchURLError IO a -> ExceptT GovernanceActionsError IO a
fetchURLErrorToGovernanceActionError adt = withExceptT (GovernanceActionsProposalFetchURLError adt)

-- TODO: Conway era - update with new ledger types from cardano-ledger-conway-1.7.0.0
runGovernanceActionCreateNoConfidenceCmd
  :: forall era e
   . ()
  => GovernanceActionCreateNoConfidenceCmdArgs era
  -> CIO e ()
runGovernanceActionCreateNoConfidenceCmd
  Cmd.GovernanceActionCreateNoConfidenceCmdArgs
    { Cmd.era
    , Cmd.networkId
    , Cmd.deposit
    , Cmd.returnStakeAddress
    , Cmd.proposalUrl
    , Cmd.proposalHash
    , Cmd.checkProposalHash
    , Cmd.mPrevGovernanceActionId
    , Cmd.outFile
    } = do
    depositStakeCredential <-
      getStakeCredentialFromIdentifier returnStakeAddress

    let proposalAnchor =
          L.Anchor
            { L.anchorUrl = unProposalUrl proposalUrl
            , L.anchorDataHash = proposalHash
            }

    fromExceptTCli $ carryHashChecks checkProposalHash proposalAnchor ProposalCheck

    let sbe = convert era
        previousGovernanceAction =
          MotionOfNoConfidence $
            L.maybeToStrictMaybe $
              obtainCommonConstraints era $
                L.GovPurposeId <$> mPrevGovernanceActionId

        proposalProcedure =
          createProposalProcedure
            sbe
            networkId
            deposit
            depositStakeCredential
            previousGovernanceAction
            proposalAnchor

    obtainCommonConstraints era $
      fromEitherIOCli $
        writeFileTextEnvelope outFile (Just "Motion of no confidence proposal") proposalProcedure

runGovernanceActionCreateConstitutionCmd
  :: forall era e
   . ()
  => GovernanceActionCreateConstitutionCmdArgs era
  -> CIO e ()
runGovernanceActionCreateConstitutionCmd
  Cmd.GovernanceActionCreateConstitutionCmdArgs
    { Cmd.era
    , Cmd.networkId
    , Cmd.deposit
    , Cmd.stakeCredential
    , Cmd.mPrevGovernanceActionId
    , Cmd.proposalUrl
    , Cmd.proposalHash
    , Cmd.checkProposalHash
    , Cmd.constitutionUrl
    , Cmd.constitutionHash
    , Cmd.constitutionScript
    , Cmd.checkConstitutionHash
    , Cmd.outFile
    } = do
    depositStakeCredential <-
      getStakeCredentialFromIdentifier stakeCredential

    let proposalAnchor =
          L.Anchor
            { L.anchorUrl = unProposalUrl proposalUrl
            , L.anchorDataHash = proposalHash
            }

    fromExceptTCli $ carryHashChecks checkProposalHash proposalAnchor ProposalCheck

    let prevGovActId =
          L.maybeToStrictMaybe $
            L.GovPurposeId <$> mPrevGovernanceActionId
        constitutionAnchor =
          L.Anchor
            { L.anchorUrl = unConstitutionUrl constitutionUrl
            , L.anchorDataHash = constitutionHash
            }
        govAct =
          ProposeNewConstitution
            prevGovActId
            constitutionAnchor
            (toShelleyScriptHash <$> L.maybeToStrictMaybe constitutionScript)
        sbe = convert era
        proposalProcedure = createProposalProcedure sbe networkId deposit depositStakeCredential govAct proposalAnchor

    fromExceptTCli $ carryHashChecks checkConstitutionHash constitutionAnchor ConstitutionCheck

    Exp.obtainCommonConstraints era $
      fromEitherIOCli $
        writeFileTextEnvelope
          outFile
          (Just "Update to the Constitution or policy proposal")
          proposalProcedure

-- TODO: Conway era - After ledger bump update this function
-- with the new ledger types
runGovernanceActionUpdateCommitteeCmd
  :: forall era e
   . ()
  => GovernanceActionUpdateCommitteeCmdArgs era
  -> CIO e ()
runGovernanceActionUpdateCommitteeCmd
  Cmd.GovernanceActionUpdateCommitteeCmdArgs
    { Cmd.era
    , Cmd.networkId
    , Cmd.deposit
    , Cmd.returnAddress
    , Cmd.proposalUrl
    , Cmd.proposalHash
    , Cmd.checkProposalHash
    , Cmd.oldCommitteeVkeySource
    , Cmd.newCommitteeVkeySource
    , Cmd.requiredThreshold
    , Cmd.mPrevGovernanceActionId
    , Cmd.outFile
    } = do
    let sbe = convert era
        govActIdentifier =
          L.maybeToStrictMaybe $
            L.GovPurposeId <$> mPrevGovernanceActionId
        thresholdRational = toRational requiredThreshold

    let proposalAnchor =
          L.Anchor
            { L.anchorUrl = unProposalUrl proposalUrl
            , L.anchorDataHash = proposalHash
            }

    fromExceptTCli $ carryHashChecks checkProposalHash proposalAnchor ProposalCheck

    oldCommitteeKeyHashes <- forM oldCommitteeVkeySource $ \vkeyOrHashOrTextFile ->
      readVerificationKeyOrHashOrFileOrScriptHash
        unCommitteeColdKeyHash
        vkeyOrHashOrTextFile

    newCommitteeKeyHashes <- forM newCommitteeVkeySource $ \(vkeyOrHashOrTextFile, expEpoch) -> do
      kh <-
        readVerificationKeyOrHashOrFileOrScriptHash
          unCommitteeColdKeyHash
          vkeyOrHashOrTextFile
      pure (kh, expEpoch)

    depositStakeCredential <-
      getStakeCredentialFromIdentifier returnAddress

    let proposeNewCommittee =
          ProposeNewCommittee
            govActIdentifier
            oldCommitteeKeyHashes
            (fromList newCommitteeKeyHashes)
            thresholdRational
        proposal =
          createProposalProcedure
            sbe
            networkId
            deposit
            depositStakeCredential
            proposeNewCommittee
            proposalAnchor

    obtainCommonConstraints era $
      fromEitherIOCli $
        writeFileTextEnvelope
          outFile
          (Just "New constitutional committee and/or threshold and/or terms proposal")
          proposal

runGovernanceActionCreateProtocolParametersUpdateCmd
  :: forall era e
   . ()
  => Cmd.GovernanceActionProtocolParametersUpdateCmdArgs era
  -> CIO e ()
runGovernanceActionCreateProtocolParametersUpdateCmd eraBasedPParams' = do
  let era = uppShelleyBasedEra eraBasedPParams'
      sbe = convert era
      oFp = uppFilePath eraBasedPParams'
      UpdateProtocolParametersConwayOnwards
        _cOnwards
        network
        deposit
        returnAddr
        proposalUrl
        proposalHash
        checkProposalHash
        mPrevGovActId
        mConstitutionalScriptHash = uppConwayOnwards eraBasedPParams'

  eraBasedPParams <- fromExceptTCli theUpdate

  depositStakeCredential <-
    getStakeCredentialFromIdentifier returnAddr

  let updateProtocolParams = createEraBasedProtocolParamUpdate sbe eraBasedPParams
      prevGovActId = L.maybeToStrictMaybe $ L.GovPurposeId <$> mPrevGovActId
      proposalAnchor =
        L.Anchor
          { L.anchorUrl = unProposalUrl proposalUrl
          , L.anchorDataHash = proposalHash
          }

  fromExceptTCli $ carryHashChecks checkProposalHash proposalAnchor ProposalCheck

  let govAct =
        UpdatePParams
          prevGovActId
          updateProtocolParams
          (toShelleyScriptHash <$> L.maybeToStrictMaybe mConstitutionalScriptHash)

  let proposalProcedure = createProposalProcedure sbe network deposit depositStakeCredential govAct proposalAnchor

  fromEitherIOCli @(FileError ()) $
    obtainCommonConstraints era $
      writeFileTextEnvelope oFp (Just "Update protocol parameters proposal") proposalProcedure
 where
  theUpdate =
    case uppCostModelsFile eraBasedPParams' of
      Nothing -> pure $ uppNewPParams eraBasedPParams'
      Just (CostModelsFile alonzoOnwards costModelsFile) -> do
        costModels <-
          firstExceptT GovernanceActionsCmdCostModelsError $
            readCostModels costModelsFile
        pure . addCostModelsToEraBasedProtocolParametersUpdate alonzoOnwards costModels $
          uppNewPParams eraBasedPParams'

addCostModelsToEraBasedProtocolParametersUpdate
  :: AlonzoEraOnwards era
  -> L.CostModels
  -> EraBasedProtocolParametersUpdate era
  -> EraBasedProtocolParametersUpdate era
addCostModelsToEraBasedProtocolParametersUpdate
  AlonzoEraOnwardsAlonzo
  cmdls
  (AlonzoEraBasedProtocolParametersUpdate common sTa aOn depAfterB) =
    AlonzoEraBasedProtocolParametersUpdate common sTa (aOn{alCostModels = SJust cmdls}) depAfterB
addCostModelsToEraBasedProtocolParametersUpdate
  AlonzoEraOnwardsBabbage
  cmdls
  (BabbageEraBasedProtocolParametersUpdate common aOn depAfterB inB) =
    BabbageEraBasedProtocolParametersUpdate common (aOn{alCostModels = SJust cmdls}) depAfterB inB
addCostModelsToEraBasedProtocolParametersUpdate
  AlonzoEraOnwardsConway
  cmdls
  (ConwayEraBasedProtocolParametersUpdate common aOn inB inC) =
    ConwayEraBasedProtocolParametersUpdate common (aOn{alCostModels = SJust cmdls}) inB inC
addCostModelsToEraBasedProtocolParametersUpdate
  AlonzoEraOnwardsDijkstra
  _
  _ = error "addCostModelsToEraBasedProtocolParametersUpdate: Dijkstra not supported yet" -- TODO: Dijkstra

runGovernanceActionTreasuryWithdrawalCmd
  :: forall era e
   . ()
  => GovernanceActionTreasuryWithdrawalCmdArgs era
  -> CIO e ()
runGovernanceActionTreasuryWithdrawalCmd
  Cmd.GovernanceActionTreasuryWithdrawalCmdArgs
    { Cmd.era
    , Cmd.networkId
    , Cmd.deposit
    , Cmd.returnAddr
    , Cmd.proposalUrl
    , Cmd.proposalHash
    , Cmd.checkProposalHash
    , Cmd.treasuryWithdrawal
    , Cmd.constitutionScriptHash
    , Cmd.outFile
    } = do
    let proposalAnchor =
          L.Anchor
            { L.anchorUrl = unProposalUrl proposalUrl
            , L.anchorDataHash = proposalHash
            }

    fromExceptTCli $ carryHashChecks checkProposalHash proposalAnchor ProposalCheck

    depositStakeCredential <-
      getStakeCredentialFromIdentifier returnAddr

    withdrawals <- forM treasuryWithdrawal $ \(stakeIdentifier, lovelace) -> do
      stakeCredential <-
        getStakeCredentialFromIdentifier stakeIdentifier
      pure (networkId, stakeCredential, lovelace)

    let sbe = convert era
        treasuryWithdrawals =
          TreasuryWithdrawal
            withdrawals
            (toShelleyScriptHash <$> L.maybeToStrictMaybe constitutionScriptHash)
        proposal =
          createProposalProcedure
            sbe
            networkId
            deposit
            depositStakeCredential
            treasuryWithdrawals
            proposalAnchor

    obtainCommonConstraints era $
      fromEitherIOCli $
        writeFileTextEnvelope outFile (Just "Treasury withdrawal proposal") proposal

runGovernanceActionHardforkInitCmd
  :: forall era e
   . ()
  => GovernanceActionHardforkInitCmdArgs era
  -> CIO e ()
runGovernanceActionHardforkInitCmd
  Cmd.GovernanceActionHardforkInitCmdArgs
    { Cmd.era
    , Cmd.networkId
    , Cmd.deposit
    , Cmd.returnStakeAddress
    , Cmd.mPrevGovernanceActionId
    , Cmd.proposalUrl
    , Cmd.proposalHash = anchorDataHash
    , Cmd.checkProposalHash
    , Cmd.protVer
    , Cmd.outFile
    } = do
    depositStakeCredential <-
      getStakeCredentialFromIdentifier returnStakeAddress

    let proposalAnchor =
          L.Anchor
            { L.anchorUrl = unProposalUrl proposalUrl
            , L.anchorDataHash
            }

    fromExceptTCli $ carryHashChecks checkProposalHash proposalAnchor ProposalCheck

    let sbe = convert era
        govActIdentifier =
          L.maybeToStrictMaybe $
            L.GovPurposeId <$> mPrevGovernanceActionId
        initHardfork =
          InitiateHardfork
            govActIdentifier
            protVer

        proposalProcedure = createProposalProcedure sbe networkId deposit depositStakeCredential initHardfork proposalAnchor

    obtainCommonConstraints era $
      fromEitherIOCli $
        writeFileTextEnvelope outFile (Just "Hardfork initiation proposal") proposalProcedure

-- | Check the hash of the anchor data against the hash in the anchor if
-- checkHash is set to CheckHash.
carryHashChecks
  :: MustCheckHash a
  -- ^ Whether to check the hash or not (CheckHash for checking or TrustHash for not checking)
  -> L.Anchor
  -- ^ The anchor data whose hash is to be checked
  -> AnchorDataTypeCheck
  -- ^ The type of anchor data to check (for error reporting purpouses)
  -> ExceptT GovernanceActionsError IO ()
carryHashChecks checkHash anchor checkType =
  case checkHash of
    CheckHash -> do
      anchorData <-
        L.AnchorData
          <$> fetchURLErrorToGovernanceActionError
            checkType
            (getByteStringFromURL httpsAndIpfsSchemes $ L.urlToText $ L.anchorUrl anchor)
      let hash = L.hashAnnotated anchorData
      when (hash /= L.anchorDataHash anchor) $
        left $
          GovernanceActionsMismatchedHashError checkType (L.anchorDataHash anchor) hash
    TrustHash -> pure ()
