{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.CLI.EraBased.Run.Governance.Actions
  ( runGovernanceActionCmds
  , GovernanceActionsError(..)
  , addCostModelsToEraBasedProtocolParametersUpdate
  ) where

import           Cardano.Api
import           Cardano.Api.Ledger (StrictMaybe (..))
import qualified Cardano.Api.Ledger as L
import           Cardano.Api.Shelley

import           Cardano.CLI.EraBased.Commands.Governance.Actions
import qualified Cardano.CLI.EraBased.Commands.Governance.Actions as Cmd
import           Cardano.CLI.Json.Friendly
import           Cardano.CLI.Read
import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Errors.GovernanceActionsError
import           Cardano.CLI.Types.Key

import           Control.Monad
import           GHC.Exts (IsList (..))

runGovernanceActionCmds :: ()
  => GovernanceActionCmds era
  -> ExceptT GovernanceActionsError IO ()
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

runGovernanceActionViewCmd :: ()
  => GovernanceActionViewCmdArgs era
  -> ExceptT GovernanceActionsError IO ()
runGovernanceActionViewCmd
  Cmd.GovernanceActionViewCmdArgs
    { Cmd.outFormat
    , Cmd.actionFile
    , Cmd.mOutFile
    , Cmd.eon
    } = do
  proposal <- fmap fst . firstExceptT GovernanceActionsCmdProposalError . newExceptT
                $ readProposal eon (actionFile, Nothing)
  firstExceptT GovernanceActionsCmdWriteFileError . newExceptT $
    friendlyProposal
      (case outFormat of
        ViewOutputFormatJson -> FriendlyJson
        ViewOutputFormatYaml -> FriendlyYaml)
      mOutFile
      eon
      proposal

runGovernanceActionInfoCmd :: ()
  => GovernanceActionInfoCmdArgs era
  -> ExceptT GovernanceActionsError IO ()
runGovernanceActionInfoCmd
    Cmd.GovernanceActionInfoCmdArgs
      { Cmd.eon
      , Cmd.networkId
      , Cmd.deposit
      , Cmd.returnStakeAddress
      , Cmd.proposalUrl
      , Cmd.proposalHash
      , Cmd.outFile
      } = do
  depositStakeCredential <- firstExceptT GovernanceActionsReadStakeCredErrror
                     $ getStakeCredentialFromIdentifier returnStakeAddress

  let proposalAnchor = L.Anchor
        { L.anchorUrl = unProposalUrl proposalUrl
        , L.anchorDataHash = proposalHash
        }

  let sbe = conwayEraOnwardsToShelleyBasedEra eon
      govAction = InfoAct
      proposalProcedure = createProposalProcedure sbe networkId deposit depositStakeCredential govAction proposalAnchor

  firstExceptT GovernanceActionsCmdWriteFileError . newExceptT
    $ conwayEraOnwardsConstraints eon
    $ writeFileTextEnvelope outFile (Just "Info proposal") proposalProcedure

-- TODO: Conway era - update with new ledger types from cardano-ledger-conway-1.7.0.0
runGovernanceActionCreateNoConfidenceCmd :: ()
  => GovernanceActionCreateNoConfidenceCmdArgs era
  -> ExceptT GovernanceActionsError IO ()
runGovernanceActionCreateNoConfidenceCmd
    Cmd.GovernanceActionCreateNoConfidenceCmdArgs
      { Cmd.eon
      , Cmd.networkId
      , Cmd.deposit
      , Cmd.returnStakeAddress
      , Cmd.proposalUrl
      , Cmd.proposalHash
      , Cmd.governanceActionId
      , Cmd.governanceActionIndex
      , Cmd.outFile
      } = do
  depositStakeCredential
    <- firstExceptT GovernanceActionsReadStakeCredErrror
         $ getStakeCredentialFromIdentifier returnStakeAddress

  let proposalAnchor = L.Anchor
        { L.anchorUrl = unProposalUrl proposalUrl
        , L.anchorDataHash = proposalHash
        }

  let sbe = conwayEraOnwardsToShelleyBasedEra eon
      previousGovernanceAction = MotionOfNoConfidence
                                   $ L.SJust
                                   $ shelleyBasedEraConstraints sbe
                                   $ createPreviousGovernanceActionId
                                       governanceActionId
                                       governanceActionIndex

      proposalProcedure = createProposalProcedure sbe networkId deposit depositStakeCredential previousGovernanceAction proposalAnchor

  firstExceptT GovernanceActionsCmdWriteFileError . newExceptT
    $ conwayEraOnwardsConstraints eon
    $ writeFileTextEnvelope outFile (Just "Motion of no confidence proposal") proposalProcedure

runGovernanceActionCreateConstitutionCmd :: ()
  => GovernanceActionCreateConstitutionCmdArgs era
  -> ExceptT GovernanceActionsError IO ()
runGovernanceActionCreateConstitutionCmd
    Cmd.GovernanceActionCreateConstitutionCmdArgs
      { Cmd.eon
      , Cmd.networkId
      , Cmd.deposit
      , Cmd.stakeCredential
      , Cmd.mPrevGovernanceActionId
      , Cmd.proposalUrl
      , Cmd.proposalHash
      , Cmd.constitutionUrl
      , Cmd.constitutionHash
      , Cmd.constitutionScript
      , Cmd.outFile
      } = do

  depositStakeCredential
    <- firstExceptT GovernanceActionsReadStakeCredErrror
         $ getStakeCredentialFromIdentifier stakeCredential

  let proposalAnchor = L.Anchor
        { L.anchorUrl = unProposalUrl proposalUrl
        , L.anchorDataHash = proposalHash
        }

  let prevGovActId = L.maybeToStrictMaybe
                       $ shelleyBasedEraConstraints sbe
                       $ uncurry createPreviousGovernanceActionId <$> mPrevGovernanceActionId
      constitutionAnchor = L.Anchor
        { L.anchorUrl = unConstitutionUrl constitutionUrl
        , L.anchorDataHash = constitutionHash
        }
      govAct = ProposeNewConstitution
                  prevGovActId
                  constitutionAnchor
                  (toShelleyScriptHash <$> L.maybeToStrictMaybe constitutionScript)
      sbe = conwayEraOnwardsToShelleyBasedEra eon
      proposalProcedure = createProposalProcedure sbe networkId deposit depositStakeCredential govAct proposalAnchor

  firstExceptT GovernanceActionsCmdWriteFileError . newExceptT
    $ conwayEraOnwardsConstraints eon
    $ writeFileTextEnvelope outFile (Just "Update to the Constitution or policy proposal") proposalProcedure

-- TODO: Conway era - After ledger bump update this function
-- with the new ledger types
runGovernanceActionUpdateCommitteeCmd :: ()
  => GovernanceActionUpdateCommitteeCmdArgs era
  -> ExceptT GovernanceActionsError IO ()
runGovernanceActionUpdateCommitteeCmd
    Cmd.GovernanceActionUpdateCommitteeCmdArgs
      { Cmd.eon
      , Cmd.networkId
      , Cmd.deposit
      , Cmd.returnAddress
      , Cmd.proposalUrl
      , Cmd.proposalHash
      , Cmd.oldCommitteeVkeySource
      , Cmd.newCommitteeVkeySource
      , Cmd.requiredThreshold
      , Cmd.mPrevGovernanceActionId
      , Cmd.outFile
      } = do
  let sbe = conwayEraOnwardsToShelleyBasedEra eon
      govActIdentifier = L.maybeToStrictMaybe
                           $ shelleyBasedEraConstraints sbe
                           $ uncurry createPreviousGovernanceActionId <$> mPrevGovernanceActionId
      thresholdRational = toRational requiredThreshold

  let proposalAnchor = L.Anchor
        { L.anchorUrl = unProposalUrl proposalUrl
        , L.anchorDataHash = proposalHash
        }

  oldCommitteeKeyHashes <- forM oldCommitteeVkeySource $ \vkeyOrHashOrTextFile ->
    modifyError GovernanceActionsCmdReadFileError $
      readVerificationKeyOrHashOrFileOrScriptHash AsCommitteeColdKey unCommitteeColdKeyHash vkeyOrHashOrTextFile

  newCommitteeKeyHashes <- forM newCommitteeVkeySource $ \(vkeyOrHashOrTextFile, expEpoch) -> do
    kh <- modifyError GovernanceActionsCmdReadFileError $
      readVerificationKeyOrHashOrFileOrScriptHash AsCommitteeColdKey unCommitteeColdKeyHash vkeyOrHashOrTextFile
    pure (kh, expEpoch)

  depositStakeCredential
    <- firstExceptT GovernanceActionsReadStakeCredErrror
         $ getStakeCredentialFromIdentifier returnAddress

  let proposeNewCommittee = ProposeNewCommittee
                              govActIdentifier
                              oldCommitteeKeyHashes
                              (fromList newCommitteeKeyHashes)
                              thresholdRational
      proposal = createProposalProcedure sbe networkId deposit depositStakeCredential proposeNewCommittee proposalAnchor

  firstExceptT GovernanceActionsCmdWriteFileError . newExceptT
    $ conwayEraOnwardsConstraints eon
    $ writeFileTextEnvelope outFile (Just "New constitutional committee and/or threshold and/or terms proposal") proposal

runGovernanceActionCreateProtocolParametersUpdateCmd :: ()
  => Cmd.GovernanceActionProtocolParametersUpdateCmdArgs era
  -> ExceptT GovernanceActionsError IO ()
runGovernanceActionCreateProtocolParametersUpdateCmd eraBasedPParams' = do
  let sbe = uppShelleyBasedEra eraBasedPParams'
  caseShelleyToBabbageOrConwayEraOnwards
    (\sToB -> do
         let oFp = uppFilePath eraBasedPParams'
             anyEra = AnyShelleyBasedEra $ shelleyToBabbageEraToShelleyBasedEra sToB
         UpdateProtocolParametersPreConway _stB expEpoch genesisVerKeys
           <- hoistMaybe (GovernanceActionsValueUpdateProtocolParametersNotFound anyEra)
                $ uppPreConway eraBasedPParams'

         eraBasedPParams <- theUpdate

         let updateProtocolParams = createEraBasedProtocolParamUpdate sbe eraBasedPParams
             apiUpdateProtocolParamsType = fromLedgerPParamsUpdate sbe updateProtocolParams

         genVKeys <- sequence
           [ firstExceptT GovernanceActionsCmdReadTextEnvelopeFileError . newExceptT
               $ readFileTextEnvelope (AsVerificationKey AsGenesisKey) vkeyFile
           | vkeyFile <- genesisVerKeys
           ]

         let genKeyHashes = fmap verificationKeyHash genVKeys
             upProp = makeShelleyUpdateProposal apiUpdateProtocolParamsType genKeyHashes expEpoch

         firstExceptT GovernanceActionsCmdWriteFileError . newExceptT
           $ writeLazyByteStringFile oFp $ textEnvelopeToJSON Nothing upProp
    )
    (\conwayOnwards -> do
        let oFp = uppFilePath eraBasedPParams'
            anyEra = AnyShelleyBasedEra $ conwayEraOnwardsToShelleyBasedEra conwayOnwards

        UpdateProtocolParametersConwayOnwards _cOnwards network deposit returnAddr proposalUrl
                                              proposalHash mPrevGovActId mConstitutionalScriptHash
          <- hoistMaybe (GovernanceActionsValueUpdateProtocolParametersNotFound anyEra)
              $ uppConwayOnwards eraBasedPParams'

        eraBasedPParams <- theUpdate

        depositStakeCredential
          <- firstExceptT GovernanceActionsReadStakeCredErrror
               $ getStakeCredentialFromIdentifier returnAddr

        let updateProtocolParams = createEraBasedProtocolParamUpdate sbe eraBasedPParams

            prevGovActId = L.maybeToStrictMaybe $ uncurry createPreviousGovernanceActionId <$> mPrevGovActId
            proposalAnchor = L.Anchor
              { L.anchorUrl = unProposalUrl proposalUrl
              , L.anchorDataHash = proposalHash
              }
            govAct = UpdatePParams prevGovActId updateProtocolParams
                      (toShelleyScriptHash <$> L.maybeToStrictMaybe mConstitutionalScriptHash)


        let proposalProcedure = createProposalProcedure sbe network deposit depositStakeCredential govAct proposalAnchor

        firstExceptT GovernanceActionsCmdWriteFileError . newExceptT
          $ conwayEraOnwardsConstraints conwayOnwards
          $ writeFileTextEnvelope oFp (Just "Update protocol parameters proposal") proposalProcedure
    )
    sbe
  where
    theUpdate =
      case uppCostModelsFile eraBasedPParams' of
        Nothing -> pure $ uppNewPParams eraBasedPParams'
        Just (Cmd.CostModelsFile alonzoOnwards costModelsFile) -> do
          costModels <- firstExceptT GovernanceActionsCmdCostModelsError
            $ readCostModels costModelsFile
          pure . addCostModelsToEraBasedProtocolParametersUpdate alonzoOnwards costModels
            $ uppNewPParams eraBasedPParams'

readStakeKeyHash :: VerificationKeyOrHashOrFile StakeKey -> ExceptT GovernanceActionsError IO (Hash StakeKey)
readStakeKeyHash stake =
  modifyError GovernanceActionsCmdReadFileError $
    readVerificationKeyOrHashOrFile AsStakeKey stake

addCostModelsToEraBasedProtocolParametersUpdate
  :: AlonzoEraOnwards era
  -> L.CostModels
  -> EraBasedProtocolParametersUpdate era
  -> EraBasedProtocolParametersUpdate era
addCostModelsToEraBasedProtocolParametersUpdate
    AlonzoEraOnwardsAlonzo
    cmdls
    (AlonzoEraBasedProtocolParametersUpdate common sTa aOn depAfterB) =
  AlonzoEraBasedProtocolParametersUpdate common sTa (aOn { alCostModels = SJust cmdls }) depAfterB
addCostModelsToEraBasedProtocolParametersUpdate
    AlonzoEraOnwardsBabbage
    cmdls
    (BabbageEraBasedProtocolParametersUpdate common aOn depAfterB inB) =
  BabbageEraBasedProtocolParametersUpdate common (aOn { alCostModels = SJust cmdls }) depAfterB inB
addCostModelsToEraBasedProtocolParametersUpdate
    AlonzoEraOnwardsConway
    cmdls
    (ConwayEraBasedProtocolParametersUpdate common aOn inB inC) =
  ConwayEraBasedProtocolParametersUpdate common (aOn { alCostModels = SJust cmdls }) inB inC

runGovernanceActionTreasuryWithdrawalCmd :: ()
  => GovernanceActionTreasuryWithdrawalCmdArgs era
  -> ExceptT GovernanceActionsError IO ()
runGovernanceActionTreasuryWithdrawalCmd
    Cmd.GovernanceActionTreasuryWithdrawalCmdArgs
      { Cmd.eon
      , Cmd.networkId
      , Cmd.deposit
      , Cmd.returnAddr
      , Cmd.proposalUrl
      , Cmd.proposalHash
      , Cmd.treasuryWithdrawal
      , Cmd.constitutionScriptHash
      , Cmd.outFile
      } = do

  let proposalAnchor = L.Anchor
        { L.anchorUrl = unProposalUrl proposalUrl
        , L.anchorDataHash = proposalHash
        }

  depositStakeCredential
          <- firstExceptT GovernanceActionsReadStakeCredErrror
               $ getStakeCredentialFromIdentifier returnAddr

  withdrawals <- forM treasuryWithdrawal $ \(verificationKeyOrHashOrFile, lovelace) -> do
    stakeKeyHash <- readStakeKeyHash verificationKeyOrHashOrFile
    pure (networkId, StakeCredentialByKey stakeKeyHash, lovelace)

  let sbe = conwayEraOnwardsToShelleyBasedEra eon
      treasuryWithdrawals = TreasuryWithdrawal withdrawals
                              (toShelleyScriptHash <$> L.maybeToStrictMaybe constitutionScriptHash)
      proposal = createProposalProcedure sbe networkId deposit depositStakeCredential treasuryWithdrawals proposalAnchor

  firstExceptT GovernanceActionsCmdWriteFileError . newExceptT
    $ conwayEraOnwardsConstraints eon
    $ writeFileTextEnvelope outFile (Just "Treasury withdrawal proposal") proposal

runGovernanceActionHardforkInitCmd :: ()
  => GovernanceActionHardforkInitCmdArgs era
  -> ExceptT GovernanceActionsError IO ()
runGovernanceActionHardforkInitCmd
    Cmd.GovernanceActionHardforkInitCmdArgs
      { Cmd.eon
      , Cmd.networkId
      , Cmd.deposit
      , Cmd.returnStakeAddress
      , Cmd.mPrevGovernanceActionId
      , Cmd.proposalUrl
      , Cmd.proposalHash = anchorDataHash
      , Cmd.protVer
      , Cmd.outFile
      } = do
  depositStakeCredential
    <- firstExceptT GovernanceActionsReadStakeCredErrror
         $ getStakeCredentialFromIdentifier returnStakeAddress

  let proposalAnchor = L.Anchor
        { L.anchorUrl = unProposalUrl proposalUrl
        , L.anchorDataHash
        }

  let sbe = conwayEraOnwardsToShelleyBasedEra eon
      govActIdentifier = L.maybeToStrictMaybe
                           $ shelleyBasedEraConstraints sbe
                           $ uncurry createPreviousGovernanceActionId <$> mPrevGovernanceActionId
      initHardfork = InitiateHardfork
                      govActIdentifier
                      protVer

      proposalProcedure = createProposalProcedure sbe networkId deposit depositStakeCredential initHardfork proposalAnchor

  firstExceptT GovernanceActionsCmdWriteFileError . newExceptT
    $ conwayEraOnwardsConstraints eon
    $ writeFileTextEnvelope outFile (Just "Hardfork initiation proposal") proposalProcedure