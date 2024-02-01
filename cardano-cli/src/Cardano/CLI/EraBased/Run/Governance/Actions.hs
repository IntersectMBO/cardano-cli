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
import qualified Cardano.Api.Ledger as Ledger
import           Cardano.Api.Shelley

import           Cardano.CLI.EraBased.Commands.Governance.Actions
import qualified Cardano.CLI.EraBased.Commands.Governance.Actions as Cmd
import           Cardano.CLI.Json.Friendly
import           Cardano.CLI.Read
import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Errors.GovernanceActionsError
import           Cardano.CLI.Types.Key
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo

import           Control.Monad
import           Control.Monad.Except (ExceptT)
import           Control.Monad.Trans (MonadTrans (..))
import           Control.Monad.Trans.Except.Extra
import           Data.Function
import qualified Data.Map.Strict as Map

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

  GoveranceActionUpdateCommitteeCmd args ->
    runGovernanceActionCreateNewCommitteeCmd args

  GovernanceActionCreateNoConfidenceCmd args ->
    runGovernanceActionCreateNoConfidenceCmd args

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
  returnKeyHash <- readStakeKeyHash returnStakeAddress

  let proposalAnchor = Ledger.Anchor
        { Ledger.anchorUrl = unProposalUrl proposalUrl
        , Ledger.anchorDataHash = proposalHash
        }

  let sbe = conwayEraOnwardsToShelleyBasedEra eon
      govAction = InfoAct
      proposalProcedure = createProposalProcedure sbe networkId deposit returnKeyHash govAction proposalAnchor

  firstExceptT GovernanceActionsCmdWriteFileError . newExceptT
    $ conwayEraOnwardsConstraints eon
    $ writeFileTextEnvelope outFile Nothing proposalProcedure

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
  returnKeyHash <- readStakeKeyHash returnStakeAddress

  let proposalAnchor = Ledger.Anchor
        { Ledger.anchorUrl = unProposalUrl proposalUrl
        , Ledger.anchorDataHash = proposalHash
        }

  let sbe = conwayEraOnwardsToShelleyBasedEra eon
      previousGovernanceAction = MotionOfNoConfidence
                                   $ Ledger.SJust
                                   $ shelleyBasedEraConstraints sbe
                                   $ createPreviousGovernanceActionId
                                       governanceActionId
                                       governanceActionIndex

      proposalProcedure = createProposalProcedure sbe networkId deposit returnKeyHash previousGovernanceAction proposalAnchor

  firstExceptT GovernanceActionsCmdWriteFileError . newExceptT
    $ conwayEraOnwardsConstraints eon
    $ writeFileTextEnvelope outFile Nothing proposalProcedure

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
      , Cmd.outFile
      } = do

  stakeKeyHash <- readStakeKeyHash stakeCredential

  let proposalAnchor = Ledger.Anchor
        { Ledger.anchorUrl = unProposalUrl proposalUrl
        , Ledger.anchorDataHash = proposalHash
        }

  let prevGovActId = Ledger.maybeToStrictMaybe
                       $ shelleyBasedEraConstraints sbe
                       $ uncurry createPreviousGovernanceActionId <$> mPrevGovernanceActionId
      constitutionAnchor = Ledger.Anchor
        { Ledger.anchorUrl = unConstitutionUrl constitutionUrl
        , Ledger.anchorDataHash = constitutionHash
        }
      govAct = ProposeNewConstitution prevGovActId constitutionAnchor
      sbe = conwayEraOnwardsToShelleyBasedEra eon
      proposalProcedure = createProposalProcedure sbe networkId deposit stakeKeyHash govAct proposalAnchor

  firstExceptT GovernanceActionsCmdWriteFileError . newExceptT
    $ conwayEraOnwardsConstraints eon
    $ writeFileTextEnvelope outFile Nothing proposalProcedure

-- TODO: Conway era - After ledger bump update this function
-- with the new ledger types
runGovernanceActionCreateNewCommitteeCmd :: ()
  => GoveranceActionUpdateCommitteeCmdArgs era
  -> ExceptT GovernanceActionsError IO ()
runGovernanceActionCreateNewCommitteeCmd
    Cmd.GoveranceActionUpdateCommitteeCmdArgs
      { Cmd.eon
      , Cmd.networkId
      , Cmd.deposit
      , Cmd.returnAddress
      , Cmd.proposalUrl
      , Cmd.proposalHash
      , Cmd.oldCommitteeVkeySource
      , Cmd.newCommitteeVkeySource
      , Cmd.requiredQuorum
      , Cmd.mPrevGovernanceActionId
      , Cmd.outFile
      } = do
  let sbe = conwayEraOnwardsToShelleyBasedEra eon
      govActIdentifier = Ledger.maybeToStrictMaybe
                           $ shelleyBasedEraConstraints sbe
                           $ uncurry createPreviousGovernanceActionId <$> mPrevGovernanceActionId
      quorumRational = toRational requiredQuorum

  let proposalAnchor = Ledger.Anchor
        { Ledger.anchorUrl = unProposalUrl proposalUrl
        , Ledger.anchorDataHash = proposalHash
        }

  oldCommitteeKeyHashes <- forM oldCommitteeVkeySource $ \vkeyOrHashOrTextFile ->
    lift (readVerificationKeyOrHashOrTextEnvFile AsCommitteeColdKey vkeyOrHashOrTextFile)
      & onLeft (left . GovernanceActionsCmdReadFileError)

  newCommitteeKeyHashes <- forM newCommitteeVkeySource $ \(vkeyOrHashOrTextFile, expEpoch) -> do
    kh <- lift (readVerificationKeyOrHashOrTextEnvFile AsCommitteeColdKey vkeyOrHashOrTextFile)
      & onLeft (left . GovernanceActionsCmdReadFileError)
    pure (kh, expEpoch)

  returnKeyHash <- readStakeKeyHash returnAddress

  let proposeNewCommittee = ProposeNewCommittee
                              govActIdentifier
                              oldCommitteeKeyHashes
                              (Map.fromList newCommitteeKeyHashes)
                              quorumRational
      proposal = createProposalProcedure sbe networkId deposit returnKeyHash proposeNewCommittee proposalAnchor

  firstExceptT GovernanceActionsCmdWriteFileError . newExceptT
    $ conwayEraOnwardsConstraints eon
    $ writeFileTextEnvelope outFile Nothing proposal

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

        returnKeyHash <- readStakeKeyHash returnAddr

        let updateProtocolParams = createEraBasedProtocolParamUpdate sbe eraBasedPParams

            prevGovActId = Ledger.maybeToStrictMaybe $ uncurry createPreviousGovernanceActionId <$> mPrevGovActId
            proposalAnchor = Ledger.Anchor
              { Ledger.anchorUrl = unProposalUrl proposalUrl
              , Ledger.anchorDataHash = proposalHash
              }
            govAct = UpdatePParams prevGovActId updateProtocolParams
                      (toShelleyScriptHash <$> Ledger.maybeToStrictMaybe mConstitutionalScriptHash)


        let proposalProcedure = createProposalProcedure sbe network deposit returnKeyHash govAct proposalAnchor

        firstExceptT GovernanceActionsCmdWriteFileError . newExceptT
          $ conwayEraOnwardsConstraints conwayOnwards
          $ writeFileTextEnvelope oFp Nothing proposalProcedure
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
  firstExceptT GovernanceActionsCmdReadFileError
    . newExceptT $ readVerificationKeyOrHashOrFile AsStakeKey stake

addCostModelsToEraBasedProtocolParametersUpdate
  :: AlonzoEraOnwards era
  -> Alonzo.CostModels
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

  let proposalAnchor = Ledger.Anchor
        { Ledger.anchorUrl = unProposalUrl proposalUrl
        , Ledger.anchorDataHash = proposalHash
        }

  returnKeyHash <- readStakeKeyHash returnAddr

  withdrawals <- forM treasuryWithdrawal $ \(verificationKeyOrHashOrFile, lovelace) -> do
    stakeKeyHash <- readStakeKeyHash verificationKeyOrHashOrFile
    pure (networkId, StakeCredentialByKey stakeKeyHash, lovelace)

  let sbe = conwayEraOnwardsToShelleyBasedEra eon
      treasuryWithdrawals = TreasuryWithdrawal withdrawals
                              (toShelleyScriptHash <$> Ledger.maybeToStrictMaybe constitutionScriptHash)
      proposal = createProposalProcedure sbe networkId deposit returnKeyHash treasuryWithdrawals proposalAnchor

  firstExceptT GovernanceActionsCmdWriteFileError . newExceptT
    $ conwayEraOnwardsConstraints eon
    $ writeFileTextEnvelope outFile Nothing proposal
