{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.CLI.Compatible.Governance.Run
  ( runCompatibleGovernanceCmds
  )
where

import Cardano.Api as Api
import Cardano.Api.Ledger qualified as L

import Cardano.CLI.Compatible.Exception
import Cardano.CLI.Compatible.Governance.Command
import Cardano.CLI.Compatible.Governance.Types
import Cardano.CLI.EraBased.Governance.Actions.Run
import Cardano.CLI.EraBased.Governance.GenesisKeyDelegationCertificate.Run
import Cardano.CLI.EraBased.Governance.Run
import Cardano.CLI.Read
import Cardano.CLI.Type.Common
import Cardano.CLI.Type.Error.GovernanceActionsError

import Data.Typeable (Typeable)

runCompatibleGovernanceCmds :: Typeable era => CompatibleGovernanceCmds era -> CIO e ()
runCompatibleGovernanceCmds = \case
  CreateCompatibleProtocolParametersUpdateCmd cmd ->
    runCompatibleGovernanceActionCreateProtocolParametersUpdateCmd cmd
  LatestCompatibleGovernanceCmds cmd -> runGovernanceCmds cmd
  CompatibleGenesisKeyDelegationCertificate sta genVk genDelegVk vrfVk out ->
    runGovernanceGenesisKeyDelegationCertificate sta genVk genDelegVk vrfVk out
  CompatibleCreateMirCertificateStakeAddressesCmd w mirpot vKeys rewards out ->
    runGovernanceMIRCertificatePayStakeAddrs w mirpot vKeys rewards out
  CompatibleCreateMirCertificateTransferToReservesCmd w ll oFp ->
    runGovernanceCreateMirCertificateTransferToReservesCmd w ll oFp
  CompatibleCreateMirCertificateTransferToTreasuryCmd w ll oFp ->
    runGovernanceCreateMirCertificateTransferToReservesCmd w ll oFp

runCompatibleGovernanceActionCreateProtocolParametersUpdateCmd
  :: forall era e
   . ()
  => GovernanceActionProtocolParametersUpdateCmdArgs era
  -> CIO e ()
runCompatibleGovernanceActionCreateProtocolParametersUpdateCmd eraBasedPParams' = do
  let sbe = uppShelleyBasedEra eraBasedPParams'
  case sbe of
    ShelleyBasedEraShelley ->
      shelleyToBabbageProtocolParametersUpdate sbe eraBasedPParams'
    ShelleyBasedEraAllegra ->
      shelleyToBabbageProtocolParametersUpdate sbe eraBasedPParams'
    ShelleyBasedEraMary ->
      shelleyToBabbageProtocolParametersUpdate sbe eraBasedPParams'
    ShelleyBasedEraAlonzo ->
      shelleyToBabbageProtocolParametersUpdate sbe eraBasedPParams'
    ShelleyBasedEraBabbage ->
      shelleyToBabbageProtocolParametersUpdate sbe eraBasedPParams'
    ShelleyBasedEraConway -> conwayProtocolParametersUpdate sbe eraBasedPParams'
 where

maybeAddUpdatedCostModel
  :: GovernanceActionProtocolParametersUpdateCmdArgs era
  -> CIO e (EraBasedProtocolParametersUpdate era)
maybeAddUpdatedCostModel args = case uppCostModelsFile args of
  Nothing -> pure $ uppNewPParams args
  Just (CostModelsFile alonzoOnwards costModelsFile') -> do
    costModels <-
      fromExceptTCli $
        readCostModels costModelsFile'
    pure . addCostModelsToEraBasedProtocolParametersUpdate alonzoOnwards costModels $
      uppNewPParams args

conwayProtocolParametersUpdate
  :: ShelleyBasedEra ConwayEra
  -> GovernanceActionProtocolParametersUpdateCmdArgs ConwayEra
  -> CIO e ()
conwayProtocolParametersUpdate sbe args = do
  let oFp = uppFilePath args
      anyEra = AnyShelleyBasedEra sbe

  UpdateProtocolParametersConwayOnwards
    _cOnwards
    network
    deposit'
    returnAddr'
    proposalUrl'
    proposalHash'
    checkProposalHash'
    mPrevGovActId
    mConstitutionalScriptHash <-
    fromExceptTCli $
      hoistMaybe (GovernanceActionsValueUpdateProtocolParametersNotFound anyEra) $
        uppConwayOnwards args

  eraBasedPParams <- maybeAddUpdatedCostModel args

  depositStakeCredential <-
    getStakeCredentialFromIdentifier returnAddr'

  let updateProtocolParams = createEraBasedProtocolParamUpdate sbe eraBasedPParams

      prevGovActId = L.maybeToStrictMaybe $ L.GovPurposeId <$> mPrevGovActId
      proposalAnchor =
        L.Anchor
          { L.anchorUrl = unProposalUrl proposalUrl'
          , L.anchorDataHash = proposalHash'
          }

  fromExceptTCli $ carryHashChecks checkProposalHash' proposalAnchor ProposalCheck

  let govAct =
        UpdatePParams
          prevGovActId
          updateProtocolParams
          (toShelleyScriptHash <$> L.maybeToStrictMaybe mConstitutionalScriptHash)

  let proposalProcedure = createProposalProcedure sbe network deposit' depositStakeCredential govAct proposalAnchor

  fromEitherIOCli @(FileError ()) $
    shelleyBasedEraConstraints sbe $
      writeFileTextEnvelope oFp (Just "Update protocol parameters proposal") proposalProcedure

shelleyToBabbageProtocolParametersUpdate
  :: Typeable era
  => ShelleyBasedEra era
  -> GovernanceActionProtocolParametersUpdateCmdArgs era
  -> CIO e ()
shelleyToBabbageProtocolParametersUpdate sbe args = do
  let oFp = uppFilePath args
      anyEra = AnyShelleyBasedEra sbe
  UpdateProtocolParametersPreConway _stB expEpoch genesisVerKeys <-
    fromExceptTCli $
      hoistMaybe (GovernanceActionsValueUpdateProtocolParametersNotFound anyEra) $
        uppPreConway args

  eraBasedPParams <- maybeAddUpdatedCostModel args

  let updateProtocolParams = createEraBasedProtocolParamUpdate sbe eraBasedPParams
      apiUpdateProtocolParamsType = fromLedgerPParamsUpdate sbe updateProtocolParams

  genVKeys <-
    sequence
      [ fromEitherIOCli $
          readFileTextEnvelope vkeyFile
      | vkeyFile <- genesisVerKeys
      ]

  let genKeyHashes = fmap verificationKeyHash genVKeys
      upProp = makeShelleyUpdateProposal apiUpdateProtocolParamsType genKeyHashes expEpoch

  fromEitherIOCli @(FileError ()) $
    shelleyBasedEraConstraints sbe $
      writeLazyByteStringFile oFp $
        textEnvelopeToJSON Nothing upProp
