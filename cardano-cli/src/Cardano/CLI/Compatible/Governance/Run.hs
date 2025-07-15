{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
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
    runGovernanceActionCreateProtocolParametersUpdateCmd cmd
  LatestCompatibleGovernanceCmds cmd -> runGovernanceCmds cmd
  CompatibleGenesisKeyDelegationCertificate sta genVk genDelegVk vrfVk out ->
    runGovernanceGenesisKeyDelegationCertificate sta genVk genDelegVk vrfVk out
  CompatibleCreateMirCertificateStakeAddressesCmd w mirpot vKeys rewards out ->
    runGovernanceMIRCertificatePayStakeAddrs w mirpot vKeys rewards out
  CompatibleCreateMirCertificateTransferToReservesCmd w ll oFp ->
    runGovernanceCreateMirCertificateTransferToReservesCmd w ll oFp
  CompatibleCreateMirCertificateTransferToTreasuryCmd w ll oFp ->
    runGovernanceCreateMirCertificateTransferToReservesCmd w ll oFp

runGovernanceActionCreateProtocolParametersUpdateCmd
  :: forall era e
   . ()
  => GovernanceActionProtocolParametersUpdateCmdArgs era
  -> CIO e ()
runGovernanceActionCreateProtocolParametersUpdateCmd eraBasedPParams' = do
  let sbe = uppShelleyBasedEra eraBasedPParams'
  caseShelleyToBabbageOrConwayEraOnwards
    ( \sToB ->
        do
          let oFp = uppFilePath eraBasedPParams'
              sbe' = convert sToB
              anyEra = AnyShelleyBasedEra (convert sToB)
          UpdateProtocolParametersPreConway _stB expEpoch genesisVerKeys <-
            fromExceptTCli $
              hoistMaybe (GovernanceActionsValueUpdateProtocolParametersNotFound anyEra) $
                uppPreConway eraBasedPParams'

          eraBasedPParams <- fromExceptTCli theUpdate

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
            shelleyBasedEraConstraints sbe' $
              writeLazyByteStringFile oFp $
                textEnvelopeToJSON Nothing upProp
    )
    ( \conwayOnwards -> do
        let oFp = uppFilePath eraBasedPParams'
            anyEra = AnyShelleyBasedEra (convert conwayOnwards)

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
              uppConwayOnwards eraBasedPParams'

        eraBasedPParams <- fromExceptTCli theUpdate

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

        fromEitherIOCli $
          conwayEraOnwardsConstraints conwayOnwards $
            writeFileTextEnvelope oFp (Just "Update protocol parameters proposal") proposalProcedure
    )
    sbe
 where
  theUpdate =
    case uppCostModelsFile eraBasedPParams' of
      Nothing -> pure $ uppNewPParams eraBasedPParams'
      Just (CostModelsFile alonzoOnwards costModelsFile') -> do
        costModels <-
          firstExceptT GovernanceActionsCmdCostModelsError $
            readCostModels costModelsFile'
        pure . addCostModelsToEraBasedProtocolParametersUpdate alonzoOnwards costModels $
          uppNewPParams eraBasedPParams'
