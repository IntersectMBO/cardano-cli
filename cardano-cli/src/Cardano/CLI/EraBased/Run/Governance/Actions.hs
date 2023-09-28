{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.CLI.EraBased.Run.Governance.Actions
  ( runGovernanceActionCmds
  , GovernanceActionsError (..)
  ) where

import Cardano.Api
import Cardano.Api.Ledger (coerceKeyRole)
import Cardano.Api.Ledger qualified as Ledger
import Cardano.Api.Shelley

import Cardano.CLI.EraBased.Commands.Governance.Actions
import Cardano.CLI.Read
import Cardano.CLI.Types.Common
import Cardano.CLI.Types.Errors.GovernanceActionsError
import Cardano.CLI.Types.Key
import Cardano.Ledger.Conway.Governance qualified as Ledger

import Control.Monad
import Control.Monad.Except (ExceptT)
import Control.Monad.Trans (MonadTrans (..))
import Control.Monad.Trans.Except.Extra
import Data.Function
import Data.Map.Strict qualified as Map

runGovernanceActionCmds
  :: ()
  => GovernanceActionCmds era
  -> ExceptT GovernanceActionsError IO ()
runGovernanceActionCmds = \case
  GovernanceActionCreateConstitutionCmd cOn newConstitution ->
    runGovernanceActionCreateConstitutionCmd cOn newConstitution
  GovernanceActionProtocolParametersUpdateCmd sbe eNo genKeys eraBasedProtocolParametersUpdate ofp ->
    runGovernanceActionCreateProtocolParametersUpdateCmd
      sbe
      eNo
      genKeys
      eraBasedProtocolParametersUpdate
      ofp
  GovernanceActionTreasuryWithdrawalCmd cOn treasuryWithdrawal ->
    runGovernanceActionTreasuryWithdrawalCmd cOn treasuryWithdrawal
  GoveranceActionCreateNewCommitteeCmd con newCommittee ->
    runGovernanceActionCreateNewCommitteeCmd con newCommittee
  GovernanceActionCreateNoConfidenceCmd cOn noConfidence ->
    runGovernanceActionCreateNoConfidenceCmd cOn noConfidence
  GovernanceActionInfoCmd cOn newInfo ->
    runGovernanceActionInfoCmd cOn newInfo

runGovernanceActionInfoCmd
  :: ConwayEraOnwards era
  -> NewInfoCmd
  -> ExceptT GovernanceActionsError IO ()
runGovernanceActionInfoCmd cOn (NewInfoCmd network deposit returnAddr proposalUrl proposalHashSource outFp) = do
  returnKeyHash <- readStakeKeyHash returnAddr

  proposalHash <-
    proposalHashSourceToHash proposalHashSource
      & firstExceptT GovernanceActionsCmdProposalError

  let proposalAnchor =
        Ledger.Anchor
          { Ledger.anchorUrl = unProposalUrl proposalUrl
          , Ledger.anchorDataHash = proposalHash
          }

  let sbe = conwayEraOnwardsToShelleyBasedEra cOn
      govAction = InfoAct
      proposalProcedure = createProposalProcedure sbe network deposit returnKeyHash govAction proposalAnchor

  firstExceptT GovernanceActionsCmdWriteFileError . newExceptT $
    conwayEraOnwardsConstraints cOn $
      writeFileTextEnvelope outFp Nothing proposalProcedure

-- TODO: Conway era - update with new ledger types from cardano-ledger-conway-1.7.0.0
runGovernanceActionCreateNoConfidenceCmd
  :: ConwayEraOnwards era
  -> NoConfidenceCmd
  -> ExceptT GovernanceActionsError IO ()
runGovernanceActionCreateNoConfidenceCmd cOn (NoConfidenceCmd network deposit returnAddr proposalUrl proposalHashSource txid ind outFp) = do
  returnKeyHash <- readStakeKeyHash returnAddr

  proposalHash <-
    proposalHashSourceToHash proposalHashSource
      & firstExceptT GovernanceActionsCmdProposalError

  let proposalAnchor =
        Ledger.Anchor
          { Ledger.anchorUrl = unProposalUrl proposalUrl
          , Ledger.anchorDataHash = proposalHash
          }

  let sbe = conwayEraOnwardsToShelleyBasedEra cOn
      previousGovernanceAction = MotionOfNoConfidence . Ledger.SJust $ createPreviousGovernanceActionId txid ind
      proposalProcedure = createProposalProcedure sbe network deposit returnKeyHash previousGovernanceAction proposalAnchor

  firstExceptT GovernanceActionsCmdWriteFileError . newExceptT $
    conwayEraOnwardsConstraints cOn $
      writeFileTextEnvelope outFp Nothing proposalProcedure

runGovernanceActionCreateConstitutionCmd
  :: ()
  => ConwayEraOnwards era
  -> NewConstitutionCmd
  -> ExceptT GovernanceActionsError IO ()
runGovernanceActionCreateConstitutionCmd
  cOn
  ( NewConstitutionCmd
      network
      deposit
      anyStake
      mPrevGovActId
      proposalUrl
      proposalHashSource
      constitutionUrl
      constitutionHashSource
      outFp
    ) = do
    stakeKeyHash <- readStakeKeyHash anyStake

    proposalHash <-
      proposalHashSourceToHash proposalHashSource
        & firstExceptT GovernanceActionsCmdProposalError

    let proposalAnchor =
          Ledger.Anchor
            { Ledger.anchorUrl = unProposalUrl proposalUrl
            , Ledger.anchorDataHash = proposalHash
            }

    constitutionHash <-
      constitutionHashSourceToHash constitutionHashSource
        & firstExceptT GovernanceActionsCmdConstitutionError

    let prevGovActId = Ledger.maybeToStrictMaybe $ uncurry createPreviousGovernanceActionId <$> mPrevGovActId
        constitutionAnchor =
          Ledger.Anchor
            { Ledger.anchorUrl = unConstitutionUrl constitutionUrl
            , Ledger.anchorDataHash = constitutionHash
            }
        govAct = ProposeNewConstitution prevGovActId constitutionAnchor
        sbe = conwayEraOnwardsToShelleyBasedEra cOn
        proposalProcedure = createProposalProcedure sbe network deposit stakeKeyHash govAct proposalAnchor

    firstExceptT GovernanceActionsCmdWriteFileError . newExceptT $
      conwayEraOnwardsConstraints cOn $
        writeFileTextEnvelope outFp Nothing proposalProcedure

-- TODO: Conway era - After ledger bump update this function
-- with the new ledger types
runGovernanceActionCreateNewCommitteeCmd
  :: ConwayEraOnwards era
  -> NewCommitteeCmd
  -> ExceptT GovernanceActionsError IO ()
runGovernanceActionCreateNewCommitteeCmd cOn (NewCommitteeCmd network deposit retAddr proposalUrl proposalHashSource old new q prevActId oFp) = do
  let sbe = conwayEraOnwardsToShelleyBasedEra cOn -- TODO: Conway era - update vote creation related function to take ConwayEraOnwards
      govActIdentifier = Ledger.maybeToStrictMaybe $ uncurry createPreviousGovernanceActionId <$> prevActId
      quorumRational = toRational q

  proposalHash <-
    proposalHashSourceToHash proposalHashSource
      & firstExceptT GovernanceActionsCmdProposalError

  let proposalAnchor =
        Ledger.Anchor
          { Ledger.anchorUrl = unProposalUrl proposalUrl
          , Ledger.anchorDataHash = proposalHash
          }

  oldCommitteeKeyHashes <- forM old $ \vkeyOrHashOrTextFile ->
    lift (readVerificationKeyOrHashOrTextEnvFile AsCommitteeColdKey vkeyOrHashOrTextFile)
      & onLeft (left . GovernanceActionsCmdReadFileError)

  newCommitteeKeyHashes <- forM new $ \(vkeyOrHashOrTextFile, expEpoch) -> do
    kh <-
      lift (readVerificationKeyOrHashOrTextEnvFile AsCommitteeColdKey vkeyOrHashOrTextFile)
        & onLeft (left . GovernanceActionsCmdReadFileError)
    pure (kh, expEpoch)

  returnKeyHash <- readStakeKeyHash retAddr

  let proposeNewCommittee =
        ProposeNewCommittee
          govActIdentifier
          oldCommitteeKeyHashes
          (Map.fromList newCommitteeKeyHashes)
          quorumRational
      proposal = createProposalProcedure sbe network deposit returnKeyHash proposeNewCommittee proposalAnchor

  firstExceptT GovernanceActionsCmdWriteFileError . newExceptT $
    conwayEraOnwardsConstraints cOn $
      writeFileTextEnvelope oFp Nothing proposal

runGovernanceActionCreateProtocolParametersUpdateCmd
  :: ()
  => ShelleyBasedEra era
  -> EpochNo
  -> [VerificationKeyFile In]
  -- ^ Genesis verification keys
  -> EraBasedProtocolParametersUpdate era
  -> File () Out
  -> ExceptT GovernanceActionsError IO ()
runGovernanceActionCreateProtocolParametersUpdateCmd sbe expEpoch genesisVerKeys eraBasedPParams oFp = do
  genVKeys <-
    sequence
      [ firstExceptT GovernanceActionsCmdReadTextEnvelopeFileError . newExceptT $
        readFileTextEnvelope (AsVerificationKey AsGenesisKey) vkeyFile
      | vkeyFile <- genesisVerKeys
      ]

  let updateProtocolParams = createEraBasedProtocolParamUpdate sbe eraBasedPParams
      apiUpdateProtocolParamsType = fromLedgerPParamsUpdate sbe updateProtocolParams
      genKeyHashes = fmap verificationKeyHash genVKeys
      -- TODO: Update EraBasedProtocolParametersUpdate to require genesis delegate keys
      -- depending on the era
      upProp = makeShelleyUpdateProposal apiUpdateProtocolParamsType genKeyHashes expEpoch

  firstExceptT GovernanceActionsCmdWriteFileError . newExceptT $
    writeLazyByteStringFile oFp $
      textEnvelopeToJSON Nothing upProp

readStakeKeyHash :: AnyStakeIdentifier -> ExceptT GovernanceActionsError IO (Hash StakeKey)
readStakeKeyHash anyStake =
  case anyStake of
    AnyStakeKey stake ->
      firstExceptT GovernanceActionsCmdReadFileError
        . newExceptT
        $ readVerificationKeyOrHashOrFile AsStakeKey stake
    AnyStakePoolKey stake -> do
      StakePoolKeyHash t <-
        firstExceptT GovernanceActionsCmdReadFileError
          . newExceptT
          $ readVerificationKeyOrHashOrFile AsStakePoolKey stake
      return $ StakeKeyHash $ coerceKeyRole t

runGovernanceActionTreasuryWithdrawalCmd
  :: ConwayEraOnwards era
  -> TreasuryWithdrawalCmd
  -> ExceptT GovernanceActionsError IO ()
runGovernanceActionTreasuryWithdrawalCmd
  cOn
  ( TreasuryWithdrawalCmd
      network
      deposit
      returnAddr
      proposalUrl
      proposalHashSource
      treasuryWithdrawal
      outFp
    ) = do
    proposalHash <-
      proposalHashSourceToHash proposalHashSource
        & firstExceptT GovernanceActionsCmdProposalError

    let proposalAnchor =
          Ledger.Anchor
            { Ledger.anchorUrl = unProposalUrl proposalUrl
            , Ledger.anchorDataHash = proposalHash
            }

    returnKeyHash <- readStakeKeyHash returnAddr

    withdrawals <-
      sequence
        [ (network,,ll) <$> stakeIdentifiertoCredential stakeIdentifier
        | (stakeIdentifier, ll) <- treasuryWithdrawal
        ]

    let sbe = conwayEraOnwardsToShelleyBasedEra cOn
        treasuryWithdrawals = TreasuryWithdrawal withdrawals
        proposal = createProposalProcedure sbe network deposit returnKeyHash treasuryWithdrawals proposalAnchor

    firstExceptT GovernanceActionsCmdWriteFileError . newExceptT $
      conwayEraOnwardsConstraints cOn $
        writeFileTextEnvelope outFp Nothing proposal

stakeIdentifiertoCredential
  :: AnyStakeIdentifier -> ExceptT GovernanceActionsError IO StakeCredential
stakeIdentifiertoCredential anyStake =
  case anyStake of
    AnyStakeKey stake -> do
      hash <-
        firstExceptT GovernanceActionsCmdReadFileError
          . newExceptT
          $ readVerificationKeyOrHashOrFile AsStakeKey stake
      return $ StakeCredentialByKey hash
    AnyStakePoolKey stake -> do
      StakePoolKeyHash t <-
        firstExceptT GovernanceActionsCmdReadFileError
          . newExceptT
          $ readVerificationKeyOrHashOrFile AsStakePoolKey stake
      -- TODO: Conway era - don't use coerceKeyRole
      return . StakeCredentialByKey $ StakeKeyHash $ coerceKeyRole t
