{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.CLI.EraBased.Run.Governance.Actions
  ( runGovernanceActionCmds
  , GovernanceActionsError(..)
  ) where

import           Cardano.Api
import           Cardano.Api.Ledger (coerceKeyRole)
import qualified Cardano.Api.Ledger as Ledger
import           Cardano.Api.Shelley

import           Cardano.CLI.EraBased.Commands.Governance.Actions
import qualified Cardano.CLI.EraBased.Commands.Governance.Actions as Cmd
import           Cardano.CLI.Read
import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Errors.GovernanceActionsError
import           Cardano.CLI.Types.Key

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
  GovernanceActionCreateConstitutionCmd eon newConstitution ->
    runGovernanceActionCreateConstitutionCmd eon newConstitution

  GovernanceActionProtocolParametersUpdateCmd args ->
    runGovernanceActionCreateProtocolParametersUpdateCmd args

  GovernanceActionTreasuryWithdrawalCmd eon treasuryWithdrawal ->
    runGovernanceActionTreasuryWithdrawalCmd eon treasuryWithdrawal

  GoveranceActionUpdateCommitteeCmd eon newCommittee ->
    runGovernanceActionCreateNewCommitteeCmd eon newCommittee

  GovernanceActionCreateNoConfidenceCmd eon noConfidence ->
    runGovernanceActionCreateNoConfidenceCmd eon noConfidence

  GovernanceActionInfoCmd eon newInfo ->
    runGovernanceActionInfoCmd eon newInfo

runGovernanceActionInfoCmd
  :: ConwayEraOnwards era
  -> GovernanceActionInfoCmdArgs era
  -> ExceptT GovernanceActionsError IO ()
runGovernanceActionInfoCmd cOn (GovernanceActionInfoCmdArgs network deposit returnAddr proposalUrl proposalHashSource  outFp) = do
  returnKeyHash <- readStakeKeyHash returnAddr

  proposalHash <-
    proposalHashSourceToHash proposalHashSource
      & firstExceptT GovernanceActionsCmdProposalError

  let proposalAnchor = Ledger.Anchor
        { Ledger.anchorUrl = unProposalUrl proposalUrl
        , Ledger.anchorDataHash = proposalHash
        }

  let sbe = conwayEraOnwardsToShelleyBasedEra cOn
      govAction = InfoAct
      proposalProcedure = createProposalProcedure sbe network deposit returnKeyHash govAction proposalAnchor

  firstExceptT GovernanceActionsCmdWriteFileError . newExceptT
    $ conwayEraOnwardsConstraints cOn
    $ writeFileTextEnvelope outFp Nothing proposalProcedure

-- TODO: Conway era - update with new ledger types from cardano-ledger-conway-1.7.0.0
runGovernanceActionCreateNoConfidenceCmd
  :: ConwayEraOnwards era
  -> GovernanceActionCreateNoConfidenceCmdArgs era
  -> ExceptT GovernanceActionsError IO ()
runGovernanceActionCreateNoConfidenceCmd cOn (GovernanceActionCreateNoConfidenceCmdArgs network deposit returnAddr proposalUrl proposalHashSource txid ind outFp) = do
  returnKeyHash <- readStakeKeyHash returnAddr

  proposalHash <-
    proposalHashSourceToHash proposalHashSource
      & firstExceptT GovernanceActionsCmdProposalError

  let proposalAnchor = Ledger.Anchor
        { Ledger.anchorUrl = unProposalUrl proposalUrl
        , Ledger.anchorDataHash = proposalHash
        }

  let sbe = conwayEraOnwardsToShelleyBasedEra cOn
      previousGovernanceAction = MotionOfNoConfidence . Ledger.SJust $ createPreviousGovernanceActionId txid ind
      proposalProcedure = createProposalProcedure sbe network deposit returnKeyHash previousGovernanceAction proposalAnchor

  firstExceptT GovernanceActionsCmdWriteFileError . newExceptT
    $ conwayEraOnwardsConstraints cOn
    $ writeFileTextEnvelope outFp Nothing proposalProcedure

runGovernanceActionCreateConstitutionCmd :: ()
  => ConwayEraOnwards era
  -> GovernanceActionCreateConstitutionCmdArgs era
  -> ExceptT GovernanceActionsError IO ()
runGovernanceActionCreateConstitutionCmd cOn (GovernanceActionCreateConstitutionCmdArgs network deposit anyStake mPrevGovActId proposalUrl proposalHashSource constitutionUrl constitutionHashSource outFp) = do

  stakeKeyHash <- readStakeKeyHash anyStake

  proposalHash <-
    proposalHashSourceToHash proposalHashSource
      & firstExceptT GovernanceActionsCmdProposalError

  let proposalAnchor = Ledger.Anchor
        { Ledger.anchorUrl = unProposalUrl proposalUrl
        , Ledger.anchorDataHash = proposalHash
        }

  constitutionHash <-
    constitutionHashSourceToHash constitutionHashSource
      & firstExceptT GovernanceActionsCmdConstitutionError

  let prevGovActId = Ledger.maybeToStrictMaybe $ uncurry createPreviousGovernanceActionId <$> mPrevGovActId
      constitutionAnchor = Ledger.Anchor
        { Ledger.anchorUrl = unConstitutionUrl constitutionUrl
        , Ledger.anchorDataHash = constitutionHash
        }
      govAct = ProposeNewConstitution prevGovActId constitutionAnchor
      sbe = conwayEraOnwardsToShelleyBasedEra cOn
      proposalProcedure = createProposalProcedure sbe network deposit stakeKeyHash govAct proposalAnchor

  firstExceptT GovernanceActionsCmdWriteFileError . newExceptT
    $ conwayEraOnwardsConstraints cOn
    $ writeFileTextEnvelope outFp Nothing proposalProcedure

-- TODO: Conway era - After ledger bump update this function
-- with the new ledger types
runGovernanceActionCreateNewCommitteeCmd
  :: ConwayEraOnwards era
  -> GoveranceActionUpdateCommitteeCmdArgs era
  -> ExceptT GovernanceActionsError IO ()
runGovernanceActionCreateNewCommitteeCmd cOn (GoveranceActionUpdateCommitteeCmdArgs network deposit retAddr proposalUrl proposalHashSource old new q prevActId oFp) = do
  let sbe = conwayEraOnwardsToShelleyBasedEra cOn -- TODO: Conway era - update vote creation related function to take ConwayEraOnwards
      govActIdentifier = Ledger.maybeToStrictMaybe $ uncurry createPreviousGovernanceActionId <$> prevActId
      quorumRational = toRational q

  proposalHash <-
    proposalHashSourceToHash proposalHashSource
      & firstExceptT GovernanceActionsCmdProposalError

  let proposalAnchor = Ledger.Anchor
        { Ledger.anchorUrl = unProposalUrl proposalUrl
        , Ledger.anchorDataHash = proposalHash
        }

  oldCommitteeKeyHashes <- forM old $ \vkeyOrHashOrTextFile ->
    lift (readVerificationKeyOrHashOrTextEnvFile AsCommitteeColdKey vkeyOrHashOrTextFile)
      & onLeft (left . GovernanceActionsCmdReadFileError)

  newCommitteeKeyHashes <- forM new $ \(vkeyOrHashOrTextFile, expEpoch) -> do
    kh <- lift (readVerificationKeyOrHashOrTextEnvFile AsCommitteeColdKey vkeyOrHashOrTextFile)
      & onLeft (left . GovernanceActionsCmdReadFileError)
    pure (kh, expEpoch)

  returnKeyHash <- readStakeKeyHash retAddr

  let proposeNewCommittee = ProposeNewCommittee
                              govActIdentifier
                              oldCommitteeKeyHashes
                              (Map.fromList newCommitteeKeyHashes)
                              quorumRational
      proposal = createProposalProcedure sbe network deposit returnKeyHash proposeNewCommittee proposalAnchor

  firstExceptT GovernanceActionsCmdWriteFileError . newExceptT
    $ conwayEraOnwardsConstraints cOn
    $ writeFileTextEnvelope oFp Nothing proposal

runGovernanceActionCreateProtocolParametersUpdateCmd :: ()
  => Cmd.GovernanceActionProtocolParametersUpdateCmdArgs era
  -> ExceptT GovernanceActionsError IO ()
runGovernanceActionCreateProtocolParametersUpdateCmd
    Cmd.GovernanceActionProtocolParametersUpdateCmdArgs
      { Cmd.eon
      , Cmd.epochNo = expEpoch
      , Cmd.genesisVkeyFiles
      , Cmd.pparamsUpdate = pparamsUpdate
      , Cmd.outFile
      } = do
  let sbe = conwayEraOnwardsToShelleyBasedEra eon

  genVKeys <- sequence
    [ firstExceptT GovernanceActionsCmdReadTextEnvelopeFileError . newExceptT
        $ readFileTextEnvelope (AsVerificationKey AsGenesisKey) vkeyFile
    | vkeyFile <- genesisVkeyFiles
    ]

  let updateProtocolParams = createEraBasedProtocolParamUpdate sbe pparamsUpdate
      apiUpdateProtocolParamsType = fromLedgerPParamsUpdate sbe updateProtocolParams
      genKeyHashes = fmap verificationKeyHash genVKeys
      -- TODO: Update EraBasedProtocolParametersUpdate to require genesis delegate keys
      -- depending on the era
      upProp = makeShelleyUpdateProposal apiUpdateProtocolParamsType genKeyHashes expEpoch

  firstExceptT GovernanceActionsCmdWriteFileError . newExceptT
    $ writeLazyByteStringFile outFile $ textEnvelopeToJSON Nothing upProp

readStakeKeyHash :: AnyStakeIdentifier -> ExceptT GovernanceActionsError IO (Hash StakeKey)
readStakeKeyHash anyStake =
  case anyStake of
    AnyStakeKey stake ->
      firstExceptT GovernanceActionsCmdReadFileError
        . newExceptT $ readVerificationKeyOrHashOrFile AsStakeKey stake

    AnyStakePoolKey stake -> do
      StakePoolKeyHash t <- firstExceptT GovernanceActionsCmdReadFileError
                              . newExceptT $ readVerificationKeyOrHashOrFile AsStakePoolKey stake
      return $ StakeKeyHash $ coerceKeyRole t

runGovernanceActionTreasuryWithdrawalCmd
  :: ConwayEraOnwards era
  -> GovernanceActionTreasuryWithdrawalCmdArgs era
  -> ExceptT GovernanceActionsError IO ()
runGovernanceActionTreasuryWithdrawalCmd cOn (GovernanceActionTreasuryWithdrawalCmdArgs network deposit returnAddr proposalUrl proposalHashSource treasuryWithdrawal outFp) = do

  proposalHash <-
    proposalHashSourceToHash proposalHashSource
      & firstExceptT GovernanceActionsCmdProposalError

  let proposalAnchor = Ledger.Anchor
        { Ledger.anchorUrl = unProposalUrl proposalUrl
        , Ledger.anchorDataHash = proposalHash
        }

  returnKeyHash <- readStakeKeyHash returnAddr

  withdrawals <- sequence
    [ (network,,ll) <$> stakeIdentifiertoCredential stakeIdentifier
    | (stakeIdentifier,ll) <- treasuryWithdrawal
    ]

  let sbe = conwayEraOnwardsToShelleyBasedEra cOn
      treasuryWithdrawals = TreasuryWithdrawal withdrawals
      proposal = createProposalProcedure sbe network deposit returnKeyHash treasuryWithdrawals proposalAnchor

  firstExceptT GovernanceActionsCmdWriteFileError . newExceptT
    $ conwayEraOnwardsConstraints cOn
    $ writeFileTextEnvelope outFp Nothing proposal

stakeIdentifiertoCredential :: AnyStakeIdentifier -> ExceptT GovernanceActionsError IO StakeCredential
stakeIdentifiertoCredential anyStake =
  case anyStake of
    AnyStakeKey stake -> do
      hash <- firstExceptT GovernanceActionsCmdReadFileError
                . newExceptT $ readVerificationKeyOrHashOrFile AsStakeKey stake
      return $ StakeCredentialByKey hash
    AnyStakePoolKey stake -> do
      StakePoolKeyHash t <- firstExceptT GovernanceActionsCmdReadFileError
                              . newExceptT $ readVerificationKeyOrHashOrFile AsStakePoolKey stake
      -- TODO: Conway era - don't use coerceKeyRole
      return . StakeCredentialByKey $ StakeKeyHash $ coerceKeyRole t
