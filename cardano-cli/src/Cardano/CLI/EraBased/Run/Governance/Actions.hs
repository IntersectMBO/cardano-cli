{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
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
import           Cardano.CLI.Read
import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Errors.GovernanceActionsError
import           Cardano.CLI.Types.Key
import qualified Cardano.Ledger.Conway.Governance as Ledger

import           Control.Monad.Except (ExceptT)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Except.Extra
import           Data.Function
import qualified Data.Map.Strict as Map

runGovernanceActionCmds :: ()
  => GovernanceActionCmds era
  -> ExceptT GovernanceActionsError IO ()
runGovernanceActionCmds = \case
  GovernanceActionCreateConstitutionCmd cOn newConstitution ->
    runGovernanceActionCreateConstitutionCmd cOn newConstitution

  GovernanceActionProtocolParametersUpdateCmd sbe eNo genKeys eraBasedProtocolParametersUpdate ofp ->
    runGovernanceActionCreateProtocolParametersUpdateCmd sbe eNo genKeys eraBasedProtocolParametersUpdate ofp

  GovernanceActionTreasuryWithdrawalCmd cOn treasuryWithdrawal ->
    runGovernanceActionTreasuryWithdrawalCmd cOn treasuryWithdrawal

  GoveranceActionCreateNewCommitteeCmd con newCommittee ->
    runGovernanceActionCreateNewCommitteeCmd con newCommittee

  GovernanceActionCreateNoConfidenceCmd cOn noConfidence ->
    runGovernanceActionCreateNoConfidenceCmd cOn noConfidence

  GoveranceActionInfoCmd cOn iFp oFp ->
    runGovernanceActionInfoCmd cOn iFp oFp

runGovernanceActionInfoCmd
  :: ConwayEraOnwards era
  -> File () In
  -> File () Out
  -> ExceptT GovernanceActionsError IO ()
runGovernanceActionInfoCmd _cOn _iFp _oFp =
  liftIO $ print @String "TODO: Conway era - implement runGovernanceActionInfoCmd - ledger currently provides a placeholder constructor"

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
  -> NewConstitutionCmd
  -> ExceptT GovernanceActionsError IO ()
runGovernanceActionCreateConstitutionCmd cOn (NewConstitutionCmd network deposit anyStake mPrevGovActId proposalUrl proposalHashSource constitutionUrl constitutionHashSource outFp) = do

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
  -> NewCommitteeCmd
  -> ExceptT GovernanceActionsError IO ()
runGovernanceActionCreateNewCommitteeCmd cOn (NewCommitteeCmd network deposit retAddr proposalUrl proposalHashSource old new q prevActId oFp) = do
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

  oldCommitteeKeyHashes <- mapM readStakeKeyHash old
  newCommitteeKeyHashes <- mapM (\(stakeKey, expEpoch) -> (,expEpoch) <$> readStakeKeyHash stakeKey) new

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
  => ShelleyBasedEra era
  -> EpochNo
  -> [VerificationKeyFile In]
  -- ^ Genesis verification keys
  -> EraBasedProtocolParametersUpdate era
  -> File () Out
  -> ExceptT GovernanceActionsError IO ()
runGovernanceActionCreateProtocolParametersUpdateCmd sbe expEpoch genesisVerKeys eraBasedPParams oFp = do
  genVKeys <- sequence
    [ firstExceptT GovernanceActionsCmdReadTextEnvelopeFileError . newExceptT
        $ readFileTextEnvelope (AsVerificationKey AsGenesisKey) vkeyFile
    | vkeyFile <- genesisVerKeys
    ]

  let updateProtocolParams = createEraBasedProtocolParamUpdate sbe eraBasedPParams
      apiUpdateProtocolParamsType = fromLedgerPParamsUpdate sbe updateProtocolParams
      genKeyHashes = fmap verificationKeyHash genVKeys
      -- TODO: Update EraBasedProtocolParametersUpdate to require genesis delegate keys
      -- depending on the era
      upProp = makeShelleyUpdateProposal apiUpdateProtocolParamsType genKeyHashes expEpoch

  firstExceptT GovernanceActionsCmdWriteFileError . newExceptT
    $ writeLazyByteStringFile oFp $ textEnvelopeToJSON Nothing upProp

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
  -> TreasuryWithdrawalCmd
  -> ExceptT GovernanceActionsError IO ()
runGovernanceActionTreasuryWithdrawalCmd cOn (TreasuryWithdrawalCmd network deposit returnAddr proposalUrl proposalHashSource treasuryWithdrawal outFp) = do

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
