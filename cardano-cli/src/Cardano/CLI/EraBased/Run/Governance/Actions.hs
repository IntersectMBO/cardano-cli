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
import qualified Data.Text.Encoding as Text

runGovernanceActionCmds :: ()
  => GovernanceActionCmds era
  -> ExceptT GovernanceActionsError IO ()
runGovernanceActionCmds = \case
  GovernanceActionCreateConstitution cOn newConstitution ->
    runGovernanceActionCreateConstitution cOn newConstitution

  GovernanceActionProtocolParametersUpdate sbe eNo genKeys eraBasedProtocolParametersUpdate ofp ->
    runGovernanceActionCreateProtocolParametersUpdate sbe eNo genKeys eraBasedProtocolParametersUpdate ofp

  GovernanceActionTreasuryWithdrawal cOn treasuryWithdrawal ->
    runGovernanceActionTreasuryWithdrawal cOn treasuryWithdrawal

  GoveranceActionCreateNewCommittee con newCommittee ->
    runGovernanceActionCreateNewCommittee con newCommittee

  GovernanceActionCreateNoConfidence cOn noConfidence ->
    runGovernanceActionCreateNoConfidence cOn noConfidence

  GoveranceActionInfo cOn iFp oFp ->
    runGovernanceActionInfo cOn iFp oFp

runGovernanceActionInfo
  :: ConwayEraOnwards era
  -> File () In
  -> File () Out
  -> ExceptT GovernanceActionsError IO ()
runGovernanceActionInfo _cOn _iFp _oFp =
  liftIO $ print @String "TODO: Conway era - implement runGovernanceActionInfo - ledger currently provides a placeholder constructor"

-- TODO: Conway era - update with new ledger types from cardano-ledger-conway-1.7.0.0
runGovernanceActionCreateNoConfidence
  :: ConwayEraOnwards era
  -> EraBasedNoConfidence
  -> ExceptT GovernanceActionsError IO ()
runGovernanceActionCreateNoConfidence cOn (EraBasedNoConfidence network deposit returnAddr propAnchor txid ind outFp) = do
  returnKeyHash <- readStakeKeyHash returnAddr
  let sbe = conwayEraOnwardsToShelleyBasedEra cOn
      proposal = createProposalProcedure
                   sbe
                   network
                   deposit
                   returnKeyHash
                   (MotionOfNoConfidence . Ledger.SJust $ createPreviousGovernanceActionId txid ind)
                   (uncurry createAnchor (fmap Text.encodeUtf8 propAnchor))

  firstExceptT GovernanceActionsCmdWriteFileError . newExceptT
    $ conwayEraOnwardsConstraints cOn
    $ writeFileTextEnvelope outFp Nothing proposal

runGovernanceActionCreateConstitution :: ()
  => ConwayEraOnwards era
  -> EraBasedNewConstitution
  -> ExceptT GovernanceActionsError IO ()
runGovernanceActionCreateConstitution cOn (EraBasedNewConstitution network deposit anyStake mPrevGovActId propAnchor constitutionUrl constitutionHashSource outFp) = do

  stakeKeyHash <- readStakeKeyHash anyStake

  constitutionHash <-
    constitutionHashSourceToHash constitutionHashSource
      & firstExceptT GovernanceActionsCmdConstitutionError

  let prevGovActId = Ledger.maybeToStrictMaybe $ uncurry createPreviousGovernanceActionId <$> mPrevGovActId
      constitutionAnchor = Ledger.Anchor
        { Ledger.anchorUrl = unConstitutionUrl constitutionUrl
        , Ledger.anchorDataHash = constitutionHash
        }
      proposalAnchor = uncurry createAnchor (fmap Text.encodeUtf8 propAnchor)
      govAct = ProposeNewConstitution prevGovActId constitutionAnchor
      sbe = conwayEraOnwardsToShelleyBasedEra cOn
      proposal = createProposalProcedure sbe network deposit stakeKeyHash govAct proposalAnchor

  firstExceptT GovernanceActionsCmdWriteFileError . newExceptT
    $ conwayEraOnwardsConstraints cOn
    $ writeFileTextEnvelope outFp Nothing proposal

-- TODO: Conway era - After ledger bump update this function
-- with the new ledger types
runGovernanceActionCreateNewCommittee
  :: ConwayEraOnwards era
  -> EraBasedNewCommittee
  -> ExceptT GovernanceActionsError IO ()
runGovernanceActionCreateNewCommittee cOn (EraBasedNewCommittee network deposit retAddr propAnchor old new q prevActId oFp) = do
  let sbe = conwayEraOnwardsToShelleyBasedEra cOn -- TODO: Conway era - update vote creation related function to take ConwayEraOnwards
      govActIdentifier = Ledger.maybeToStrictMaybe $ uncurry createPreviousGovernanceActionId <$> prevActId
      quorumRational = toRational q

  oldCommitteeKeyHashes <- mapM readStakeKeyHash old
  newCommitteeKeyHashes <- mapM (\(stakeKey, expEpoch) -> (,expEpoch) <$> readStakeKeyHash stakeKey) new

  returnKeyHash <- readStakeKeyHash retAddr

  let proposeNewCommittee = ProposeNewCommittee
                              govActIdentifier
                              oldCommitteeKeyHashes
                              (Map.fromList newCommitteeKeyHashes)
                              quorumRational
      proposal = createProposalProcedure
                   sbe
                   network
                   deposit
                   returnKeyHash
                   proposeNewCommittee
                   (uncurry createAnchor (fmap Text.encodeUtf8 propAnchor))

  firstExceptT GovernanceActionsCmdWriteFileError . newExceptT
    $ conwayEraOnwardsConstraints cOn
    $ writeFileTextEnvelope oFp Nothing proposal

runGovernanceActionCreateProtocolParametersUpdate :: ()
  => ShelleyBasedEra era
  -> EpochNo
  -> [VerificationKeyFile In]
  -- ^ Genesis verification keys
  -> EraBasedProtocolParametersUpdate era
  -> File () Out
  -> ExceptT GovernanceActionsError IO ()
runGovernanceActionCreateProtocolParametersUpdate sbe expEpoch genesisVerKeys eraBasedPParams oFp = do
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

runGovernanceActionTreasuryWithdrawal
  :: ConwayEraOnwards era
  -> EraBasedTreasuryWithdrawal
  -> ExceptT GovernanceActionsError IO ()
runGovernanceActionTreasuryWithdrawal cOn (EraBasedTreasuryWithdrawal network deposit returnAddr propAnchor treasuryWithdrawal outFp) = do
  returnKeyHash <- readStakeKeyHash returnAddr
  withdrawals <- sequence [ (network,,ll) <$> stakeIdentifiertoCredential stakeIdentifier
                           | (stakeIdentifier,ll) <- treasuryWithdrawal
                           ]
  let sbe = conwayEraOnwardsToShelleyBasedEra cOn
      proposal = createProposalProcedure
                   sbe
                   network
                   deposit
                   returnKeyHash
                   (TreasuryWithdrawal withdrawals)
                   (uncurry createAnchor (fmap Text.encodeUtf8 propAnchor))

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
