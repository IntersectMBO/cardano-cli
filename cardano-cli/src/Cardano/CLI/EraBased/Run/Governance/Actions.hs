{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.CLI.EraBased.Run.Governance.Actions
  ( runGovernanceActionCmds
  , GovernanceActionsError(..)
  ) where

import           Cardano.Api
import           Cardano.Api.Ledger (HasKeyRole (coerceKeyRole))
import           Cardano.Api.Shelley

import           Cardano.CLI.EraBased.Commands.Governance.Actions
import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Key

import           Control.Monad.Except (ExceptT)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Except.Extra
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as Text
import           Data.Text.Encoding.Error


data GovernanceActionsError
  = GovernanceActionsCmdWriteFileError (FileError ())
  | GovernanceActionsCmdReadFileError (FileError InputDecodeError)
  | GovernanceActionsCmdReadTextEnvelopeFileError (FileError TextEnvelopeError)
  | GovernanceActionsCmdNonUtf8EncodedConstitution UnicodeException
  deriving Show

instance Error GovernanceActionsError where
  displayError = \case
    GovernanceActionsCmdWriteFileError e ->
      "Cannot write file: " <> displayError e
    GovernanceActionsCmdReadFileError e ->
      "Cannot read file: " <> displayError e
    GovernanceActionsCmdReadTextEnvelopeFileError e ->
      "Cannot read text envelope from file: " <> displayError e
    GovernanceActionsCmdNonUtf8EncodedConstitution e ->
      "Cannot read constitution: " <> show e

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
runGovernanceActionCreateNoConfidence cOn (EraBasedNoConfidence deposit returnAddr _txid _ind outFp) = do
  returnKeyHash <- readStakeKeyHash returnAddr
  let sbe = conwayEraOnwardsToShelleyBasedEra cOn
      proposal = createProposalProcedure sbe deposit returnKeyHash MotionOfNoConfidence

  firstExceptT GovernanceActionsCmdWriteFileError . newExceptT
    $ conwayEraOnwardsConstraints cOn
    $ writeFileTextEnvelope outFp Nothing proposal

runGovernanceActionCreateConstitution :: ()
  => ConwayEraOnwards era
  -> EraBasedNewConstitution
  -> ExceptT GovernanceActionsError IO ()
runGovernanceActionCreateConstitution cOn (EraBasedNewConstitution deposit anyStake constit outFp) = do

  stakeKeyHash <- readStakeKeyHash anyStake

  case constit of
    ConstitutionFromFile fp  -> do
      cBs <- liftIO $ BS.readFile $ unFile fp
      _utf8EncodedText <- firstExceptT GovernanceActionsCmdNonUtf8EncodedConstitution . hoistEither $ Text.decodeUtf8' cBs
      let govAct = ProposeNewConstitution cBs
          sbe = conwayEraOnwardsToShelleyBasedEra cOn
          proposal = createProposalProcedure sbe deposit stakeKeyHash govAct

      firstExceptT GovernanceActionsCmdWriteFileError . newExceptT
        $ conwayEraOnwardsConstraints cOn
        $ writeFileTextEnvelope outFp Nothing proposal

    ConstitutionFromText c -> do
      let constitBs = Text.encodeUtf8 c
          sbe = conwayEraOnwardsToShelleyBasedEra cOn
          govAct = ProposeNewConstitution constitBs
          proposal = createProposalProcedure sbe deposit stakeKeyHash govAct

      firstExceptT GovernanceActionsCmdWriteFileError . newExceptT
        $ conwayEraOnwardsConstraints cOn
        $ writeFileTextEnvelope outFp Nothing proposal

-- TODO: Conway era - After ledger bump update this function
-- with the new ledger types
runGovernanceActionCreateNewCommittee
  :: ConwayEraOnwards era
  -> EraBasedNewCommittee
  -> ExceptT GovernanceActionsError IO ()
runGovernanceActionCreateNewCommittee cOn (EraBasedNewCommittee deposit retAddr old new q prevActId oFp) = do
  let sbe = conwayEraOnwardsToShelleyBasedEra cOn -- TODO: Conway era - update vote creation related function to take ConwayEraOnwards
      _govActIdentifier = makeGoveranceActionId sbe prevActId
      quorumRational = toRational q

  _oldCommitteeKeyHashes <- mapM readStakeKeyHash old
  newCommitteeKeyHashes <- mapM (readStakeKeyHash . fst) new

  returnKeyHash <- readStakeKeyHash retAddr

  let proposeNewCommittee = ProposeNewCommittee newCommitteeKeyHashes quorumRational
      proposal = createProposalProcedure sbe deposit returnKeyHash proposeNewCommittee

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
runGovernanceActionTreasuryWithdrawal cOn (EraBasedTreasuryWithdrawal deposit returnAddr treasuryWithdrawal outFp) = do
  returnKeyHash <- readStakeKeyHash returnAddr
  withdrawals <- sequence [ (,ll) <$> stakeIdentifiertoCredential stakeIdentifier
                          | (stakeIdentifier,ll) <- treasuryWithdrawal
                          ]
  let sbe = conwayEraOnwardsToShelleyBasedEra cOn
      proposal = createProposalProcedure sbe deposit returnKeyHash (TreasuryWithdrawal withdrawals)

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
