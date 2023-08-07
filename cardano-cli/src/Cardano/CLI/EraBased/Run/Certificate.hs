{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.CLI.EraBased.Run.Certificate
  ( EraBasedDelegationError(..)
  , EraBasedRegistrationError(..)

  , runGovernanceRegistrationCertificate
  , runGovernanceDelegationCertificate

  , getStakeCredentialFromVerifier
  , getStakeCredentialFromIdentifier
  , getStakeAddressFromVerifier
  ) where

import           Cardano.Api
import qualified Cardano.Api.Ledger as Ledger
import           Cardano.Api.Shelley

import           Cardano.CLI.EraBased.Errors.StakeAddress
import           Cardano.CLI.Read
import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Key

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra
import           Data.Function
import           GHC.Generics (Generic)

-- Delegation Certificate related

data EraBasedDelegationError
  = EraBasedDelegReadError !(FileError InputDecodeError)
  | EraBasedCredentialError !ShelleyStakeAddressCmdError -- TODO: Refactor. We shouldn't be using legacy error types
  | EraBasedCertificateWriteFileError !(FileError ())
  | EraBasedDRepReadError !(FileError InputDecodeError)
  deriving (Show, Generic)

instance Error EraBasedDelegationError where
  displayError = \case
    EraBasedDelegReadError e ->
      "Cannot read delegation target: " <> displayError e
    EraBasedCredentialError e ->
      "Cannot get stake credential: " <> displayError e
    EraBasedCertificateWriteFileError e ->
      "Cannot write certificate: " <> displayError e
    EraBasedDRepReadError e ->
      "Cannot read DRep key: " <> displayError e

runGovernanceDelegationCertificate
  :: StakeIdentifier
  -> AnyDelegationTarget
  -> File () Out
  -> ExceptT EraBasedDelegationError IO ()
runGovernanceDelegationCertificate stakeIdentifier delegationTarget outFp = do
  stakeCred <-
    getStakeCredentialFromIdentifier stakeIdentifier
      & firstExceptT EraBasedCredentialError

  case delegationTarget of
    ShelleyToBabbageDelegTarget sTob stakePool -> do
      poolId <- lift (readVerificationKeyOrHashOrFile AsStakePoolKey stakePool)
                  & onLeft (left . EraBasedDelegReadError)
      let req = StakeDelegationRequirementsPreConway sTob stakeCred poolId
          delegCert = makeStakeAddressDelegationCertificate req
          description = Just @TextEnvelopeDescr "Stake Address Delegation Certificate"
      firstExceptT EraBasedCertificateWriteFileError
        . newExceptT
        $ writeLazyByteStringFile outFp
        $ shelleyToBabbageEraConstraints sTob
        $ textEnvelopeToJSON description delegCert

    ConwayOnwardDelegTarget cOnwards target -> do
      delegatee <- toLedgerDelegatee target
      let req = StakeDelegationRequirementsConwayOnwards cOnwards stakeCred delegatee
          delegCert = makeStakeAddressDelegationCertificate req
          -- TODO: Conway era - update description to say if its delegating voting stake or "regular" stake
          description = Just @TextEnvelopeDescr "Stake Address Delegation Certificate"
      firstExceptT EraBasedCertificateWriteFileError
        . newExceptT
        $ writeLazyByteStringFile outFp
        $ conwayEraOnwardsConstraints cOnwards
        $ textEnvelopeToJSON description delegCert

toLedgerDelegatee
  :: StakeTarget era
  -> ExceptT EraBasedDelegationError IO (Ledger.Delegatee (Ledger.EraCrypto (ShelleyLedgerEra era)))
toLedgerDelegatee t =
  case t of
    TargetStakePool cOnwards keyOrHashOrFile -> do
      StakePoolKeyHash kHash
        <- lift (readVerificationKeyOrHashOrFile AsStakePoolKey keyOrHashOrFile)
             & onLeft (left . EraBasedDelegReadError)
      right $ Ledger.DelegStake $ conwayEraOnwardsConstraints cOnwards kHash

    TargetVotingDrep cOnwards keyOrHashOrFile -> do
      DRepKeyHash drepKeyHash <- firstExceptT EraBasedDRepReadError
                                   . newExceptT
                                   $ readVerificationKeyOrHashOrTextEnvFile AsDRepKey keyOrHashOrFile
      let drepCred = Ledger.DRepCredential $ Ledger.KeyHashObj drepKeyHash
      right $ Ledger.DelegVote $ conwayEraOnwardsConstraints cOnwards drepCred

    TargetVotingDrepAndStakePool cOnwards drepKeyOrHashOrFile  poolKeyOrHashOrFile -> do
      StakePoolKeyHash kHash
        <- lift (readVerificationKeyOrHashOrFile AsStakePoolKey poolKeyOrHashOrFile)
             & onLeft (left . EraBasedDelegReadError)
      DRepKeyHash drepKeyHash
        <- firstExceptT EraBasedDRepReadError
             . newExceptT
             $ readVerificationKeyOrHashOrTextEnvFile AsDRepKey drepKeyOrHashOrFile
      let drepCred = Ledger.DRepCredential $ Ledger.KeyHashObj drepKeyHash
      right $ Ledger.DelegStakeVote
                (conwayEraOnwardsConstraints cOnwards kHash)
                (conwayEraOnwardsConstraints cOnwards drepCred)

    TargetAlwaysAbstain{}-> right $ Ledger.DelegVote Ledger.DRepAlwaysAbstain

    TargetAlwaysNoConfidence{} -> right $ Ledger.DelegVote Ledger.DRepAlwaysNoConfidence

    TargetVotingDRepScriptHash _cOn (ScriptHash _scriptHash) ->
      error "TODO: Conway era - DRepScriptHash not exposed by ledger yet"
      -- right $ Ledger.DelegVote $ Ledger.DRepScriptHash scriptHash

--------------------------------------------------------------------------------

-- Registration Certificate related


data EraBasedRegistrationError
  = EraBasedRegistReadError !(FileError InputDecodeError)
  | EraBasedRegistWriteFileError !(FileError ())
  | EraBasedRegistStakeCredReadError !ShelleyStakeAddressCmdError -- TODO: Conway era - don't use legacy error type
  | EraBasedRegistStakeError !StakeAddressRegistrationError
  deriving Show

instance Error EraBasedRegistrationError where
  displayError = \case
    EraBasedRegistReadError e -> "Cannot read registration certificate: " <> displayError e
    EraBasedRegistWriteFileError e -> "Cannot write registration certificate: " <> displayError e
    EraBasedRegistStakeCredReadError e -> "Cannot read stake credential: " <> displayError e
    EraBasedRegistStakeError e -> "Stake address registation error: " <> displayError e

runGovernanceRegistrationCertificate
  :: AnyRegistrationTarget
  -> File () Out
  -> ExceptT EraBasedRegistrationError IO ()
runGovernanceRegistrationCertificate anyReg outfp =
  case anyReg of
    ShelleyToBabbageStakePoolRegTarget stoB regReqs -> do
      -- Pool verification key
      stakePoolVerKey <- firstExceptT EraBasedRegistReadError
        . newExceptT
        $ readVerificationKeyOrFile AsStakePoolKey $ sprStakePoolKey regReqs
      let stakePoolId' = verificationKeyHash stakePoolVerKey

      -- VRF verification key
      vrfVerKey <- firstExceptT EraBasedRegistReadError
        . newExceptT
        $ readVerificationKeyOrFile AsVrfKey $ sprVrfKey regReqs
      let vrfKeyHash' = verificationKeyHash vrfVerKey

      -- Pool reward account
      rwdStakeVerKey <- firstExceptT EraBasedRegistReadError
        . newExceptT
        $ readVerificationKeyOrFile AsStakeKey $ sprRewardAccountKey regReqs
      let stakeCred = StakeCredentialByKey (verificationKeyHash rwdStakeVerKey)
          rewardAccountAddr = makeStakeAddress (sprNetworkId regReqs) stakeCred

      -- Pool owner(s)
      sPoolOwnerVkeys <-
        mapM
          (firstExceptT EraBasedRegistReadError
            . newExceptT
            . readVerificationKeyOrFile AsStakeKey
          )
          (spoPoolOwnerKeys regReqs)
      let stakePoolOwners' = map verificationKeyHash sPoolOwnerVkeys

      let stakePoolParams =
            StakePoolParameters
              { stakePoolId = stakePoolId'
              , stakePoolVRF = vrfKeyHash'
              , stakePoolCost = sprPoolCost regReqs
              , stakePoolMargin = sprPoolMargin regReqs
              , stakePoolRewardAccount = rewardAccountAddr
              , stakePoolPledge = sprPoolPledge regReqs
              , stakePoolOwners = stakePoolOwners'
              , stakePoolRelays = sprRelays regReqs
              , stakePoolMetadata = sprMetadata regReqs
              }

      let ledgerStakePoolParams = toShelleyPoolParams stakePoolParams
          req = StakePoolRegistrationRequirementsPreConway stoB $ shelleyToBabbageEraConstraints stoB ledgerStakePoolParams
          registrationCert = makeStakePoolRegistrationCertificate req
          description = Just @TextEnvelopeDescr "Stake Pool Registration Certificate"
      firstExceptT EraBasedRegistWriteFileError
        . newExceptT
        . writeLazyByteStringFile outfp
        $ shelleyToBabbageEraConstraints stoB
        $ textEnvelopeToJSON description registrationCert

    ShelleyToBabbageStakeKeyRegTarget sToB stakeIdentifier -> do
      stakeCred <- firstExceptT EraBasedRegistStakeCredReadError
                     $ getStakeCredentialFromIdentifier stakeIdentifier
      let req = StakeAddrRegistrationPreConway sToB stakeCred
          registrationCert = makeStakeAddressRegistrationCertificate req
          description = Just @TextEnvelopeDescr "Stake Key Registration Certificate"
      firstExceptT EraBasedRegistWriteFileError
        . newExceptT
        . writeLazyByteStringFile outfp
        $ shelleyToBabbageEraConstraints sToB
        $ textEnvelopeToJSON description registrationCert

    ConwayOnwardRegTarget _ regTarget ->
      case regTarget of
        RegisterStakePool cOnwards regReqs -> do
          -- Pool verification key
          stakePoolVerKey <- firstExceptT EraBasedRegistReadError
            . newExceptT
            $ readVerificationKeyOrFile AsStakePoolKey $ sprStakePoolKey regReqs
          let stakePoolId' = verificationKeyHash stakePoolVerKey
          -- VRF verification key
          vrfVerKey <- firstExceptT EraBasedRegistReadError
                         . newExceptT
                         $ readVerificationKeyOrFile AsVrfKey $ sprVrfKey regReqs
          let vrfKeyHash' = verificationKeyHash vrfVerKey
          -- Pool reward account
          rwdStakeVerKey <- firstExceptT EraBasedRegistReadError
                              . newExceptT
                              $ readVerificationKeyOrFile AsStakeKey $ sprRewardAccountKey regReqs
          let stakeCred = StakeCredentialByKey (verificationKeyHash rwdStakeVerKey)
              rewardAccountAddr = makeStakeAddress (sprNetworkId regReqs) stakeCred
          -- Pool owner(s)
          sPoolOwnerVkeys <-
                mapM
                  (firstExceptT EraBasedRegistReadError
                    . newExceptT
                    . readVerificationKeyOrFile AsStakeKey
                  )
                  (spoPoolOwnerKeys regReqs)
          let stakePoolOwners' = map verificationKeyHash sPoolOwnerVkeys

          let stakePoolParams =
                StakePoolParameters
                  { stakePoolId = stakePoolId'
                  , stakePoolVRF = vrfKeyHash'
                  , stakePoolCost = sprPoolCost regReqs
                  , stakePoolMargin = sprPoolMargin regReqs
                  , stakePoolRewardAccount = rewardAccountAddr
                  , stakePoolPledge = sprPoolPledge regReqs
                  , stakePoolOwners = stakePoolOwners'
                  , stakePoolRelays = sprRelays regReqs
                  , stakePoolMetadata = sprMetadata regReqs
                  }

          let ledgerStakePoolParams = toShelleyPoolParams stakePoolParams
              req = StakePoolRegistrationRequirementsConwayOnwards cOnwards
                      $ conwayEraOnwardsConstraints cOnwards ledgerStakePoolParams
              registrationCert = makeStakePoolRegistrationCertificate req
              description = Just @TextEnvelopeDescr "Stake Pool Registration Certificate"
          firstExceptT EraBasedRegistWriteFileError
            . newExceptT
            . writeLazyByteStringFile outfp
            $ conwayEraOnwardsConstraints cOnwards
            $ textEnvelopeToJSON description registrationCert
        RegisterStakeKey cOnwards sIdentifier deposit -> do
          stakeCred <- firstExceptT EraBasedRegistStakeCredReadError
                         $ getStakeCredentialFromIdentifier sIdentifier
          let req = StakeAddrRegistrationConway cOnwards deposit stakeCred
              registrationCert = makeStakeAddressRegistrationCertificate req
              description = Just @TextEnvelopeDescr "Stake Key Registration Certificate"
          firstExceptT EraBasedRegistWriteFileError
            . newExceptT
            . writeLazyByteStringFile outfp
            $ conwayEraOnwardsConstraints cOnwards
            $ textEnvelopeToJSON description registrationCert
        RegisterDRep cOnwards drepVKey deposit -> do
          DRepKeyHash drepKeyHash <- firstExceptT EraBasedRegistReadError
            . newExceptT
            $ readVerificationKeyOrHashOrFile AsDRepKey drepVKey
          let drepCred = Ledger.KeyHashObj $ conwayEraOnwardsConstraints cOnwards drepKeyHash
              votingCredential = VotingCredential drepCred
              req = DRepRegistrationRequirements cOnwards votingCredential deposit
              registrationCert = makeDrepRegistrationCertificate req
              description = Just @TextEnvelopeDescr "DRep Key Registration Certificate"

          firstExceptT EraBasedRegistWriteFileError
            . newExceptT
            . writeLazyByteStringFile outfp
            $ conwayEraOnwardsConstraints cOnwards
            $ textEnvelopeToJSON description registrationCert

--------------------------------------------------------------------------------

getStakeCredentialFromVerifier
  :: StakeVerifier
  -> ExceptT ShelleyStakeAddressCmdError IO StakeCredential
getStakeCredentialFromVerifier = \case
  StakeVerifierScriptFile (ScriptFile sFile) -> do
    ScriptInAnyLang _ script <-
      firstExceptT ShelleyStakeAddressCmdReadScriptFileError $
        readFileScriptInAnyLang sFile
    pure $ StakeCredentialByScript $ hashScript script

  StakeVerifierKey stakeVerKeyOrFile -> do
    stakeVerKey <-
      firstExceptT ShelleyStakeAddressCmdReadKeyFileError
        . newExceptT
        $ readVerificationKeyOrFile AsStakeKey stakeVerKeyOrFile
    pure $ StakeCredentialByKey $ verificationKeyHash stakeVerKey

getStakeCredentialFromIdentifier
  :: StakeIdentifier
  -> ExceptT ShelleyStakeAddressCmdError IO StakeCredential
getStakeCredentialFromIdentifier = \case
  StakeIdentifierAddress stakeAddr -> pure $ stakeAddressCredential stakeAddr
  StakeIdentifierVerifier stakeVerifier -> getStakeCredentialFromVerifier stakeVerifier

getStakeAddressFromVerifier
  :: NetworkId
  -> StakeVerifier
  -> ExceptT ShelleyStakeAddressCmdError IO StakeAddress
getStakeAddressFromVerifier networkId stakeVerifier =
  makeStakeAddress networkId <$> getStakeCredentialFromVerifier stakeVerifier
