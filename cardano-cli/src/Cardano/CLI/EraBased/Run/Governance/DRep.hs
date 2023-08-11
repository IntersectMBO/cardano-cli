{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.CLI.EraBased.Run.Governance.DRep
  ( GovernanceDRepError(..)

  , runGovernanceDRepCmds
  ) where

import           Cardano.Api
import qualified Cardano.Api.Ledger as Ledger
import           Cardano.Api.Shelley

import           Cardano.CLI.Byron.Commands (VerificationKeyFile)
import           Cardano.CLI.EraBased.Commands.Governance.DRep (GovernanceDRepCmds (..))
import           Cardano.CLI.Run.Legacy.StakeAddress
import           Cardano.CLI.Types.Key
import           Cardano.CLI.Types.Legacy (SigningKeyFile)

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra
import           Data.Function


data GovernanceDRepError
  -- delegation
  = GovernanceDRepDelegReadError !(FileError InputDecodeError)
  | GovernanceDRepCredentialError !ShelleyStakeAddressCmdError -- TODO: Refactor. We shouldn't be using legacy error types
  | GovernanceDRepCertificateWriteFileError !(FileError ())
  | GovernanceDRepReadError !(FileError InputDecodeError)
  | GovernanceDRepDelegationGenericError -- TODO Delete and replace with more specific errors
  -- registration
  | GovernanceDRepRegistReadError !(FileError InputDecodeError)
  | GovernanceDRepRegistWriteFileError !(FileError ())
  | GovernanceDRepRegistStakeCredReadError !ShelleyStakeAddressCmdError -- TODO: Conway era - don't use legacy error type
  | GovernanceDRepRegistStakeError StakeAddressRegistrationError
  -- key-gen
  | GovernanceDRepKeyGenWriteFileError !(FileError ())


runGovernanceDRepCmds
  :: GovernanceDRepCmds era
  -> ExceptT GovernanceDRepError IO ()
runGovernanceDRepCmds = \case
  GovernanceDRepDelegationCertificate stakeIdentifier delegationTarget outFp ->
    runGovernanceDRepDelegationCertificate stakeIdentifier delegationTarget outFp
  GovernanceDRepRegistrationCertificate regTarget outFp ->
    runGovernanceDRepRegistrationCertificate regTarget outFp
  GovernanceDRepKeyGen vrf sgn ->
    runGovernanceDRepKeyGen vrf sgn

--------------------------------------------------------------------------------

-- Delegation Certificate related

runGovernanceDRepDelegationCertificate
  :: StakeIdentifier
  -> AnyDelegationTarget
  -> File () Out
  -> ExceptT GovernanceDRepError IO ()
runGovernanceDRepDelegationCertificate stakeIdentifier delegationTarget outFp = do
  stakeCred <-
    getStakeCredentialFromIdentifier stakeIdentifier
      & firstExceptT GovernanceDRepCredentialError

  case delegationTarget of
    ShelleyToBabbageDelegTarget sTob stakePool -> do
      poolId <- lift (readVerificationKeyOrHashOrFile AsStakePoolKey stakePool)
                  & onLeft (left . GovernanceDRepDelegReadError)
      let req = StakeDelegationRequirementsPreConway sTob stakeCred poolId
          delegCert = makeStakeAddressDelegationCertificate req
          description = Just @TextEnvelopeDescr "Stake Address Delegation Certificate"
      firstExceptT GovernanceDRepCertificateWriteFileError
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
      firstExceptT GovernanceDRepCertificateWriteFileError
        . newExceptT
        $ writeLazyByteStringFile outFp
        $ conwayEraOnwardsConstraints cOnwards
        $ textEnvelopeToJSON description delegCert

toLedgerDelegatee
  :: StakeTarget era
  -> ExceptT GovernanceDRepError IO (Ledger.Delegatee (Ledger.EraCrypto (ShelleyLedgerEra era)))
toLedgerDelegatee t =
  case t of
    TargetStakePool cOnwards keyOrHashOrFile -> do
      StakePoolKeyHash kHash
        <- lift (readVerificationKeyOrHashOrFile AsStakePoolKey keyOrHashOrFile)
             & onLeft (left . GovernanceDRepDelegReadError)
      right $ Ledger.DelegStake $ conwayEraOnwardsConstraints cOnwards kHash

    TargetVotingDrep cOnwards keyOrHashOrFile -> do
      DRepKeyHash drepKeyHash <- firstExceptT GovernanceDRepReadError
                                   . newExceptT
                                   $ readVerificationKeyOrHashOrTextEnvFile AsDRepKey keyOrHashOrFile
      let drepCred = Ledger.DRepCredential $ Ledger.KeyHashObj drepKeyHash
      right $ Ledger.DelegVote $ conwayEraOnwardsConstraints cOnwards drepCred

    TargetVotingDrepAndStakePool cOnwards drepKeyOrHashOrFile  poolKeyOrHashOrFile -> do
      StakePoolKeyHash kHash
        <- lift (readVerificationKeyOrHashOrFile AsStakePoolKey poolKeyOrHashOrFile)
             & onLeft (left . GovernanceDRepDelegReadError)
      DRepKeyHash drepKeyHash
        <- firstExceptT GovernanceDRepReadError
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

runGovernanceDRepRegistrationCertificate
  :: AnyRegistrationTarget
  -> File () Out
  -> ExceptT GovernanceDRepError IO ()
runGovernanceDRepRegistrationCertificate anyReg outfp =
  case anyReg of
    ShelleyToBabbageStakePoolRegTarget stoB regReqs -> do
      -- Pool verification key
      stakePoolVerKey <- firstExceptT GovernanceDRepRegistReadError
        . newExceptT
        $ readVerificationKeyOrFile AsStakePoolKey $ sprStakePoolKey regReqs
      let stakePoolId' = verificationKeyHash stakePoolVerKey

      -- VRF verification key
      vrfVerKey <- firstExceptT GovernanceDRepRegistReadError
        . newExceptT
        $ readVerificationKeyOrFile AsVrfKey $ sprVrfKey regReqs
      let vrfKeyHash' = verificationKeyHash vrfVerKey

      -- Pool reward account
      rwdStakeVerKey <- firstExceptT GovernanceDRepRegistReadError
        . newExceptT
        $ readVerificationKeyOrFile AsStakeKey $ sprRewardAccountKey regReqs
      let stakeCred = StakeCredentialByKey (verificationKeyHash rwdStakeVerKey)
          rewardAccountAddr = makeStakeAddress (sprNetworkId regReqs) stakeCred

      -- Pool owner(s)
      sPoolOwnerVkeys <-
        mapM
          (firstExceptT GovernanceDRepRegistReadError
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
      firstExceptT GovernanceDRepRegistWriteFileError
        . newExceptT
        . writeLazyByteStringFile outfp
        $ shelleyToBabbageEraConstraints stoB
        $ textEnvelopeToJSON description registrationCert

    ShelleyToBabbageStakeKeyRegTarget sToB stakeIdentifier -> do
      stakeCred <- firstExceptT GovernanceDRepRegistStakeCredReadError
                     $ getStakeCredentialFromIdentifier stakeIdentifier
      let req = StakeAddrRegistrationPreConway sToB stakeCred
          registrationCert = makeStakeAddressRegistrationCertificate req
          description = Just @TextEnvelopeDescr "Stake Key Registration Certificate"
      firstExceptT GovernanceDRepRegistWriteFileError
        . newExceptT
        . writeLazyByteStringFile outfp
        $ shelleyToBabbageEraConstraints sToB
        $ textEnvelopeToJSON description registrationCert

    ConwayOnwardRegTarget _ regTarget ->
      case regTarget of
        RegisterStakePool cOnwards regReqs -> do
          -- Pool verification key
          stakePoolVerKey <- firstExceptT GovernanceDRepRegistReadError
            . newExceptT
            $ readVerificationKeyOrFile AsStakePoolKey $ sprStakePoolKey regReqs
          let stakePoolId' = verificationKeyHash stakePoolVerKey
          -- VRF verification key
          vrfVerKey <- firstExceptT GovernanceDRepRegistReadError
                         . newExceptT
                         $ readVerificationKeyOrFile AsVrfKey $ sprVrfKey regReqs
          let vrfKeyHash' = verificationKeyHash vrfVerKey
          -- Pool reward account
          rwdStakeVerKey <- firstExceptT GovernanceDRepRegistReadError
                              . newExceptT
                              $ readVerificationKeyOrFile AsStakeKey $ sprRewardAccountKey regReqs
          let stakeCred = StakeCredentialByKey (verificationKeyHash rwdStakeVerKey)
              rewardAccountAddr = makeStakeAddress (sprNetworkId regReqs) stakeCred
          -- Pool owner(s)
          sPoolOwnerVkeys <-
                mapM
                  (firstExceptT GovernanceDRepRegistReadError
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
          firstExceptT GovernanceDRepRegistWriteFileError
            . newExceptT
            . writeLazyByteStringFile outfp
            $ conwayEraOnwardsConstraints cOnwards
            $ textEnvelopeToJSON description registrationCert
        RegisterStakeKey cOnwards sIdentifier deposit -> do
          stakeCred <- firstExceptT GovernanceDRepRegistStakeCredReadError
                         $ getStakeCredentialFromIdentifier sIdentifier
          let req = StakeAddrRegistrationConway cOnwards deposit stakeCred
              registrationCert = makeStakeAddressRegistrationCertificate req
              description = Just @TextEnvelopeDescr "Stake Key Registration Certificate"
          firstExceptT GovernanceDRepRegistWriteFileError
            . newExceptT
            . writeLazyByteStringFile outfp
            $ conwayEraOnwardsConstraints cOnwards
            $ textEnvelopeToJSON description registrationCert
        RegisterDRep cOnwards drepVKey deposit -> do
          DRepKeyHash drepKeyHash <- firstExceptT GovernanceDRepRegistReadError
            . newExceptT
            $ readVerificationKeyOrHashOrFile AsDRepKey drepVKey
          let drepCred = Ledger.KeyHashObj $ conwayEraOnwardsConstraints cOnwards drepKeyHash
              votingCredential = VotingCredential drepCred
              req = DRepRegistrationRequirements cOnwards votingCredential deposit
              registrationCert = makeDrepRegistrationCertificate req
              description = Just @TextEnvelopeDescr "DRep Key Registration Certificate"

          firstExceptT GovernanceDRepRegistWriteFileError
            . newExceptT
            . writeLazyByteStringFile outfp
            $ conwayEraOnwardsConstraints cOnwards
            $ textEnvelopeToJSON description registrationCert

--------------------------------------------------------------------------------

-- Key generation related


runGovernanceDRepKeyGen
  :: VerificationKeyFile Out
  -> SigningKeyFile Out
  -> ExceptT GovernanceDRepError IO ()
runGovernanceDRepKeyGen vkeyPath skeyPath = firstExceptT GovernanceDRepKeyGenWriteFileError $ do
  skey <- liftIO $ generateSigningKey AsDRepKey
  let vkey = getVerificationKey skey
  newExceptT $ writeLazyByteStringFile skeyPath (textEnvelopeToJSON (Just skeyDesc) skey)
  newExceptT $ writeLazyByteStringFile vkeyPath (textEnvelopeToJSON (Just vkeyDesc) vkey)
  where
    skeyDesc :: TextEnvelopeDescr
    skeyDesc = "Delegate Representative Signing Key"
    vkeyDesc :: TextEnvelopeDescr
    vkeyDesc = "Delegate Representative Verification Key"
