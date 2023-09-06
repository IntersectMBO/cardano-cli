{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- HLINT ignore "Use let" -}

module Cardano.CLI.EraBased.Run.Governance.DRep
  ( runGovernanceDRepCmds
  ) where

import           Cardano.Api
import qualified Cardano.Api.Ledger as Ledger
import           Cardano.Api.Shelley

import           Cardano.CLI.EraBased.Commands.Governance.DRep
import           Cardano.CLI.EraBased.Run.Governance
import           Cardano.CLI.Read
import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Errors.CmdError
import           Cardano.CLI.Types.Errors.DelegationError
import           Cardano.CLI.Types.Errors.GovernanceCmdError
import           Cardano.CLI.Types.Errors.RegistrationError
import           Cardano.CLI.Types.Key

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Except.Extra
import           Data.Function
import qualified Data.Text.Encoding as Text

runGovernanceDRepCmds :: ()
  => GovernanceDRepCmds era
  -> ExceptT CmdError IO ()
runGovernanceDRepCmds = \case
  GovernanceDRepDelegationCertificateCmd stakeIdentifier delegationTarget outFp ->
    runGovernanceDelegationCertificateCmd stakeIdentifier delegationTarget outFp
      & firstExceptT CmdEraDelegationError

  GovernanceDRepGenerateKey w vrf sgn ->
    runGovernanceDRepKeyGen w vrf sgn
      & firstExceptT CmdGovernanceCmdError

  GovernanceDRepIdCmd w vkey idOutputFormat mOutFp ->
    runGovernanceDRepIdCmd w vkey idOutputFormat mOutFp
      & firstExceptT CmdGovernanceCmdError

  GovernanceDRepRegistrationCertificateCmd regTarget outFp ->
    runGovernanceRegistrationCertificateCmd regTarget outFp
      & firstExceptT CmdRegistrationError

runGovernanceDelegationCertificateCmd
  :: StakeIdentifier
  -> AnyDelegationTarget
  -> File () Out
  -> ExceptT DelegationError IO ()
runGovernanceDelegationCertificateCmd stakeIdentifier delegationTarget outFp = do
  stakeCred <-
    getStakeCredentialFromIdentifier stakeIdentifier
      & firstExceptT DelegationStakeCredentialError

  case delegationTarget of
    ShelleyToBabbageDelegTarget sTob stakePool -> do
      poolId <- lift (readVerificationKeyOrHashOrFile AsStakePoolKey stakePool)
                  & onLeft (left . DelegationReadError)
      let req = StakeDelegationRequirementsPreConway sTob stakeCred poolId
          delegCert = makeStakeAddressDelegationCertificate req
          description = Just @TextEnvelopeDescr "Stake Address Delegation Certificate"
      firstExceptT DelegationCertificateWriteFileError
        . newExceptT
        $ writeLazyByteStringFile outFp
        $ shelleyToBabbageEraConstraints sTob
        $ textEnvelopeToJSON description delegCert

    ConwayOnwardDelegTarget cOnwards target -> do
      delegatee <- toLedgerDelegatee target
      let req = StakeDelegationRequirementsConwayOnwards cOnwards stakeCred delegatee
          delegCert = makeStakeAddressDelegationCertificate req
          description = Just $ toDelegateeEnvelope delegatee
      firstExceptT DelegationCertificateWriteFileError
        . newExceptT
        $ writeLazyByteStringFile outFp
        $ conwayEraOnwardsConstraints cOnwards
        $ textEnvelopeToJSON description delegCert

toLedgerDelegatee
  :: StakeTarget era
  -> ExceptT DelegationError IO (Ledger.Delegatee (Ledger.EraCrypto (ShelleyLedgerEra era)))
toLedgerDelegatee t =
  case t of
    TargetStakePool cOnwards keyOrHashOrFile -> do
      StakePoolKeyHash kHash
        <- lift (readVerificationKeyOrHashOrFile AsStakePoolKey keyOrHashOrFile)
             & onLeft (left . DelegationReadError)
      right $ Ledger.DelegStake $ conwayEraOnwardsConstraints cOnwards kHash

    TargetVotingDrep cOnwards keyOrHashOrFile -> do
      DRepKeyHash drepKeyHash <- firstExceptT DelegationDRepReadError
                                   . newExceptT
                                   $ readVerificationKeyOrHashOrTextEnvFile AsDRepKey keyOrHashOrFile
      let drepCred = Ledger.DRepCredential $ Ledger.KeyHashObj drepKeyHash
      right $ Ledger.DelegVote $ conwayEraOnwardsConstraints cOnwards drepCred

    TargetVotingDrepAndStakePool cOnwards drepKeyOrHashOrFile  poolKeyOrHashOrFile -> do
      StakePoolKeyHash kHash
        <- lift (readVerificationKeyOrHashOrFile AsStakePoolKey poolKeyOrHashOrFile)
             & onLeft (left . DelegationReadError)
      DRepKeyHash drepKeyHash
        <- firstExceptT DelegationDRepReadError
             . newExceptT
             $ readVerificationKeyOrHashOrTextEnvFile AsDRepKey drepKeyOrHashOrFile
      let drepCred = Ledger.DRepCredential $ Ledger.KeyHashObj drepKeyHash
      right $ Ledger.DelegStakeVote
                (conwayEraOnwardsConstraints cOnwards kHash)
                (conwayEraOnwardsConstraints cOnwards drepCred)

    TargetAlwaysAbstain{}-> right $ Ledger.DelegVote Ledger.DRepAlwaysAbstain

    TargetAlwaysNoConfidence{} -> right $ Ledger.DelegVote Ledger.DRepAlwaysNoConfidence

    TargetVotingDRepScriptHash cOn (ScriptHash scriptHash) ->
      conwayEraOnwardsConstraints cOn $
        right $ Ledger.DelegVote $ Ledger.DRepCredential $ Ledger.ScriptHashObj scriptHash

toDelegateeEnvelope :: Ledger.Delegatee ledgerera -> TextEnvelopeDescr
toDelegateeEnvelope = \case
  Ledger.DelegStake{} -> "Stake Delegation Certificate"
  Ledger.DelegVote{} -> "Vote Delegation Certificate"
  Ledger.DelegStakeVote{} -> "Stake and Vote Delegation Certificate"

runGovernanceDRepIdCmd :: ()
  => ConwayEraOnwards era
  -> VerificationKeyOrFile DRepKey
  -> IdOutputFormat
  -> Maybe (File () Out)
  -> ExceptT GovernanceCmdError IO ()
runGovernanceDRepIdCmd _ vkOrFp idOutputFormat mOutFile = do
  drepVerKey <-
    lift (readVerificationKeyOrTextEnvFile AsDRepKey vkOrFp)
      & onLeft (left . ReadFileError)

  content <-
    pure $ case idOutputFormat of
      IdOutputFormatHex -> serialiseToRawBytesHex $ verificationKeyHash drepVerKey
      IdOutputFormatBech32 -> Text.encodeUtf8 $ serialiseToBech32 $ verificationKeyHash drepVerKey

  lift (writeByteStringOutput mOutFile content)
    & onLeft (left . WriteFileError)

--------------------------------------------------------------------------------

-- Registration Certificate related

runGovernanceRegistrationCertificateCmd
  :: AnyRegistrationTarget
  -> File () Out
  -> ExceptT RegistrationError IO ()
runGovernanceRegistrationCertificateCmd anyReg outfp =
  case anyReg of
    ShelleyToBabbageStakePoolRegTarget stoB regReqs -> do
      -- Pool verification key
      stakePoolVerKey <- firstExceptT RegistrationReadError
        . newExceptT
        $ readVerificationKeyOrFile AsStakePoolKey $ sprStakePoolKey regReqs
      let stakePoolId' = verificationKeyHash stakePoolVerKey

      -- VRF verification key
      vrfVerKey <- firstExceptT RegistrationReadError
        . newExceptT
        $ readVerificationKeyOrFile AsVrfKey $ sprVrfKey regReqs
      let vrfKeyHash' = verificationKeyHash vrfVerKey

      -- Pool reward account
      rwdStakeVerKey <- firstExceptT RegistrationReadError
        . newExceptT
        $ readVerificationKeyOrFile AsStakeKey $ sprRewardAccountKey regReqs
      let stakeCred = StakeCredentialByKey (verificationKeyHash rwdStakeVerKey)
          rewardAccountAddr = makeStakeAddress (sprNetworkId regReqs) stakeCred

      -- Pool owner(s)
      sPoolOwnerVkeys <-
        mapM
          (firstExceptT RegistrationReadError
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
      firstExceptT RegistrationWriteFileError
        . newExceptT
        . writeLazyByteStringFile outfp
        $ shelleyToBabbageEraConstraints stoB
        $ textEnvelopeToJSON description registrationCert

    ShelleyToBabbageStakeKeyRegTarget sToB stakeIdentifier -> do
      stakeCred <-
        getStakeCredentialFromIdentifier stakeIdentifier
          & firstExceptT RegistrationStakeCredentialError
      let req = StakeAddrRegistrationPreConway sToB stakeCred
          registrationCert = makeStakeAddressRegistrationCertificate req
          description = Just @TextEnvelopeDescr "Stake Key Registration Certificate"
      firstExceptT RegistrationWriteFileError
        . newExceptT
        . writeLazyByteStringFile outfp
        $ shelleyToBabbageEraConstraints sToB
        $ textEnvelopeToJSON description registrationCert

    ConwayOnwardRegTarget _ regTarget ->
      case regTarget of
        RegisterStakePool cOnwards regReqs -> do
          -- Pool verification key
          stakePoolVerKey <- firstExceptT RegistrationReadError
            . newExceptT
            $ readVerificationKeyOrFile AsStakePoolKey $ sprStakePoolKey regReqs
          let stakePoolId' = verificationKeyHash stakePoolVerKey
          -- VRF verification key
          vrfVerKey <- firstExceptT RegistrationReadError
                         . newExceptT
                         $ readVerificationKeyOrFile AsVrfKey $ sprVrfKey regReqs
          let vrfKeyHash' = verificationKeyHash vrfVerKey
          -- Pool reward account
          rwdStakeVerKey <- firstExceptT RegistrationReadError
                              . newExceptT
                              $ readVerificationKeyOrFile AsStakeKey $ sprRewardAccountKey regReqs
          let stakeCred = StakeCredentialByKey (verificationKeyHash rwdStakeVerKey)
              rewardAccountAddr = makeStakeAddress (sprNetworkId regReqs) stakeCred
          -- Pool owner(s)
          sPoolOwnerVkeys <-
                mapM
                  (firstExceptT RegistrationReadError
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
          firstExceptT RegistrationWriteFileError
            . newExceptT
            . writeLazyByteStringFile outfp
            $ conwayEraOnwardsConstraints cOnwards
            $ textEnvelopeToJSON description registrationCert
        RegisterStakeKey cOnwards sIdentifier deposit -> do
          stakeCred <-
            getStakeCredentialFromIdentifier sIdentifier
              & firstExceptT RegistrationStakeCredentialError
          let req = StakeAddrRegistrationConway cOnwards deposit stakeCred
              registrationCert = makeStakeAddressRegistrationCertificate req
              description = Just @TextEnvelopeDescr "Stake Key Registration Certificate"
          firstExceptT RegistrationWriteFileError
            . newExceptT
            . writeLazyByteStringFile outfp
            $ conwayEraOnwardsConstraints cOnwards
            $ textEnvelopeToJSON description registrationCert
        RegisterDRep cOnwards drepVKey deposit -> do
          DRepKeyHash drepKeyHash <- firstExceptT RegistrationReadError
            . newExceptT
            $ readVerificationKeyOrHashOrFile AsDRepKey drepVKey
          let drepCred = Ledger.KeyHashObj $ conwayEraOnwardsConstraints cOnwards drepKeyHash
              votingCredential = VotingCredential drepCred
              req = DRepRegistrationRequirements cOnwards votingCredential deposit
              registrationCert = makeDrepRegistrationCertificate req
              description = Just @TextEnvelopeDescr "DRep Key Registration Certificate"

          firstExceptT RegistrationWriteFileError
            . newExceptT
            . writeLazyByteStringFile outfp
            $ conwayEraOnwardsConstraints cOnwards
            $ textEnvelopeToJSON description registrationCert
