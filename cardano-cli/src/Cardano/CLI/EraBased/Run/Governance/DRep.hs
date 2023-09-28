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

import Cardano.Api
import qualified Cardano.Api.Ledger as Ledger
import Cardano.Api.Shelley

import Cardano.CLI.EraBased.Commands.Governance.DRep
import Cardano.CLI.EraBased.Run.Governance
import Cardano.CLI.Read
import Cardano.CLI.Types.Common
import Cardano.CLI.Types.Errors.CmdError
import Cardano.CLI.Types.Errors.GovernanceCmdError
import Cardano.CLI.Types.Errors.RegistrationError
import Cardano.CLI.Types.Key

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Except.Extra
import Data.Function
import qualified Data.Text.Encoding as Text

runGovernanceDRepCmds
  :: ()
  => GovernanceDRepCmds era
  -> ExceptT CmdError IO ()
runGovernanceDRepCmds = \case
  GovernanceDRepGenerateKeyCmd w vrf sgn ->
    runGovernanceDRepKeyGen w vrf sgn
      & firstExceptT CmdGovernanceCmdError
  GovernanceDRepIdCmd w vkey idOutputFormat mOutFp ->
    runGovernanceDRepIdCmd w vkey idOutputFormat mOutFp
      & firstExceptT CmdGovernanceCmdError
  GovernanceDRepRegistrationCertificateCmd regTarget outFp ->
    runGovernanceRegistrationCertificateCmd regTarget outFp
      & firstExceptT CmdRegistrationError

runGovernanceDRepIdCmd
  :: ()
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
      stakePoolVerKey <-
        firstExceptT RegistrationReadError
          . newExceptT
          $ readVerificationKeyOrFile AsStakePoolKey
          $ sprStakePoolKey regReqs
      let stakePoolId' = verificationKeyHash stakePoolVerKey

      -- VRF verification key
      vrfVerKey <-
        firstExceptT RegistrationReadError
          . newExceptT
          $ readVerificationKeyOrFile AsVrfKey
          $ sprVrfKey regReqs
      let vrfKeyHash' = verificationKeyHash vrfVerKey

      -- Pool reward account
      rwdStakeVerKey <-
        firstExceptT RegistrationReadError
          . newExceptT
          $ readVerificationKeyOrFile AsStakeKey
          $ sprRewardAccountKey regReqs
      let stakeCred = StakeCredentialByKey (verificationKeyHash rwdStakeVerKey)
          rewardAccountAddr = makeStakeAddress (sprNetworkId regReqs) stakeCred

      -- Pool owner(s)
      sPoolOwnerVkeys <-
        mapM
          ( firstExceptT RegistrationReadError
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
          req =
            StakePoolRegistrationRequirementsPreConway stoB $
              shelleyToBabbageEraConstraints stoB ledgerStakePoolParams
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
          stakePoolVerKey <-
            firstExceptT RegistrationReadError
              . newExceptT
              $ readVerificationKeyOrFile AsStakePoolKey
              $ sprStakePoolKey regReqs
          let stakePoolId' = verificationKeyHash stakePoolVerKey
          -- VRF verification key
          vrfVerKey <-
            firstExceptT RegistrationReadError
              . newExceptT
              $ readVerificationKeyOrFile AsVrfKey
              $ sprVrfKey regReqs
          let vrfKeyHash' = verificationKeyHash vrfVerKey
          -- Pool reward account
          rwdStakeVerKey <-
            firstExceptT RegistrationReadError
              . newExceptT
              $ readVerificationKeyOrFile AsStakeKey
              $ sprRewardAccountKey regReqs
          let stakeCred = StakeCredentialByKey (verificationKeyHash rwdStakeVerKey)
              rewardAccountAddr = makeStakeAddress (sprNetworkId regReqs) stakeCred
          -- Pool owner(s)
          sPoolOwnerVkeys <-
            mapM
              ( firstExceptT RegistrationReadError
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
              req =
                StakePoolRegistrationRequirementsConwayOnwards cOnwards $
                  conwayEraOnwardsConstraints cOnwards ledgerStakePoolParams
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
          DRepKeyHash drepKeyHash <-
            firstExceptT RegistrationReadError
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
