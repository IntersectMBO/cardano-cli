{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.CLI.EraBased.Run.StakePool
  ( runStakePoolCmds
  , runStakePoolIdCmd
  , runStakePoolMetadataHashCmd
  , runStakePoolRegistrationCertificateCmd
  , runStakePoolDeregistrationCertificateCmd
  )
where

import           Cardano.Api
import qualified Cardano.Api.Ledger as L
import           Cardano.Api.Shelley

import           Cardano.CLI.EraBased.Commands.StakePool
import qualified Cardano.CLI.EraBased.Commands.StakePool as Cmd
import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Errors.StakePoolCmdError
import           Cardano.CLI.Types.Key (readVerificationKeyOrFile)

import qualified Data.ByteString.Char8 as BS
import           Data.Function ((&))

runStakePoolCmds
  :: ()
  => StakePoolCmds era
  -> ExceptT StakePoolCmdError IO ()
runStakePoolCmds = \case
  StakePoolDeregistrationCertificateCmd args -> runStakePoolDeregistrationCertificateCmd args
  StakePoolIdCmd args -> runStakePoolIdCmd args
  StakePoolMetadataHashCmd args -> runStakePoolMetadataHashCmd args
  StakePoolRegistrationCertificateCmd args -> runStakePoolRegistrationCertificateCmd args

--
-- Stake pool command implementations
--

-- | Create a stake pool registration cert.
-- TODO: Metadata and more stake pool relay support to be
-- added in the future.
runStakePoolRegistrationCertificateCmd
  :: ()
  => StakePoolRegistrationCertificateCmdArgs era
  -> ExceptT StakePoolCmdError IO ()
runStakePoolRegistrationCertificateCmd
  Cmd.StakePoolRegistrationCertificateCmdArgs
    { sbe
    , poolVerificationKeyOrFile
    , vrfVerificationKeyOrFile
    , poolPledge
    , poolCost
    , poolMargin
    , rewardStakeVerificationKeyOrFile
    , ownerStakeVerificationKeyOrFiles
    , relays
    , mMetadata
    , network
    , outFile
    } =
    shelleyBasedEraConstraints sbe $ do
      -- Pool verification key
      stakePoolVerKey <-
        firstExceptT StakePoolCmdReadKeyFileError $
          readVerificationKeyOrFile AsStakePoolKey poolVerificationKeyOrFile
      let stakePoolId' = verificationKeyHash stakePoolVerKey

      -- VRF verification key
      vrfVerKey <-
        firstExceptT StakePoolCmdReadKeyFileError $
          readVerificationKeyOrFile AsVrfKey vrfVerificationKeyOrFile
      let vrfKeyHash' = verificationKeyHash vrfVerKey

      -- Pool reward account
      rwdStakeVerKey <-
        firstExceptT StakePoolCmdReadKeyFileError $
          readVerificationKeyOrFile AsStakeKey rewardStakeVerificationKeyOrFile
      let stakeCred = StakeCredentialByKey (verificationKeyHash rwdStakeVerKey)
          rewardAccountAddr = makeStakeAddress network stakeCred

      -- Pool owner(s)
      sPoolOwnerVkeys <-
        mapM
          ( firstExceptT StakePoolCmdReadKeyFileError
              . readVerificationKeyOrFile AsStakeKey
          )
          ownerStakeVerificationKeyOrFiles
      let stakePoolOwners' = map verificationKeyHash sPoolOwnerVkeys

      let stakePoolParams =
            StakePoolParameters
              { stakePoolId = stakePoolId'
              , stakePoolVRF = vrfKeyHash'
              , stakePoolCost = poolCost
              , stakePoolMargin = poolMargin
              , stakePoolRewardAccount = rewardAccountAddr
              , stakePoolPledge = poolPledge
              , stakePoolOwners = stakePoolOwners'
              , stakePoolRelays = relays
              , stakePoolMetadata = mMetadata
              }

      let ledgerStakePoolParams = toShelleyPoolParams stakePoolParams
          req =
            createStakePoolRegistrationRequirements sbe $
              shelleyBasedEraConstraints sbe ledgerStakePoolParams
          registrationCert = makeStakePoolRegistrationCertificate req

      firstExceptT StakePoolCmdWriteFileError
        . newExceptT
        $ writeLazyByteStringFile outFile
        $ textEnvelopeToJSON (Just registrationCertDesc) registrationCert
   where
    registrationCertDesc :: TextEnvelopeDescr
    registrationCertDesc = "Stake Pool Registration Certificate"

createStakePoolRegistrationRequirements
  :: ()
  => ShelleyBasedEra era
  -> L.PoolParams (L.EraCrypto (ShelleyLedgerEra era))
  -> StakePoolRegistrationRequirements era
createStakePoolRegistrationRequirements sbe pparams =
  case sbe of
    ShelleyBasedEraShelley ->
      StakePoolRegistrationRequirementsPreConway ShelleyToBabbageEraShelley pparams
    ShelleyBasedEraAllegra ->
      StakePoolRegistrationRequirementsPreConway ShelleyToBabbageEraAllegra pparams
    ShelleyBasedEraMary ->
      StakePoolRegistrationRequirementsPreConway ShelleyToBabbageEraMary pparams
    ShelleyBasedEraAlonzo ->
      StakePoolRegistrationRequirementsPreConway ShelleyToBabbageEraAlonzo pparams
    ShelleyBasedEraBabbage ->
      StakePoolRegistrationRequirementsPreConway ShelleyToBabbageEraBabbage pparams
    ShelleyBasedEraConway ->
      StakePoolRegistrationRequirementsConwayOnwards ConwayEraOnwardsConway pparams

runStakePoolDeregistrationCertificateCmd
  :: ()
  => StakePoolDeregistrationCertificateCmdArgs era
  -> ExceptT StakePoolCmdError IO ()
runStakePoolDeregistrationCertificateCmd
  Cmd.StakePoolDeregistrationCertificateCmdArgs
    { sbe
    , poolVerificationKeyOrFile
    , retireEpoch
    , outFile
    } =
    shelleyBasedEraConstraints sbe $ do
      -- Pool verification key
      stakePoolVerKey <-
        firstExceptT StakePoolCmdReadKeyFileError $
          readVerificationKeyOrFile AsStakePoolKey poolVerificationKeyOrFile

      let stakePoolId' = verificationKeyHash stakePoolVerKey
          req = createStakePoolRetirementRequirements sbe stakePoolId' retireEpoch
          retireCert = makeStakePoolRetirementCertificate req

      firstExceptT StakePoolCmdWriteFileError
        . newExceptT
        $ writeLazyByteStringFile outFile
        $ textEnvelopeToJSON (Just retireCertDesc) retireCert
   where
    retireCertDesc :: TextEnvelopeDescr
    retireCertDesc = "Stake Pool Retirement Certificate"

createStakePoolRetirementRequirements
  :: ()
  => ShelleyBasedEra era
  -> PoolId
  -> L.EpochNo
  -> StakePoolRetirementRequirements era
createStakePoolRetirementRequirements sbe pid epoch =
  case sbe of
    ShelleyBasedEraShelley ->
      StakePoolRetirementRequirementsPreConway ShelleyToBabbageEraShelley pid epoch
    ShelleyBasedEraAllegra ->
      StakePoolRetirementRequirementsPreConway ShelleyToBabbageEraAllegra pid epoch
    ShelleyBasedEraMary ->
      StakePoolRetirementRequirementsPreConway ShelleyToBabbageEraMary pid epoch
    ShelleyBasedEraAlonzo ->
      StakePoolRetirementRequirementsPreConway ShelleyToBabbageEraAlonzo pid epoch
    ShelleyBasedEraBabbage ->
      StakePoolRetirementRequirementsPreConway ShelleyToBabbageEraBabbage pid epoch
    ShelleyBasedEraConway ->
      StakePoolRetirementRequirementsConwayOnwards ConwayEraOnwardsConway pid epoch

runStakePoolIdCmd
  :: ()
  => StakePoolIdCmdArgs era
  -> ExceptT StakePoolCmdError IO ()
runStakePoolIdCmd
  Cmd.StakePoolIdCmdArgs
    { poolVerificationKeyOrFile
    , outputFormat
    , mOutFile
    } = do
    stakePoolVerKey <-
      firstExceptT StakePoolCmdReadKeyFileError $
        readVerificationKeyOrFile AsStakePoolKey poolVerificationKeyOrFile

    case outputFormat of
      IdOutputFormatHex ->
        firstExceptT StakePoolCmdWriteFileError
          . newExceptT
          $ writeByteStringOutput mOutFile
          $ serialiseToRawBytesHex (verificationKeyHash stakePoolVerKey)
      IdOutputFormatBech32 ->
        firstExceptT StakePoolCmdWriteFileError
          . newExceptT
          $ writeTextOutput mOutFile
          $ serialiseToBech32 (verificationKeyHash stakePoolVerKey)

runStakePoolMetadataHashCmd
  :: ()
  => StakePoolMetadataHashCmdArgs era
  -> ExceptT StakePoolCmdError IO ()
runStakePoolMetadataHashCmd
  Cmd.StakePoolMetadataHashCmdArgs
    { poolMetadataFile
    , mOutFile
    } = do
    metadataBytes <-
      lift (readByteStringFile poolMetadataFile)
        & onLeft (left . StakePoolCmdReadFileError)

    (_metadata, metadataHash) <-
      firstExceptT StakePoolCmdMetadataValidationError
        . hoistEither
        $ validateAndHashStakePoolMetadata metadataBytes
    case mOutFile of
      Nothing -> liftIO $ BS.putStrLn (serialiseToRawBytesHex metadataHash)
      Just (File fpath) ->
        handleIOExceptT (StakePoolCmdWriteFileError . FileIOError fpath) $
          BS.writeFile fpath (serialiseToRawBytesHex metadataHash)
