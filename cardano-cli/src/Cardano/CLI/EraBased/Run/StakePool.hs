{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.CLI.EraBased.Run.StakePool
  ( runStakePoolCmds
  , runStakePoolIdCmd
  , runStakePoolMetadataHashCmd
  , runStakePoolRegistrationCertificateCmd
  , runStakePoolRetirementCertificateCmd
  ) where

import Cardano.Api
import Cardano.Api.Ledger qualified as Ledger
import Cardano.Api.Shelley

import Cardano.CLI.EraBased.Commands.StakePool
import Cardano.CLI.Types.Common
import Cardano.CLI.Types.Errors.StakePoolCmdError
import Cardano.CLI.Types.Key (VerificationKeyOrFile, readVerificationKeyOrFile)
import Cardano.Ledger.Slot qualified as Shelley

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Except.Extra
  ( firstExceptT
  , handleIOExceptT
  , hoistEither
  , left
  , newExceptT
  , onLeft
  )
import Data.ByteString.Char8 qualified as BS
import Data.Function ((&))

runStakePoolCmds
  :: ()
  => StakePoolCmds era
  -> ExceptT StakePoolCmdError IO ()
runStakePoolCmds = \case
  StakePoolDeregistrationCertificateCmd sbe sPvkeyFp retireEpoch outfp ->
    runStakePoolRetirementCertificateCmd sbe sPvkeyFp retireEpoch outfp
  StakePoolIdCmd sPvkey outputFormat mOutFile ->
    runStakePoolIdCmd sPvkey outputFormat mOutFile
  StakePoolMetadataHashCmd poolMdFile mOutFile ->
    runStakePoolMetadataHashCmd poolMdFile mOutFile
  StakePoolRegistrationCertificateCmd
    sbe
    sPvkey
    vrfVkey
    pldg
    pCost
    pMrgn
    rwdVerFp
    ownerVerFps
    relays
    mbMetadata
    network
    outfp ->
      runStakePoolRegistrationCertificateCmd
        sbe
        sPvkey
        vrfVkey
        pldg
        pCost
        pMrgn
        rwdVerFp
        ownerVerFps
        relays
        mbMetadata
        network
        outfp

--
-- Stake pool command implementations
--

-- | Create a stake pool registration cert.
-- TODO: Metadata and more stake pool relay support to be
-- added in the future.
runStakePoolRegistrationCertificateCmd
  :: ShelleyBasedEra era
  -> VerificationKeyOrFile StakePoolKey
  -- ^ Stake pool verification key.
  -> VerificationKeyOrFile VrfKey
  -- ^ VRF Verification key.
  -> Lovelace
  -- ^ Pool pledge.
  -> Lovelace
  -- ^ Pool cost.
  -> Rational
  -- ^ Pool margin.
  -> VerificationKeyOrFile StakeKey
  -- ^ Stake verification key for reward account.
  -> [VerificationKeyOrFile StakeKey]
  -- ^ Pool owner stake verification key(s).
  -> [StakePoolRelay]
  -- ^ Stake pool relays.
  -> Maybe StakePoolMetadataReference
  -- ^ Stake pool metadata.
  -> NetworkId
  -> File () Out
  -> ExceptT StakePoolCmdError IO ()
runStakePoolRegistrationCertificateCmd
  sbe
  stakePoolVerKeyOrFile
  vrfVerKeyOrFile
  pldg
  pCost
  pMrgn
  rwdStakeVerKeyOrFile
  ownerStakeVerKeyOrFiles
  relays
  mbMetadata
  network
  outfp = shelleyBasedEraConstraints sbe $ do
    -- Pool verification key
    stakePoolVerKey <-
      firstExceptT StakePoolCmdReadKeyFileError
        . newExceptT
        $ readVerificationKeyOrFile AsStakePoolKey stakePoolVerKeyOrFile
    let stakePoolId' = verificationKeyHash stakePoolVerKey

    -- VRF verification key
    vrfVerKey <-
      firstExceptT StakePoolCmdReadKeyFileError
        . newExceptT
        $ readVerificationKeyOrFile AsVrfKey vrfVerKeyOrFile
    let vrfKeyHash' = verificationKeyHash vrfVerKey

    -- Pool reward account
    rwdStakeVerKey <-
      firstExceptT StakePoolCmdReadKeyFileError
        . newExceptT
        $ readVerificationKeyOrFile AsStakeKey rwdStakeVerKeyOrFile
    let stakeCred = StakeCredentialByKey (verificationKeyHash rwdStakeVerKey)
        rewardAccountAddr = makeStakeAddress network stakeCred

    -- Pool owner(s)
    sPoolOwnerVkeys <-
      mapM
        ( firstExceptT StakePoolCmdReadKeyFileError
            . newExceptT
            . readVerificationKeyOrFile AsStakeKey
        )
        ownerStakeVerKeyOrFiles
    let stakePoolOwners' = map verificationKeyHash sPoolOwnerVkeys

    let stakePoolParams =
          StakePoolParameters
            { stakePoolId = stakePoolId'
            , stakePoolVRF = vrfKeyHash'
            , stakePoolCost = pCost
            , stakePoolMargin = pMrgn
            , stakePoolRewardAccount = rewardAccountAddr
            , stakePoolPledge = pldg
            , stakePoolOwners = stakePoolOwners'
            , stakePoolRelays = relays
            , stakePoolMetadata = mbMetadata
            }

    let ledgerStakePoolParams = toShelleyPoolParams stakePoolParams
        req =
          createStakePoolRegistrationRequirements sbe $
            shelleyBasedEraConstraints sbe ledgerStakePoolParams
        registrationCert = makeStakePoolRegistrationCertificate req

    firstExceptT StakePoolCmdWriteFileError
      . newExceptT
      $ writeLazyByteStringFile outfp
      $ textEnvelopeToJSON (Just registrationCertDesc) registrationCert
   where
    registrationCertDesc :: TextEnvelopeDescr
    registrationCertDesc = "Stake Pool Registration Certificate"

createStakePoolRegistrationRequirements
  :: ()
  => ShelleyBasedEra era
  -> Ledger.PoolParams (Ledger.EraCrypto (ShelleyLedgerEra era))
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

runStakePoolRetirementCertificateCmd
  :: ()
  => ShelleyBasedEra era
  -> VerificationKeyOrFile StakePoolKey
  -> Shelley.EpochNo
  -> File () Out
  -> ExceptT StakePoolCmdError IO ()
runStakePoolRetirementCertificateCmd sbe stakePoolVerKeyOrFile retireEpoch outfp =
  shelleyBasedEraConstraints sbe $ do
    -- Pool verification key
    stakePoolVerKey <-
      firstExceptT StakePoolCmdReadKeyFileError
        . newExceptT
        $ readVerificationKeyOrFile AsStakePoolKey stakePoolVerKeyOrFile

    let stakePoolId' = verificationKeyHash stakePoolVerKey
        req = createStakePoolRetirementRequirements sbe stakePoolId' retireEpoch
        retireCert = makeStakePoolRetirementCertificate req

    firstExceptT StakePoolCmdWriteFileError
      . newExceptT
      $ writeLazyByteStringFile outfp
      $ textEnvelopeToJSON (Just retireCertDesc) retireCert
 where
  retireCertDesc :: TextEnvelopeDescr
  retireCertDesc = "Stake Pool Retirement Certificate"

createStakePoolRetirementRequirements
  :: ShelleyBasedEra era
  -> PoolId
  -> Ledger.EpochNo
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
  :: VerificationKeyOrFile StakePoolKey
  -> IdOutputFormat
  -> Maybe (File () Out)
  -> ExceptT StakePoolCmdError IO ()
runStakePoolIdCmd verKeyOrFile outputFormat mOutFile = do
  stakePoolVerKey <-
    firstExceptT StakePoolCmdReadKeyFileError
      . newExceptT
      $ readVerificationKeyOrFile AsStakePoolKey verKeyOrFile

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
  :: StakePoolMetadataFile In -> Maybe (File () Out) -> ExceptT StakePoolCmdError IO ()
runStakePoolMetadataHashCmd poolMDPath mOutFile = do
  metadataBytes <-
    lift (readByteStringFile poolMDPath)
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
