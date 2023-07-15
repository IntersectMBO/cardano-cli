{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.CLI.Run.Legacy.Pool
  ( ShelleyPoolCmdError(ShelleyPoolCmdReadFileError)
  , renderShelleyPoolCmdError
  , runPoolCmd
  ) where

import           Cardano.Api
import           Cardano.Api.Shelley

import           Cardano.CLI.Commands.Legacy
import           Cardano.CLI.Types.Key (VerificationKeyOrFile, readVerificationKeyOrFile)
import           Cardano.CLI.Types.Legacy (PoolIdOutputFormat (..))
import qualified Cardano.Ledger.Slot as Shelley

import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Trans (lift)
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT, hoistEither, left,
                   newExceptT, onLeft)
import qualified Data.ByteString.Char8 as BS
import           Data.Function ((&))
import           Data.Text (Text)
import qualified Data.Text as Text

data ShelleyPoolCmdError
  = ShelleyPoolCmdReadFileError !(FileError TextEnvelopeError)
  | ShelleyPoolCmdReadKeyFileError !(FileError InputDecodeError)
  | ShelleyPoolCmdWriteFileError !(FileError ())
  | ShelleyPoolCmdMetadataValidationError !StakePoolMetadataValidationError
  deriving Show

renderShelleyPoolCmdError :: ShelleyPoolCmdError -> Text
renderShelleyPoolCmdError err =
  case err of
    ShelleyPoolCmdReadFileError fileErr -> Text.pack (displayError fileErr)
    ShelleyPoolCmdReadKeyFileError fileErr -> Text.pack (displayError fileErr)
    ShelleyPoolCmdWriteFileError fileErr -> Text.pack (displayError fileErr)
    ShelleyPoolCmdMetadataValidationError validationErr ->
      "Error validating stake pool metadata: " <> Text.pack (displayError validationErr)



runPoolCmd :: PoolCmd -> ExceptT ShelleyPoolCmdError IO ()
runPoolCmd = \case
  PoolRegistrationCert anyEra sPvkey vrfVkey pldg pCost pMrgn rwdVerFp ownerVerFps relays mbMetadata network outfp ->
    runStakePoolRegistrationCert anyEra sPvkey vrfVkey pldg pCost pMrgn rwdVerFp ownerVerFps relays mbMetadata network outfp
  PoolRetirementCert anyEra sPvkeyFp retireEpoch outfp ->
    runStakePoolRetirementCert anyEra sPvkeyFp retireEpoch outfp
  PoolGetId sPvkey outputFormat mOutFile ->
    runPoolId sPvkey outputFormat mOutFile
  PoolMetadataHash poolMdFile mOutFile ->
    runPoolMetadataHash poolMdFile mOutFile

--
-- Stake pool command implementations
--

-- | Create a stake pool registration cert.
-- TODO: Metadata and more stake pool relay support to be
-- added in the future.
runStakePoolRegistrationCert
  :: AnyShelleyBasedEra
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
  -> ExceptT ShelleyPoolCmdError IO ()
runStakePoolRegistrationCert
  anyEra
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
  outfp = do
    AnyShelleyBasedEra sbe <- pure anyEra

    -- Pool verification key
    stakePoolVerKey <- firstExceptT ShelleyPoolCmdReadKeyFileError
      . newExceptT
      $ readVerificationKeyOrFile AsStakePoolKey stakePoolVerKeyOrFile
    let stakePoolId' = verificationKeyHash stakePoolVerKey

    -- VRF verification key
    vrfVerKey <- firstExceptT ShelleyPoolCmdReadKeyFileError
      . newExceptT
      $ readVerificationKeyOrFile AsVrfKey vrfVerKeyOrFile
    let vrfKeyHash' = verificationKeyHash vrfVerKey

    -- Pool reward account
    rwdStakeVerKey <- firstExceptT ShelleyPoolCmdReadKeyFileError
      . newExceptT
      $ readVerificationKeyOrFile AsStakeKey rwdStakeVerKeyOrFile
    let stakeCred = StakeCredentialByKey (verificationKeyHash rwdStakeVerKey)
        rewardAccountAddr = makeStakeAddress network stakeCred

    -- Pool owner(s)
    sPoolOwnerVkeys <-
      mapM
        (firstExceptT ShelleyPoolCmdReadKeyFileError
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

    let registrationCert = makeStakePoolRegistrationCertificate sbe stakePoolParams

    firstExceptT ShelleyPoolCmdWriteFileError
      . newExceptT
      $ writeLazyByteStringFile outfp
      $ textEnvelopeToJSON (Just registrationCertDesc) registrationCert
  where
    registrationCertDesc :: TextEnvelopeDescr
    registrationCertDesc = "Stake Pool Registration Certificate"

runStakePoolRetirementCert
  :: AnyShelleyBasedEra
  -> VerificationKeyOrFile StakePoolKey
  -> Shelley.EpochNo
  -> File () Out
  -> ExceptT ShelleyPoolCmdError IO ()
runStakePoolRetirementCert anyEra stakePoolVerKeyOrFile retireEpoch outfp = do
    AnyShelleyBasedEra sbe <- pure anyEra

    -- Pool verification key
    stakePoolVerKey <- firstExceptT ShelleyPoolCmdReadKeyFileError
      . newExceptT
      $ readVerificationKeyOrFile AsStakePoolKey stakePoolVerKeyOrFile

    let stakePoolId' = verificationKeyHash stakePoolVerKey
        retireCert = makeStakePoolRetirementCertificate sbe stakePoolId' retireEpoch

    firstExceptT ShelleyPoolCmdWriteFileError
      . newExceptT
      $ writeLazyByteStringFile outfp
      $ textEnvelopeToJSON (Just retireCertDesc) retireCert
  where
    retireCertDesc :: TextEnvelopeDescr
    retireCertDesc = "Stake Pool Retirement Certificate"

runPoolId
  :: VerificationKeyOrFile StakePoolKey
  -> PoolIdOutputFormat
  -> Maybe (File () Out)
  -> ExceptT ShelleyPoolCmdError IO ()
runPoolId verKeyOrFile outputFormat mOutFile = do
  stakePoolVerKey <- firstExceptT ShelleyPoolCmdReadKeyFileError
    . newExceptT
    $ readVerificationKeyOrFile AsStakePoolKey verKeyOrFile

  case outputFormat of
    PoolIdOutputFormatHex ->
      firstExceptT ShelleyPoolCmdWriteFileError
        . newExceptT
        $ writeByteStringOutput mOutFile
        $ serialiseToRawBytesHex (verificationKeyHash stakePoolVerKey)
    PoolIdOutputFormatBech32 ->
      firstExceptT ShelleyPoolCmdWriteFileError
        . newExceptT
        $ writeTextOutput mOutFile
        $ serialiseToBech32 (verificationKeyHash stakePoolVerKey)

runPoolMetadataHash :: StakePoolMetadataFile In -> Maybe (File () Out) -> ExceptT ShelleyPoolCmdError IO ()
runPoolMetadataHash poolMDPath mOutFile = do
  metadataBytes <- lift (readByteStringFile poolMDPath)
    & onLeft (left . ShelleyPoolCmdReadFileError)

  (_metadata, metadataHash) <-
      firstExceptT ShelleyPoolCmdMetadataValidationError
    . hoistEither
    $ validateAndHashStakePoolMetadata metadataBytes
  case mOutFile of
    Nothing -> liftIO $ BS.putStrLn (serialiseToRawBytesHex metadataHash)
    Just (File fpath) ->
      handleIOExceptT (ShelleyPoolCmdWriteFileError . FileIOError fpath)
        $ BS.writeFile fpath (serialiseToRawBytesHex metadataHash)
