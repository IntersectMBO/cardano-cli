{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.CLI.EraBased.Run.Pool
  ( PoolCmdError(PoolCmdReadFileError)
  , renderPoolCmdError
  , runPoolCmd
  ) where

import           Cardano.Api
import           Cardano.Api.Shelley

import           Cardano.CLI.Commands.Legacy
import           Cardano.CLI.EraBased.Options.Pool
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

data PoolCmdError
  = PoolCmdReadFileError !(FileError TextEnvelopeError)
  | PoolCmdReadKeyFileError !(FileError InputDecodeError)
  | PoolCmdWriteFileError !(FileError ())
  | PoolCmdMetadataValidationError !StakePoolMetadataValidationError
  deriving Show

renderPoolCmdError :: PoolCmdError -> Text
renderPoolCmdError err =
  case err of
    PoolCmdReadFileError fileErr -> Text.pack (displayError fileErr)
    PoolCmdReadKeyFileError fileErr -> Text.pack (displayError fileErr)
    PoolCmdWriteFileError fileErr -> Text.pack (displayError fileErr)
    PoolCmdMetadataValidationError validationErr ->
      "Error validating stake pool metadata: " <> Text.pack (displayError validationErr)

runPoolCmd :: PoolCmd era -> ExceptT PoolCmdError IO ()
runPoolCmd = \case
  PoolRegistrationCert sbe sPvkey vrfVkey pldg pCost pMrgn rwdVerFp ownerVerFps relays mbMetadata network outfp ->
    runStakePoolRegistrationCert sbe sPvkey vrfVkey pldg pCost pMrgn rwdVerFp ownerVerFps relays mbMetadata network outfp
  PoolRetirementCert sbe sPvkeyFp retireEpoch outfp ->
    runStakePoolRetirementCert sbe sPvkeyFp retireEpoch outfp
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
  -> ExceptT PoolCmdError IO ()
runStakePoolRegistrationCert
  _sbe
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
    -- Pool verification key
    stakePoolVerKey <- firstExceptT PoolCmdReadKeyFileError
      . newExceptT
      $ readVerificationKeyOrFile AsStakePoolKey stakePoolVerKeyOrFile
    let stakePoolId' = verificationKeyHash stakePoolVerKey

    -- VRF verification key
    vrfVerKey <- firstExceptT PoolCmdReadKeyFileError
      . newExceptT
      $ readVerificationKeyOrFile AsVrfKey vrfVerKeyOrFile
    let vrfKeyHash' = verificationKeyHash vrfVerKey

    -- Pool reward account
    rwdStakeVerKey <- firstExceptT PoolCmdReadKeyFileError
      . newExceptT
      $ readVerificationKeyOrFile AsStakeKey rwdStakeVerKeyOrFile
    let stakeCred = StakeCredentialByKey (verificationKeyHash rwdStakeVerKey)
        rewardAccountAddr = makeStakeAddress network stakeCred

    -- Pool owner(s)
    sPoolOwnerVkeys <-
      mapM
        (firstExceptT PoolCmdReadKeyFileError
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

    let registrationCert = error "TODO: Conway era" -- makeStakePoolRegistrationCertificate sbe stakePoolParams

    firstExceptT PoolCmdWriteFileError
      . newExceptT
      $ writeLazyByteStringFile outfp
      $ error "TODO: Conway era"  -- textEnvelopeToJSON (Just registrationCertDesc) registrationCert
  where
    registrationCertDesc :: TextEnvelopeDescr
    registrationCertDesc = "Stake Pool Registration Certificate"

runStakePoolRetirementCert
  :: ShelleyBasedEra era
  -> VerificationKeyOrFile StakePoolKey
  -> Shelley.EpochNo
  -> File () Out
  -> ExceptT PoolCmdError IO ()
runStakePoolRetirementCert _sbe stakePoolVerKeyOrFile retireEpoch outfp = do
    -- Pool verification key
    stakePoolVerKey <- firstExceptT PoolCmdReadKeyFileError
      . newExceptT
      $ readVerificationKeyOrFile AsStakePoolKey stakePoolVerKeyOrFile

    let stakePoolId' = verificationKeyHash stakePoolVerKey
        retireCert = error "TODO: Conway era" -- makeStakePoolRetirementCertificate sbe stakePoolId' retireEpoch

    firstExceptT PoolCmdWriteFileError
      . newExceptT
      $ writeLazyByteStringFile outfp
      $ error "TODO: Conway era" -- textEnvelopeToJSON (Just retireCertDesc) retireCert
  where
    retireCertDesc :: TextEnvelopeDescr
    retireCertDesc = "Stake Pool Retirement Certificate"

runPoolId
  :: VerificationKeyOrFile StakePoolKey
  -> PoolIdOutputFormat
  -> Maybe (File () Out)
  -> ExceptT PoolCmdError IO ()
runPoolId verKeyOrFile outputFormat mOutFile = do
  stakePoolVerKey <- firstExceptT PoolCmdReadKeyFileError
    . newExceptT
    $ readVerificationKeyOrFile AsStakePoolKey verKeyOrFile

  case outputFormat of
    PoolIdOutputFormatHex ->
      firstExceptT PoolCmdWriteFileError
        . newExceptT
        $ writeByteStringOutput mOutFile
        $ serialiseToRawBytesHex (verificationKeyHash stakePoolVerKey)
    PoolIdOutputFormatBech32 ->
      firstExceptT PoolCmdWriteFileError
        . newExceptT
        $ writeTextOutput mOutFile
        $ serialiseToBech32 (verificationKeyHash stakePoolVerKey)

runPoolMetadataHash :: StakePoolMetadataFile In -> Maybe (File () Out) -> ExceptT PoolCmdError IO ()
runPoolMetadataHash poolMDPath mOutFile = do
  metadataBytes <- lift (readByteStringFile poolMDPath)
    & onLeft (left . PoolCmdReadFileError)

  (_metadata, metadataHash) <-
      firstExceptT PoolCmdMetadataValidationError
    . hoistEither
    $ validateAndHashStakePoolMetadata metadataBytes
  case mOutFile of
    Nothing -> liftIO $ BS.putStrLn (serialiseToRawBytesHex metadataHash)
    Just (File fpath) ->
      handleIOExceptT (PoolCmdWriteFileError . FileIOError fpath)
        $ BS.writeFile fpath (serialiseToRawBytesHex metadataHash)
