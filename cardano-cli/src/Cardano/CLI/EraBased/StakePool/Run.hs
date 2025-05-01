{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

{- HLINT ignore "Redundant id" -}

module Cardano.CLI.EraBased.StakePool.Run
  ( runStakePoolCmds
  , runStakePoolIdCmd
  , runStakePoolMetadataHashCmd
  , runStakePoolRegistrationCertificateCmd
  , runStakePoolDeregistrationCertificateCmd
  )
where

import Cardano.Api.Ledger qualified as L
import Cardano.Api.Shelley

import Cardano.CLI.EraBased.StakePool.Command
import Cardano.CLI.EraBased.StakePool.Command qualified as Cmd
import Cardano.CLI.EraBased.StakePool.Internal.Metadata (carryHashChecks)
import Cardano.CLI.EraIndependent.Hash.Command qualified as Cmd
import Cardano.CLI.EraIndependent.Hash.Internal.Common
  ( allSchemes
  , getByteStringFromURL
  )
import Cardano.CLI.Read (getVerificationKeyFromStakePoolVerificationKeySource)
import Cardano.CLI.Type.Common
import Cardano.CLI.Type.Error.HashCmdError (FetchURLError (..))
import Cardano.CLI.Type.Error.StakePoolCmdError
import Cardano.CLI.Type.Key (readVerificationKeyOrFile)

import Data.ByteString.Char8 qualified as BS
import Data.Function ((&))
import Vary qualified

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
      stakePoolVerKey <- getVerificationKeyFromStakePoolVerificationKeySource poolVerificationKeyOrFile
      let stakePoolId' = anyStakePoolVerificationKeyHash stakePoolVerKey

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
              , stakePoolMetadata = pcaAnchor <$> mMetadata
              }

      let ledgerStakePoolParams = toShelleyPoolParams stakePoolParams
          req =
            createStakePoolRegistrationRequirements sbe $
              shelleyBasedEraConstraints sbe ledgerStakePoolParams
          registrationCert = makeStakePoolRegistrationCertificate req

      mapM_ carryHashChecks mMetadata

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
  -> L.PoolParams
  -> StakePoolRegistrationRequirements era
createStakePoolRegistrationRequirements sbe pparams =
  caseShelleyToBabbageOrConwayEraOnwards
    (`StakePoolRegistrationRequirementsPreConway` pparams)
    (`StakePoolRegistrationRequirementsConwayOnwards` pparams)
    sbe

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
      stakePoolVerKey <- getVerificationKeyFromStakePoolVerificationKeySource poolVerificationKeyOrFile

      let stakePoolId' = anyStakePoolVerificationKeyHash stakePoolVerKey
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
  caseShelleyToBabbageOrConwayEraOnwards
    (\stb -> StakePoolRetirementRequirementsPreConway stb pid epoch)
    (\ceo -> StakePoolRetirementRequirementsConwayOnwards ceo pid epoch)
    sbe

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
    stakePoolVerKey <- getVerificationKeyFromStakePoolVerificationKeySource poolVerificationKeyOrFile
    let stakePoolKeyHash = anyStakePoolVerificationKeyHash stakePoolVerKey
    outputFormat
      & ( id
            . Vary.on
              ( \FormatBech32 ->
                  firstExceptT StakePoolCmdWriteFileError
                    . newExceptT
                    $ writeTextOutput mOutFile
                    $ serialiseToBech32 stakePoolKeyHash
              )
            . Vary.on
              ( \FormatHex ->
                  firstExceptT StakePoolCmdWriteFileError
                    . newExceptT
                    $ writeByteStringOutput mOutFile
                    $ serialiseToRawBytesHex stakePoolKeyHash
              )
            $ Vary.exhaustiveCase
        )

runStakePoolMetadataHashCmd
  :: ()
  => StakePoolMetadataHashCmdArgs era
  -> ExceptT StakePoolCmdError IO ()
runStakePoolMetadataHashCmd
  Cmd.StakePoolMetadataHashCmdArgs
    { poolMetadataSource
    , hashGoal
    } = do
    metadataBytes <-
      case poolMetadataSource of
        StakePoolMetadataFileIn poolMetadataFile ->
          firstExceptT StakePoolCmdReadFileError
            . newExceptT
            $ readByteStringFile poolMetadataFile
        StakePoolMetadataURL urlText ->
          fetchURLToStakePoolCmdError $ getByteStringFromURL allSchemes $ L.urlToText urlText

    (_metadata, metadataHash) <-
      firstExceptT StakePoolCmdMetadataValidationError
        . hoistEither
        $ validateAndHashStakePoolMetadata metadataBytes

    case hashGoal of
      Cmd.CheckHash expectedHash
        | metadataHash /= expectedHash ->
            left $ StakePoolCmdHashMismatchError expectedHash metadataHash
        | otherwise -> liftIO $ putStrLn "Hashes match!"
      Cmd.HashToFile outFile -> writeOutput (Just outFile) metadataHash
      Cmd.HashToStdout -> writeOutput Nothing metadataHash
   where
    writeOutput :: Maybe (File () Out) -> Hash StakePoolMetadata -> ExceptT StakePoolCmdError IO ()
    writeOutput mOutFile metadataHash =
      case mOutFile of
        Nothing -> liftIO $ BS.putStrLn (serialiseToRawBytesHex metadataHash)
        Just (File fpath) ->
          handleIOExceptT (StakePoolCmdWriteFileError . FileIOError fpath) $
            BS.writeFile fpath (serialiseToRawBytesHex metadataHash)

    fetchURLToStakePoolCmdError
      :: ExceptT FetchURLError IO BS.ByteString -> ExceptT StakePoolCmdError IO BS.ByteString
    fetchURLToStakePoolCmdError = withExceptT StakePoolCmdFetchURLError
