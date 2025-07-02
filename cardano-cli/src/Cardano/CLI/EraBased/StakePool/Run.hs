{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- HLINT ignore "Redundant id" -}

module Cardano.CLI.EraBased.StakePool.Run
  ( runStakePoolCmds
  , runStakePoolIdCmd
  , runStakePoolMetadataHashCmd
  , runStakePoolRegistrationCertificateCmd
  , runStakePoolDeregistrationCertificateCmd
  )
where

import Cardano.Api
import Cardano.Api.Experimental
import Cardano.Api.Ledger qualified as L

import Cardano.CLI.Compatible.Exception
import Cardano.CLI.EraBased.StakePool.Command
import Cardano.CLI.EraBased.StakePool.Command qualified as Cmd
import Cardano.CLI.EraBased.StakePool.Internal.Metadata (carryHashChecks)
import Cardano.CLI.EraIndependent.Hash.Command qualified as Cmd
import Cardano.CLI.EraIndependent.Hash.Internal.Common
  ( allSchemes
  , getByteStringFromURL
  )
import Cardano.CLI.Orphan ()
import Cardano.CLI.Read (getVerificationKeyFromStakePoolVerificationKeySource)
import Cardano.CLI.Type.Common
import Cardano.CLI.Type.Error.HashCmdError (FetchURLError (..))
import Cardano.CLI.Type.Error.StakePoolCmdError
import Cardano.CLI.Type.Key (readVerificationKeyOrFile)

import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Function ((&))
import Data.Text.Encoding qualified as Text
import Vary qualified

runStakePoolCmds
  :: IsEra era
  => StakePoolCmds era
  -> CIO e ()
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
  :: forall era e
   . IsEra era
  => StakePoolRegistrationCertificateCmdArgs era
  -> CIO e ()
runStakePoolRegistrationCertificateCmd
  Cmd.StakePoolRegistrationCertificateCmdArgs
    { era
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
    shelleyBasedEraConstraints (convert era) $ do
      -- Pool verification key
      stakePoolVerKey <- getVerificationKeyFromStakePoolVerificationKeySource poolVerificationKeyOrFile
      let stakePoolId' = anyStakePoolVerificationKeyHash stakePoolVerKey

      -- VRF verification key
      vrfVerKey <-
        readVerificationKeyOrFile vrfVerificationKeyOrFile
      let vrfKeyHash' = verificationKeyHash vrfVerKey

      -- Pool reward account
      rwdStakeVerKey <-
        readVerificationKeyOrFile rewardStakeVerificationKeyOrFile
      let stakeCred = StakeCredentialByKey (verificationKeyHash rwdStakeVerKey)
          rewardAccountAddr = makeStakeAddress network stakeCred

      -- Pool owner(s)
      sPoolOwnerVkeys <-
        mapM
          readVerificationKeyOrFile
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
            createStakePoolRegistrationRequirements ledgerStakePoolParams
              :: StakePoolRegistrationRequirements era
          registrationCert = makeStakePoolRegistrationCertificate req

      mapM_ (fromExceptTCli . carryHashChecks) mMetadata
      fromEitherIOCli @(FileError ()) $
        writeLazyByteStringFile outFile $
          textEnvelopeToJSON (Just registrationCertDesc) registrationCert
   where
    registrationCertDesc :: TextEnvelopeDescr
    registrationCertDesc = "Stake Pool Registration Certificate"

createStakePoolRegistrationRequirements
  :: forall era
   . IsEra era
  => L.PoolParams
  -> StakePoolRegistrationRequirements era
createStakePoolRegistrationRequirements =
  StakePoolRegistrationRequirementsConwayOnwards (convert useEra)

runStakePoolDeregistrationCertificateCmd
  :: forall era e
   . IsEra era
  => StakePoolDeregistrationCertificateCmdArgs era
  -> CIO e ()
runStakePoolDeregistrationCertificateCmd
  Cmd.StakePoolDeregistrationCertificateCmdArgs
    { era
    , poolVerificationKeyOrFile
    , retireEpoch
    , outFile
    } =
    shelleyBasedEraConstraints (convert era) $ do
      -- Pool verification key
      stakePoolVerKey <- getVerificationKeyFromStakePoolVerificationKeySource poolVerificationKeyOrFile

      let stakePoolId' = anyStakePoolVerificationKeyHash stakePoolVerKey
          req :: StakePoolRetirementRequirements era = createStakePoolRetirementRequirements stakePoolId' retireEpoch
          retireCert = makeStakePoolRetirementCertificate req

      fromEitherIOCli @(FileError ()) $
        writeLazyByteStringFile outFile $
          textEnvelopeToJSON (Just retireCertDesc) retireCert
   where
    retireCertDesc :: TextEnvelopeDescr
    retireCertDesc = "Stake Pool Retirement Certificate"

createStakePoolRetirementRequirements
  :: IsEra era
  => PoolId
  -> L.EpochNo
  -> StakePoolRetirementRequirements era
createStakePoolRetirementRequirements =
  StakePoolRetirementRequirementsConwayOnwards (convert useEra)

runStakePoolIdCmd
  :: ()
  => StakePoolIdCmdArgs era
  -> CIO e ()
runStakePoolIdCmd
  Cmd.StakePoolIdCmdArgs
    { poolVerificationKeyOrFile
    , outputFormat
    , mOutFile
    } = do
    stakePoolVerKey <- getVerificationKeyFromStakePoolVerificationKeySource poolVerificationKeyOrFile
    let stakePoolKeyHash = anyStakePoolVerificationKeyHash stakePoolVerKey

    let output =
          outputFormat
            & ( id
                  . Vary.on (\FormatBech32 -> Text.encodeUtf8 . serialiseToBech32)
                  . Vary.on (\FormatHex -> serialiseToRawBytesHex)
                  $ Vary.exhaustiveCase
              )
            $ stakePoolKeyHash

    fromEitherIOCli @(FileError ()) $
      writeByteStringOutput mOutFile output

runStakePoolMetadataHashCmd
  :: ()
  => StakePoolMetadataHashCmdArgs era
  -> CIO e ()
runStakePoolMetadataHashCmd
  Cmd.StakePoolMetadataHashCmdArgs
    { poolMetadataSource
    , hashGoal
    } = do
    metadataBytes <-
      case poolMetadataSource of
        StakePoolMetadataFileIn poolMetadataFile ->
          fromEitherIOCli @(FileError ()) $
            readByteStringFile poolMetadataFile
        StakePoolMetadataURL urlText ->
          fromExceptTCli . fetchURLToStakePoolCmdError $ getByteStringFromURL allSchemes $ L.urlToText urlText

    (_metadata, metadataHash) <-
      fromEitherCli $
        validateAndHashStakePoolMetadata metadataBytes

    case hashGoal of
      Cmd.CheckHash expectedHash
        | metadataHash /= expectedHash ->
            throwCliError $ StakePoolCmdHashMismatchError expectedHash metadataHash
        | otherwise -> liftIO $ putStrLn "Hashes match!"
      Cmd.HashToFile outFile -> writeOutput (Just outFile) metadataHash
      Cmd.HashToStdout -> writeOutput Nothing metadataHash
   where
    writeOutput :: Maybe (File () Out) -> Hash StakePoolMetadata -> CIO e ()
    writeOutput mOutFile metadataHash = do
      let output = LBS.fromStrict $ serialiseToRawBytesHex metadataHash

      fromEitherIOCli @(FileError ()) $
        writeLazyByteStringOutput mOutFile output

    fetchURLToStakePoolCmdError
      :: ExceptT FetchURLError IO BS.ByteString -> ExceptT StakePoolCmdError IO BS.ByteString
    fetchURLToStakePoolCmdError = withExceptT StakePoolCmdFetchURLError
