{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Legacy.Run.StakePool
  ( runLegacyStakePoolCmds
  ) where

import           Cardano.Api
import           Cardano.Api.Shelley

import           Cardano.CLI.EraBased.Run.StakePool
import           Cardano.CLI.Legacy.Commands.StakePool
import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Errors.StakePoolCmdError
import           Cardano.CLI.Types.Key (VerificationKeyOrFile)
import qualified Cardano.Ledger.Slot as Shelley

import           Control.Monad.Trans.Except (ExceptT)

runLegacyStakePoolCmds :: ()
  => LegacyStakePoolCmds
  -> ExceptT StakePoolCmdError IO ()
runLegacyStakePoolCmds = \case
  StakePoolDeregistrationCertificateCmd anyEra sPvkeyFp retireEpoch outfp ->
    runLegacyStakePoolDeregistrationCertificateCmd anyEra sPvkeyFp retireEpoch outfp
  StakePoolIdCmd sPvkey outputFormat mOutFile ->
    runLegacyStakePoolIdCmd sPvkey outputFormat mOutFile
  StakePoolMetadataHashCmd poolMdFile mOutFile ->
    runLegacyStakePoolMetadataHashCmd poolMdFile mOutFile
  StakePoolRegistrationCertificateCmd anyEra sPvkey vrfVkey pldg pCost pMrgn rwdVerFp ownerVerFps relays mbMetadata network outfp ->
    runLegacyStakePoolRegistrationCertificateCmd anyEra sPvkey vrfVkey pldg pCost pMrgn rwdVerFp ownerVerFps relays mbMetadata network outfp

-- | Create a stake pool registration cert.
-- TODO: Metadata and more stake pool relay support to be
-- added in the future.
runLegacyStakePoolRegistrationCertificateCmd :: ()
  => EraInEon ShelleyBasedEra
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
runLegacyStakePoolRegistrationCertificateCmd = \case
  EraInEon sbe ->
    runStakePoolRegistrationCertificateCmd sbe

runLegacyStakePoolDeregistrationCertificateCmd :: ()
  => EraInEon ShelleyBasedEra
  -> VerificationKeyOrFile StakePoolKey
  -> Shelley.EpochNo
  -> File () Out
  -> ExceptT StakePoolCmdError IO ()
runLegacyStakePoolDeregistrationCertificateCmd = \case
  EraInEon sbe ->
    runStakePoolRetirementCertificateCmd sbe

runLegacyStakePoolIdCmd :: ()
  => VerificationKeyOrFile StakePoolKey
  -> IdOutputFormat
  -> Maybe (File () Out)
  -> ExceptT StakePoolCmdError IO ()
runLegacyStakePoolIdCmd =
  runStakePoolIdCmd

runLegacyStakePoolMetadataHashCmd :: ()
  => StakePoolMetadataFile In
  -> Maybe (File () Out)
  -> ExceptT StakePoolCmdError IO ()
runLegacyStakePoolMetadataHashCmd =
  runStakePoolMetadataHashCmd
