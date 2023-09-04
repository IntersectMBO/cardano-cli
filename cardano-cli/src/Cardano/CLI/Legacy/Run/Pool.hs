{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Legacy.Run.Pool
  ( runLegacyPoolCmds
  ) where

import           Cardano.Api
import           Cardano.Api.Shelley

import           Cardano.CLI.EraBased.Run.Pool
import           Cardano.CLI.Legacy.Commands.Pool
import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Errors.ShelleyPoolCmdError
import           Cardano.CLI.Types.Key (VerificationKeyOrFile)
import qualified Cardano.Ledger.Slot as Shelley

import           Control.Monad.Trans.Except (ExceptT)

runLegacyPoolCmds :: ()
  => LegacyPoolCmds
  -> ExceptT ShelleyPoolCmdError IO ()
runLegacyPoolCmds = \case
  PoolRegistrationCert anyEra sPvkey vrfVkey pldg pCost pMrgn rwdVerFp ownerVerFps relays mbMetadata network outfp ->
    runLegacyStakePoolRegistrationCertCmd anyEra sPvkey vrfVkey pldg pCost pMrgn rwdVerFp ownerVerFps relays mbMetadata network outfp
  PoolRetirementCert anyEra sPvkeyFp retireEpoch outfp ->
    runLegacyStakePoolRetirementCertCmd anyEra sPvkeyFp retireEpoch outfp
  PoolGetId sPvkey outputFormat mOutFile ->
    runLegacyPoolIdCmd sPvkey outputFormat mOutFile
  PoolMetadataHash poolMdFile mOutFile ->
    runLegacyPoolMetadataHashCmd poolMdFile mOutFile

-- | Create a stake pool registration cert.
-- TODO: Metadata and more stake pool relay support to be
-- added in the future.
runLegacyStakePoolRegistrationCertCmd :: ()
  => AnyShelleyBasedEra
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
runLegacyStakePoolRegistrationCertCmd = runStakePoolRegistrationCertCmd

runLegacyStakePoolRetirementCertCmd :: ()
  => AnyShelleyBasedEra
  -> VerificationKeyOrFile StakePoolKey
  -> Shelley.EpochNo
  -> File () Out
  -> ExceptT ShelleyPoolCmdError IO ()
runLegacyStakePoolRetirementCertCmd = runStakePoolRetirementCertCmd

runLegacyPoolIdCmd :: ()
  => VerificationKeyOrFile StakePoolKey
  -> IdOutputFormat
  -> Maybe (File () Out)
  -> ExceptT ShelleyPoolCmdError IO ()
runLegacyPoolIdCmd = runPoolIdCmd

runLegacyPoolMetadataHashCmd :: ()
  => StakePoolMetadataFile In
  -> Maybe (File () Out)
  -> ExceptT ShelleyPoolCmdError IO ()
runLegacyPoolMetadataHashCmd = runPoolMetadataHashCmd
