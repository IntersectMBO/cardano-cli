{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Legacy.Run.StakePool
  ( runLegacyStakePoolCmds
  ) where

import           Cardano.Api
import           Cardano.Api.Shelley

import qualified Cardano.CLI.EraBased.Commands.StakePool as Cmd
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
  StakePoolDeregistrationCertificateCmd anyEra sPvkeyFp retireEpoch outFile ->
    runLegacyStakePoolDeregistrationCertificateCmd anyEra sPvkeyFp retireEpoch outFile
  StakePoolIdCmd poolVerificationKeyOrFile outputFormat mOutFile ->
    runLegacyStakePoolIdCmd poolVerificationKeyOrFile outputFormat mOutFile
  StakePoolMetadataHashCmd poolMdFile mOutFile ->
    runLegacyStakePoolMetadataHashCmd poolMdFile mOutFile
  StakePoolRegistrationCertificateCmd anyEra poolVerificationKeyOrFile vrfVkey poolPledge pCost pMrgn rwdVerFp ownerVerFps relays mbMetadata network outFile ->
    runLegacyStakePoolRegistrationCertificateCmd anyEra poolVerificationKeyOrFile vrfVkey poolPledge pCost pMrgn rwdVerFp ownerVerFps relays mbMetadata network outFile

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
runLegacyStakePoolRegistrationCertificateCmd
    inSbe
    poolVerificationKeyOrFile
    vrfVerificationKeyOrFile
    poolPledge
    poolCost
    poolMargin
    rewardStakeVerificationKeyOrFile
    ownerStakeVerificationKeyOrFiles
    relays
    mbMetadata
    network
    outFile =
  case inSbe of
    EraInEon sbe ->
      runStakePoolRegistrationCertificateCmd $
        Cmd.StakePoolRegistrationCertificateCmdArgs
          sbe
          poolVerificationKeyOrFile
          vrfVerificationKeyOrFile
          poolPledge
          poolCost
          poolMargin
          rewardStakeVerificationKeyOrFile
          ownerStakeVerificationKeyOrFiles
          relays
          mbMetadata
          network
          outFile

runLegacyStakePoolDeregistrationCertificateCmd :: ()
  => EraInEon ShelleyBasedEra
  -> VerificationKeyOrFile StakePoolKey
  -> Shelley.EpochNo
  -> File () Out
  -> ExceptT StakePoolCmdError IO ()
runLegacyStakePoolDeregistrationCertificateCmd inSbe poolVerificationKeyOrFile retireEpoch outFile =
  case inSbe of
    EraInEon sbe ->
      runStakePoolDeregistrationCertificateCmd $
        Cmd.StakePoolDeregistrationCertificateCmdArgs
          sbe
          poolVerificationKeyOrFile
          retireEpoch
          outFile

runLegacyStakePoolIdCmd :: ()
  => VerificationKeyOrFile StakePoolKey
  -> IdOutputFormat
  -> Maybe (File () Out)
  -> ExceptT StakePoolCmdError IO ()
runLegacyStakePoolIdCmd poolVerificationKeyOrFile outputFormat mOutFile =
  runStakePoolIdCmd $
    Cmd.StakePoolIdCmdArgs
      poolVerificationKeyOrFile
      outputFormat
      mOutFile

runLegacyStakePoolMetadataHashCmd :: ()
  => StakePoolMetadataFile In
  -> Maybe (File () Out)
  -> ExceptT StakePoolCmdError IO ()
runLegacyStakePoolMetadataHashCmd poolMetadataFile mOutFile =
  runStakePoolMetadataHashCmd $
    Cmd.StakePoolMetadataHashCmdArgs
      poolMetadataFile
      mOutFile
