{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.CLI.Compatible.StakePool.Run
  ( runCompatibleStakePoolCmds
  )
where

import Cardano.Api.Ledger qualified as L
import Cardano.Api.Shelley

import Cardano.CLI.Compatible.Exception
import Cardano.CLI.Compatible.StakePool.Command
import Cardano.CLI.EraBased.StakePool.Internal.Metadata
import Cardano.CLI.Read
  ( getVerificationKeyFromStakePoolVerificationKeySource
  )
import Cardano.CLI.Type.Common
import Cardano.CLI.Type.Error.StakePoolCmdError
import Cardano.CLI.Type.Key (readVerificationKeyOrFile)

import Control.Monad

runCompatibleStakePoolCmds
  :: ()
  => CompatibleStakePoolCmds era
  -> CIO e ()
runCompatibleStakePoolCmds = \case
  CompatibleStakePoolRegistrationCertificateCmd args -> runStakePoolRegistrationCertificateCmd args

runStakePoolRegistrationCertificateCmd
  :: ()
  => CompatibleStakePoolRegistrationCertificateCmdArgs era
  -> CIO e ()
runStakePoolRegistrationCertificateCmd
  CompatibleStakePoolRegistrationCertificateCmdArgs
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
        fromExceptTCli $
          firstExceptT StakePoolCmdReadKeyFileError $
            readVerificationKeyOrFile vrfVerificationKeyOrFile
      let vrfKeyHash' = verificationKeyHash vrfVerKey

      -- Pool reward account
      rwdStakeVerKey <-
        fromExceptTCli $
          firstExceptT StakePoolCmdReadKeyFileError $
            readVerificationKeyOrFile rewardStakeVerificationKeyOrFile
      let stakeCred = StakeCredentialByKey (verificationKeyHash rwdStakeVerKey)
          rewardAccountAddr = makeStakeAddress network stakeCred

      -- Pool owner(s)
      sPoolOwnerVkeys <-
        forM ownerStakeVerificationKeyOrFiles $
          fromExceptTCli
            . firstExceptT StakePoolCmdReadKeyFileError
            . readVerificationKeyOrFile
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

      mapM_ (fromExceptTCli . carryHashChecks) mMetadata

      fromExceptTCli
        . firstExceptT StakePoolCmdWriteFileError
        . newExceptT
        . writeLazyByteStringFile outFile
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
