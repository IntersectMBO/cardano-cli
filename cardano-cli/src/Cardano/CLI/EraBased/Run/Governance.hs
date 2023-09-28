{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.EraBased.Run.Governance
  ( runGovernanceMIRCertificatePayStakeAddrs
  , runGovernanceMIRCertificateTransfer
  , runGovernanceDRepKeyGen
  ) where

import Cardano.Api
import Cardano.Api.Ledger qualified as Ledger
import Cardano.Api.Shelley

import Cardano.CLI.Types.Common
import Cardano.CLI.Types.Errors.GovernanceCmdError
import Cardano.Ledger.Shelley.TxBody qualified as Shelley

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Except.Extra
import Data.Map.Strict qualified as Map

runGovernanceMIRCertificatePayStakeAddrs
  :: ShelleyToBabbageEra era
  -> Shelley.MIRPot
  -> [StakeAddress]
  -- ^ Stake addresses
  -> [Lovelace]
  -- ^ Corresponding reward amounts (same length)
  -> File () Out
  -> ExceptT GovernanceCmdError IO ()
runGovernanceMIRCertificatePayStakeAddrs w mirPot sAddrs rwdAmts oFp = do
  unless (length sAddrs == length rwdAmts) $
    left $
      GovernanceCmdMIRCertificateKeyRewardMistmach
        (unFile oFp)
        (length sAddrs)
        (length rwdAmts)

  let sCreds = map stakeAddressCredential sAddrs
      mirTarget =
        Ledger.StakeAddressesMIR $
          Map.fromList
            [ (toShelleyStakeCredential scred, Ledger.toDeltaCoin (toShelleyLovelace rwdAmt))
            | (scred, rwdAmt) <- zip sCreds rwdAmts
            ]
  let mirCert =
        makeMIRCertificate $
          MirCertificateRequirements w mirPot $
            shelleyToBabbageEraConstraints w mirTarget

  firstExceptT GovernanceCmdTextEnvWriteError
    . newExceptT
    $ shelleyBasedEraConstraints (shelleyToBabbageEraToShelleyBasedEra w)
    $ writeLazyByteStringFile oFp
    $ textEnvelopeToJSON (Just mirCertDesc) mirCert
 where
  mirCertDesc :: TextEnvelopeDescr
  mirCertDesc = "Move Instantaneous Rewards Certificate"

runGovernanceMIRCertificateTransfer
  :: ShelleyToBabbageEra era
  -> Lovelace
  -> File () Out
  -> TransferDirection
  -> ExceptT GovernanceCmdError IO ()
runGovernanceMIRCertificateTransfer w ll oFp direction = do
  let mirTarget = Ledger.SendToOppositePotMIR (toShelleyLovelace ll)

  let mirCert =
        makeMIRCertificate $
          case direction of
            TransferToReserves -> MirCertificateRequirements w Ledger.TreasuryMIR mirTarget
            TransferToTreasury -> MirCertificateRequirements w Ledger.ReservesMIR mirTarget

  firstExceptT GovernanceCmdTextEnvWriteError
    . newExceptT
    $ shelleyBasedEraConstraints (shelleyToBabbageEraToShelleyBasedEra w)
    $ writeLazyByteStringFile oFp
    $ textEnvelopeToJSON (Just $ mirCertDesc direction) mirCert
 where
  mirCertDesc :: TransferDirection -> TextEnvelopeDescr
  mirCertDesc TransferToTreasury = "MIR Certificate Send To Treasury"
  mirCertDesc TransferToReserves = "MIR Certificate Send To Reserves"

runGovernanceDRepKeyGen
  :: ConwayEraOnwards era
  -> VerificationKeyFile Out
  -> SigningKeyFile Out
  -> ExceptT GovernanceCmdError IO ()
runGovernanceDRepKeyGen _w vkeyPath skeyPath = firstExceptT GovernanceCmdWriteFileError $ do
  skey <- liftIO $ generateSigningKey AsDRepKey
  let vkey = getVerificationKey skey
  newExceptT $ writeLazyByteStringFile skeyPath (textEnvelopeToJSON (Just skeyDesc) skey)
  newExceptT $ writeLazyByteStringFile vkeyPath (textEnvelopeToJSON (Just vkeyDesc) vkey)
 where
  skeyDesc :: TextEnvelopeDescr
  skeyDesc = "Delegate Representative Signing Key"
  vkeyDesc :: TextEnvelopeDescr
  vkeyDesc = "Delegate Representative Verification Key"
