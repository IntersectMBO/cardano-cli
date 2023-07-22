{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.CLI.EraBased.Run.Certificate
  ( runGovernanceDelegrationCertificate
  , runGovernanceMIRCertificatePayStakeAddrs
  , runGovernanceMIRCertificateTransfer
  , runGovernanceGenesisKeyDelegationCertificate
  ) where

import           Cardano.Api
import qualified Cardano.Api.Ledger as Ledger
import           Cardano.Api.Shelley

import           Cardano.CLI.EraBased.Constraints
import           Cardano.CLI.Run.Legacy.StakeAddress
import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Key
import qualified Cardano.Ledger.Shelley.TxBody as Shelley

import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra
import           Data.Function
import qualified Data.Map.Strict as Map

data EraBasedDelegationError
  = EraBasedDelegReadError !(FileError InputDecodeError)
  | EraBasedCredentialError !ShelleyStakeAddressCmdError -- TODO: Refactor. We shouldn't be using legacy error types
  | EraBasedCertificateWriteFileError !(FileError ())
  | EraBasedDelegationGenericError -- TODO Delete and replace with more specific errors

runGovernanceDelegrationCertificate
  :: StakeIdentifier
  -> AnyDelegationTarget
  -> File () Out
  -> ExceptT EraBasedDelegationError IO ()
runGovernanceDelegrationCertificate stakeIdentifier delegationTarget outFp = do
  stakeCred <-
    getStakeCredentialFromIdentifier stakeIdentifier
      & firstExceptT EraBasedCredentialError

  case delegationTarget of
    ShelleyToBabbageDelegTarget sTob stakePool -> do
      poolId <- lift (readVerificationKeyOrHashOrFile AsStakePoolKey stakePool)
                  & onLeft (left . EraBasedDelegReadError)
      let req = StakeDelegationRequirementsPreConway sTob stakeCred poolId
          delegCert = makeStakeAddressDelegationCertificate req
          description = Just @TextEnvelopeDescr "Stake Address Delegation Certificate"
      firstExceptT EraBasedCertificateWriteFileError
        . newExceptT
        $ writeLazyByteStringFile outFp
        $ obtainIsShelleyBasedEraShelleyToBabbage sTob
        $ textEnvelopeToJSON description delegCert

    ConwayOnwardDelegTarget cOnwards target -> do
      delegatee <- toLedgerDelegatee target
      let req = StakeDelegationRequirementsConwayOnwards cOnwards stakeCred delegatee
          delegCert = makeStakeAddressDelegationCertificate req
          -- TODO: Conway era - update description to say if its delegating voting stake or "regular" stake
          description = Just @TextEnvelopeDescr "Stake Address Delegation Certificate"
      firstExceptT EraBasedCertificateWriteFileError
        . newExceptT
        $ writeLazyByteStringFile outFp
        $ obtainIsShelleyBasedEraConwayOnwards cOnwards
        $ textEnvelopeToJSON description delegCert

toLedgerDelegatee
  :: StakeTarget era
  -> ExceptT EraBasedDelegationError IO (Ledger.Delegatee (Ledger.EraCrypto (ShelleyLedgerEra era)))
toLedgerDelegatee t =
  case t of
    TargetStakePool cOnwards vk -> do
      StakePoolKeyHash kHash
        <- lift (readVerificationKeyOrHashOrFile AsStakePoolKey vk)
             & onLeft (left . EraBasedDelegReadError)

      right $ Ledger.DelegStake $ obtainIsShelleyBasedEraConwayOnwards cOnwards kHash
    TargetVotingDrep _ -> error "TODO: Conway era - Ledger.DelegVote"
    TargetVotingDrepAndStakePool _ -> error "TODO: Conway era - Ledger.DelegStakeVote"




runGovernanceMIRCertificatePayStakeAddrs :: forall era. ()
  => Ledger.EraCrypto (ShelleyLedgerEra era) ~ Ledger.StandardCrypto
  => ShelleyToBabbageEra era
  -> Shelley.MIRPot
  -> [StakeAddress] -- ^ Stake addresses
  -> [Lovelace]     -- ^ Corresponding reward amounts (same length)
  -> File () Out
  -> ExceptT EraBasedDelegationError IO ()
runGovernanceMIRCertificatePayStakeAddrs w mirPot sAddrs rwdAmts oFp =
  obtainIsShelleyBasedEraShelleyToBabbage w $ do
    unless (length sAddrs == length rwdAmts) $
      left EraBasedDelegationGenericError
        -- TODO throw specific error:
        -- (GovernanceCmdMIRCertificateKeyRewardMistmach)
        --       (unFile oFp) (length sAddrs) (length rwdAmts)

    let sCreds  = map stakeAddressCredential sAddrs
        mirTarget = Ledger.StakeAddressesMIR
                      $ Map.fromList [ (toShelleyStakeCredential scred, Ledger.toDeltaCoin (toShelleyLovelace rwdAmt))
                                      | (scred, rwdAmt) <- zip sCreds rwdAmts
                                      ]
        mirReq = MirCertificateRequirements w mirPot (obtainEraCryptoConstraints (shelleyBasedEra @era) mirTarget)
        mirCert = makeMIRCertificate mirReq

    firstExceptT (const EraBasedDelegationGenericError)
      . newExceptT
      $ writeLazyByteStringFile oFp
      $ textEnvelopeToJSON (Just mirCertDesc) mirCert

  where
    mirCertDesc :: TextEnvelopeDescr
    mirCertDesc = "Move Instantaneous Rewards Certificate"


runGovernanceMIRCertificateTransfer :: forall era. ()
  => Ledger.EraCrypto (ShelleyLedgerEra era) ~ Ledger.StandardCrypto
  => ShelleyToBabbageEra era
  -> Lovelace
  -> File () Out
  -> TransferDirection
  -> ExceptT EraBasedDelegationError IO ()
runGovernanceMIRCertificateTransfer w ll oFp direction =
  obtainIsShelleyBasedEraShelleyToBabbage w $ do
    let mirTarget = Ledger.SendToOppositePotMIR (toShelleyLovelace ll)
    let mirReq mirPot = MirCertificateRequirements w mirPot mirTarget

    mirCert <-
      case direction of
        TransferToReserves -> return $ makeMIRCertificate $ mirReq Ledger.TreasuryMIR
        TransferToTreasury -> return $ makeMIRCertificate $ mirReq Ledger.ReservesMIR

    firstExceptT (const EraBasedDelegationGenericError)
      . newExceptT
      $ writeLazyByteStringFile oFp
      $ textEnvelopeToJSON (Just $ mirCertDesc direction) mirCert

    where
      mirCertDesc :: TransferDirection -> TextEnvelopeDescr
      mirCertDesc TransferToTreasury = "MIR Certificate Send To Treasury"
      mirCertDesc TransferToReserves = "MIR Certificate Send To Reserves"

runGovernanceGenesisKeyDelegationCertificate :: forall era. ()
  => ShelleyToBabbageEra era
  -> VerificationKeyOrHashOrFile GenesisKey
  -> VerificationKeyOrHashOrFile GenesisDelegateKey
  -> VerificationKeyOrHashOrFile VrfKey
  -> File () Out
  -> ExceptT EraBasedDelegationError IO ()
runGovernanceGenesisKeyDelegationCertificate w genVkOrHashOrFp genDelVkOrHashOrFp vrfVkOrHashOrFp oFp =
  obtainIsShelleyBasedEraShelleyToBabbage w $ do
    genesisVkHash <- firstExceptT (const EraBasedDelegationGenericError)
      . newExceptT
      $ readVerificationKeyOrHashOrTextEnvFile AsGenesisKey genVkOrHashOrFp
    genesisDelVkHash <-firstExceptT (const EraBasedDelegationGenericError)
      . newExceptT
      $ readVerificationKeyOrHashOrTextEnvFile AsGenesisDelegateKey genDelVkOrHashOrFp
    vrfVkHash <- firstExceptT (const EraBasedDelegationGenericError)
      . newExceptT
      $ readVerificationKeyOrHashOrFile AsVrfKey vrfVkOrHashOrFp

    let req = GenesisKeyDelegationRequirements w genesisVkHash genesisDelVkHash vrfVkHash
        genKeyDelegCert = makeGenesisKeyDelegationCertificate req

    firstExceptT (const EraBasedDelegationGenericError)
      . newExceptT
      $ writeLazyByteStringFile oFp
      $ textEnvelopeToJSON (Just genKeyDelegCertDesc) genKeyDelegCert
  where
    genKeyDelegCertDesc :: TextEnvelopeDescr
    genKeyDelegCertDesc = "Genesis Key Delegation Certificate"
