{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.CLI.EraBased.Certificate
  ( runGovernanceDelegrationCertificate
  ) where

import           Cardano.Api
import qualified Cardano.Api.Ledger as Ledger
import           Cardano.Api.Shelley

import           Cardano.CLI.Run.Legacy.StakeAddress
import           Cardano.CLI.Types.Key

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra
import           Data.Function

data EraBasedDelegationError
  = EraBasedDelegReadError !(FileError InputDecodeError)
  | EraBasedCredentialError !ShelleyStakeAddressCmdError -- TODO: Refactor. We shouldn't be using legacy error types
  | EraBasedCertificateWriteFileError !(FileError ())

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




obtainIsShelleyBasedEraShelleyToBabbage
  :: ShelleyToBabbageEra era
  -> (IsShelleyBasedEra era => a)
  -> a
obtainIsShelleyBasedEraShelleyToBabbage ShelleyToBabbageEraShelley f = f
obtainIsShelleyBasedEraShelleyToBabbage ShelleyToBabbageEraAllegra f = f
obtainIsShelleyBasedEraShelleyToBabbage ShelleyToBabbageEraMary f = f
obtainIsShelleyBasedEraShelleyToBabbage ShelleyToBabbageEraAlonzo f = f
obtainIsShelleyBasedEraShelleyToBabbage ShelleyToBabbageEraBabbage f = f

obtainIsShelleyBasedEraConwayOnwards
  :: ConwayEraOnwards era
  -> ((IsShelleyBasedEra era, Ledger.EraCrypto (ShelleyLedgerEra era) ~ Ledger.StandardCrypto) => a)
  -> a
obtainIsShelleyBasedEraConwayOnwards ConwayEraOnwardsConway f = f
