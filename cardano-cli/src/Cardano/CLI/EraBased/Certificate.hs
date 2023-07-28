{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.CLI.EraBased.Certificate
  ( EraBasedDelegationError(..)
  , runGovernanceDelegrationCertificate
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
  | EraBasedDRepReadError !(FileError InputDecodeError)
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
        $ shelleyToBabbageEraConstraints sTob
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
        $ conwayEraOnwardsConstraints cOnwards
        $ textEnvelopeToJSON description delegCert

toLedgerDelegatee
  :: StakeTarget era
  -> ExceptT EraBasedDelegationError IO (Ledger.Delegatee (Ledger.EraCrypto (ShelleyLedgerEra era)))
toLedgerDelegatee t =
  case t of
    TargetStakePool cOnwards keyOrHashOrFile -> do
      StakePoolKeyHash kHash
        <- lift (readVerificationKeyOrHashOrFile AsStakePoolKey keyOrHashOrFile)
             & onLeft (left . EraBasedDelegReadError)
      right $ Ledger.DelegStake $ conwayEraOnwardsConstraints cOnwards kHash

    TargetVotingDrep cOnwards keyOrHashOrFile -> do
      DRepKeyHash drepKeyHash <- firstExceptT EraBasedDRepReadError
                                   . newExceptT
                                   $ readVerificationKeyOrHashOrTextEnvFile AsDRepKey keyOrHashOrFile
      let drepCred = Ledger.DRepCredential $ Ledger.KeyHashObj drepKeyHash
      right $ Ledger.DelegVote $ conwayEraOnwardsConstraints cOnwards drepCred

    TargetVotingDrepAndStakePool cOnwards drepKeyOrHashOrFile  poolKeyOrHashOrFile -> do
      StakePoolKeyHash kHash
        <- lift (readVerificationKeyOrHashOrFile AsStakePoolKey poolKeyOrHashOrFile)
             & onLeft (left . EraBasedDelegReadError)
      DRepKeyHash drepKeyHash
        <- firstExceptT EraBasedDRepReadError
             . newExceptT
             $ readVerificationKeyOrHashOrTextEnvFile AsDRepKey drepKeyOrHashOrFile
      let drepCred = Ledger.DRepCredential $ Ledger.KeyHashObj drepKeyHash
      right $ Ledger.DelegStakeVote
                (conwayEraOnwardsConstraints cOnwards kHash)
                (conwayEraOnwardsConstraints cOnwards drepCred)
