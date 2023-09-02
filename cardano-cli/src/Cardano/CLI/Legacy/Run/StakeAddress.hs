{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Legacy.Run.StakeAddress
  ( runLegacyStakeAddressCmds
  ) where

import           Cardano.Api

import qualified Cardano.CLI.EraBased.Run.StakeAddress as EraBased
import           Cardano.CLI.Legacy.Commands.StakeAddress
import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Errors.ShelleyStakeAddressCmdError
import           Cardano.CLI.Types.Key (DelegationTarget (..), StakeIdentifier (..),
                   StakeVerifier (..), VerificationKeyOrFile)

import           Control.Monad.Trans.Except (ExceptT)

runLegacyStakeAddressCmds :: ()
  => LegacyStakeAddressCmds
  -> ExceptT ShelleyStakeAddressCmdError IO ()
runLegacyStakeAddressCmds = \case
  StakeAddressKeyGen fmt vk sk ->
    runLegacyStakeAddressKeyGenToFileCmd fmt vk sk
  StakeAddressKeyHash vk mOutputFp ->
    runLegacyStakeAddressKeyHashCmd vk mOutputFp
  StakeAddressBuild stakeVerifier nw mOutputFp ->
    runLegacyStakeAddressBuildCmd stakeVerifier nw mOutputFp
  StakeRegistrationCert anyEra stakeIdentifier mDeposit outputFp ->
    runLegacyStakeCredentialRegistrationCertCmd anyEra stakeIdentifier mDeposit outputFp
  StakeCredentialDelegationCert anyEra stakeIdentifier stkPoolVerKeyHashOrFp outputFp ->
    runLegacyStakeCredentialDelegationCertCmd anyEra stakeIdentifier stkPoolVerKeyHashOrFp outputFp
  StakeCredentialDeRegistrationCert anyEra stakeIdentifier mDeposit outputFp ->
    runLegacyStakeCredentialDeRegistrationCertCmd anyEra stakeIdentifier mDeposit outputFp

runLegacyStakeAddressKeyGenToFileCmd :: ()
  => KeyOutputFormat
  -> VerificationKeyFile Out
  -> SigningKeyFile Out
  -> ExceptT ShelleyStakeAddressCmdError IO ()
runLegacyStakeAddressKeyGenToFileCmd = EraBased.runLegacyStakeAddressKeyGenToFileCmd

runLegacyStakeAddressKeyHashCmd :: ()
  => VerificationKeyOrFile StakeKey
  -> Maybe (File () Out)
  -> ExceptT ShelleyStakeAddressCmdError IO ()
runLegacyStakeAddressKeyHashCmd = EraBased.runLegacyStakeAddressKeyHashCmd

runLegacyStakeAddressBuildCmd :: ()
  => StakeVerifier
  -> NetworkId
  -> Maybe (File () Out)
  -> ExceptT ShelleyStakeAddressCmdError IO ()
runLegacyStakeAddressBuildCmd = EraBased.runLegacyStakeAddressBuildCmd

runLegacyStakeCredentialRegistrationCertCmd :: ()
  => AnyShelleyBasedEra
  -> StakeIdentifier
  -> Maybe Lovelace -- ^ Deposit required in conway era
  -> File () Out
  -> ExceptT ShelleyStakeAddressCmdError IO ()
runLegacyStakeCredentialRegistrationCertCmd = EraBased.runLegacyStakeCredentialRegistrationCertCmd

runLegacyStakeCredentialDelegationCertCmd :: ()
  => AnyShelleyBasedEra
  -> StakeIdentifier
  -- ^ Delegator stake verification key, verification key file or script file.
  -> DelegationTarget
  -- ^ Delegatee stake pool verification key or verification key file or
  -- verification key hash.
  -> File () Out
  -> ExceptT ShelleyStakeAddressCmdError IO ()
runLegacyStakeCredentialDelegationCertCmd = EraBased.runLegacyStakeCredentialDelegationCertCmd

runLegacyStakeCredentialDeRegistrationCertCmd :: ()
  => AnyShelleyBasedEra
  -> StakeIdentifier
  -> Maybe Lovelace -- ^ Deposit required in conway era
  -> File () Out
  -> ExceptT ShelleyStakeAddressCmdError IO ()
runLegacyStakeCredentialDeRegistrationCertCmd = EraBased.runLegacyStakeCredentialDeRegistrationCertCmd
