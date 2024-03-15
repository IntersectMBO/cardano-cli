{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Legacy.Run.StakeAddress
  ( runLegacyStakeAddressCmds
  ) where

import           Cardano.Api
import           Cardano.Api.Ledger (Coin)
import           Cardano.Api.Shelley

import           Cardano.CLI.EraBased.Run.StakeAddress
import           Cardano.CLI.Legacy.Commands.StakeAddress
import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Errors.StakeAddressCmdError
import           Cardano.CLI.Types.Key

import           Control.Monad (void)

runLegacyStakeAddressCmds :: ()
  => LegacyStakeAddressCmds
  -> ExceptT StakeAddressCmdError IO ()
runLegacyStakeAddressCmds = \case
  StakeAddressKeyGenCmd fmt vk sk ->
    runLegacyStakeAddressKeyGenCmd fmt vk sk
  StakeAddressKeyHashCmd vk mOutputFp ->
    runLegacyStakeAddressKeyHashCmd vk mOutputFp
  StakeAddressBuildCmd stakeVerifier nw mOutputFp ->
    runLegacyStakeAddressBuildCmd stakeVerifier nw mOutputFp
  StakeAddressRegistrationCertificateCmd anyEra stakeIdentifier mDeposit outputFp ->
    runLegacyStakeAddressRegistrationCertificateCmd anyEra stakeIdentifier mDeposit outputFp
  StakeAddressDelegationCertificateCmd anyEra stakeIdentifier stkPoolVerKeyHashOrFp outputFp ->
    runLegacyStakeAddresslDelegationCertificateCmd anyEra stakeIdentifier stkPoolVerKeyHashOrFp outputFp
  StakeAddressDeregistrationCertificateCmd anyEra stakeIdentifier mDeposit outputFp ->
    runLegacyStakeAddressDeregistrationCertificateCmd anyEra stakeIdentifier mDeposit outputFp

runLegacyStakeAddressKeyGenCmd :: ()
  => KeyOutputFormat
  -> VerificationKeyFile Out
  -> SigningKeyFile Out
  -> ExceptT StakeAddressCmdError IO ()
runLegacyStakeAddressKeyGenCmd vk sk =
  void <$> runStakeAddressKeyGenCmd vk sk

runLegacyStakeAddressKeyHashCmd :: ()
  => VerificationKeyOrFile StakeKey
  -> Maybe (File () Out)
  -> ExceptT StakeAddressCmdError IO ()
runLegacyStakeAddressKeyHashCmd =
  runStakeAddressKeyHashCmd

runLegacyStakeAddressBuildCmd :: ()
  => StakeVerifier
  -> NetworkId
  -> Maybe (File () Out)
  -> ExceptT StakeAddressCmdError IO ()
runLegacyStakeAddressBuildCmd =
  runStakeAddressBuildCmd

runLegacyStakeAddressRegistrationCertificateCmd :: ()
  => EraInEon ShelleyBasedEra
  -> StakeIdentifier
  -> Maybe Coin -- ^ Deposit required in conway era
  -> File () Out
  -> ExceptT StakeAddressCmdError IO ()
runLegacyStakeAddressRegistrationCertificateCmd (EraInEon sbe) =
  runStakeAddressRegistrationCertificateCmd sbe

runLegacyStakeAddresslDelegationCertificateCmd :: ()
  => EraInEon ShelleyBasedEra
  -> StakeIdentifier
  -- ^ Delegator stake verification key, verification key file or script file.
  -> VerificationKeyOrHashOrFile StakePoolKey
  -- ^ Delegatee stake pool verification key or verification key file or
  -- verification key hash.
  -> File () Out
  -> ExceptT StakeAddressCmdError IO ()
runLegacyStakeAddresslDelegationCertificateCmd (EraInEon sbe) =
  runStakeAddressStakeDelegationCertificateCmd sbe

runLegacyStakeAddressDeregistrationCertificateCmd :: ()
  => EraInEon ShelleyBasedEra
  -> StakeIdentifier
  -> Maybe Coin -- ^ Deposit required in conway era
  -> File () Out
  -> ExceptT StakeAddressCmdError IO ()
runLegacyStakeAddressDeregistrationCertificateCmd (EraInEon sbe) =
  runStakeAddressDeregistrationCertificateCmd sbe
