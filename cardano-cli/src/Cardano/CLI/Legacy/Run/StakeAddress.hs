{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Legacy.Run.StakeAddress
  ( runLegacyStakeAddressCmds
  ) where

import           Cardano.Api
import           Cardano.Api.Shelley

import           Cardano.CLI.EraBased.Run.StakeAddress
import           Cardano.CLI.Legacy.Commands.StakeAddress
import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Errors.StakeAddressCmdError
import           Cardano.CLI.Types.Key

import           Control.Monad.Trans.Except (ExceptT)

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
runLegacyStakeAddressKeyGenCmd =
  runStakeAddressKeyGenCmd

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
  => AnyShelleyBasedEra
  -> StakeIdentifier
  -> Maybe Lovelace -- ^ Deposit required in conway era
  -> File () Out
  -> ExceptT StakeAddressCmdError IO ()
runLegacyStakeAddressRegistrationCertificateCmd (AnyShelleyBasedEra sbe) =
  runStakeAddressRegistrationCertificateCmd sbe

runLegacyStakeAddresslDelegationCertificateCmd :: ()
  => AnyShelleyBasedEra
  -> StakeIdentifier
  -- ^ Delegator stake verification key, verification key file or script file.
  -> VerificationKeyOrHashOrFile StakePoolKey
  -- ^ Delegatee stake pool verification key or verification key file or
  -- verification key hash.
  -> File () Out
  -> ExceptT StakeAddressCmdError IO ()
runLegacyStakeAddresslDelegationCertificateCmd (AnyShelleyBasedEra sbe) =
  runStakeAddressStakeDelegationCertificateCmd sbe

runLegacyStakeAddressDeregistrationCertificateCmd :: ()
  => AnyShelleyBasedEra
  -> StakeIdentifier
  -> Maybe Lovelace -- ^ Deposit required in conway era
  -> File () Out
  -> ExceptT StakeAddressCmdError IO ()
runLegacyStakeAddressDeregistrationCertificateCmd (AnyShelleyBasedEra sbe) =
  runStakeAddressDeregistrationCertificateCmd sbe
