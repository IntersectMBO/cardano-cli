{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.CLI.Compatible.StakeAddress.Run
  ( runCompatibleStakeAddressCmds
  )
where

import Cardano.Api
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Shelley

import Cardano.CLI.Compatible.Exception
import Cardano.CLI.Compatible.StakeAddress.Command
import Cardano.CLI.Read
import Cardano.CLI.Type.Error.StakeAddressRegistrationError
import Cardano.CLI.Type.Key

runCompatibleStakeAddressCmds
  :: ()
  => CompatibleStakeAddressCmds era
  -> CIO e ()
runCompatibleStakeAddressCmds = \case
  CompatibleStakeAddressRegistrationCertificateCmd sbe stakeIdentifier mDeposit outputFp ->
    runStakeAddressRegistrationCertificateCmd sbe stakeIdentifier mDeposit outputFp
  CompatibleStakeAddressStakeDelegationCertificateCmd
    sbe
    stakeIdentifier
    stkPoolVerKeyHashOrFp
    outputFp ->
      runStakeAddressStakeDelegationCertificateCmd sbe stakeIdentifier stkPoolVerKeyHashOrFp outputFp

runStakeAddressRegistrationCertificateCmd
  :: ()
  => ShelleyBasedEra era
  -> StakeIdentifier
  -> Maybe Lovelace
  -- ^ Deposit required in conway era
  -> File () Out
  -> CIO e ()
runStakeAddressRegistrationCertificateCmd sbe stakeIdentifier mDeposit oFp = do
  stakeCred <-
    fromExceptTCli $
      getStakeCredentialFromIdentifier stakeIdentifier

  req <- createRegistrationCertRequirements sbe stakeCred mDeposit

  let regCert = makeStakeAddressRegistrationCertificate req

  fromEitherIOCli @_ @(FileError ()) $
    writeLazyByteStringFile oFp $
      shelleyBasedEraConstraints sbe $
        textEnvelopeToJSON (Just regCertDesc) regCert
 where
  regCertDesc :: TextEnvelopeDescr
  regCertDesc = "Stake Address Registration Certificate"

createRegistrationCertRequirements
  :: ()
  => ShelleyBasedEra era
  -> StakeCredential
  -> Maybe Lovelace
  -- ^ Deposit required in conway era
  -> CIO e (StakeAddressRequirements era)
createRegistrationCertRequirements sbe stakeCred mdeposit =
  case sbe of
    ShelleyBasedEraShelley ->
      return $ StakeAddrRegistrationPreConway ShelleyToBabbageEraShelley stakeCred
    ShelleyBasedEraAllegra ->
      return $ StakeAddrRegistrationPreConway ShelleyToBabbageEraAllegra stakeCred
    ShelleyBasedEraMary ->
      return $ StakeAddrRegistrationPreConway ShelleyToBabbageEraMary stakeCred
    ShelleyBasedEraAlonzo ->
      return $ StakeAddrRegistrationPreConway ShelleyToBabbageEraAlonzo stakeCred
    ShelleyBasedEraBabbage ->
      return $ StakeAddrRegistrationPreConway ShelleyToBabbageEraBabbage stakeCred
    ShelleyBasedEraConway ->
      case mdeposit of
        Nothing ->
          -- This case is made impossible by the parser, that distinguishes between Conway
          -- and pre-Conway.
          throwCliError StakeAddressRegistrationDepositRequired
        Just dep ->
          return $ StakeAddrRegistrationConway ConwayEraOnwardsConway dep stakeCred

runStakeAddressStakeDelegationCertificateCmd
  :: ()
  => ShelleyBasedEra era
  -> StakeIdentifier
  -- ^ Delegator stake verification key, verification key file or script file.
  -> VerificationKeyOrHashOrFile StakePoolKey
  -- ^ Delegatee stake pool verification key or verification key file or
  -- verification key hash.
  -> File () Out
  -> CIO e ()
runStakeAddressStakeDelegationCertificateCmd sbe stakeVerifier poolVKeyOrHashOrFile outFp =
  shelleyBasedEraConstraints sbe $ do
    poolStakeVKeyHash <-
      fromExceptTCli $
        readVerificationKeyOrHashOrFile AsStakePoolKey poolVKeyOrHashOrFile

    stakeCred <-
      fromExceptTCli $ getStakeCredentialFromIdentifier stakeVerifier

    let certificate = createStakeDelegationCertificate stakeCred poolStakeVKeyHash sbe

    fromEitherIOCli @_ @(FileError ()) $
      writeLazyByteStringFile outFp $
        textEnvelopeToJSON (Just @TextEnvelopeDescr "Stake Delegation Certificate") certificate

createStakeDelegationCertificate
  :: StakeCredential
  -> Hash StakePoolKey
  -> ShelleyBasedEra era
  -> Certificate era
createStakeDelegationCertificate stakeCredential (StakePoolKeyHash poolStakeVKeyHash) = do
  caseShelleyToBabbageOrConwayEraOnwards
    ( \w ->
        shelleyToBabbageEraConstraints w $
          ShelleyRelatedCertificate w $
            L.mkDelegStakeTxCert (toShelleyStakeCredential stakeCredential) poolStakeVKeyHash
    )
    ( \w ->
        conwayEraOnwardsConstraints w $
          ConwayCertificate w $
            L.mkDelegTxCert (toShelleyStakeCredential stakeCredential) (L.DelegStake poolStakeVKeyHash)
    )
