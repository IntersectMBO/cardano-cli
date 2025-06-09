{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.CLI.Compatible.StakeAddress.Run
  ( runCompatibleStakeAddressCmds
  )
where

import Cardano.Api
import Cardano.Api.Ledger qualified as L

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
  -> Maybe (Featured ConwayEraOnwards era Lovelace)
  -- ^ Deposit required in conway era
  -> File () Out
  -> CIO e ()
runStakeAddressRegistrationCertificateCmd sbe stakeIdentifier mDeposit oFp = do
  stakeCred <-
    getStakeCredentialFromIdentifier stakeIdentifier

  req <- createRegistrationCertRequirements sbe stakeCred mDeposit

  let regCert = makeStakeAddressRegistrationCertificate req

  fromEitherIOCli @(FileError ())
    $ writeLazyByteStringFile
      oFp
    $ shelleyBasedEraConstraints sbe
    $ textEnvelopeToJSON (Just regCertDesc) regCert
 where
  regCertDesc :: TextEnvelopeDescr
  regCertDesc = "Stake Address Registration Certificate"

createRegistrationCertRequirements
  :: ()
  => ShelleyBasedEra era
  -> StakeCredential
  -> Maybe (Featured ConwayEraOnwards era Lovelace)
  -- ^ Deposit required in conway era
  -> CIO e (StakeAddressRequirements era)
createRegistrationCertRequirements sbe stakeCred mDeposit =
  caseShelleyToBabbageOrConwayEraOnwards
    (\stb -> pure $ StakeAddrRegistrationPreConway stb stakeCred)
    ( \ceo -> do
        case mDeposit of
          Nothing ->
            -- This case is made impossible by the parser, that distinguishes between Conway
            -- and pre-Conway.
            throwCliError StakeAddressRegistrationDepositRequired
          Just (Featured _ dep) ->
            pure $ StakeAddrRegistrationConway ceo dep stakeCred
    )
    sbe

runStakeAddressStakeDelegationCertificateCmd
  :: ()
  => ShelleyBasedEra era
  -> StakeIdentifier
  -- ^ Delegator stake verification key, verification key file or script file.
  -> StakePoolKeyHashSource
  -- ^ Delegatee stake pool verification key or verification key file or
  -- verification key hash.
  -> File () Out
  -> CIO e ()
runStakeAddressStakeDelegationCertificateCmd sbe stakeVerifier poolVKeyOrHashOrFile outFp =
  shelleyBasedEraConstraints sbe $ do
    poolStakeVKeyHash <- getHashFromStakePoolKeyHashSource poolVKeyOrHashOrFile

    stakeCred <- getStakeCredentialFromIdentifier stakeVerifier

    let certificate =
          createStakeDelegationCertificate sbe stakeCred poolStakeVKeyHash

    fromEitherIOCli @(FileError ()) $
      writeLazyByteStringFile outFp $
        textEnvelopeToJSON (Just @TextEnvelopeDescr "Stake Delegation Certificate") certificate

createStakeDelegationCertificate
  :: ShelleyBasedEra era
  -> StakeCredential
  -> Hash StakePoolKey
  -> Certificate era
createStakeDelegationCertificate sbe stakeCredential stakePoolHash = do
  caseShelleyToBabbageOrConwayEraOnwards
    ( \w ->
        shelleyToBabbageEraConstraints w $
          ShelleyRelatedCertificate w $
            L.mkDelegStakeTxCert (toShelleyStakeCredential stakeCredential) (toLedgerHash stakePoolHash)
    )
    ( \w ->
        conwayEraOnwardsConstraints w $
          ConwayCertificate w $
            L.mkDelegTxCert
              (toShelleyStakeCredential stakeCredential)
              (L.DelegStake (toLedgerHash stakePoolHash))
    )
    sbe
 where
  toLedgerHash :: Hash StakePoolKey -> L.KeyHash L.StakePool
  toLedgerHash (StakePoolKeyHash poolStakeVKeyHash) = poolStakeVKeyHash
