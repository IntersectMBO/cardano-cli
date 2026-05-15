{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
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
import Cardano.Api.Experimental qualified as Exp
import Cardano.Api.Ledger qualified as L

import Cardano.CLI.Compatible.Exception
import Cardano.CLI.Compatible.StakeAddress.Command
import Cardano.CLI.Read
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
  => forall era e
   . ShelleyBasedEra era
  -> StakeIdentifier
  -> Maybe L.Coin
  -- ^ Deposit required in conway era
  -> File () Out
  -> CIO e ()
runStakeAddressRegistrationCertificateCmd sbe stakeIdentifier mDeposit oFp = do
  stakeCred <-
    getStakeCredentialFromIdentifier stakeIdentifier
  regCert <- createRegCert sbe stakeCred mDeposit
  fromEitherIOCli @(FileError ())
    $ writeLazyByteStringFile
      oFp
    $ shelleyBasedEraConstraints sbe
    $ textEnvelopeToJSON (Just regCertDesc) regCert
 where
  regCertDesc :: TextEnvelopeDescr
  regCertDesc = "Stake Address Registration Certificate"

  createRegCert
    :: ShelleyBasedEra era
    -> StakeCredential
    -> Maybe L.Coin
    -> CIO e (Exp.Certificate (ShelleyLedgerEra era))
  createRegCert sbe' sCred mDep =
    case sbe' of
      ShelleyBasedEraShelley -> pure $ shelleyToBabbageReg sCred
      ShelleyBasedEraAllegra -> pure $ shelleyToBabbageReg sCred
      ShelleyBasedEraMary -> pure $ shelleyToBabbageReg sCred
      ShelleyBasedEraAlonzo -> pure $ shelleyToBabbageReg sCred
      ShelleyBasedEraBabbage -> pure $ shelleyToBabbageReg sCred
      ShelleyBasedEraConway -> createRegistrationCertificate Exp.ConwayEra
      ShelleyBasedEraDijkstra -> createRegistrationCertificate Exp.DijkstraEra
   where
    createRegistrationCertificate
      :: Exp.IsEra era'
      => Exp.Era era'
      -> CIO e (Exp.Certificate (Exp.LedgerEra era'))
    createRegistrationCertificate era =
      case mDep of
        Nothing ->
          throwCliError @String $
            "Deposit required for stake address registration certificate in "
              <> prettyShow era
              <> " era"
        Just dep -> pure $ Exp.makeStakeAddressRegistrationCertificate sCred dep

-- | Pre-Conway stake address registration (no deposit).
shelleyToBabbageReg
  :: L.ShelleyEraTxCert (ShelleyLedgerEra era)
  => StakeCredential -> Exp.Certificate (ShelleyLedgerEra era)
shelleyToBabbageReg sCred =
  Exp.Certificate $ L.mkRegTxCert $ toShelleyStakeCredential sCred

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
  -> Exp.Certificate (ShelleyLedgerEra era)
createStakeDelegationCertificate sbe stakeCredential stakePoolHash = do
  case sbe of
    ShelleyBasedEraShelley ->
      shelleyToBabbage stakeCredential stakePoolHash
    ShelleyBasedEraAllegra ->
      shelleyToBabbage stakeCredential stakePoolHash
    ShelleyBasedEraMary ->
      shelleyToBabbage stakeCredential stakePoolHash
    ShelleyBasedEraAlonzo ->
      shelleyToBabbage stakeCredential stakePoolHash
    ShelleyBasedEraBabbage ->
      shelleyToBabbage stakeCredential stakePoolHash
    ShelleyBasedEraConway ->
      conwayOnwards stakeCredential stakePoolHash
    ShelleyBasedEraDijkstra ->
      conwayOnwards stakeCredential stakePoolHash
 where
  shelleyToBabbage scred sPoolHash =
    Exp.Certificate $
      L.mkDelegStakeTxCert (toShelleyStakeCredential scred) (toLedgerHash sPoolHash)
  conwayOnwards scred sPoolHash =
    Exp.Certificate $
      L.mkDelegTxCert
        (toShelleyStakeCredential scred)
        (L.DelegStake (toLedgerHash sPoolHash))
  toLedgerHash :: Hash StakePoolKey -> L.KeyHash L.StakePool
  toLedgerHash (StakePoolKeyHash poolStakeVKeyHash) = poolStakeVKeyHash
