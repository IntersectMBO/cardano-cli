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

import Cardano.Api hiding
  ( Certificate
  , makeStakeAddressRegistrationCertificate
  )
import Cardano.Api.Compatible.Certificate
import Cardano.Api.Experimental qualified as Exp
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
  :: forall era e
   . ShelleyBasedEra era
  -> StakeIdentifier
  -> Maybe (Featured ConwayEraOnwards era Lovelace)
  -- ^ Deposit required in conway era
  -> File () Out
  -> CIO e ()
runStakeAddressRegistrationCertificateCmd sbe stakeIdentifier mDeposit oFp = do
  stakeCred <-
    getStakeCredentialFromIdentifier stakeIdentifier

  case createRegistrationCertRequirements sbe stakeCred mDeposit of
    Right req -> do
      let regCert =
            shelleyBasedEraConstraints sbe $ makeStakeAddressRegistrationCertificate req
              :: Exp.Certificate (ShelleyLedgerEra era)

      fromEitherIOCli @(FileError ())
        $ writeLazyByteStringFile
          oFp
        $ shelleyBasedEraConstraints sbe
        $ textEnvelopeToJSON (Just regCertDesc) regCert
    Left err -> throwCliError err
 where
  regCertDesc :: TextEnvelopeDescr
  regCertDesc = "Stake Address Registration Certificate"

createRegistrationCertRequirements
  :: ()
  => ShelleyBasedEra era
  -> StakeCredential
  -> Maybe (Featured ConwayEraOnwards era Lovelace)
  -- ^ Deposit required in conway era onwards
  -> Either StakeAddressRegistrationError (StakeRegistrationRequirements era)
createRegistrationCertRequirements sbe stakeCred mDeposit =
  case sbe of
    ShelleyBasedEraShelley -> pure stakeCred
    ShelleyBasedEraAllegra -> pure stakeCred
    ShelleyBasedEraMary -> pure stakeCred
    ShelleyBasedEraAlonzo -> pure stakeCred
    ShelleyBasedEraBabbage -> pure stakeCred
    ShelleyBasedEraConway -> hasDeposit mDeposit
    ShelleyBasedEraDijkstra -> hasDeposit mDeposit
 where
  hasDeposit
    :: Maybe (Featured ConwayEraOnwards era Lovelace)
    -> Either StakeAddressRegistrationError StakeCredentialAndDeposit
  hasDeposit mdep =
    case mdep of
      Nothing ->
        -- This case is made impossible by the parser, that distinguishes between Conway
        -- and pre-Conway.
        Left StakeAddressRegistrationDepositRequired
      Just (Featured _ dep) ->
        return $ StakeCredentialAndDeposit stakeCred dep

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
  caseShelleyToBabbageOrConwayEraOnwards
    ( \w ->
        shelleyToBabbageEraConstraints w $
          Exp.Certificate $
            L.mkDelegStakeTxCert (toShelleyStakeCredential stakeCredential) (toLedgerHash stakePoolHash)
    )
    ( \w ->
        conwayEraOnwardsConstraints w $
          Exp.Certificate $
            L.mkDelegTxCert
              (toShelleyStakeCredential stakeCredential)
              (L.DelegStake (toLedgerHash stakePoolHash))
    )
    sbe
 where
  toLedgerHash :: Hash StakePoolKey -> L.KeyHash L.StakePool
  toLedgerHash (StakePoolKeyHash poolStakeVKeyHash) = poolStakeVKeyHash
