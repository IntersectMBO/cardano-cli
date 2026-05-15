{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.CLI.EraBased.Governance.GenesisKeyDelegationCertificate.Run
  ( runGovernanceGenesisKeyDelegationCertificate
  )
where

import Cardano.Api
import Cardano.Api.Experimental qualified as Exp
import Cardano.Api.Ledger qualified as L

import Cardano.CLI.Compatible.Exception
import Cardano.CLI.Type.Key

runGovernanceGenesisKeyDelegationCertificate
  :: VerificationKeyOrHashOrFile GenesisKey
  -> VerificationKeyOrHashOrFile GenesisDelegateKey
  -> VerificationKeyOrHashOrFile VrfKey
  -> File () Out
  -> CIO e ()
runGovernanceGenesisKeyDelegationCertificate
  genVkOrHashOrFp
  genDelVkOrHashOrFp
  vrfVkOrHashOrFp
  oFp = do
    GenesisKeyHash hGenKey <-
      readVerificationKeyOrHashOrTextEnvFile genVkOrHashOrFp
    GenesisDelegateKeyHash hGenDelegKey <-
      readVerificationKeyOrHashOrTextEnvFile genDelVkOrHashOrFp
    VrfKeyHash hVrfKey <-
      readVerificationKeyOrHashOrFile vrfVkOrHashOrFp

    -- Genesis key delegation only exists up to the Babbage era. The serialization
    -- of @ShelleyTxCert@ is uniform across Shelley→Babbage, so we anchor the
    -- text envelope type to @BabbageEra@.
    let genKeyDelegCert :: Exp.Certificate (ShelleyLedgerEra BabbageEra)
        genKeyDelegCert =
          Exp.Certificate $
            L.mkGenesisDelegTxCert $
              L.GenesisDelegCert hGenKey hGenDelegKey (L.toVRFVerKeyHash hVrfKey)

    fromEitherIOCli @(FileError ()) $
      writeLazyByteStringFile oFp $
        textEnvelopeToJSON (Just genKeyDelegCertDesc) genKeyDelegCert
   where
    genKeyDelegCertDesc :: TextEnvelopeDescr
    genKeyDelegCertDesc = "Genesis Key Delegation Certificate"
