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
import Cardano.Api.Shelley

import Cardano.CLI.Compatible.Exception
import Cardano.CLI.Type.Key

import Data.Typeable (Typeable)

runGovernanceGenesisKeyDelegationCertificate
  :: forall era e
   . Typeable era
  => ShelleyToBabbageEra era
  -> VerificationKeyOrHashOrFile GenesisKey
  -> VerificationKeyOrHashOrFile GenesisDelegateKey
  -> VerificationKeyOrHashOrFile VrfKey
  -> File () Out
  -> CIO e ()
runGovernanceGenesisKeyDelegationCertificate
  stb
  genVkOrHashOrFp
  genDelVkOrHashOrFp
  vrfVkOrHashOrFp
  oFp = do
    genesisVkHash <-
      fromExceptTCli $
        readVerificationKeyOrHashOrTextEnvFile genVkOrHashOrFp
    genesisDelVkHash <-
      fromExceptTCli $
        readVerificationKeyOrHashOrTextEnvFile genDelVkOrHashOrFp
    vrfVkHash <-
      fromExceptTCli $
        readVerificationKeyOrHashOrFile vrfVkOrHashOrFp

    let req = GenesisKeyDelegationRequirements stb genesisVkHash genesisDelVkHash vrfVkHash
        genKeyDelegCert = makeGenesisKeyDelegationCertificate req

    fromEitherIOCli @(FileError ()) $
      writeLazyByteStringFile oFp $
        shelleyBasedEraConstraints (convert stb) $
          textEnvelopeToJSON (Just genKeyDelegCertDesc) genKeyDelegCert
   where
    genKeyDelegCertDesc :: TextEnvelopeDescr
    genKeyDelegCertDesc = "Genesis Key Delegation Certificate"
