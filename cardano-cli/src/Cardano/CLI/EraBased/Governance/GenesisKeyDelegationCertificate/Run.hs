{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.CLI.EraBased.Governance.GenesisKeyDelegationCertificate.Run
  ( runGovernanceGenesisKeyDelegationCertificate
  )
where

import Cardano.Api hiding (makeGenesisKeyDelegationCertificate)
import Cardano.Api.Compatible.Certificate

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
    genesisVkHash <-
      readVerificationKeyOrHashOrTextEnvFile genVkOrHashOrFp
    genesisDelVkHash <-
      readVerificationKeyOrHashOrTextEnvFile genDelVkOrHashOrFp
    vrfVkHash <-
      readVerificationKeyOrHashOrFile vrfVkOrHashOrFp

    let genKeyDelegCert = makeGenesisKeyDelegationCertificate genesisVkHash genesisDelVkHash vrfVkHash

    fromEitherIOCli @(FileError ()) $
      writeLazyByteStringFile oFp $
        textEnvelopeToJSON (Just genKeyDelegCertDesc) genKeyDelegCert
   where
    genKeyDelegCertDesc :: TextEnvelopeDescr
    genKeyDelegCertDesc = "Genesis Key Delegation Certificate"
