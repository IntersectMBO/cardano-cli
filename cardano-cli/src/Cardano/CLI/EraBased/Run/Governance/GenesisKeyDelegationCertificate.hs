{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Cardano.CLI.EraBased.Run.Governance.GenesisKeyDelegationCertificate
  ( runGovernanceGenesisKeyDelegationCertificate
  )
where

import           Cardano.Api
import           Cardano.Api.Shelley

import           Cardano.CLI.Types.Errors.GovernanceCmdError
import           Cardano.CLI.Types.Key

runGovernanceGenesisKeyDelegationCertificate
  :: ShelleyToBabbageEra era
  -> VerificationKeyOrHashOrFile GenesisKey
  -> VerificationKeyOrHashOrFile GenesisDelegateKey
  -> VerificationKeyOrHashOrFile VrfKey
  -> File () Out
  -> ExceptT GovernanceCmdError IO ()
runGovernanceGenesisKeyDelegationCertificate
  stb
  genVkOrHashOrFp
  genDelVkOrHashOrFp
  vrfVkOrHashOrFp
  oFp = do
    genesisVkHash <-
      modifyError GovernanceCmdKeyReadError $
        readVerificationKeyOrHashOrTextEnvFile AsGenesisKey genVkOrHashOrFp
    genesisDelVkHash <-
      modifyError GovernanceCmdKeyReadError $
        readVerificationKeyOrHashOrTextEnvFile AsGenesisDelegateKey genDelVkOrHashOrFp
    vrfVkHash <-
      modifyError GovernanceCmdKeyReadError $
        readVerificationKeyOrHashOrFile AsVrfKey vrfVkOrHashOrFp

    let req = GenesisKeyDelegationRequirements stb genesisVkHash genesisDelVkHash vrfVkHash
        genKeyDelegCert = makeGenesisKeyDelegationCertificate req

    firstExceptT GovernanceCmdTextEnvWriteError
      . newExceptT
      $ writeLazyByteStringFile oFp
      $ shelleyBasedEraConstraints (shelleyToBabbageEraToShelleyBasedEra stb)
      $ textEnvelopeToJSON (Just genKeyDelegCertDesc) genKeyDelegCert
   where
    genKeyDelegCertDesc :: TextEnvelopeDescr
    genKeyDelegCertDesc = "Genesis Key Delegation Certificate"
