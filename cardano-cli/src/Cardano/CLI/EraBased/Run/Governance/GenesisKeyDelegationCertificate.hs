{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Cardano.CLI.EraBased.Run.Governance.GenesisKeyDelegationCertificate
  ( runGovernanceGenesisKeyDelegationCertificate
  ) where


import           Cardano.Api
import           Cardano.Api.Shelley

import           Cardano.CLI.Types.Errors.GovernanceCmdError
import           Cardano.CLI.Types.Key

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra

runGovernanceGenesisKeyDelegationCertificate
  :: ShelleyToBabbageEra era
  -> VerificationKeyOrHashOrFile GenesisKey
  -> VerificationKeyOrHashOrFile GenesisDelegateKey
  -> VerificationKeyOrHashOrFile VrfKey
  -> File () Out
  -> ExceptT GovernanceCmdError IO ()
runGovernanceGenesisKeyDelegationCertificate stb
                                             genVkOrHashOrFp
                                             genDelVkOrHashOrFp
                                             vrfVkOrHashOrFp
                                             oFp = do
  genesisVkHash <- firstExceptT GovernanceCmdKeyReadError
    . newExceptT
    $ readVerificationKeyOrHashOrTextEnvFile AsGenesisKey genVkOrHashOrFp
  genesisDelVkHash <-firstExceptT GovernanceCmdKeyReadError
    . newExceptT
    $ readVerificationKeyOrHashOrTextEnvFile AsGenesisDelegateKey genDelVkOrHashOrFp
  vrfVkHash <- firstExceptT GovernanceCmdKeyReadError
    . newExceptT
    $ readVerificationKeyOrHashOrFile AsVrfKey vrfVkOrHashOrFp

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
