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
  :: ShelleyBasedEra era
  -> VerificationKeyOrHashOrFile GenesisKey
  -> VerificationKeyOrHashOrFile GenesisDelegateKey
  -> VerificationKeyOrHashOrFile VrfKey
  -> File () Out
  -> ExceptT GovernanceCmdError IO ()
runGovernanceGenesisKeyDelegationCertificate sbe
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

  req <- hoistEither $ createGenesisDelegationRequirements sbe genesisVkHash genesisDelVkHash vrfVkHash
  let genKeyDelegCert = makeGenesisKeyDelegationCertificate req

  firstExceptT GovernanceCmdTextEnvWriteError
    . newExceptT
    $ writeLazyByteStringFile oFp
    $ shelleyBasedEraConstraints sbe
    $ textEnvelopeToJSON (Just genKeyDelegCertDesc) genKeyDelegCert
  where
    genKeyDelegCertDesc :: TextEnvelopeDescr
    genKeyDelegCertDesc = "Genesis Key Delegation Certificate"

createGenesisDelegationRequirements
  :: ShelleyBasedEra era
  -> Hash GenesisKey
  -> Hash GenesisDelegateKey
  -> Hash VrfKey
  -> Either GovernanceCmdError (GenesisKeyDelegationRequirements era)
createGenesisDelegationRequirements sbe hGen hGenDeleg hVrf =
  caseShelleyToBabbageOrConwayEraOnwards
      (\eon -> return $ GenesisKeyDelegationRequirements eon hGen hGenDeleg hVrf)
      (const $ Left GovernanceCmdGenesisDelegationNotSupportedInConway)
      sbe
