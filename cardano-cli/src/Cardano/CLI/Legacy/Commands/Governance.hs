{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Legacy.Commands.Governance where

import           Cardano.Api
import           Cardano.Api.Shelley

import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Key

import           Data.Text (Text)

data LegacyGovernanceCmds
  = GovernanceMIRPayStakeAddressesCertificate
      (EraInEon ShelleyToBabbageEra)
      MIRPot
      [StakeAddress]
      [Lovelace]
      (File () Out)
  | GovernanceMIRTransfer
      (EraInEon ShelleyToBabbageEra)
      Lovelace
      (File () Out)
      TransferDirection
  | GovernanceGenesisKeyDelegationCertificate
      (EraInEon ShelleyBasedEra)
      (VerificationKeyOrHashOrFile GenesisKey)
      (VerificationKeyOrHashOrFile GenesisDelegateKey)
      (VerificationKeyOrHashOrFile VrfKey)
      (File () Out)
  | GovernanceUpdateProposal
      (File () Out) EpochNo
      [VerificationKeyFile In]
      ProtocolParametersUpdate
      (Maybe FilePath)
  deriving Show

renderLegacyGovernanceCmds :: LegacyGovernanceCmds -> Text
renderLegacyGovernanceCmds = \case
  GovernanceGenesisKeyDelegationCertificate {} -> "governance create-genesis-key-delegation-certificate"
  GovernanceMIRPayStakeAddressesCertificate {} -> "governance create-mir-certificate stake-addresses"
  GovernanceMIRTransfer _ _ _ TransferToTreasury -> "governance create-mir-certificate transfer-to-treasury"
  GovernanceMIRTransfer _ _ _ TransferToReserves -> "governance create-mir-certificate transfer-to-reserves"
  GovernanceUpdateProposal {} -> "governance create-update-proposal"

