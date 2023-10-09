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
  | GovernanceCreatePoll
      Text -- ^ Prompt
      [Text] -- ^ Choices
      (Maybe Word) -- ^ Nonce
      (File GovernancePoll Out)
  | GovernanceAnswerPoll
      (File GovernancePoll In) -- ^ Poll file
      (Maybe Word) -- ^ Answer index
      (Maybe (File () Out)) -- ^ Tx file
  | GovernanceVerifyPoll
      (File GovernancePoll In) -- ^ Poll file
      (File (Tx ()) In) -- ^ Tx file
      (Maybe (File () Out)) -- ^ Tx file
  deriving Show

renderLegacyGovernanceCmds :: LegacyGovernanceCmds -> Text
renderLegacyGovernanceCmds = \case
  GovernanceGenesisKeyDelegationCertificate {} -> "governance create-genesis-key-delegation-certificate"
  GovernanceMIRPayStakeAddressesCertificate {} -> "governance create-mir-certificate stake-addresses"
  GovernanceMIRTransfer _ _ _ TransferToTreasury -> "governance create-mir-certificate transfer-to-treasury"
  GovernanceMIRTransfer _ _ _ TransferToReserves -> "governance create-mir-certificate transfer-to-reserves"
  GovernanceUpdateProposal {} -> "governance create-update-proposal"
  GovernanceCreatePoll{} -> "governance create-poll"
  GovernanceAnswerPoll{} -> "governance answer-poll"
  GovernanceVerifyPoll{} -> "governance verify-poll"

