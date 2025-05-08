{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Legacy.Governance.Command where

import Cardano.Api
import Cardano.Api.Ledger (Coin)
import Cardano.Api.Shelley

import Cardano.CLI.Type.Common
import Cardano.CLI.Type.Key

import Data.Text (Text)

data LegacyGovernanceCmds
  = GovernanceCreateMirCertificateStakeAddressesCmd
      (EraInEon ShelleyToBabbageEra)
      MIRPot
      [StakeAddress]
      [Coin]
      (File () Out)
  | GovernanceCreateMirCertificateTransferToTreasuryCmd
      (EraInEon ShelleyToBabbageEra)
      Coin
      (File () Out)
  | GovernanceCreateMirCertificateTransferToReservesCmd
      (EraInEon ShelleyToBabbageEra)
      Coin
      (File () Out)
  | GovernanceGenesisKeyDelegationCertificate
      (EraInEon ShelleyToBabbageEra)
      (VerificationKeyOrHashOrFile GenesisKey)
      (VerificationKeyOrHashOrFile GenesisDelegateKey)
      (VerificationKeyOrHashOrFile VrfKey)
      (File () Out)
  | GovernanceUpdateProposal
      (File () Out)
      EpochNo
      [VerificationKeyFile In]
      ProtocolParametersUpdate
      (Maybe FilePath)
  deriving Show

renderLegacyGovernanceCmds :: LegacyGovernanceCmds -> Text
renderLegacyGovernanceCmds = \case
  GovernanceGenesisKeyDelegationCertificate{} -> "governance create-genesis-key-delegation-certificate"
  GovernanceCreateMirCertificateStakeAddressesCmd{} -> "governance create-mir-certificate stake-addresses"
  GovernanceCreateMirCertificateTransferToTreasuryCmd{} -> "governance create-mir-certificate transfer-to-treasury"
  GovernanceCreateMirCertificateTransferToReservesCmd{} -> "governance create-mir-certificate transfer-to-reserves"
  GovernanceUpdateProposal{} -> "governance create-update-proposal"
