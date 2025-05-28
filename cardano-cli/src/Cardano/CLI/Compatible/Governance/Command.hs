{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Compatible.Governance.Command
  ( CompatibleGovernanceCmds (..)
  , renderCompatibleGovernanceCmds
  )
where

import Cardano.Api
import Cardano.Api.Ledger (Coin)

import Cardano.CLI.EraBased.Governance.Actions.Command
import Cardano.CLI.EraBased.Governance.Option

import Data.Text

-- TODO: After QA confirmms that the new compatibility commands meet their needs
-- we can remove all remaining legacy commands. We can also remove/move the exising
-- byron era commands under the new compatiblilty commands.
data CompatibleGovernanceCmds era
  = CreateCompatibleProtocolParametersUpdateCmd (GovernanceActionCmds era)
  | CompatibleCreateMirCertificateStakeAddressesCmd
      (ShelleyToBabbageEra era)
      MIRPot
      [StakeAddress]
      [Coin]
      (File () Out)
  | CompatibleCreateMirCertificateTransferToReservesCmd
      (ShelleyToBabbageEra era)
      Coin
      (File () Out)
  | CompatibleCreateMirCertificateTransferToTreasuryCmd
      (ShelleyToBabbageEra era)
      Coin
      (File () Out)
  | CreateCompatibleGenesisKeyDelegationCertificateCmd (GovernanceCmds era)
  | LatestCompatibleGovernanceCmds (GovernanceCmds era)

renderCompatibleGovernanceCmds :: CompatibleGovernanceCmds era -> Text
renderCompatibleGovernanceCmds = \case
  CreateCompatibleProtocolParametersUpdateCmd cmd ->
    renderGovernanceActionCmds cmd
  CompatibleCreateMirCertificateStakeAddressesCmd{} ->
    "governance create-mir-certificate stake-addresses"
  CompatibleCreateMirCertificateTransferToReservesCmd{} ->
    "governance create-mir-certificate transfer-to-reserves"
  CompatibleCreateMirCertificateTransferToTreasuryCmd{} ->
    "governance create-mir-certificate transfer-to-treasury"
  CreateCompatibleGenesisKeyDelegationCertificateCmd cmd ->
    renderGovernanceCmds cmd
  LatestCompatibleGovernanceCmds cmd -> renderGovernanceCmds cmd
