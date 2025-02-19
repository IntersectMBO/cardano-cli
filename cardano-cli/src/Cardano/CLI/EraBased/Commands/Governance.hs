{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.Commands.Governance
  ( GovernanceCmds (..)
  , renderGovernanceCmds
  )
where

import Cardano.Api
import Cardano.Api.Ledger (Coin)
import Cardano.Api.Shelley (VrfKey)

import Cardano.CLI.EraBased.Commands.Governance.Actions
import Cardano.CLI.EraBased.Commands.Governance.Committee
import Cardano.CLI.EraBased.Commands.Governance.DRep
import Cardano.CLI.EraBased.Commands.Governance.Poll
import Cardano.CLI.EraBased.Commands.Governance.Vote
import Cardano.CLI.Types.Key (VerificationKeyOrHashOrFile)

import Data.Text (Text)

data GovernanceCmds era
  = GovernanceCreateMirCertificateStakeAddressesCmd
      (ShelleyToBabbageEra era)
      MIRPot
      [StakeAddress]
      [Coin]
      (File () Out)
  | GovernanceCreateMirCertificateTransferToTreasuryCmd
      (ShelleyToBabbageEra era)
      Coin
      (File () Out)
  | GovernanceCreateMirCertificateTransferToReservesCmd
      (ShelleyToBabbageEra era)
      Coin
      (File () Out)
  | GovernanceGenesisKeyDelegationCertificate
      (ShelleyToBabbageEra era)
      (VerificationKeyOrHashOrFile GenesisKey)
      (VerificationKeyOrHashOrFile GenesisDelegateKey)
      (VerificationKeyOrHashOrFile VrfKey)
      (File () Out)
  | GovernanceActionCmds
      (GovernanceActionCmds era)
  | GovernanceCommitteeCmds
      (GovernanceCommitteeCmds era)
  | GovernanceDRepCmds
      (GovernanceDRepCmds era)
  | GovernancePollCmds
      (GovernancePollCmds era)
  | GovernanceVoteCmds
      (GovernanceVoteCmds era)

renderGovernanceCmds :: GovernanceCmds era -> Text
renderGovernanceCmds = \case
  GovernanceCreateMirCertificateStakeAddressesCmd{} ->
    "governance create-mir-certificate stake-addresses"
  GovernanceCreateMirCertificateTransferToTreasuryCmd{} ->
    "governance create-mir-certificate transfer-to-treasury"
  GovernanceCreateMirCertificateTransferToReservesCmd{} ->
    "governance create-mir-certificate transfer-to-reserves"
  GovernanceGenesisKeyDelegationCertificate{} ->
    "governance create-genesis-key-delegation-certificate"
  GovernanceActionCmds cmds ->
    renderGovernanceActionCmds cmds
  GovernanceCommitteeCmds cmds ->
    renderGovernanceCommitteeCmds cmds
  GovernanceDRepCmds cmds ->
    renderGovernanceDRepCmds cmds
  GovernancePollCmds cmds ->
    renderGovernancePollCmds cmds
  GovernanceVoteCmds cmds ->
    renderGovernanceVoteCmds cmds
