{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.Commands.Governance
  ( EraBasedGovernanceCmds(..)
  , renderEraBasedGovernanceCmds
  ) where

import           Cardano.Api

import           Cardano.CLI.EraBased.Commands.Governance.Actions
import           Cardano.CLI.EraBased.Commands.Governance.Committee
import           Cardano.CLI.EraBased.Commands.Governance.DRep
import           Cardano.CLI.EraBased.Commands.Governance.Query
import           Cardano.CLI.EraBased.Commands.Governance.Vote
import           Cardano.CLI.Types.Common

import           Data.Text (Text)

data EraBasedGovernanceCmds era
  = EraBasedGovernanceMIRPayStakeAddressesCertificate
      (ShelleyToBabbageEra era)
      MIRPot
      [StakeAddress]
      [Lovelace]
      (File () Out)
  | EraBasedGovernanceMIRTransfer
      (ShelleyToBabbageEra era)
      Lovelace
      (File () Out)
      TransferDirection
  | EraBasedGovernanceActionCmds
      (GovernanceActionCmds era)
  | EraBasedGovernanceCommitteeCmds
      (GovernanceCommitteeCmds era)
  | EraBasedGovernanceDRepCmds
      (GovernanceDRepCmds era)
  | EraBasedGovernanceVoteCmds
      (GovernanceVoteCmds era)
  | EraBasedGovernanceQueryCmds
      (GovernanceQueryCmds era)

renderEraBasedGovernanceCmds :: EraBasedGovernanceCmds era -> Text
renderEraBasedGovernanceCmds = \case
  EraBasedGovernanceMIRPayStakeAddressesCertificate {} ->
    "governance create-mir-certificate stake-addresses"
  EraBasedGovernanceMIRTransfer _ _ _ TransferToTreasury ->
    "governance create-mir-certificate transfer-to-treasury"
  EraBasedGovernanceMIRTransfer _ _ _ TransferToReserves ->
    "governance create-mir-certificate transfer-to-reserves"
  EraBasedGovernanceActionCmds cmds ->
    renderGovernanceActionCmds cmds
  EraBasedGovernanceCommitteeCmds cmds ->
    renderGovernanceCommitteeCmds cmds
  EraBasedGovernanceDRepCmds cmds ->
    renderGovernanceDRepCmds cmds
  EraBasedGovernanceVoteCmds cmds ->
    renderGovernanceVoteCmds cmds
  EraBasedGovernanceQueryCmds cmds ->
    renderGovernanceQueryCmds cmds
