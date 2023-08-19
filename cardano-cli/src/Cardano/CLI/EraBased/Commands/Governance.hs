{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.Commands.Governance
  ( EraBasedGovernanceCmds(..)
  , renderEraBasedGovernanceCmds
  ) where

import           Cardano.Api

import           Cardano.CLI.EraBased.Commands.Governance.Actions (GovernanceActionCmds,
                   renderGovernanceActionCmds)
import           Cardano.CLI.EraBased.Commands.Governance.Committee
import           Cardano.CLI.EraBased.Commands.Governance.DRep
import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Governance

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
  | EraBasedGovernanceVoteCmd
      AnyVote
      (File () Out)
  | EraBasedGovernanceCommitteeCmds
      (GovernanceCommitteeCmds era)
  | EraBasedGovernanceActionCmds
      (GovernanceActionCmds era)
  | EraBasedGovernanceDRepCmds
      (GovernanceDRepCmds era)

renderEraBasedGovernanceCmds :: EraBasedGovernanceCmds era -> Text
renderEraBasedGovernanceCmds = \case
  EraBasedGovernanceMIRPayStakeAddressesCertificate {} ->
    "governance create-mir-certificate stake-addresses"
  EraBasedGovernanceMIRTransfer _ _ _ TransferToTreasury ->
    "governance create-mir-certificate transfer-to-treasury"
  EraBasedGovernanceMIRTransfer _ _ _ TransferToReserves ->
    "governance create-mir-certificate transfer-to-reserves"
  EraBasedGovernanceVoteCmd {} ->
    "goverance vote"
  EraBasedGovernanceCommitteeCmds cmds ->
    renderGovernanceCommitteeCmds cmds
  EraBasedGovernanceActionCmds cmds ->
    renderGovernanceActionCmds cmds
  EraBasedGovernanceDRepCmds cmds ->
    renderGovernanceDRepCmds cmds
