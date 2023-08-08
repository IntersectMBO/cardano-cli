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
import           Cardano.CLI.EraBased.Vote
import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Key

import           Data.Text (Text)

data EraBasedGovernanceCmds era
  = EraBasedGovernancePreConwayCmd (ShelleyToBabbageEra era)
  | EraBasedGovernancePostConwayCmd (ConwayEraOnwards era)
  | EraBasedGovernanceMIRPayStakeAddressesCertificate
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
  | EraBasedGovernanceDelegationCertificateCmd
      StakeIdentifier
      AnyDelegationTarget
      (File () Out)
  | EraBasedGovernanceRegistrationCertificateCmd
      AnyRegistrationTarget
      (File () Out)
  | EraBasedGovernanceVoteCmd
      AnyVote
      (File () Out)
  | EraBasedGovernanceCommitteeCmds
      (GovernanceCommitteeCmds era)
  | EraBasedGovernanceActionCmds
      (GovernanceActionCmds era)

renderEraBasedGovernanceCmds :: EraBasedGovernanceCmds era -> Text
renderEraBasedGovernanceCmds = \case
  EraBasedGovernancePreConwayCmd {} ->
    "governance pre-conway"
  EraBasedGovernancePostConwayCmd {} ->
    "governance post-conway"
  EraBasedGovernanceMIRPayStakeAddressesCertificate {} ->
    "governance create-mir-certificate stake-addresses"
  EraBasedGovernanceMIRTransfer _ _ _ TransferToTreasury ->
    "governance create-mir-certificate transfer-to-treasury"
  EraBasedGovernanceMIRTransfer _ _ _ TransferToReserves ->
    "governance create-mir-certificate transfer-to-reserves"
  EraBasedGovernanceDelegationCertificateCmd {} ->
    "governance delegation-certificate"
  EraBasedGovernanceRegistrationCertificateCmd {} ->
    "governance registration-certificate"
  EraBasedGovernanceVoteCmd {} ->
    "goverance vote"
  EraBasedGovernanceCommitteeCmds cmds ->
    renderGovernanceCommitteeCmds cmds
  EraBasedGovernanceActionCmds cmds ->
    renderGovernanceActionCmds cmds
