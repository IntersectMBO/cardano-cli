{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.Commands.Governance
  ( EraBasedGovernanceCmd(..)
  , renderEraBasedGovernanceCmd
  ) where

import           Cardano.Api

import           Cardano.CLI.EraBased.Commands.Governance.Committee
import           Cardano.CLI.EraBased.Vote
import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Key

import           Data.Text (Text)

data EraBasedGovernanceCmd era
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

renderEraBasedGovernanceCmd :: EraBasedGovernanceCmd era -> Text
renderEraBasedGovernanceCmd = \case
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
