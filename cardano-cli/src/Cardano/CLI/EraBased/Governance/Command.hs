{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.Governance.Command
  ( GovernanceCmds (..)
  , renderGovernanceCmds
  )
where

import Cardano.Api
import Cardano.Api.Shelley (VrfKey)

import Cardano.CLI.EraBased.Governance.Actions.Command
import Cardano.CLI.EraBased.Governance.Committee.Command
import Cardano.CLI.EraBased.Governance.DRep.Command
import Cardano.CLI.EraBased.Governance.Vote.Command
import Cardano.CLI.Type.Key (VerificationKeyOrHashOrFile)

import Data.Text (Text)

data GovernanceCmds era
  = GovernanceGenesisKeyDelegationCertificate
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
  | GovernanceVoteCmds
      (GovernanceVoteCmds era)

renderGovernanceCmds :: GovernanceCmds era -> Text
renderGovernanceCmds = \case
  GovernanceGenesisKeyDelegationCertificate{} ->
    "governance create-genesis-key-delegation-certificate"
  GovernanceActionCmds cmds ->
    renderGovernanceActionCmds cmds
  GovernanceCommitteeCmds cmds ->
    renderGovernanceCommitteeCmds cmds
  GovernanceDRepCmds cmds ->
    renderGovernanceDRepCmds cmds
  GovernanceVoteCmds cmds ->
    renderGovernanceVoteCmds cmds
