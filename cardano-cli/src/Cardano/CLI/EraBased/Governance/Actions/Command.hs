{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cardano.CLI.EraBased.Governance.Actions.Command
  ( GovernanceActionCmds (..)
  , GovernanceActionUpdateCommitteeCmdArgs (..)
  , GovernanceActionCreateConstitutionCmdArgs (..)
  , GovernanceActionCreateNoConfidenceCmdArgs (..)
  , GovernanceActionInfoCmdArgs (..)
  , GovernanceActionViewCmdArgs (..)
  , GovernanceActionProtocolParametersUpdateCmdArgs (..)
  , GovernanceActionTreasuryWithdrawalCmdArgs (..)
  , UpdateProtocolParametersConwayOnwards (..)
  , UpdateProtocolParametersPreConway (..)
  , GovernanceActionHardforkInitCmdArgs (..)
  , renderGovernanceActionCmds
  )
where

import Cardano.Api
import Cardano.Api.Experimental qualified as Exp
import Cardano.Api.Ledger qualified as L

import Cardano.CLI.Compatible.Governance.Types
import Cardano.CLI.Type.Common
import Cardano.CLI.Type.Key

import Data.Text (Text)
import Vary (Vary)

data GovernanceActionCmds era
  = GovernanceActionCreateConstitutionCmd !(GovernanceActionCreateConstitutionCmdArgs era)
  | GovernanceActionUpdateCommitteeCmd !(GovernanceActionUpdateCommitteeCmdArgs era)
  | GovernanceActionCreateNoConfidenceCmd !(GovernanceActionCreateNoConfidenceCmdArgs era)
  | GovernanceActionProtocolParametersUpdateCmd !(GovernanceActionProtocolParametersUpdateCmdArgs era)
  | GovernanceActionTreasuryWithdrawalCmd !(GovernanceActionTreasuryWithdrawalCmdArgs era)
  | GovernanceActionHardforkInitCmd !(GovernanceActionHardforkInitCmdArgs era)
  | GovernanceActionInfoCmd !(GovernanceActionInfoCmdArgs era)
  | GovernanceActionViewCmd !(GovernanceActionViewCmdArgs era)
  deriving Show

data GovernanceActionUpdateCommitteeCmdArgs era
  = GovernanceActionUpdateCommitteeCmdArgs
  { era :: !(Exp.Era era)
  , networkId :: !L.Network
  , deposit :: !Lovelace
  , returnAddress :: !StakeIdentifier
  , proposalUrl :: !ProposalUrl
  , proposalHash :: !(L.SafeHash L.AnchorData)
  , checkProposalHash :: !(MustCheckHash ProposalUrl)
  , oldCommitteeVkeySource :: ![VerificationKeyOrHashOrFileOrScriptHash CommitteeColdKey]
  , newCommitteeVkeySource :: ![(VerificationKeyOrHashOrFileOrScriptHash CommitteeColdKey, EpochNo)]
  , requiredThreshold :: !Rational
  , mPrevGovernanceActionId :: !(Maybe L.GovActionId)
  , outFile :: !(File () Out)
  }
  deriving Show

data GovernanceActionCreateConstitutionCmdArgs era
  = GovernanceActionCreateConstitutionCmdArgs
  { era :: !(Exp.Era era)
  , networkId :: !L.Network
  , deposit :: !Lovelace
  , stakeCredential :: !StakeIdentifier
  , mPrevGovernanceActionId :: !(Maybe L.GovActionId)
  , proposalUrl :: !ProposalUrl
  , proposalHash :: !(L.SafeHash L.AnchorData)
  , checkProposalHash :: !(MustCheckHash ProposalUrl)
  , constitutionUrl :: !ConstitutionUrl
  , constitutionHash :: !(L.SafeHash L.AnchorData)
  , checkConstitutionHash :: !(MustCheckHash ConstitutionUrl)
  , constitutionScript :: !(Maybe ScriptHash)
  , outFile :: !(File () Out)
  }
  deriving Show

-- | Datatype to carry data for the create-info governance action
data GovernanceActionInfoCmdArgs era
  = GovernanceActionInfoCmdArgs
  { era :: !(Exp.Era era)
  , networkId :: !L.Network
  , deposit :: !Lovelace
  , returnStakeAddress :: !StakeIdentifier
  , proposalUrl :: !ProposalUrl
  , proposalHash :: !(L.SafeHash L.AnchorData)
  , checkProposalHash :: !(MustCheckHash ProposalUrl)
  , outFile :: !(File () Out)
  }
  deriving Show

data GovernanceActionCreateNoConfidenceCmdArgs era
  = GovernanceActionCreateNoConfidenceCmdArgs
  { era :: !(Exp.Era era)
  , networkId :: !L.Network
  , deposit :: !Lovelace
  , returnStakeAddress :: !StakeIdentifier
  , proposalUrl :: !ProposalUrl
  , proposalHash :: !(L.SafeHash L.AnchorData)
  , checkProposalHash :: !(MustCheckHash ProposalUrl)
  , mPrevGovernanceActionId :: !(Maybe L.GovActionId)
  , outFile :: !(File () Out)
  }
  deriving Show

data GovernanceActionTreasuryWithdrawalCmdArgs era
  = GovernanceActionTreasuryWithdrawalCmdArgs
  { era :: !(Exp.Era era)
  , networkId :: !L.Network
  , deposit :: !Lovelace
  , returnAddr :: !StakeIdentifier
  , proposalUrl :: !ProposalUrl
  , proposalHash :: !(L.SafeHash L.AnchorData)
  , checkProposalHash :: !(MustCheckHash ProposalUrl)
  , treasuryWithdrawal :: ![(StakeIdentifier, Lovelace)]
  , constitutionScriptHash :: !(Maybe ScriptHash)
  , outFile :: !(File () Out)
  }
  deriving Show

data GovernanceActionHardforkInitCmdArgs era
  = GovernanceActionHardforkInitCmdArgs
  { era :: !(Exp.Era era)
  , networkId :: !L.Network
  , deposit :: !Lovelace
  , returnStakeAddress :: !StakeIdentifier
  , mPrevGovernanceActionId :: !(Maybe L.GovActionId)
  , proposalUrl :: !ProposalUrl
  , proposalHash :: !(L.SafeHash L.AnchorData)
  , checkProposalHash :: !(MustCheckHash ProposalUrl)
  , protVer :: !L.ProtVer
  , outFile :: !(File () Out)
  }
  deriving Show

data GovernanceActionViewCmdArgs era
  = GovernanceActionViewCmdArgs
  { era :: !(Exp.Era era)
  , actionFile :: !(ProposalFile In)
  , outputFormat :: !(Vary [FormatJson, FormatYaml])
  , mOutFile :: !(Maybe (File () Out))
  }
  deriving Show

renderGovernanceActionCmds :: GovernanceActionCmds era -> Text
renderGovernanceActionCmds =
  ("governance action " <>) . \case
    GovernanceActionCreateConstitutionCmd{} ->
      "create-constitution"
    GovernanceActionProtocolParametersUpdateCmd{} ->
      "create-protocol-parameters-update"
    GovernanceActionTreasuryWithdrawalCmd{} ->
      "create-treasury-withdrawal"
    GovernanceActionUpdateCommitteeCmd{} ->
      "update-committee"
    GovernanceActionCreateNoConfidenceCmd{} ->
      "create-no-confidence"
    GovernanceActionHardforkInitCmd{} ->
      "create-hardfork"
    GovernanceActionInfoCmd{} ->
      "create-info"
    GovernanceActionViewCmd{} ->
      "view"
