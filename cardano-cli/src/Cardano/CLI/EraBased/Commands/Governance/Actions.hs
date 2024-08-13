{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cardano.CLI.EraBased.Commands.Governance.Actions
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
  , CostModelsFile (..)
  , renderGovernanceActionCmds
  )
where

import           Cardano.Api
import qualified Cardano.Api.Ledger as L
import           Cardano.Api.Shelley

import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Key

import           Data.Text (Text)
import           Data.Word

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
  { eon :: !(ConwayEraOnwards era)
  , networkId :: !L.Network
  , deposit :: !Lovelace
  , returnAddress :: !StakeIdentifier
  , proposalUrl :: !ProposalUrl
  , proposalHash :: !(L.SafeHash L.StandardCrypto L.AnchorData)
  , oldCommitteeVkeySource :: ![VerificationKeyOrHashOrFileOrScriptHash CommitteeColdKey]
  , newCommitteeVkeySource :: ![(VerificationKeyOrHashOrFileOrScriptHash CommitteeColdKey, EpochNo)]
  , requiredThreshold :: !Rational
  , mPrevGovernanceActionId :: !(Maybe (TxId, Word16))
  , outFile :: !(File () Out)
  }
  deriving Show

data GovernanceActionCreateConstitutionCmdArgs era
  = GovernanceActionCreateConstitutionCmdArgs
  { eon :: !(ConwayEraOnwards era)
  , networkId :: !L.Network
  , deposit :: !Lovelace
  , stakeCredential :: !StakeIdentifier
  , mPrevGovernanceActionId :: !(Maybe (TxId, Word16))
  , proposalUrl :: !ProposalUrl
  , proposalHash :: !(L.SafeHash L.StandardCrypto L.AnchorData)
  , constitutionUrl :: !ConstitutionUrl
  , constitutionHash :: !(L.SafeHash L.StandardCrypto L.AnchorData)
  , constitutionScript :: !(Maybe ScriptHash)
  , outFile :: !(File () Out)
  }
  deriving Show

-- | Datatype to carry data for the create-info governance action
data GovernanceActionInfoCmdArgs era
  = GovernanceActionInfoCmdArgs
  { eon :: !(ConwayEraOnwards era)
  , networkId :: !L.Network
  , deposit :: !Lovelace
  , returnStakeAddress :: !StakeIdentifier
  , proposalUrl :: !ProposalUrl
  , proposalHash :: !(L.SafeHash L.StandardCrypto L.AnchorData)
  , outFile :: !(File () Out)
  }
  deriving Show

data GovernanceActionCreateNoConfidenceCmdArgs era
  = GovernanceActionCreateNoConfidenceCmdArgs
  { eon :: !(ConwayEraOnwards era)
  , networkId :: !L.Network
  , deposit :: !Lovelace
  , returnStakeAddress :: !StakeIdentifier
  , proposalUrl :: !ProposalUrl
  , proposalHash :: !(L.SafeHash L.StandardCrypto L.AnchorData)
  , mPrevGovernanceActionId :: !(Maybe (TxId, Word16))
  , outFile :: !(File () Out)
  }
  deriving Show

data GovernanceActionProtocolParametersUpdateCmdArgs era
  = GovernanceActionProtocolParametersUpdateCmdArgs
  { uppShelleyBasedEra :: !(ShelleyBasedEra era)
  , uppPreConway :: !(Maybe (UpdateProtocolParametersPreConway era))
  , uppConwayOnwards :: !(Maybe (UpdateProtocolParametersConwayOnwards era))
  , uppNewPParams :: !(EraBasedProtocolParametersUpdate era)
  -- ^ New parameters to be proposed. From Alonzo onwards, the type
  -- 'EraBasedProtocolParametersUpdate' also contains cost models. Since all
  -- other protocol parameters are read from command line arguments, whereas
  -- the cost models are read from a file, we separate the cost models from
  -- the rest of the protocol parameters to ease parsing.
  , uppCostModelsFile :: !(Maybe (CostModelsFile era))
  -- ^ The new cost models proposed. See the comment at 'uppNewPParams' for
  -- why this is a separate field.
  , uppFilePath :: !(File () Out)
  }
  deriving Show

data GovernanceActionTreasuryWithdrawalCmdArgs era
  = GovernanceActionTreasuryWithdrawalCmdArgs
  { eon :: !(ConwayEraOnwards era)
  , networkId :: !L.Network
  , deposit :: !Lovelace
  , returnAddr :: !StakeIdentifier
  , proposalUrl :: !ProposalUrl
  , proposalHash :: !(L.SafeHash L.StandardCrypto L.AnchorData)
  , treasuryWithdrawal :: ![(VerificationKeyOrHashOrFile StakeKey, Lovelace)]
  , constitutionScriptHash :: !(Maybe ScriptHash)
  , outFile :: !(File () Out)
  }
  deriving Show

data GovernanceActionHardforkInitCmdArgs era
  = GovernanceActionHardforkInitCmdArgs
  { eon :: !(ConwayEraOnwards era)
  , networkId :: !L.Network
  , deposit :: !Lovelace
  , returnStakeAddress :: !StakeIdentifier
  , mPrevGovernanceActionId :: !(Maybe (TxId, Word16))
  , proposalUrl :: !ProposalUrl
  , proposalHash :: !(L.SafeHash L.StandardCrypto L.AnchorData)
  , protVer :: !L.ProtVer
  , outFile :: !(File () Out)
  }
  deriving Show

data GovernanceActionViewCmdArgs era
  = GovernanceActionViewCmdArgs
  { eon :: !(ConwayEraOnwards era)
  , actionFile :: !(ProposalFile In)
  , outFormat :: !ViewOutputFormat
  , mOutFile :: !(Maybe (File () Out))
  }
  deriving Show

data UpdateProtocolParametersConwayOnwards era
  = UpdateProtocolParametersConwayOnwards
  { eon :: !(ConwayEraOnwards era)
  , networkId :: !L.Network
  , deposit :: !Lovelace
  , returnAddr :: !StakeIdentifier
  , proposalUrl :: !ProposalUrl
  , proposalHash :: !(L.SafeHash L.StandardCrypto L.AnchorData)
  , governanceActionId :: !(Maybe (TxId, Word16))
  , constitutionScriptHash :: !(Maybe ScriptHash)
  }

data CostModelsFile era
  = CostModelsFile
  { eon :: !(AlonzoEraOnwards era)
  , costModelsFile :: !(File L.CostModels In)
  }
  deriving Show

deriving instance Show (UpdateProtocolParametersConwayOnwards era)

data UpdateProtocolParametersPreConway era
  = UpdateProtocolParametersPreConway
  { eon :: !(ShelleyToBabbageEra era)
  , expiryEpoch :: !EpochNo
  , genesisVerificationKeys :: ![VerificationKeyFile In]
  }

deriving instance Show (UpdateProtocolParametersPreConway era)

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
