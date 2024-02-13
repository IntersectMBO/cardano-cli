{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}


module Cardano.CLI.EraBased.Commands.Governance.Actions
  ( GovernanceActionCmds(..)
  , GoveranceActionUpdateCommitteeCmdArgs(..)
  , GovernanceActionCreateConstitutionCmdArgs(..)
  , GovernanceActionCreateNoConfidenceCmdArgs(..)
  , GovernanceActionInfoCmdArgs(..)
  , GovernanceActionViewCmdArgs(..)
  , GovernanceActionProtocolParametersUpdateCmdArgs(..)
  , GovernanceActionTreasuryWithdrawalCmdArgs(..)
  , UpdateProtocolParametersConwayOnwards(..)
  , UpdateProtocolParametersPreConway(..)
  , CostModelsFile(..)
  , renderGovernanceActionCmds
  ) where

import           Cardano.Api
import qualified Cardano.Api.Ledger as L
import           Cardano.Api.Shelley

import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Key

import           Data.Text (Text)
import           Data.Word

data GovernanceActionCmds era
  = GovernanceActionCreateConstitutionCmd         !(GovernanceActionCreateConstitutionCmdArgs era)
  | GoveranceActionUpdateCommitteeCmd             !(GoveranceActionUpdateCommitteeCmdArgs era)
  | GovernanceActionCreateNoConfidenceCmd         !(GovernanceActionCreateNoConfidenceCmdArgs era)
  | GovernanceActionProtocolParametersUpdateCmd   !(GovernanceActionProtocolParametersUpdateCmdArgs era)
  | GovernanceActionTreasuryWithdrawalCmd         !(GovernanceActionTreasuryWithdrawalCmdArgs era)
  | GovernanceActionInfoCmd                       !(GovernanceActionInfoCmdArgs era)
  | GovernanceActionViewCmd                       !(GovernanceActionViewCmdArgs era)
  deriving Show

data GoveranceActionUpdateCommitteeCmdArgs era
  = GoveranceActionUpdateCommitteeCmdArgs
      { eon                     :: !(ConwayEraOnwards era)
      , networkId               :: !L.Network
      , deposit                 :: !Lovelace
      , returnAddress           :: !(VerificationKeyOrHashOrFile StakeKey)
      , proposalUrl             :: !ProposalUrl
      , proposalHash            :: !(L.SafeHash L.StandardCrypto L.AnchorData)
      , oldCommitteeVkeySource  :: ![VerificationKeyOrHashOrFile CommitteeColdKey]
      , newCommitteeVkeySource  :: ![(VerificationKeyOrHashOrFile CommitteeColdKey, EpochNo)]
      , requiredQuorum          :: !Rational
      , mPrevGovernanceActionId :: !(Maybe (TxId, Word32))
      , outFile                 :: !(File () Out)
      } deriving Show

data GovernanceActionCreateConstitutionCmdArgs era
  = GovernanceActionCreateConstitutionCmdArgs
      { eon                     :: !(ConwayEraOnwards era)
      , networkId               :: !L.Network
      , deposit                 :: !Lovelace
      , stakeCredential         :: !(VerificationKeyOrHashOrFile StakeKey)
      , mPrevGovernanceActionId :: !(Maybe (TxId, Word32))
      , proposalUrl             :: !ProposalUrl
      , proposalHash            :: !(L.SafeHash L.StandardCrypto L.AnchorData)
      , constitutionUrl         :: !ConstitutionUrl
      , constitutionHash        :: !(L.SafeHash L.StandardCrypto L.AnchorData)
      , outFile                 :: !(File () Out)
      } deriving Show

-- | Datatype to carry data for the create-info governance action
data GovernanceActionInfoCmdArgs era
   = GovernanceActionInfoCmdArgs
      { eon                 :: !(ConwayEraOnwards era)
      , networkId           :: !L.Network
      , deposit             :: !Lovelace
      , returnStakeAddress  :: !(VerificationKeyOrHashOrFile StakeKey)
      , proposalUrl         :: !ProposalUrl
      , proposalHash        :: !(L.SafeHash L.StandardCrypto L.AnchorData)
      , outFile             :: !(File () Out)
      } deriving Show

data GovernanceActionCreateNoConfidenceCmdArgs era
  = GovernanceActionCreateNoConfidenceCmdArgs
      { eon                   :: !(ConwayEraOnwards era)
      , networkId             :: !L.Network
      , deposit               :: !Lovelace
      , returnStakeAddress    :: !(VerificationKeyOrHashOrFile StakeKey)
      , proposalUrl           :: !ProposalUrl
      , proposalHash          :: !(L.SafeHash L.StandardCrypto L.AnchorData)
      , governanceActionId    :: !TxId
      , governanceActionIndex :: !Word32
      , outFile               :: !(File () Out)
      } deriving Show

data GovernanceActionProtocolParametersUpdateCmdArgs era
  = GovernanceActionProtocolParametersUpdateCmdArgs
      { uppShelleyBasedEra :: !(ShelleyBasedEra era)
      , uppPreConway       :: !(Maybe (UpdateProtocolParametersPreConway era))
      , uppConwayOnwards   :: !(Maybe (UpdateProtocolParametersConwayOnwards era))
      -- | New parameters to be proposed. From Alonzo onwards, the type
      -- 'EraBasedProtocolParametersUpdate' also contains cost models. Since all
      -- other protocol parameters are read from command line arguments, whereas
      -- the cost models are read from a file, we separate the cost models from
      -- the rest of the protocol parameters to ease parsing.
      , uppNewPParams :: !(EraBasedProtocolParametersUpdate era)
      -- | The new cost models proposed. See the comment at 'uppNewPParams' for
      -- why this is a separate field.
      , uppCostModelsFile :: !(Maybe (CostModelsFile era))
      , uppFilePath        :: !(File () Out)
      } deriving Show

data GovernanceActionTreasuryWithdrawalCmdArgs era
  = GovernanceActionTreasuryWithdrawalCmdArgs
      { eon                    :: !(ConwayEraOnwards era)
      , networkId              :: !L.Network
      , deposit                :: !Lovelace
      , returnAddr             :: !(VerificationKeyOrHashOrFile StakeKey)
      , proposalUrl            :: !ProposalUrl
      , proposalHash           :: !(L.SafeHash L.StandardCrypto L.AnchorData)
      , treasuryWithdrawal     :: ![(VerificationKeyOrHashOrFile StakeKey, Lovelace)]
      , constitutionScriptHash :: !(Maybe ScriptHash)
      , outFile                :: !(File () Out)
      } deriving Show

data GovernanceActionViewCmdArgs era
  = GovernanceActionViewCmdArgs
      { eon        :: !(ConwayEraOnwards era)
      , actionFile :: !(ProposalFile In)
      , outFormat  :: !ViewOutputFormat
      , mOutFile   :: !(Maybe (File () Out))
      } deriving Show

data UpdateProtocolParametersConwayOnwards era
  = UpdateProtocolParametersConwayOnwards
      { eon                 :: !(ConwayEraOnwards era)
      , networkId           :: !L.Network
      , deposit             :: !Lovelace
      , returnAddr          :: !(VerificationKeyOrHashOrFile StakeKey)
      , proposalUrl         :: !ProposalUrl
      , proposalHash        :: !(L.SafeHash L.StandardCrypto L.AnchorData)
      , governanceActionId  :: !(Maybe (TxId, Word32))
      , constitutionScriptHash :: !(Maybe ScriptHash)
      }

data CostModelsFile era
  = CostModelsFile
      { eon :: !(AlonzoEraOnwards era)
      , costModelsFile :: !(File L.CostModels In)
      } deriving Show

deriving instance Show (UpdateProtocolParametersConwayOnwards era)

data UpdateProtocolParametersPreConway era
  = UpdateProtocolParametersPreConway
      { eon                     :: !(ShelleyToBabbageEra era)
      , expiryEpoch             :: !EpochNo
      , genesisVerificationKeys :: ![VerificationKeyFile In]
      }


deriving instance Show (UpdateProtocolParametersPreConway era)

renderGovernanceActionCmds :: GovernanceActionCmds era -> Text
renderGovernanceActionCmds = ("governance action " <>) . \case
  GovernanceActionCreateConstitutionCmd {} ->
    "create-constitution"

  GovernanceActionProtocolParametersUpdateCmd {} ->
    "create-protocol-parameters-update"

  GovernanceActionTreasuryWithdrawalCmd {} ->
    "create-treasury-withdrawal"

  GoveranceActionUpdateCommitteeCmd {} ->
    "update-committee"

  GovernanceActionCreateNoConfidenceCmd {} ->
    "create-no-confidence"

  GovernanceActionInfoCmd {} ->
    "create-info"

  GovernanceActionViewCmd {} ->
    "view"
