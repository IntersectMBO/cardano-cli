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
  , renderGovernanceActionCmds
  ) where

import           Cardano.Api
import qualified Cardano.Api.Ledger as Ledger
import           Cardano.Api.Shelley

import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Key
import qualified Cardano.Ledger.BaseTypes as Ledger
import qualified Cardano.Ledger.Crypto as Crypto
import qualified Cardano.Ledger.SafeHash as Ledger

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
      , networkId               :: !Ledger.Network
      , deposit                 :: !Lovelace
      , returnAddress           :: !(VerificationKeyOrHashOrFile StakeKey)
      , proposalUrl             :: !ProposalUrl
      , proposalHash            :: !(Ledger.SafeHash Crypto.StandardCrypto Ledger.AnchorData)
      , oldCommitteeVkeySource  :: ![VerificationKeyOrHashOrFile CommitteeColdKey]
      , newCommitteeVkeySource  :: ![(VerificationKeyOrHashOrFile CommitteeColdKey, EpochNo)]
      , requiredQuorum          :: !Rational
      , mPrevGovernanceActionId :: !(Maybe (TxId, Word32))
      , outFile                 :: !(File () Out)
      } deriving Show

data GovernanceActionCreateConstitutionCmdArgs era
  = GovernanceActionCreateConstitutionCmdArgs
      { eon                     :: !(ConwayEraOnwards era)
      , networkId               :: !Ledger.Network
      , deposit                 :: !Lovelace
      , stakeCredential         :: !(VerificationKeyOrHashOrFile StakeKey)
      , mPrevGovernanceActionId :: !(Maybe (TxId, Word32))
      , proposalUrl             :: !ProposalUrl
      , proposalHashSource      :: !ProposalHashSource
      , constitutionUrl         :: !ConstitutionUrl
      , constitutionHashSource  :: !ConstitutionHashSource
      , outFile                 :: !(File () Out)
      } deriving Show

-- | Datatype to carry data for the create-info governance action
data GovernanceActionInfoCmdArgs era
   = GovernanceActionInfoCmdArgs
      { eon                 :: !(ConwayEraOnwards era)
      , networkId           :: !Ledger.Network
      , deposit             :: !Lovelace
      , returnStakeAddress  :: !(VerificationKeyOrHashOrFile StakeKey)
      , proposalUrl         :: !ProposalUrl
      , proposalHash        :: !(Ledger.SafeHash Crypto.StandardCrypto Ledger.AnchorData)
      , outFile             :: !(File () Out)
      } deriving Show

data GovernanceActionCreateNoConfidenceCmdArgs era
  = GovernanceActionCreateNoConfidenceCmdArgs
      { eon                   :: !(ConwayEraOnwards era)
      , networkId             :: !Ledger.Network
      , deposit               :: !Lovelace
      , returnStakeAddress    :: !(VerificationKeyOrHashOrFile StakeKey)
      , proposalUrl           :: !ProposalUrl
      , proposalHashSource    :: !ProposalHashSource
      , governanceActionId    :: !TxId
      , governanceActionIndex :: !Word32
      , outFile               :: !(File () Out)
      } deriving Show

data GovernanceActionProtocolParametersUpdateCmdArgs era
  = GovernanceActionProtocolParametersUpdateCmdArgs
      { uppShelleyBasedEra :: !(ShelleyBasedEra era)
      , uppPreConway       :: !(Maybe (UpdateProtocolParametersPreConway era))
      , uppConwayOnwards   :: !(Maybe (UpdateProtocolParametersConwayOnwards era))
      , uppNewPParams      :: !(EraBasedProtocolParametersUpdate era)
      , uppFilePath        :: !(File () Out)
      } deriving Show

data GovernanceActionTreasuryWithdrawalCmdArgs era
  = GovernanceActionTreasuryWithdrawalCmdArgs
      { eon                 :: !(ConwayEraOnwards era)
      , networkId           :: !Ledger.Network
      , deposit             :: !Lovelace
      , returnAddr          :: !(VerificationKeyOrHashOrFile StakeKey)
      , proposalUrl         :: !ProposalUrl
      , proposalHashSource  :: !ProposalHashSource
      , treasuryWithdrawal  :: ![(VerificationKeyOrHashOrFile StakeKey, Lovelace)]
      , outFile             :: !(File () Out)
      } deriving Show

data GovernanceActionViewCmdArgs era
  = GovernanceActionViewCmdArgs
      { eon        :: !(ConwayEraOnwards era)
      , actionFile :: !(ProposalFile In)
      , outFormat  :: !GovernanceActionViewOutputFormat
      , mOutFile   :: !(Maybe (File () Out))
      } deriving Show

data UpdateProtocolParametersConwayOnwards era
  = UpdateProtocolParametersConwayOnwards
      { eon                 :: !(ConwayEraOnwards era)
      , networkId           :: !Ledger.Network
      , deposit             :: !Lovelace
      , returnAddr          :: !(VerificationKeyOrHashOrFile StakeKey)
      , proposalUrl         :: !ProposalUrl
      , proposalHashSource  :: !ProposalHashSource
      , governanceActionId  :: !(Maybe (TxId, Word32))
      }

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
