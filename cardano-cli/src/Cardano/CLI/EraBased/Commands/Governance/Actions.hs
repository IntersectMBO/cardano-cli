{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.Commands.Governance.Actions
  ( GovernanceActionCmds(..)
  , GoveranceActionUpdateCommitteeCmdArgs(..)
  , GovernanceActionCreateConstitutionCmdArgs(..)
  , GovernanceActionCreateNoConfidenceCmdArgs(..)
  , GovernanceActionInfoCmdArgs(..)
  , GovernanceActionProtocolParametersUpdateCmdArgs(..)
  , GovernanceActionTreasuryWithdrawalCmdArgs(..)
  , renderGovernanceActionCmds

  , AnyStakeIdentifier(..)
  ) where

import           Cardano.Api
import qualified Cardano.Api.Ledger as Ledger
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
  deriving Show

data GoveranceActionUpdateCommitteeCmdArgs era
  = GoveranceActionUpdateCommitteeCmdArgs
      { eon                     :: !(ConwayEraOnwards era)
      , networkId               :: !Ledger.Network
      , deposit                 :: !Lovelace
      , returnAddress           :: !AnyStakeIdentifier
      , proposalUrl             :: !ProposalUrl
      , proposalHashSource      :: !ProposalHashSource
      , oldCommitteeVkeySource  :: ![VerificationKeyOrHashOrFile CommitteeColdKey]
      , newCommitteeVkeySource  :: ![(VerificationKeyOrHashOrFile CommitteeColdKey, EpochNo)]
      , requiredQuorum          :: !Rational
      , previousGovActionId     :: !(Maybe (TxId, Word32))
      , filePath                :: !(File () Out)
      } deriving Show

data GovernanceActionCreateConstitutionCmdArgs era
  = GovernanceActionCreateConstitutionCmdArgs
      { eon                     :: !(ConwayEraOnwards era)
      , networkId               :: !Ledger.Network
      , deposit                 :: !Lovelace
      , stakeCredential         :: !AnyStakeIdentifier
      , prevGovActId            :: !(Maybe (TxId, Word32))
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
      , stakeCredential     :: !AnyStakeIdentifier
      , proposalUrl         :: !ProposalUrl
      , proposalHashSource  :: !ProposalHashSource
      , outFile             :: !(File () Out)
      } deriving Show

data GovernanceActionCreateNoConfidenceCmdArgs era
  = GovernanceActionCreateNoConfidenceCmdArgs
      { eon                 :: !(ConwayEraOnwards era)
      , networkId           :: !Ledger.Network
      , deposit             :: !Lovelace
      , stakeCredential     :: !AnyStakeIdentifier
      , proposalUrl         :: !ProposalUrl
      , proposalHashSource  :: !ProposalHashSource
      , govAct              :: !TxId
      , govActIndex         :: !Word32
      , outFile             :: !(File () Out)
      } deriving Show

data GovernanceActionProtocolParametersUpdateCmdArgs era
  = GovernanceActionProtocolParametersUpdateCmdArgs
      { eon               :: !(ConwayEraOnwards era)
      , epochNo           :: !EpochNo
      , genesisVkeyFiles  :: ![VerificationKeyFile In]
      , pparamsUpdate     :: !(EraBasedProtocolParametersUpdate era)
      , outFile           :: !(File () Out)
      } deriving Show

data GovernanceActionTreasuryWithdrawalCmdArgs era
  = GovernanceActionTreasuryWithdrawalCmdArgs
      { eon                 :: !(ConwayEraOnwards era)
      , networkId           :: !Ledger.Network
      , deposit             :: !Lovelace -- ^ Deposit
      , returnAddr          :: !AnyStakeIdentifier -- ^ Return address
      , proposalUrl         :: !ProposalUrl
      , proposalHashSource  :: !ProposalHashSource
      , treasuryWithdrawal  :: ![(AnyStakeIdentifier, Lovelace)]
      , filePath            :: !(File () Out)
      } deriving Show

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

data AnyStakeIdentifier
  = AnyStakeKey (VerificationKeyOrHashOrFile StakeKey)
  | AnyStakePoolKey (VerificationKeyOrHashOrFile StakePoolKey)
  deriving Show
