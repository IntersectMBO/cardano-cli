{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.Commands.Governance.Actions
  ( GovernanceActionCmds(..)
  , GoveranceActionUpdateCommitteeCmdArgs(..)
  , GovernanceActionCreateConstitutionCmdArgs(..)
  , GovernanceActionInfoCmdArgs(..)
  , GovernanceActionCreateNoConfidenceCmdArgs(..)
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
  = GovernanceActionCreateConstitutionCmd
      (ConwayEraOnwards era)
      (GovernanceActionCreateConstitutionCmdArgs era)
  | GoveranceActionUpdateCommitteeCmd
      (ConwayEraOnwards era)
      (GoveranceActionUpdateCommitteeCmdArgs era)
  | GovernanceActionCreateNoConfidenceCmd
      (ConwayEraOnwards era)
      (GovernanceActionCreateNoConfidenceCmdArgs era)
  | GovernanceActionProtocolParametersUpdateCmd
      (ConwayEraOnwards era)
      EpochNo
      [VerificationKeyFile In]
      (EraBasedProtocolParametersUpdate era)
      (File () Out)
  | GovernanceActionTreasuryWithdrawalCmd
      (ConwayEraOnwards era)
      (GovernanceActionTreasuryWithdrawalCmdArgs era)
  | GovernanceActionInfoCmd
      (ConwayEraOnwards era)
      (GovernanceActionInfoCmdArgs era)
  deriving Show

data GoveranceActionUpdateCommitteeCmdArgs era
  = GoveranceActionUpdateCommitteeCmdArgs
    { ucNetwork :: Ledger.Network
    , ucDeposit :: Lovelace
    , ucReturnAddress :: AnyStakeIdentifier
    , ucProposalUrl :: ProposalUrl
    , ucProposalHashSource :: ProposalHashSource
    , ucOldCommittee :: [VerificationKeyOrHashOrFile CommitteeColdKey]
    , ucNewCommittee :: [(VerificationKeyOrHashOrFile CommitteeColdKey, EpochNo)]
    , ucRequiredQuorum :: Rational
    , ucPreviousGovActionId :: Maybe (TxId, Word32)
    , ucFilePath :: File () Out
    } deriving Show

data GovernanceActionCreateConstitutionCmdArgs era
  = GovernanceActionCreateConstitutionCmdArgs
      { encNetwork :: Ledger.Network
      , encDeposit :: Lovelace
      , encStakeCredential :: AnyStakeIdentifier
      , encPrevGovActId :: Maybe (TxId, Word32)
      , encProposalUrl :: ProposalUrl
      , encProposalHashSource :: ProposalHashSource
      , encConstitutionUrl :: ConstitutionUrl
      , encConstitutionHashSource :: ConstitutionHashSource
      , encFilePath :: File () Out
      } deriving Show

-- | Datatype to carry data for the create-info governance action
data GovernanceActionInfoCmdArgs era
   = GovernanceActionInfoCmdArgs
      { niNetwork :: Ledger.Network
      , niDeposit :: Lovelace
      , niStakeCredential :: AnyStakeIdentifier
      , niProposalUrl :: ProposalUrl
      , niProposalHashSource :: ProposalHashSource
      , niOutputFilePath :: File () Out
      } deriving Show

data GovernanceActionCreateNoConfidenceCmdArgs era
  = GovernanceActionCreateNoConfidenceCmdArgs
      { ncNetwork :: Ledger.Network
      , ncDeposit :: Lovelace
      , ncStakeCredential :: AnyStakeIdentifier
      , ncProposalUrl :: ProposalUrl
      , ncProposalHashSource :: ProposalHashSource
      , ncGovAct :: TxId
      , ncGovActIndex :: Word32
      , ncFilePath :: File () Out
      } deriving Show

data GovernanceActionTreasuryWithdrawalCmdArgs era
  = GovernanceActionTreasuryWithdrawalCmdArgs
    { twNetwork :: Ledger.Network
    , twDeposit :: Lovelace -- ^ Deposit
    , twReturnAddr :: AnyStakeIdentifier -- ^ Return address
    , twProposalUrl :: ProposalUrl
    , twProposalHashSource :: ProposalHashSource
    , twTreasuryWithdrawal :: [(AnyStakeIdentifier, Lovelace)]
    , twFilePath :: File () Out
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
