{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.Commands.Governance.Actions
  ( AnyStakeIdentifier(..)
  , GovernanceActionCmds(..)
  , NewCommitteeCmd(..)
  , NewConstitutionCmd(..)
  , NewInfoCmd(..)
  , NoConfidenceCmd(..)
  , TreasuryWithdrawalCmd(..)
  , renderGovernanceActionCmds
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
      NewConstitutionCmd
  | GoveranceActionCreateNewCommitteeCmd
      (ConwayEraOnwards era)
      NewCommitteeCmd
  | GovernanceActionCreateNoConfidenceCmd
      (ConwayEraOnwards era)
      NoConfidenceCmd
  | GovernanceActionProtocolParametersUpdateCmd
      (ShelleyBasedEra era)
      EpochNo
      [VerificationKeyFile In]
      (EraBasedProtocolParametersUpdate era)
      (File () Out)
  | GovernanceActionTreasuryWithdrawalCmd
      (ConwayEraOnwards era)
      TreasuryWithdrawalCmd
  | GovernanceActionInfoCmd
      (ConwayEraOnwards era)
      NewInfoCmd
  deriving Show

data NewCommitteeCmd
  = NewCommitteeCmd
    { ebNetwork :: Ledger.Network
    , ebDeposit :: Lovelace
    , ebReturnAddress :: AnyStakeIdentifier
    , ebProposalUrl :: ProposalUrl
    , ebProposalHashSource :: ProposalHashSource
    , ebOldCommittee :: [VerificationKeyOrHashOrFile CommitteeColdKey]
    , ebNewCommittee :: [(VerificationKeyOrHashOrFile CommitteeColdKey, EpochNo)]
    , ebRequiredQuorum :: Rational
    , ebPreviousGovActionId :: Maybe (TxId, Word32)
    , ebFilePath :: File () Out
    } deriving Show

data NewConstitutionCmd
  = NewConstitutionCmd
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
data NewInfoCmd
   = NewInfoCmd
      { niNetwork :: Ledger.Network
      , niDeposit :: Lovelace
      , niStakeCredential :: AnyStakeIdentifier
      , niProposalUrl :: ProposalUrl
      , niProposalHashSource :: ProposalHashSource
      , niOutputFilePath :: File () Out
      } deriving Show

data NoConfidenceCmd
  = NoConfidenceCmd
      { ncNetwork :: Ledger.Network
      , ncDeposit :: Lovelace
      , ncStakeCredential :: AnyStakeIdentifier
      , ncProposalUrl :: ProposalUrl
      , ncProposalHashSource :: ProposalHashSource
      , ncGovAct :: TxId
      , ncGovActIndex :: Word32
      , ncFilePath :: File () Out
      } deriving Show

data TreasuryWithdrawalCmd
  = TreasuryWithdrawalCmd
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

  GoveranceActionCreateNewCommitteeCmd {} ->
    "create-new-committee"

  GovernanceActionCreateNoConfidenceCmd {} ->
    "create-no-confidence"

  GovernanceActionInfoCmd {} ->
    "create-info"

data AnyStakeIdentifier
  = AnyStakeKey (VerificationKeyOrHashOrFile StakeKey)
  | AnyStakePoolKey (VerificationKeyOrHashOrFile StakePoolKey)
  deriving Show
