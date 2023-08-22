{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cardano.CLI.EraBased.Commands.Governance.Actions
  ( AnyStakeIdentifier(..)
  , GovernanceActionCmds(..)
  , EraBasedNewCommittee(..)
  , EraBasedNewConstitution(..)
  , EraBasedNoConfidence(..)
  , EraBasedTreasuryWithdrawal(..)
  , renderGovernanceActionCmds
  ) where

import           Cardano.Api
import qualified Cardano.Api.Ledger as Ledger
import           Cardano.Api.Shelley

import           Cardano.CLI.Types.Common (Constitution, VerificationKeyFile)
import           Cardano.CLI.Types.Key

import           Data.Text (Text)
import           Data.Word

data GovernanceActionCmds era
  = GovernanceActionCreateConstitution
      (ConwayEraOnwards era)
      EraBasedNewConstitution
  | GoveranceActionCreateNewCommittee
      (ConwayEraOnwards era)
      EraBasedNewCommittee
  | GovernanceActionCreateNoConfidence
      (ConwayEraOnwards era)
      EraBasedNoConfidence
  | GovernanceActionProtocolParametersUpdate
      (ShelleyBasedEra era)
      EpochNo
      [VerificationKeyFile In]
      (EraBasedProtocolParametersUpdate era)
      (File () Out)
  | GovernanceActionTreasuryWithdrawal
      (ConwayEraOnwards era)
      EraBasedTreasuryWithdrawal
  | GoveranceActionInfo -- TODO: Conway era - ledger currently provides a placeholder constructor
      (ConwayEraOnwards era)
      (File () In)
      (File () Out)
  deriving Show

data EraBasedNewCommittee
  = EraBasedNewCommittee
    { ebNetwork :: Ledger.Network
    , ebDeposit :: Lovelace
    , ebReturnAddress :: AnyStakeIdentifier
    , ebPropAnchor :: (Ledger.Url, Text)
    , ebOldCommittee :: [AnyStakeIdentifier]
    , ebNewCommittee :: [(AnyStakeIdentifier, EpochNo)]
    , ebRequiredQuorum :: Rational
    , ebPreviousGovActionId :: Maybe (TxId, Word32)
    , ebFilePath :: File () Out
    } deriving Show

data EraBasedNewConstitution
  = EraBasedNewConstitution
      { encNetwork :: Ledger.Network
      , encDeposit :: Lovelace
      , encStakeCredential :: AnyStakeIdentifier
      , encPrevGovActId :: Maybe (TxId, Word32)
      , encPropAnchor :: (Ledger.Url, Text)
      , encConstitution :: Constitution
      , encFilePath :: File () Out
      } deriving Show

data EraBasedNoConfidence
  = EraBasedNoConfidence
      { ncNetwork :: Ledger.Network
      , ncDeposit :: Lovelace
      , ncStakeCredential :: AnyStakeIdentifier
      , ncProposalAnchor :: (Ledger.Url, Text)
      , ncGovAct :: TxId
      , ncGovActIndex :: Word32
      , ncFilePath :: File () Out
      } deriving Show

data EraBasedTreasuryWithdrawal where
  EraBasedTreasuryWithdrawal
    :: Ledger.Network
    -> Lovelace -- ^ Deposit
    -> AnyStakeIdentifier -- ^ Return address
    -> (Ledger.Url, Text) -- ^ Proposal anchor
    -> [(AnyStakeIdentifier, Lovelace)]
    -> File () Out
    -> EraBasedTreasuryWithdrawal

deriving instance Show EraBasedTreasuryWithdrawal

renderGovernanceActionCmds :: GovernanceActionCmds era -> Text
renderGovernanceActionCmds = \case
  GovernanceActionCreateConstitution {} ->
    "governance action create-constitution"

  GovernanceActionProtocolParametersUpdate {} ->
    "governance action create-protocol-parameters-update"

  GovernanceActionTreasuryWithdrawal {} ->
    "governance action create-treasury-withdrawal"

  GoveranceActionCreateNewCommittee {} ->
    "governance action create-new-committee"

  GovernanceActionCreateNoConfidence {} ->
    "governance action create-no-confidence"

  GoveranceActionInfo {} ->
    "governance action create-info"

data AnyStakeIdentifier
  = AnyStakeKey (VerificationKeyOrHashOrFile StakeKey)
  | AnyStakePoolKey (VerificationKeyOrHashOrFile StakePoolKey)
  deriving Show
