{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.Commands.Governance.Actions
  ( AnyStakeIdentifier(..)
  , GovernanceActionCmds(..)
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
  = GovernanceActionCreateConstitution
      (ConwayEraOnwards era)
      Ledger.Network
      Lovelace
      AnyStakeIdentifier
      (Maybe (TxId, Word32))
      ProposalUrl
      ProposalHashSource
      ConstitutionUrl
      ConstitutionHashSource
      (File () Out)
  | GoveranceActionCreateNewCommittee
      (ConwayEraOnwards era)
      Ledger.Network
      Lovelace
      AnyStakeIdentifier
      ProposalUrl
      ProposalHashSource
      [AnyStakeIdentifier]
      [(AnyStakeIdentifier, EpochNo)]
      Rational
      (Maybe (TxId, Word32))
      (File () Out)
  | GovernanceActionCreateNoConfidence
      (ConwayEraOnwards era)
      Ledger.Network
      Lovelace
      AnyStakeIdentifier
      ProposalUrl
      ProposalHashSource
      TxId
      Word32
      (File () Out)
  | GovernanceActionProtocolParametersUpdate
      (ShelleyBasedEra era)
      EpochNo
      [VerificationKeyFile In]
      (EraBasedProtocolParametersUpdate era)
      (File () Out)
  | GovernanceActionTreasuryWithdrawal
      (ConwayEraOnwards era)
      Ledger.Network
      Lovelace -- ^ Deposit
      AnyStakeIdentifier -- ^ Return address
      ProposalUrl
      ProposalHashSource
      [(AnyStakeIdentifier, Lovelace)]
      (File () Out)
  | GoveranceActionInfo -- TODO: Conway era - ledger currently provides a placeholder constructor
      (ConwayEraOnwards era)
      (File () In)
      (File () Out)
  deriving Show

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
