{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cardano.CLI.EraBased.Commands.Governance.Actions
  ( AnyStakeIdentifier(..)
  , GovernanceActionCmds(..)
  , EraBasedNewCommittee(..)
  , EraBasedNewConstitution(..)
  , EraBasedTreasuryWithdrawal(..)
  , renderGovernanceActionCmds
  ) where

import           Cardano.Api
import           Cardano.Api.Shelley

import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Key

import           Data.Text (Text)

data GovernanceActionCmds era
  = GovernanceActionCreateConstitution
      (ConwayEraOnwards era)
      EraBasedNewConstitution
  | GoveranceActionCreateNewCommittee
      (ConwayEraOnwards era)
      EraBasedNewCommittee
  | GovernanceActionProtocolParametersUpdate
      (ShelleyBasedEra era)
      EpochNo
      [VerificationKeyFile In]
      (EraBasedProtocolParametersUpdate era)
      (File () Out)
  | GovernanceActionTreasuryWithdrawal
      (ConwayEraOnwards era)
      EraBasedTreasuryWithdrawal
  deriving Show

data EraBasedNewCommittee
  = EraBasedNewCommittee
    { ebDeposit :: Lovelace
    , ebReturnAddress :: AnyStakeIdentifier
    , ebOldCommittee :: [AnyStakeIdentifier]
    , ebNewCommittee :: [(AnyStakeIdentifier, EpochNo)]
    , ebRequiredQuorum :: Rational
    , ebPreviousGovActionId :: TxIn
    , ebFilePath :: File () Out
    } deriving Show

data EraBasedNewConstitution
  = EraBasedNewConstitution
      { encDeposit :: Lovelace
      , encStakeCredential :: AnyStakeIdentifier
      , encConstitution :: Constitution
      , encFilePath :: File () Out
      } deriving Show

data EraBasedTreasuryWithdrawal where
  EraBasedTreasuryWithdrawal
    :: Lovelace -- ^ Deposit
    -> AnyStakeIdentifier -- ^ Return address
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

data AnyStakeIdentifier
  = AnyStakeKey (VerificationKeyOrHashOrFile StakeKey)
  | AnyStakePoolKey (VerificationKeyOrHashOrFile StakePoolKey)
  deriving Show
