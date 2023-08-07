{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cardano.CLI.EraBased.Commands.Governance.Actions
  ( AnyStakeIdentifier(..)
  , GovernanceActionCmds(..)
  , EraBasedUpdateProposal(..)
  , EraBasedNewConstitution(..)
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
  | GovernanceActionCreateUpdateProposal
      (ConwayEraOnwards era)
      EraBasedUpdateProposal
    deriving Show

data EraBasedNewConstitution
  = EraBasedNewConstitution
      { encDeposit :: Lovelace
      , encStakeCredential :: AnyStakeIdentifier
      , encConstitution :: Constitution
      , encFilePath :: File () Out
      } deriving Show

renderGovernanceActionCmds :: GovernanceActionCmds era -> Text
renderGovernanceActionCmds = \case
  GovernanceActionCreateConstitution {} ->
    "governance action create-constitution"

  GovernanceActionCreateUpdateProposal {} ->
    "governance action create-update-proposal"

data AnyStakeIdentifier
  = AnyStakeKey (VerificationKeyOrHashOrFile StakeKey)
  | AnyStakePoolKey (VerificationKeyOrHashOrFile StakePoolKey)
  deriving Show


data EraBasedUpdateProposal where
  ConwayOnwardsUpdateProposal
    :: ConwayEraOnwards era
    -> ProtocolParametersUpdate
    -> Lovelace -- ^ Deposit
    -> AnyStakeIdentifier -- ^ Return address
    -> Maybe (File () In) -- ^ Cost model file
    -> File () Out
    -> EraBasedUpdateProposal
  -- TODO: Conway era - add constructor for pre-conway
  -- update proposals. This will require genesis delegate keys
  -- see UpdateProposal type in cardano-api.

deriving instance  Show EraBasedUpdateProposal

