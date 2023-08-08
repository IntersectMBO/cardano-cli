{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.Commands.Governance.Actions
  ( AnyStakeIdentifier(..)
  , GovernanceActionCmds(..)
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


data AnyStakeIdentifier
  = AnyStakeKey (VerificationKeyOrHashOrFile StakeKey)
  | AnyStakePoolKey (VerificationKeyOrHashOrFile StakePoolKey)
  deriving Show
