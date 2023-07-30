{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.Commands.Governance.Vote
  ( GovernanceVoteCmds(..)
  , renderGovernanceVoteCmds
  ) where

import           Cardano.Api
import           Cardano.Api.Shelley

import           Cardano.CLI.Types.Governance
import           Cardano.CLI.Types.Key

import           Data.Text (Text)

data GovernanceVoteCmds
  = GovernanceVoteCreateCmd
      Vote
      VType
      TxIn
      (VerificationKeyOrFile StakePoolKey)
      AnyShelleyBasedEra
      (File () Out) -- TODO Use specific file type
  deriving Show

renderGovernanceVoteCmds :: ()
  => GovernanceVoteCmds
  -> Text
renderGovernanceVoteCmds = \case
  GovernanceVoteCreateCmd {} -> "governance vote create"
