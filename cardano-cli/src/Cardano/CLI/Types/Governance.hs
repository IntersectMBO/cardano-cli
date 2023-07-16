{-# LANGUAGE DataKinds #-}

module Cardano.CLI.Types.Governance where

import           Cardano.Api
import           Cardano.Api.Shelley

import           Cardano.CLI.Types.Key

import           Data.Text (Text)

data NewConstitution = NewConstitution
  { ncEra :: AnyShelleyBasedEra
  , ncDeposit :: Lovelace
  , ncVotingStakeCredential :: VerificationKeyOrFile StakePoolKey
  , ncConstitution :: Constitution
  , ncFilePath :: File () Out -- Constitution file
  } deriving Show

data Constitution
  = ConstitutionFromFile (File () In)
  | ConstitutionFromText Text deriving Show

-- Vote type -- TODO: Conway era - remove me
data VType = VCC -- committee
           | VDR -- drep
           | VSP -- spo
           deriving Show
