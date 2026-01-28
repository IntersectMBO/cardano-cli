{-# LANGUAGE DataKinds #-}

module Cardano.CLI.Type.Key.VerificationKey
  ( AnyVerificationKeySource (..)
  , AnyVerificationKeyText (..)
  )
where

import Cardano.Api

-- | A bech32 text encoded verification key of an unspecified key role.
newtype AnyVerificationKeyText = AnyVerificationKeyText
  { unAnyVerificationKeyText :: Text
  }
  deriving (Eq, Show)

-- | The source from which a verification key of an unspecified key role can be
-- derived.
data AnyVerificationKeySource
  = AnyVerificationKeySourceOfText !AnyVerificationKeyText
  | AnyVerificationKeySourceOfFile !(File (VerificationKey ()) In)
  deriving (Eq, Show)
