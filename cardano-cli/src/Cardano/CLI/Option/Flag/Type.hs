{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}

module Cardano.CLI.Option.Flag.Type
  ( Flag (..)
  , Defaultness (..)
  , FlagOptions (..)
  , defaultFlagOptions
  )
where

import GHC.Generics

data Defaultness
  = IsDefault
  | NonDefault
  deriving (Show, Eq, Generic)

-- | Options for a flag that control how it is rendered and parsed.
newtype FlagOptions = FlagOptions
  { isDefault :: Defaultness
  -- ^ Whether the flag is a default value.
  }
  deriving (Show, Eq, Generic)

-- instance Semigroup FlagOptions where
--   FlagOptions IsDefault <> FlagOptions _ = FlagOptions IsDefault
--   FlagOptions _ <> FlagOptions IsDefault = FlagOptions IsDefault
--   FlagOptions _ <> FlagOptions _ = FlagOptions NonDefault

-- instance Monoid FlagOptions where
--   mempty = FlagOptions NonDefault
--   mappend = (<>)

-- | A flag that can be used in the command line interface.
--
-- It contains information about how to render the flag, its long name,
-- its content, and its value.
-- The type variable 'a' represents the type of the value that the flag holds.
--
-- The reason for this type instead of using 'Parser a' directly is to
-- allow for more complex parsing logic, such as handling default values.
data Flag a = Flag
  { longName :: String
  , format :: String
  , options :: FlagOptions
  , value :: a
  }
  deriving (Show, Eq, Generic)

defaultFlagOptions :: FlagOptions
defaultFlagOptions = FlagOptions NonDefault
