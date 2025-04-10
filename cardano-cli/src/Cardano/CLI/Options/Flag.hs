{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module Cardano.CLI.Options.Flag
  ( Flag (..)
  , FlagOptions (..)
  , isDefault
  , mkFlag
  , parserFromFlags
  )
where

import Cardano.CLI.Vary
import Cardano.CLI.Vary qualified as Vary

import Control.Applicative
import Options.Applicative (Parser)
import Options.Applicative qualified as Opt

-- | Options for a flag that control how it is rendered and parsed.
newtype FlagOptions = FlagOptions
  { isDefault :: Bool
  -- ^ Whether the flag is a default value.
  }

instance Semigroup FlagOptions where
  FlagOptions a <> FlagOptions b = FlagOptions (a || b)

instance Monoid FlagOptions where
  mempty = FlagOptions False
  mappend = (<>)

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

-- | Create a parser from a help rendering function and list of flags.
-- A default parser is included at the end of parser alternatives for
-- the default flag (there should only be one default, but if more than
-- one is specified, the first such one is used as the default).
parserFromFlags :: (Flag a -> String) -> [Flag a] -> Parser a
parserFromFlags _ [] = empty
parserFromFlags mkHelp fs =
  alternatives fs <|> defaults fs
 where
  alternatives [] = empty
  alternatives (x : xs) =
    parserFromFlag mkHelp x <|> alternatives xs

  defaults :: [Flag a] -> Parser a
  defaults [] = empty
  defaults (x : xs) =
    parserFromFlagDefault x <|> defaults xs

parserFromFlag :: (Flag a -> String) -> Flag a -> Parser a
parserFromFlag mkHelp flag =
  Opt.flag' flag.value $
    mconcat
      [ Opt.long flag.longName
      , Opt.help $ mkHelp flag
      ]

parserFromFlagDefault :: Flag a -> Parser a
parserFromFlagDefault flag =
  pure flag.value

mkFlag
  :: a :| fs
  => String
  -> String
  -> a
  -> Flag (Vary fs)
mkFlag longName format value =
  Flag
    { longName = longName
    , format = format
    , value = Vary.from value
    , options = mempty
    }

isDefault :: Flag a -> Flag a
isDefault ff =
  ff
    { options = FlagOptions True
    }
