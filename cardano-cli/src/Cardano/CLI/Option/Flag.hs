{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.CLI.Option.Flag
  ( Flag (Flag)
  , FlagOptions (FlagOptions)
  , Defaultness (..)
  , setDefault
  , mkFlag
  , parserFromFlags
  )
where

import Cardano.CLI.Option.Flag.Type
  ( Defaultness (IsDefault)
  , Flag (Flag)
  , FlagOptions (FlagOptions)
  , defaultFlagOptions
  )
import Cardano.CLI.Option.Flag.Type qualified as Z

import Control.Applicative
import Data.Function
import Data.Generics.Product.Any
import Lens.Micro
import Options.Applicative (Parser)
import Options.Applicative qualified as Opt
import Vary

-- | Create a parser from a help rendering function and list of flags.
-- A default parser is included at the end of parser alternatives for
-- the default flag (there should only be one default, but if more than
-- one is specified, the first such one is used as the default).
parserFromFlags :: Parser a -> (Flag a -> String) -> [Flag a] -> Parser a
parserFromFlags p mkHelp fs =
  alternatives fs <|> p <|> defaults fs
 where
  alternatives [] = empty
  alternatives (x : xs) =
    parserFromFlag mkHelp x <|> alternatives xs

  defaults :: [Flag a] -> Parser a
  defaults = \case
    [] -> empty
    (x : xs) | flagIsDefault x -> parserFromFlagDefault x <|> defaults xs
    (_ : xs) -> defaults xs

flagIsDefault :: Flag a -> Bool
flagIsDefault flag =
  Z.isDefault (Z.options flag) == Z.IsDefault

parserFromFlag :: (Flag a -> String) -> Flag a -> Parser a
parserFromFlag mkHelp flag =
  Opt.flag' (flag & Z.value) $
    mconcat
      [ Opt.long $ flag & Z.longName
      , Opt.help $ mkHelp flag
      ]

parserFromFlagDefault :: Flag a -> Parser a
parserFromFlagDefault flag =
  pure $ flag & Z.value

mkFlag
  :: a :| fs
  => String
  -> String
  -> a
  -> Flag (Vary fs)
mkFlag longName format value =
  Flag longName format defaultFlagOptions (Vary.from value)

setDefault :: Flag a -> Flag a
setDefault flag =
  flag & the @"options" . the @"isDefault" .~ IsDefault
