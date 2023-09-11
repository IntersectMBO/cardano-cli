{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.EraBased.Options.TextView
  ( pTextViewCmds
  ) where

import           Cardano.CLI.EraBased.Commands.TextView
import           Cardano.CLI.EraBased.Options.Common

import           Data.Foldable
import           Options.Applicative hiding (help, str)
import qualified Options.Applicative as Opt

{- HLINT ignore "Use <$>" -}
{- HLINT ignore "Move brackets to avoid $" -}

pTextViewCmds :: Parser (TextViewCmds era)
pTextViewCmds =
  asum
    [ subParser "decode-cbor"
        $ Opt.info (TextViewInfo <$> pCBORInFile <*> pMaybeOutputFile)
        $ Opt.progDesc "Print a TextView file as decoded CBOR."
    ]
