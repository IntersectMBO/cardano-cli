{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- HLINT ignore "Use <$>" -}
{- HLINT ignore "Move brackets to avoid $" -}

module Cardano.CLI.Legacy.Options.TextView
  ( parseTextViewCmds
  ) where

import           Cardano.CLI.EraBased.Options.Common
import           Cardano.CLI.Legacy.Commands.TextView

import           Data.Foldable
import           Options.Applicative hiding (help, str)
import qualified Options.Applicative as Opt


parseTextViewCmds :: Parser LegacyTextViewCmds
parseTextViewCmds =
  asum
    [ subParser "decode-cbor"
        (Opt.info (TextViewInfo <$> pCBORInFile <*> pMaybeOutputFile)
          $ Opt.progDesc "Print a TextView file as decoded CBOR."
          )
    ]
