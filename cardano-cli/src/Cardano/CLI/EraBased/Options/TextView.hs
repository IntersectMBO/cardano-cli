{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.EraBased.Options.TextView
  ( pTextViewCmds
  )
where

import           Cardano.CLI.EraBased.Commands.TextView
import           Cardano.CLI.EraBased.Options.Common
import           Cardano.CLI.Parser

import           Options.Applicative hiding (help, str)
import qualified Options.Applicative as Opt

{- HLINT ignore "Use <$>" -}
{- HLINT ignore "Move brackets to avoid $" -}

pTextViewCmds :: Maybe (Parser (TextViewCmds era))
pTextViewCmds =
  subInfoParser
    "text-view"
    ( Opt.progDesc $
        mconcat
          [ "Commands for dealing with Shelley TextView files. Transactions, addresses etc "
          , "are stored on disk as TextView files."
          ]
    )
    [ Just $
        subParser "decode-cbor" $
          Opt.info (TextViewInfo <$> pCBORInFile <*> pMaybeOutputFile) $
            Opt.progDesc "Print a TextView file as decoded CBOR."
    ]
