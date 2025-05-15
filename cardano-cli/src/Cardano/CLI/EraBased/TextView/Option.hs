{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.EraBased.TextView.Option
  ( pTextViewCmds
  )
where

import Cardano.CLI.EraBased.Common.Option
import Cardano.CLI.EraBased.TextView.Command
import Cardano.CLI.Option.Flag
import Cardano.CLI.Parser

import Data.Function ((&))
import Options.Applicative hiding (help, str)
import Options.Applicative qualified as Opt

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
    [ Just
        . Opt.hsubparser
        . commandWithMetavar "decode-cbor"
        $ Opt.info
          ( fmap
              TextViewDecodeCborCmd
              $ TextViewDecodeCborCmdArgs
                <$> pCBORInFile
                <*> pFormatFlags
                  "text view info output format"
                  [ flagFormatCborHex
                  , flagFormatText & setDefault
                  ]
                <*> pMaybeOutputFile
          )
        $ Opt.progDesc "Print a TextView file as decoded CBOR."
    ]
