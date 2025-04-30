{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.CLI.EraIndependent.Cip.Common
  ( -- * Input related
    Input (..)
  , InputError (..)
  , pInputFile
  , pInputHexText
  , pInputBech32Text

    -- * Output related
  , Output (..)
  , pOutputFile
  , pOutputText
  )
where

import Cardano.Api

import Cardano.CLI.EraBased.Common.Option hiding (pOutputFile)

import Data.Text (Text)
import Data.Text qualified as Text
import Options.Applicative qualified as Opt

data Input
  = InputTextEnvelopeFile (File () In)
  | InputHexText Text
  | InputBech32Text Text

newtype InputError = InputError Text deriving Show

instance Error InputError where
  prettyError (InputError err) = pretty err

pInputFile :: String -> String -> Opt.Parser Input
pInputFile optName desc =
  InputTextEnvelopeFile <$> pFileInDirection optName desc

pInputHexText :: String -> String -> String -> Opt.Parser Input
pInputHexText optName metavar help =
  fmap (InputHexText . Text.pack) $
    Opt.strOption $
      mconcat
        [ Opt.long optName
        , Opt.metavar metavar
        , Opt.help help
        ]

pInputBech32Text :: String -> String -> String -> Opt.Parser Input
pInputBech32Text optName metavar help =
  fmap (InputBech32Text . Text.pack) $
    Opt.strOption $
      mconcat
        [ Opt.long optName
        , Opt.metavar metavar
        , Opt.help help
        ]

data Output
  = OutputFile (File () Out)
  | OutputText

pOutputFile :: String -> String -> Opt.Parser Output
pOutputFile optName desc =
  OutputFile <$> pFileOutDirection optName desc

pOutputText :: String -> String -> Opt.Parser Output
pOutputText optName help =
  Opt.flag' OutputText $
    mconcat
      [ Opt.long optName
      , Opt.help help
      ]
