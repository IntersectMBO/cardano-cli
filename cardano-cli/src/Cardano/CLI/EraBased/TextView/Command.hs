{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.TextView.Command
  ( TextViewCmds (..)
  , TextViewDecodeCborCmdArgs (..)
  , renderTextViewCmds
  )
where

import Cardano.Api.Shelley

import Cardano.CLI.Type.Common (FormatCborHex, FormatText)

import Data.Text (Text)
import Vary

newtype TextViewCmds era
  = TextViewDecodeCborCmd TextViewDecodeCborCmdArgs
  deriving Show

data TextViewDecodeCborCmdArgs
  = TextViewDecodeCborCmdArgs
  { inputFile :: !FilePath
  , outputFormat :: !(Vary [FormatCborHex, FormatText])
  , mOutFile :: Maybe (File () Out)
  }
  deriving Show

renderTextViewCmds :: TextViewCmds era -> Text
renderTextViewCmds = \case
  TextViewDecodeCborCmd _ -> "text-view decode-cbor"
