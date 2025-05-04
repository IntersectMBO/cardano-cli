{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.TextView.Command
  ( TextViewCmds (..)
  , TextViewInfoCmdArgs (..)
  , renderTextViewCmds
  )
where

import Cardano.Api.Shelley

import Data.Text (Text)

newtype TextViewCmds era
  = TextViewInfoCmd TextViewInfoCmdArgs
  deriving Show

data TextViewInfoCmdArgs
  = TextViewInfoCmdArgs
  { inputFile :: !FilePath
  , mOutFile :: Maybe (File () Out)
  }
  deriving Show

renderTextViewCmds :: TextViewCmds era -> Text
renderTextViewCmds = \case
  TextViewInfoCmd _ -> "text-view decode-cbor"
