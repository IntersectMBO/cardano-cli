{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.TextView.Command
  ( TextViewCmds (..)
  , renderTextViewCmds
  )
where

import Cardano.Api.Shelley

import Data.Text (Text)

data TextViewCmds era
  = TextViewInfo
      !FilePath
      (Maybe (File () Out))
  deriving Show

renderTextViewCmds :: TextViewCmds era -> Text
renderTextViewCmds = \case
  TextViewInfo _ _ -> "text-view decode-cbor"
