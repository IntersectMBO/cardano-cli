{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Legacy.Commands.TextView
  ( LegacyTextViewCmds (..)
  , renderLegacyTextViewCmds
  ) where

import Cardano.Api.Shelley

import Data.Text (Text)

data LegacyTextViewCmds
  = TextViewInfo !FilePath (Maybe (File () Out))
  deriving (Show)

renderLegacyTextViewCmds :: LegacyTextViewCmds -> Text
renderLegacyTextViewCmds = \case
  TextViewInfo _ _ -> "text-view decode-cbor"
