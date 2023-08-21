{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.Types.Errors.ProtocolParamsError
  ( ProtocolParamsError(..)
  , renderProtocolParamsError
  ) where

import           Cardano.Api

import           Data.Text (Text)
import qualified Data.Text as Text

data ProtocolParamsError
  = ProtocolParamsErrorFile (FileError ())
  | ProtocolParamsErrorJSON !FilePath !Text

renderProtocolParamsError :: ProtocolParamsError -> Text
renderProtocolParamsError (ProtocolParamsErrorFile fileErr) =
  Text.pack $ displayError fileErr
renderProtocolParamsError (ProtocolParamsErrorJSON fp jsonErr) =
  "Error while decoding the protocol parameters at: " <> Text.pack fp <> " Error: " <> jsonErr
