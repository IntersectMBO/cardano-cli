{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.Types.Errors.ProtocolParamsError
  ( ProtocolParamsError(..)
  , renderProtocolParamsError
  ) where

import           Cardano.Api
import           Cardano.Api.Pretty

import           Data.Text (Text)

data ProtocolParamsError
  = ProtocolParamsErrorFile (FileError ())
  | ProtocolParamsErrorJSON !FilePath !Text

renderProtocolParamsError :: ProtocolParamsError -> Doc ann
renderProtocolParamsError = \case
  ProtocolParamsErrorFile fileErr ->
    prettyError fileErr
  ProtocolParamsErrorJSON fp jsonErr ->
    "Error while decoding the protocol parameters at: " <> pshow fp <> " Error: " <> pshow jsonErr
