{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.Type.Error.ProtocolParamsError
  ( ProtocolParamsError (..)
  , renderProtocolParamsError
  )
where

import Cardano.Api

data ProtocolParamsError
  = ProtocolParamsErrorFile (FileError ())
  | ProtocolParamsErrorJSON !FilePath !Text
  deriving Show

instance Error ProtocolParamsError where
  prettyError = renderProtocolParamsError

renderProtocolParamsError :: ProtocolParamsError -> Doc ann
renderProtocolParamsError = \case
  ProtocolParamsErrorFile fileErr ->
    prettyError fileErr
  ProtocolParamsErrorJSON fp jsonErr ->
    "Error while decoding the protocol parameters at: " <> pshow fp <> " Error: " <> pshow jsonErr
