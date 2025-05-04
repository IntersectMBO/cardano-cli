{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Cardano.CLI.EraBased.TextView.Run
  ( runTextViewCmds
  , runTextViewInfoCmd
  )
where

import Cardano.Api

import Cardano.CLI.EraBased.TextView.Command
import Cardano.CLI.Helper (pPrintCBOR)
import Cardano.CLI.Type.Error.TextViewFileError

import Data.ByteString.Lazy.Char8 qualified as LBS

runTextViewCmds :: TextViewCmds era -> ExceptT TextViewFileError IO ()
runTextViewCmds = \case
  TextViewInfoCmd cmd -> runTextViewInfoCmd cmd

runTextViewInfoCmd
  :: ()
  => TextViewInfoCmdArgs
  -> ExceptT TextViewFileError IO ()
runTextViewInfoCmd
  TextViewInfoCmdArgs
    { inputFile
    , mOutFile
    } = do
    tv <- firstExceptT TextViewReadFileError $ newExceptT (readTextEnvelopeFromFile inputFile)
    let lbCBOR = LBS.fromStrict (textEnvelopeRawCBOR tv)
    case mOutFile of
      Just (File oFpath) -> liftIO $ LBS.writeFile oFpath lbCBOR
      Nothing -> firstExceptT TextViewCBORPrettyPrintError $ pPrintCBOR lbCBOR
