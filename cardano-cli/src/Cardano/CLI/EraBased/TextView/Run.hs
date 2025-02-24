{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

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
  TextViewInfo fpath mOutfile -> runTextViewInfoCmd fpath mOutfile

runTextViewInfoCmd
  :: ()
  => FilePath
  -> Maybe (File () Out)
  -> ExceptT TextViewFileError IO ()
runTextViewInfoCmd fpath mOutFile = do
  tv <- firstExceptT TextViewReadFileError $ newExceptT (readTextEnvelopeFromFile fpath)
  let lbCBOR = LBS.fromStrict (textEnvelopeRawCBOR tv)
  case mOutFile of
    Just (File oFpath) -> liftIO $ LBS.writeFile oFpath lbCBOR
    Nothing -> firstExceptT TextViewCBORPrettyPrintError $ pPrintCBOR lbCBOR
