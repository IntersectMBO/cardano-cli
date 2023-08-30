{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Legacy.Run.TextView
  ( runLegacyTextViewCmds
  ) where

import           Cardano.Api

import           Cardano.CLI.Helpers (pPrintCBOR)
import           Cardano.CLI.Legacy.Commands.TextView
import           Cardano.CLI.Types.Errors.ShelleyTextViewFileError

import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, newExceptT)
import qualified Data.ByteString.Lazy.Char8 as LBS

runLegacyTextViewCmds :: LegacyTextViewCmds -> ExceptT ShelleyTextViewFileError IO ()
runLegacyTextViewCmds = \case
  TextViewInfo fpath mOutfile -> runLegacyTextViewInfoCmd fpath mOutfile

runLegacyTextViewInfoCmd :: FilePath -> Maybe (File () Out) -> ExceptT ShelleyTextViewFileError IO ()
runLegacyTextViewInfoCmd fpath mOutFile = do
  tv <- firstExceptT TextViewReadFileError $ newExceptT (readTextEnvelopeFromFile fpath)
  let lbCBOR = LBS.fromStrict (textEnvelopeRawCBOR tv)
  case mOutFile of
    Just (File oFpath) -> liftIO $ LBS.writeFile oFpath lbCBOR
    Nothing -> firstExceptT TextViewCBORPrettyPrintError $ pPrintCBOR lbCBOR
