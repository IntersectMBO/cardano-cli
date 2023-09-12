{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.Run.TextView
  ( runTextViewCmds

  , runTextViewInfoCmd
  ) where

import           Cardano.Api

import           Cardano.CLI.EraBased.Commands.TextView
import           Cardano.CLI.Helpers (pPrintCBOR)
import           Cardano.CLI.Types.Errors.TextViewFileError

import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, newExceptT)
import qualified Data.ByteString.Lazy.Char8 as LBS

runTextViewCmds :: TextViewCmds era -> ExceptT TextViewFileError IO ()
runTextViewCmds = \case
  TextViewInfo fpath mOutfile -> runTextViewInfoCmd fpath mOutfile

runTextViewInfoCmd :: ()
  => FilePath
  -> Maybe (File () Out)
  -> ExceptT TextViewFileError IO ()
runTextViewInfoCmd fpath mOutFile = do
  tv <- firstExceptT TextViewReadFileError $ newExceptT (readTextEnvelopeFromFile fpath)
  let lbCBOR = LBS.fromStrict (textEnvelopeRawCBOR tv)
  case mOutFile of
    Just (File oFpath) -> liftIO $ LBS.writeFile oFpath lbCBOR
    Nothing -> firstExceptT TextViewCBORPrettyPrintError $ pPrintCBOR lbCBOR
