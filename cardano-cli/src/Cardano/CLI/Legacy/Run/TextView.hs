{-# LANGUAGE DataKinds #-}

module Cardano.CLI.Legacy.Run.TextView
  ( ShelleyTextViewFileError(..)
  , renderShelleyTextViewFileError
  , runTextViewCmds
  ) where

import           Cardano.Api

import           Cardano.CLI.Helpers (HelpersError, pPrintCBOR, renderHelpersError)
import           Cardano.CLI.Legacy.Options

import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, newExceptT)
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Text (Text)
import qualified Data.Text as Text

data ShelleyTextViewFileError
  = TextViewReadFileError (FileError TextEnvelopeError)
  | TextViewCBORPrettyPrintError !HelpersError
  deriving Show

renderShelleyTextViewFileError :: ShelleyTextViewFileError -> Text
renderShelleyTextViewFileError err =
  case err of
    TextViewReadFileError fileErr -> Text.pack (displayError fileErr)
    TextViewCBORPrettyPrintError hlprsErr ->
      "Error pretty printing CBOR: " <> renderHelpersError hlprsErr


runTextViewCmds :: LegacyTextViewCmds -> ExceptT ShelleyTextViewFileError IO ()
runTextViewCmds cmd =
  case cmd of
    TextViewInfo fpath mOutfile -> runTextViewInfo fpath mOutfile

runTextViewInfo :: FilePath -> Maybe (File () Out) -> ExceptT ShelleyTextViewFileError IO ()
runTextViewInfo fpath mOutFile = do
  tv <- firstExceptT TextViewReadFileError $ newExceptT (readTextEnvelopeFromFile fpath)
  let lbCBOR = LBS.fromStrict (textEnvelopeRawCBOR tv)
  case mOutFile of
    Just (File oFpath) -> liftIO $ LBS.writeFile oFpath lbCBOR
    Nothing -> firstExceptT TextViewCBORPrettyPrintError $ pPrintCBOR lbCBOR
