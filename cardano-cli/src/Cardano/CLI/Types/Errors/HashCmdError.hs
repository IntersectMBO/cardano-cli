{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Types.Errors.HashCmdError
  ( HashCmdError (..)
  , HttpRequestError (..)
  )
where

import           Cardano.Api
import qualified Cardano.Api.Ledger as L

import           Cardano.CLI.Read (ScriptDecodeError)
import           Cardano.Ledger.SafeHash (extractHash)
import           Cardano.Prelude (Exception (displayException), IOException)

import           Network.HTTP.Client (HttpException)

data HashCmdError
  = HashMismatchedHashError
      !(L.SafeHash L.StandardCrypto L.AnchorData)
      !(L.SafeHash L.StandardCrypto L.AnchorData)
  | HashReadFileError !FilePath !IOException
  | HashWriteFileError !(FileError ())
  | HashReadScriptError !FilePath !(FileError ScriptDecodeError)
  | HashInvalidURLError !String
  | HashUnsupportedURLSchemeError !String
  | HashGetFileFromHttpError !HttpRequestError
  deriving Show

instance Error HashCmdError where
  prettyError = \case
    HashMismatchedHashError expectedHash actualHash ->
      "Hashes do not match! \n"
        <> "Expected: "
        <> pretty (show (extractHash expectedHash))
        <> "\n  Actual: "
        <> pretty (show (extractHash actualHash))
    HashReadFileError filepath exc ->
      "Cannot read " <> pretty filepath <> ": " <> pretty (displayException exc)
    HashWriteFileError fileErr ->
      prettyError fileErr
    HashReadScriptError filepath err ->
      "Cannot read script at " <> pretty filepath <> ": " <> prettyError err
    HashInvalidURLError text -> "Cannot parse URL: " <> pretty text
    HashUnsupportedURLSchemeError text -> "Unsupported URL scheme: " <> pretty text
    HashGetFileFromHttpError err -> pretty $ displayException err

data HttpRequestError
  = BadStatusCodeHRE !Int !String
  | HttpExceptionHRE !HttpException
  | IOExceptionHRE !IOException
  deriving Show

instance Exception HttpRequestError where
  displayException :: HttpRequestError -> String
  displayException (BadStatusCodeHRE code description) = "Bad status code when downloading anchor data: " <> show code <> " (" <> description <> ")"
  displayException (HttpExceptionHRE exc) = "HTTP request error when downloading anchor data: " <> displayException exc
  displayException (IOExceptionHRE exc) = "I/O error when downloading anchor data: " <> displayException exc
