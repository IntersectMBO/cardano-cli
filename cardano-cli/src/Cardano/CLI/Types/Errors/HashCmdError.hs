{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Types.Errors.HashCmdError
  ( HashCmdError (..)
  , HttpRequestError (..)
  , FetchURLError (..)
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
      -- ^ Expected hash
      !(L.SafeHash L.StandardCrypto L.AnchorData)
      -- ^ Actual hash
  | HashReadFileError !FilePath !IOException
  | HashWriteFileError !(FileError ())
  | HashReadScriptError !FilePath !(FileError ScriptDecodeError)
  | HashFetchURLError !FetchURLError
  deriving Show

instance Error HashCmdError where
  prettyError = \case
    HashMismatchedHashError expectedHash actualHash ->
      "Hashes do not match!"
        <> "\nExpected:"
          <+> pretty (show (extractHash expectedHash))
        <> "\n  Actual:"
          <+> pretty (show (extractHash actualHash))
    HashReadFileError filepath exc ->
      "Cannot read" <+> pretty filepath <> ":" <+> pretty (displayException exc)
    HashWriteFileError fileErr ->
      prettyError fileErr
    HashReadScriptError filepath err ->
      "Cannot read script at" <+> pretty filepath <> ":" <+> prettyError err
    HashFetchURLError fetchErr ->
      pretty (displayException fetchErr)

data FetchURLError
  = FetchURLInvalidURLError !String
  | FetchURLReadFileError !FilePath !IOException
  | FetchURLUnsupportedURLSchemeError !String
  | FetchURLReadEnvVarError !IOException
  | FetchURLGetFileFromHttpError !HttpRequestError
  | FetchURLIpfsGatewayNotSetError
  deriving Show

instance Exception FetchURLError where
  displayException :: FetchURLError -> String
  displayException (FetchURLInvalidURLError text) = "Cannot parse URI: " <> text
  displayException (FetchURLReadFileError filepath exc) =
    "Cannot read " <> filepath <> ": " <> displayException exc
  displayException (FetchURLUnsupportedURLSchemeError text) = "Unsupported URL scheme: " <> text
  displayException (FetchURLReadEnvVarError exc) = "Cannot read environment variable: " <> displayException exc
  displayException (FetchURLGetFileFromHttpError err) = displayException err
  displayException FetchURLIpfsGatewayNotSetError = "IPFS schema requires IPFS_GATEWAY_URI environment variable to be set."

data HttpRequestError
  = BadStatusCodeHRE !Int !String
  | HttpExceptionHRE !HttpException
  | IOExceptionHRE !IOException
  deriving Show

instance Exception HttpRequestError where
  displayException :: HttpRequestError -> String
  displayException (BadStatusCodeHRE code description) = "Bad status code when downloading anchor data: " <> show code <> " (" <> description <> ")"
  displayException (HttpExceptionHRE exc) = "HTTP(S) request error when downloading anchor data: " <> displayException exc
  displayException (IOExceptionHRE exc) = "I/O error when downloading anchor data: " <> displayException exc
