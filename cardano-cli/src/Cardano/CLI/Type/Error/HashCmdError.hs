{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.CLI.Type.Error.HashCmdError
  ( HashCmdError (..)
  , HttpRequestError (..)
  , FetchURLError (..)
  , HashCheckError (..)
  )
where

import Cardano.Api
import Cardano.Api.Ledger qualified as L

import Cardano.CLI.Read (ScriptDecodeError)
import Cardano.Prelude (Exception (displayException), IOException)

import Network.HTTP.Client (HttpException)

data HashCmdError
  = HashMismatchedHashError
      !(L.SafeHash L.AnchorData)
      -- ^ Expected hash
      !(L.SafeHash L.AnchorData)
      -- ^ Actual hash
  | HashReadFileError !FilePath !IOException
  | HashWriteFileError !(FileError ())
  | HashReadScriptError !FilePath !(FileError ScriptDecodeError)
  | HashFetchURLError !FetchURLError
  | HashGenesisCmdGenesisFileError !(FileError ())
  deriving Show

instance Error HashCmdError where
  prettyError = \case
    HashMismatchedHashError expectedHash actualHash ->
      "Hashes do not match!"
        <> "\nExpected:"
          <+> pretty (show (L.extractHash expectedHash))
        <> "\n  Actual:"
          <+> pretty (show (L.extractHash actualHash))
    HashReadFileError filepath exc ->
      "Cannot read" <+> pretty filepath <> ":" <+> pretty (displayException exc)
    HashWriteFileError fileErr ->
      prettyError fileErr
    HashReadScriptError filepath err ->
      "Cannot read script at" <+> pretty filepath <> ":" <+> prettyError err
    HashFetchURLError fetchErr ->
      pretty (displayException fetchErr)
    HashGenesisCmdGenesisFileError fe ->
      prettyError fe

data FetchURLError
  = FetchURLInvalidURLError !String
  | FetchURLReadFileError !FilePath !IOException
  | FetchURLUnsupportedURLSchemeError !String
  | FetchURLReadEnvVarError !IOException
  | FetchURLGetFileFromHttpError !HttpRequestError
  | FetchURLIpfsGatewayNotSetError
  deriving Show

instance Error FetchURLError where
  prettyError (FetchURLInvalidURLError text) = "Cannot parse URI: " <> pshow text
  prettyError (FetchURLReadFileError filepath exc) =
    "Cannot read " <> pshow filepath <> ": " <> pshow (displayException exc)
  prettyError (FetchURLUnsupportedURLSchemeError text) = pshow $ "Unsupported URL scheme: " <> text
  prettyError (FetchURLReadEnvVarError exc) = pshow $ "Cannot read environment variable: " <> displayException exc
  prettyError (FetchURLGetFileFromHttpError err) = pshow $ displayException err
  prettyError FetchURLIpfsGatewayNotSetError =
    pshow @String "IPFS scheme requires IPFS_GATEWAY_URI environment variable to be set."

instance Exception FetchURLError where
  displayException :: FetchURLError -> String
  displayException (FetchURLInvalidURLError text) = "Cannot parse URI: " <> text
  displayException (FetchURLReadFileError filepath exc) =
    "Cannot read " <> filepath <> ": " <> displayException exc
  displayException (FetchURLUnsupportedURLSchemeError text) = "Unsupported URL scheme: " <> text
  displayException (FetchURLReadEnvVarError exc) = "Cannot read environment variable: " <> displayException exc
  displayException (FetchURLGetFileFromHttpError err) = displayException err
  displayException FetchURLIpfsGatewayNotSetError = "IPFS scheme requires IPFS_GATEWAY_URI environment variable to be set."

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

data HashCheckError
  = HashMismatchError
      (L.SafeHash L.AnchorData)
      -- ^ The expected DRep metadata hash.
      (L.SafeHash L.AnchorData)
      -- ^ The actual DRep metadata hash.
  | FetchURLError FetchURLError
  deriving Show

instance Error HashCheckError where
  prettyError = \case
    HashMismatchError expectedHash actualHash ->
      "Hashes do not match!"
        <> "\nExpected:"
          <+> pretty (show (L.extractHash expectedHash))
        <> "\n  Actual:"
          <+> pretty (show (L.extractHash actualHash))
    FetchURLError fetchErr ->
      prettyError fetchErr

instance Exception HashCheckError where
  displayException :: HashCheckError -> String
  displayException (HashMismatchError expectedHash actualHash) =
    "Hashes do not match!"
      <> "\nExpected: "
      <> show (L.extractHash expectedHash)
      <> "\n  Actual: "
      <> show (L.extractHash actualHash)
  displayException (FetchURLError fetchErr) = displayException fetchErr
