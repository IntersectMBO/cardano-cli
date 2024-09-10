{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Types.Errors.HashCmdError
  ( HashCmdError (..)
  )
where

import           Cardano.Api
import qualified Cardano.Api.Ledger as L

import           Cardano.CLI.Read (ScriptDecodeError)
import           Cardano.Ledger.SafeHash (extractHash)
import           Cardano.Prelude (Exception (displayException), IOException)

data HashCmdError
  = HashMismatchedHashError
      !(L.SafeHash L.StandardCrypto L.AnchorData)
      !(L.SafeHash L.StandardCrypto L.AnchorData)
  | HashReadFileError !FilePath !IOException
  | HashWriteFileError !(FileError ())
  | HashReadScriptError !FilePath !(FileError ScriptDecodeError)
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
