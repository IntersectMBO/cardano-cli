{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Types.Errors.DelegationError
  ( DelegationError(..)
  ) where

import           Cardano.Api

import           Cardano.CLI.Types.Errors.StakeCredentialError

import           GHC.Generics (Generic)
import           Prettyprinter

data DelegationError
  = DelegationReadError !(FileError InputDecodeError)
  | DelegationCertificateWriteFileError !(FileError ())
  | DelegationDRepReadError !(FileError InputDecodeError)
  | DelegationStakeCredentialError !StakeCredentialError
  deriving (Show, Generic)

instance Error DelegationError where
  prettyError = \case
    DelegationReadError e ->
      "Cannot read delegation target: " <> pretty e
    DelegationStakeCredentialError e ->
      "Cannot get stake credential: " <> prettyError e
    DelegationCertificateWriteFileError e ->
      "Cannot write certificate: " <> pretty e
    DelegationDRepReadError e ->
      "Cannot read DRep key: " <> pretty e
