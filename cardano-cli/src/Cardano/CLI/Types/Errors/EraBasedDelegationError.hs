{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Types.Errors.EraBasedDelegationError
  ( EraBasedDelegationError(..)
  ) where

import           Cardano.Api

import           Cardano.CLI.Types.Errors.StakeCredentialError

import           GHC.Generics (Generic)

data EraBasedDelegationError
  = EraBasedDelegReadError !(FileError InputDecodeError)
  | EraBasedCertificateWriteFileError !(FileError ())
  | EraBasedDRepReadError !(FileError InputDecodeError)
  | EraBasedDelegationStakeCredentialError !StakeCredentialError
  deriving (Show, Generic)

instance Error EraBasedDelegationError where
  displayError = \case
    EraBasedDelegReadError e ->
      "Cannot read delegation target: " <> displayError e
    EraBasedDelegationStakeCredentialError e ->
      "Cannot get stake credential: " <> displayError e
    EraBasedCertificateWriteFileError e ->
      "Cannot write certificate: " <> displayError e
    EraBasedDRepReadError e ->
      "Cannot read DRep key: " <> displayError e
