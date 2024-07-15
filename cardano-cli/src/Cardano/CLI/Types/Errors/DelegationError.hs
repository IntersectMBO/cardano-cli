{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Types.Errors.DelegationError
  ( DelegationError (..)
  )
where

import           Cardano.Api

import           Cardano.CLI.Types.Errors.StakeCredentialError

import           GHC.Generics (Generic)

data DelegationError
  = DelegationReadError !(FileError InputDecodeError)
  | DelegationCertificateWriteFileError !(FileError ())
  | DelegationDRepReadError !(FileError InputDecodeError)
  | DelegationStakeCredentialError !StakeCredentialError
  deriving (Show, Generic)

instance Error DelegationError where
  prettyError = \case
    DelegationReadError e ->
      "Cannot read delegation target: " <> prettyError e
    DelegationStakeCredentialError e ->
      "Cannot get stake credential: " <> prettyError e
    DelegationCertificateWriteFileError e ->
      "Cannot write certificate: " <> prettyError e
    DelegationDRepReadError e ->
      "Cannot read DRep key: " <> prettyError e
