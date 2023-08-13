{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Types.Errors.TxFeeValidationError
  ( TxFeeValidationError(..)
  ) where

import           Cardano.Api

import qualified Data.Text as Text

data TxFeeValidationError
  = TxFeatureImplicitFeesE AnyCardanoEra -- ^ Expected an explicit fee
  | TxFeatureExplicitFeesE AnyCardanoEra -- ^ Expected an implicit fee
  deriving Show

instance Error TxFeeValidationError where
  displayError = \case
    TxFeatureImplicitFeesE era ->
      "Implicit transaction fee not supported in " <> Text.unpack (renderEra era)
    TxFeatureExplicitFeesE era ->
      "Explicit transaction fee not supported in " <> Text.unpack (renderEra era)
