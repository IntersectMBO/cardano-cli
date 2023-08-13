module Cardano.CLI.Types.TxFeature
  ( TxFeature(..)
  , txFeatureMismatch
  , txFeatureMismatchPure
  , renderFeature
  ) where

import           Cardano.Api

import           Cardano.CLI.Types.Errors.TxCmdError
import           Cardano.CLI.Types.TxFeature.Core

import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Except.Extra

txFeatureMismatch :: ()
  => Monad m
  => CardanoEra era
  -> TxFeature
  -> ExceptT TxCmdError m a
txFeatureMismatch era feature =
    hoistEither . Left $ TxCmdTxFeatureMismatchError (anyCardanoEra era) feature

txFeatureMismatchPure :: CardanoEra era
                      -> TxFeature
                      -> Either TxCmdError a
txFeatureMismatchPure era feature =
    Left (TxCmdTxFeatureMismatchError (anyCardanoEra era) feature)
