module Test.Cli.MonadWarning
  ( hprop_monad_warning
  ) where

import           Cardano.CLI.Types.MonadWarning (MonadWarning, monadWarningInStateT, reportIssue)

import           Control.Monad (when)
import           Control.Monad.Trans.State (State, runState)

import           Hedgehog (Property, property, (===))

hprop_monad_warning :: Property
hprop_monad_warning = property $ do
  (-8, [warning]) === duplicateNumber (-4)
  (4, []) === duplicateNumber 2
  where
    duplicateNumber :: Int -> (Int, [String])
    duplicateNumber n = runState (monadWarningInStateT $ computeWithWarning n :: State [String] Int) []

    computeWithWarning :: (MonadWarning m) => Int -> m Int
    computeWithWarning x = do
      when (x < 0) $ reportIssue warning
      return (x * 2)

    warning :: String
    warning = "Input value is negative!"
