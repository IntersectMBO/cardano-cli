{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cardano.CLI.Compatible.Exception
  ( CIO
  , CustomCliException (..)
  , throwCliError
  , fromEitherCli
  , fromEitherIOCli
  )
where

import Cardano.Api

import RIO

import GHC.Stack

-- | Type alias that enforces the presence of a call stack.
type CIO e a = HasCallStack => RIO e a

-- | Custom exception type for CLI commands. Any custom errors created
-- in `cardano-cl` should be wrapped in this exception type.
data CustomCliException where
  CustomCliException
    :: (HasCallStack, Show error, Typeable error, Error error)
    => error -> CustomCliException

deriving instance Show CustomCliException

instance Exception CustomCliException where
  displayException (CustomCliException e) =
    unlines
      [ show (prettyError e)
      , prettyCallStack callStack
      ]

throwCliError :: MonadIO m => CustomCliException -> m a
throwCliError = throwIO

fromEitherCli :: (HasCallStack, MonadIO m, Show e, Typeable e, Error e) => Either e a -> m a
fromEitherCli = withFrozenCallStack $ \case
  Left e -> throwCliError $ CustomCliException e
  Right a -> return a

fromEitherIOCli :: (HasCallStack, MonadIO m, Show e, Typeable e, Error e) => IO (Either e a) -> m a
fromEitherIOCli action = liftIO action >>= fromEitherCli
