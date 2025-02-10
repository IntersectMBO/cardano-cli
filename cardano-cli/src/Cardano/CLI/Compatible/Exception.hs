{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.CLI.Compatible.Exception
  ( CustomCliException (..)
  , throwCliError
  , fromEitherCli
  , fromEitherIOCli
  )
where

import           Cardano.Api

import           GHC.Stack

import           RIO

-- | Custom exception type for CLI commands. Any custom errors created
-- in `cardano-cl` should be wrapped in this exception type.
data CustomCliException where
  CustomCliException
    :: (Show error, Typeable error, Error error)
    => error -> CallStack -> CustomCliException

deriving instance Show CustomCliException

instance Exception CustomCliException where
  displayException (CustomCliException e cStack) =
    unlines
      [ show (prettyError e)
      , prettyCallStack cStack
      ]

throwCliError :: MonadIO m => CustomCliException -> m a
throwCliError = throwIO

fromEitherCli :: (HasCallStack, MonadIO m, Show e, Typeable e, Error e) => Either e a -> m a
fromEitherCli = \case
  Left e -> throwCliError $ CustomCliException e callStack
  Right a -> return a

fromEitherIOCli :: (HasCallStack, MonadIO m, Show e, Typeable e, Error e) => IO (Either e a) -> m a
fromEitherIOCli action = do
  result <- liftIO action
  case result of
    Left e -> throwCliError $ CustomCliException e callStack
    Right a -> return a
