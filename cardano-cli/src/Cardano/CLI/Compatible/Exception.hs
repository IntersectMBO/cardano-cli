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
  , runCIO
  , runIO
  )
where

import           Cardano.Api

import           Data.Typeable
import           GHC.Stack

import           RIO

-- | Type alias that enforces the presence of a call stack.
newtype CIO e a = CIO {unCIO :: HasCallStack => RIO e a}

runCIO :: MonadIO m => e -> CIO e a -> m a
runCIO e = runRIO e . unCIO

instance Functor (CIO e) where
  fmap f = CIO . fmap f . unCIO

instance Applicative (CIO e) where
  pure = CIO . pure
  f <*> a = CIO $ unCIO f <*> unCIO a

instance Monad (CIO e) where
  return = pure
  m >>= k = CIO $ unCIO m >>= unCIO . k

-- No `MonadIO (CIO e)` instance!
-- This way we force CIO users to use runIO to lift IO operations to CIO.
-- The problem with liftIO is that it does not have `HasCallStack` constraint - you can't even enforce
-- it in the instance. So this is an alternative to `liftIO`, making sure that we'll get the call stack to the
-- place where runIO was placed.

runIO :: HasCallStack => IO a -> CIO e a
runIO m = withFrozenCallStack $ CIO $ catchSyncOrAsync (liftIO m) (throwIO . mkAx)
 where
  -- \| makes the original exception type visible
  mkAx :: SomeException -> AppException
  mkAx e@(SomeException i) = withFrozenCallStack $ AppException (typeRepTyCon $ typeOf i) e

data AppException = HasCallStack => AppException TyCon SomeException

deriving instance Show AppException

instance Exception AppException where
  displayException (AppException tyCon (SomeException e)) =
    tyConName tyCon <> ": " <> displayException e <> "\n" <> prettyCallStack callStack

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
