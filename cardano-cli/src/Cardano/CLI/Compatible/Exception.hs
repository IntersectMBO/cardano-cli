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
  , fromExceptTCli
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
    :: (HasCallStack, Show err, Typeable err, Error err)
    => err -> CustomCliException

deriving instance Show CustomCliException

instance Exception CustomCliException where
  displayException (CustomCliException e) =
    unlines
      [ show (prettyError e)
      , prettyCallStack callStack
      ]

-- | Wrapper function which allows throwing of types of instance `Error`, attaching call stack
-- from the call site
throwCliError
  :: forall e m a
   . (HasCallStack, Show e, Typeable e, Error e, MonadIO m)
  => e
  -> m a
throwCliError = withFrozenCallStack $ throwIO . CustomCliException

fromEitherCli
  :: forall e m a
   . (HasCallStack, MonadIO m, Show e, Typeable e, Error e)
  => Either e a
  -> m a
fromEitherCli = withFrozenCallStack $ \case
  Left e -> throwCliError e
  Right a -> return a

fromEitherIOCli
  :: forall e m a
   . (HasCallStack, MonadIO m, Show e, Typeable e, Error e)
  => IO (Either e a)
  -> m a
fromEitherIOCli action = withFrozenCallStack $ liftIO action >>= fromEitherCli

fromExceptTCli
  :: forall e m a
   . (HasCallStack, MonadIO m, Show e, Typeable e, Error e)
  => ExceptT e IO a
  -> m a
fromExceptTCli = withFrozenCallStack $ fromEitherIOCli . runExceptT
