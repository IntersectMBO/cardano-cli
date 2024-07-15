-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

-- |
-- Module      :  Cardano.CLI.Types.MonadWarning
--
-- This module defines the 'MonadWarning' type class, which provides a common
-- interface for monads that support reporting warning messages without
-- aborting the computation (unlike with exceptions, 'Either', or
-- 'MonadFail', which either fail or return a value).
--
-- It also includes two functions that instantiate it into either a 'MonadIO'
-- ('runWarningIO') or a 'StateT' monad with a @[String]@ as state
-- ('runWarningStateT') respectively.
--
-- In the case of 'MonadIO', warnings are printed to 'stderr'.
-- In the case of 'StateT', with a @[String]@ state, warnings are added to the
-- list in the state.
--
-- By using the 'MonadWarning' type class, users can write code that remains
-- agnostic to the specific monad in which it operates, and to easily change
-- it at a later stage if necessary.
--
-- Example usage:
--
-- @
-- computeWithWarning :: (MonadWarning m) => Int -> m Int
-- computeWithWarning x = do
--   when (x < 0) $ reportIssue "Input value is negative!"
--   return (x * 2)
--
-- -- Using 'IO' monad to perform computation and report warnings.
-- main :: IO ()
-- main = do
--   result <- runWarningIO $ computeWithWarning (-4)
--   putStrLn $ "Result: " ++ show result
-- @
module Cardano.CLI.Types.MonadWarning
  ( MonadWarning (..)
  , WarningIO
  , WarningStateT
  , eitherToWarning
  , runWarningIO
  , runWarningStateT
  )
where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.State (MonadState (..))
import           Control.Monad.Trans.State (StateT)
import           System.IO (hPutStrLn, stderr)

-- | Type class for monads that support reporting warnings without aborting
-- their execution in the process.
class Monad m => MonadWarning m where
  -- | Report a non-fatal issue.
  reportIssue
    :: String
    -- ^ The warning message to report.
    -> m ()
    -- ^ The action that reports the warning.

-- | Wrapper newtype for 'MonadIO' with 'MonadWarning' instance.
-- This type is not meant to be constructed directly but just to serve
-- as an instance of 'MonadWarning' that can be converted to 'MonadIO'.
-- It is only necessary in order to avoid overlapping instances.
newtype WarningIO m a = WarningIO
  { runWarningIO :: m a
  -- ^ Interpret a 'MonadWarning' as a 'MonadIO' by reporting
  -- warnings to 'stderr'.
  }
  deriving (Functor, Applicative, Monad, MonadIO)

-- | This instance prints the issue to 'stderr'.
instance MonadIO m => MonadWarning (WarningIO m) where
  reportIssue :: String -> WarningIO m ()
  reportIssue issue = liftIO (hPutStrLn stderr issue)

-- | Wrapper newtype for @StateT [String]@ with 'MonadWarning' instance.
-- This type is not meant to be constructed directly but just to serve
-- as an instance of 'MonadWarning' that can be converted to 'StateT'.
-- It is only necessary in order to avoid overlapping instances.
newtype WarningStateT m a = WarningStateT
  { runWarningStateT :: StateT [String] m a
  -- ^ Interpret a 'MonadWarning' as a @StateT [String]@ monad,
  -- by accumulating warnings into the state.
  }
  deriving (Functor, Applicative, Monad, MonadState [String])

-- | This instance adds the issue to the @[String]@ in the state.
instance Monad m => MonadWarning (WarningStateT m) where
  reportIssue :: String -> WarningStateT m ()
  reportIssue issue = state (\x -> ((), issue : x))

-- | Convert an 'Either' into a 'MonadWarning'. If 'Either' is 'Left'
-- it returns the default value (first parameter) and reports the 'String'
-- as an error. If 'Either' is 'Right' it just returns that value.
eitherToWarning :: MonadWarning m => a -> Either String a -> m a
eitherToWarning def = either (\issue -> do reportIssue issue; return def) return
