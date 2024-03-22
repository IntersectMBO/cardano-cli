-----------------------------------------------------------------------------
-- |
-- Module      :  Cardano.CLI.Types.MonadWarning
--
-- This module defines the 'MonadWarning' type class, which provides a common
-- interface for monads that support reporting warning messages without
-- aborting the computation (unlike with exceptions, Either, or MonadFail,
-- which either fail or return a value).
--
-- It also includes two functions that instantiate it into either a 'MonadIO'
-- ('monadWarningInMonadIO') or a 'StateT' monad with a '[String]' as state
-- ('monadWarningInStateT') respectively.
--
-- In the case of 'MonadIO', warnings are printed to 'stderr'.
-- In the case of 'StateT', with a '[String]' state, warnings are added to the
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
--     result <- monadWarningInMonadIO $ computeWithWarning (-4)
--     putStrLn $ "Result: " ++ show result
-- @
-----------------------------------------------------------------------------

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

module Cardano.CLI.Types.MonadWarning
  ( MonadWarning(..)
  , eitherToWarning
  , monadWarningInMonadIO
  , monadWarningInStateT
  ) where

import           Cardano.Prelude (MonadIO (liftIO), MonadState (state), Print (hPutStrLn), StateT,
                   stderr)

-- | Type class for monads that support reporting warnings.
class Monad m => MonadWarning m where
    -- | Report a warning issue.
    reportIssue :: String -- ^ The warning message to report.
                -> m ()   -- ^ The action that reports the warning.

-- | Wrapper newtype for 'MonadIO' with 'MonadWarning' instance.
-- We need to have wrapper to avoid overlapping instances.
newtype WarningMonadIO m a = WarningMonadIO (m a)
    deriving (Functor, Applicative, Monad, MonadIO)

-- | This instance prints the issue to the 'stderr'.
instance MonadIO m => MonadWarning (WarningMonadIO m) where
    reportIssue :: String -> WarningMonadIO m ()
    reportIssue issue = liftIO (hPutStrLn stderr issue)

-- | Convert a 'MonadWarning' into a 'MonadIO' by reporting
-- warnings to 'stderr'.
monadWarningInMonadIO :: WarningMonadIO m a -> m a
monadWarningInMonadIO (WarningMonadIO m) = m

-- | Wrapper newtype for 'StateT [String]' with 'MonadWarning' instance.
newtype WarningStateT m a = WarningState (StateT [String] m a)
    deriving (Functor, Applicative, Monad, MonadState [String])

-- | This instance adds the issue to the '[String]' in the state.
instance Monad m => MonadWarning (WarningStateT m) where
    reportIssue :: String -> WarningStateT m ()
    reportIssue issue = state (\ x -> ((), issue : x))

-- | Convert a 'MonadWarning' into a 'StateT [String]' monad,
-- by accumulating warnings into the state.
monadWarningInStateT :: WarningStateT m a -> StateT [String] m a
monadWarningInStateT (WarningState m) = m

-- | Convert an 'Either' into a 'MonadWarning'. If 'Either' is 'Left'
-- it returns the default value (first parameter) and reports the value
-- as an error. -- If 'Either' is 'Right' it just returns the value.
eitherToWarning :: MonadWarning m => a -> Either String a -> m a
eitherToWarning def = either (\issue -> do {reportIssue issue; return def}) return
