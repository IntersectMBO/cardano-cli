{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.IO.Lazy
  ( replicateM
  , sequenceM
  , traverseStateM
  , forStateM
  )
where

import Control.Monad.IO.Unlift
  ( MonadIO (liftIO)
  , MonadUnliftIO
  , UnliftIO (unliftIO)
  , askUnliftIO
  )
import Data.List qualified as L
import System.IO.Unsafe qualified as IO

replicateM :: MonadUnliftIO m => Int -> m a -> m [a]
replicateM n f = sequenceM (L.replicate n f)

sequenceM :: MonadUnliftIO m => [m a] -> m [a]
sequenceM as = do
  f <- askUnliftIO
  liftIO $ sequenceIO (L.map (unliftIO f) as)

traverseStateM :: forall m s a b. MonadUnliftIO m => s -> (s -> a -> m (s, b)) -> [a] -> m [b]
traverseStateM s f as = do
  u <- askUnliftIO
  liftIO $ IO.unsafeInterleaveIO (go s u as)
 where
  go :: s -> UnliftIO m -> [a] -> IO [b]
  go _ _ [] = pure []
  go t !u (v : vs) = do
    (t', !res) <- unliftIO u (f t v)
    rest <- IO.unsafeInterleaveIO (go t' u vs)
    pure (res : rest)

forStateM :: MonadUnliftIO m => s -> [a] -> (s -> a -> m (s, b)) -> m [b]
forStateM s as f = traverseStateM s f as

-- Internal
sequenceIO :: [IO a] -> IO [a]
sequenceIO = IO.unsafeInterleaveIO . go
 where
  go :: [IO a] -> IO [a]
  go [] = return []
  go (fa : fas) = (:) <$> fa <*> IO.unsafeInterleaveIO (go fas)
