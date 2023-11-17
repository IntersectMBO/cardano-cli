module Cardano.CLI.Pretty
  ( module Pretty
  , putLn
  , hPutLn
  ) where

import           Cardano.Api.Pretty
import qualified Cardano.Api.Pretty as Pretty

import qualified Control.Concurrent.QSem as IO
import           Control.Exception (bracket_)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Text.Lazy.IO as TextLazy
import           Prettyprinter.Render.Terminal
import qualified System.IO as IO
import qualified System.IO.Unsafe as IO

sem :: IO.QSem
sem = IO.unsafePerformIO $ IO.newQSem 1
{-# NOINLINE sem #-}

consoleBracket :: IO a -> IO a
consoleBracket = bracket_ (IO.waitQSem sem) (IO.signalQSem sem)

putLn :: MonadIO m => Doc AnsiStyle -> m ()
putLn = liftIO . consoleBracket . TextLazy.putStrLn . prettyToLazyText

hPutLn :: MonadIO m => IO.Handle -> Doc AnsiStyle -> m ()
hPutLn h = liftIO . consoleBracket . TextLazy.hPutStr h . prettyToLazyText
