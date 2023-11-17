module Cardano.CLI.Pretty
  ( Ann,
    putLn,
    hPutLn,
    prettyToStrictText,
  ) where

import           Cardano.Api.Pretty (prettyToText)

import qualified Control.Concurrent.QSem as IO
import           Control.Exception (bracket_)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Text (Text)
import qualified Data.Text.Lazy as TextLazy
import qualified Data.Text.Lazy.IO as TextLazy
import           Prettyprinter
import           Prettyprinter.Render.Terminal
import qualified System.IO as IO
import qualified System.IO.Unsafe as IO

type Ann = AnsiStyle

sem :: IO.QSem
sem = IO.unsafePerformIO $ IO.newQSem 1
{-# NOINLINE sem #-}

consoleBracket :: IO a -> IO a
consoleBracket = bracket_ (IO.waitQSem sem) (IO.signalQSem sem)

putLn :: MonadIO m => Doc AnsiStyle -> m ()
putLn = liftIO . consoleBracket . TextLazy.putStrLn . prettyToText

hPutLn :: MonadIO m => IO.Handle -> Doc AnsiStyle -> m ()
hPutLn h = liftIO . consoleBracket . TextLazy.hPutStr h . prettyToText

prettyToStrictText :: Doc AnsiStyle -> Text
prettyToStrictText = TextLazy.toStrict . prettyToText
