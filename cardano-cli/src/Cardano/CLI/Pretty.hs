module Cardano.CLI.Pretty
  ( putLn
  , hPutLn

  -- Re-exported functions from Cardano.Api related to pretty-printing
  , black
  , blue
  , cyan
  , docToLazyText
  , docToString
  , docToText
  , green
  , magenta
  , prettyException
  , pshow
  , red
  , white
  , yellow
  , (<+>)
  , hsep
  , vsep
  , MonadIO(..)
  , Ann
  , ShowOf(..)
  , Doc
  , Pretty(..)

  ) where

import Cardano.Api (black, blue, cyan, docToLazyText, docToString,
                    docToText, green, magenta, prettyException, pshow,
                    red, white, yellow, (<+>), hsep, vsep, MonadIO(..),
                    Ann, ShowOf(..), Doc, Pretty(..))
import qualified Control.Concurrent.QSem as IO
import           Control.Exception (bracket_)
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
putLn = liftIO . consoleBracket . TextLazy.putStrLn . docToLazyText

hPutLn :: MonadIO m => IO.Handle -> Doc AnsiStyle -> m ()
hPutLn h = liftIO . consoleBracket . TextLazy.hPutStr h . docToLazyText
