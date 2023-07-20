{-# LANGUAGE CPP #-}

module Cardano.CLI.OS.Posix
  (
#if !defined(mingw32_HOST_OS)
    module Posix
#endif
  ) where

#if !defined(mingw32_HOST_OS)
import           System.Posix.Files as Posix
import           System.Posix.IO as Posix
import           System.IO as Posix
#endif
