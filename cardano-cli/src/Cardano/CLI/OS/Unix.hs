{-# LANGUAGE CPP #-}

module Cardano.CLI.OS.Unix
  ( module System.Posix.Files
  , module System.Posix.IO
  ) where

#if !defined(mingw32_HOST_OS)
import           System.Posix.Files
import           System.Posix.IO
#endif
