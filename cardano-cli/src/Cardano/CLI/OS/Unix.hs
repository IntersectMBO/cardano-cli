{-# LANGUAGE CPP #-}

module Cardano.CLI.OS.Unix where

#if !defined(mingw32_HOST_OS)
import           System.Posix.Files
import           System.Posix.IO
#endif
