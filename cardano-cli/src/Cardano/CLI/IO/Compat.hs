{-# LANGUAGE CPP #-}

module Cardano.CLI.IO.Compat
  ( posixSetOtherAndGroupModes
  ) where

#if !defined(mingw32_HOST_OS)
#define UNIX
#endif

#ifdef UNIX
import           System.Posix.Files
#endif

posixSetOtherAndGroupModes :: IO ()
posixSetOtherAndGroupModes = do
#ifdef UNIX
  _ <- setFileCreationMask (otherModes `unionFileModes` groupModes)
#endif

pure ()
