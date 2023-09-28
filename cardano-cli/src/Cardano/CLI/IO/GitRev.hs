{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Cardano.CLI.IO.GitRev
  ( gitRev
  ) where

import Data.Text (Text)
import Data.Text qualified as T

import Cardano.Git.RevFromGit (gitRevFromGit)
import Foreign.C.String (CString)
import GHC.Foreign (peekCStringLen)
import System.IO (utf8)
import System.IO.Unsafe (unsafeDupablePerformIO)

foreign import ccall "&_cardano_git_rev" c_gitrev :: CString

gitRev :: Text
gitRev
  | gitRevEmbed /= zeroRev = gitRevEmbed
  | T.null fromGit = zeroRev
  | otherwise = fromGit
 where
  -- Git revision embedded after compilation using
  -- Data.FileEmbed.injectWith. If nothing has been injected,
  -- this will be filled with 0 characters.
  gitRevEmbed :: Text
  gitRevEmbed = T.pack $ drop 28 $ unsafeDupablePerformIO (peekCStringLen utf8 (c_gitrev, 68))

-- Git revision found during compilation by running git. If
-- git could not be run, then this will be empty.
#if defined(arm_HOST_ARCH)
  -- cross compiling to arm fails; due to a linker bug
  fromGit = ""
#else
  fromGit = T.strip (T.pack $(gitRevFromGit))
#endif

zeroRev :: Text
zeroRev = "0000000000000000000000000000000000000000"
