{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Cardano.CLI.IO.GitRev
  ( gitRev
  ) where

import           Data.Text (Text)
import qualified Data.Text as T

import           Cardano.Git.RevFromGit (gitRevFromGit)
import           GHC.Foreign (peekCStringLen) 
import           Foreign.C.String (CString)
import           System.IO (utf8)
import           System.IO.Unsafe (unsafeDupablePerformIO)

foreign import ccall "&_cardano_git_rev" c_gitrev :: CString

gitRev :: Text
gitRev | gitRevEmbed /= zeroRev      = gitRevEmbed
       | not (T.null fromGit)        = fromGit
       | not (T.null gitRevFromEnv)  = gitRevFromEnv
       | otherwise                   = zeroRev
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

  gitRevFromEnv :: Text
#if defined(__GIT_REV__)
  gitRevFromEnv = T.pack __GIT_REV__
#else
  gitRevFromEnv = ""
#endif

zeroRev :: Text
zeroRev = "0000000000000000000000000000000000000000"
