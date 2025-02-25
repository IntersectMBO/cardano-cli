{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-unused-imports #-} -- Required to compile across GHC versions.

{- HLINT ignore "Use fewer imports" -}

module Test.Cli.Pipes
  ( hprop_readFromPipe
  ) where

#if !defined(mingw32_HOST_OS)
#define UNIX
#else
-- Need this to avoid an unused-package error on Windows when compiling with
-- cabal-3.10 and ghc-9.6.
import           System.FilePath ()
import           Control.Monad.Morph ()
#endif

import qualified Hedgehog as H
import           Hedgehog (Property)
import           System.FilePath ((</>))

#ifdef UNIX
import           Cardano.CLI.Read
import           Cardano.CLI.OS.Posix
import           Test.Cardano.CLI.Util

import           Control.Monad.Morph (hoist)
import           Control.Monad.Trans.Resource (runResourceT)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS

import           Hedgehog ((===), forAll)
import qualified Hedgehog.Gen as G
import           Hedgehog.Internal.Property (failWith)
import qualified Hedgehog.Range as R
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H

hprop_readFromPipe :: Property
hprop_readFromPipe = watchdogProp . H.withTests 10 . H.property . hoist runResourceT . H.moduleWorkspace "tmp" $ \ws -> do

  s <- forAll $ G.string (R.linear 1 8192) G.ascii

  let testFile = ws </> "test-file"

  H.writeFile testFile s

  -- We first test that we can read a filepath
  testFp <- noteInputFile testFile
  testFileOrPipe <- H.evalIO $ fileOrPipe testFp
  testBs <- H.evalIO $ readFileOrPipe testFileOrPipe

  if LBS.null testBs
  then failWith Nothing
         $ "readFileOrPipe failed to read file: " <> fileOrPipePath testFileOrPipe
  else do
    -- We now test that we can read from a pipe.
    -- We first check that the IORef has Nothing
    mContents <- H.evalIO $ fileOrPipeCache testFileOrPipe
    case mContents of
      Just{} -> failWith Nothing "readFileOrPipe has incorrectly populated its IORef with contents read from a filepath."
      Nothing -> do
        -- We can reuse testFileOrPipe because we know the cache (IORef) is empty
        let txBodyStr = BSC.unpack $ LBS.toStrict testBs
        fromPipeBs <- H.evalIO $ withPipe txBodyStr
        if LBS.null fromPipeBs
        then failWith Nothing "readFileOrPipe failed to read from a pipe"
        else testBs === fromPipeBs

-- | Create a pipe, write some String into it, read its contents and return the contents
withPipe :: String -> IO LBS.ByteString
withPipe contents = do
  (readEnd, writeEnd) <- createPipe

  writeHandle <- fdToHandle writeEnd

  -- Write contents to pipe
  hPutStr writeHandle contents
  hFlush writeHandle
  hClose writeHandle
  pipe <- fileOrPipe $ "/dev/fd/" ++ show readEnd

  -- Read contents from pipe
  readContents <- readFileOrPipe pipe
  closeFd readEnd
  pure readContents

#else
hprop_readFromPipe :: Property
hprop_readFromPipe = H.property H.success
#endif
