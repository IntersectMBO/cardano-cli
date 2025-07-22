{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

#if !defined(mingw32_HOST_OS)
#define UNIX
#endif

import           Cardano.CLI.Environment (getEnvCli)
import           Cardano.CLI.Option (opts, pref)
import           Cardano.CLI.Run (runClientCommand)
import           Cardano.CLI.TopHandler
import qualified Cardano.Crypto.Init as Crypto

import qualified GHC.IO.Encoding as GHC
import qualified Options.Applicative as Opt
import           RIO

#ifdef UNIX
import           Cardano.CLI.OS.Posix
#endif

import           System.Console.Terminal.Size (Window(..), size)

main :: IO ()
main = toplevelExceptionHandler $ do
  Crypto.cryptoInit

  envCli <- getEnvCli

  GHC.mkTextEncoding "UTF-8" >>= GHC.setLocaleEncoding
#ifdef UNIX
  _ <- setFileCreationMask (otherModes `unionFileModes` groupModes)
#endif

  mWin <- size

  let termWidth = maybe 80 width mWin
      dynamicPrefs = pref { Opt.prefColumns = termWidth }

  co <- Opt.customExecParser dynamicPrefs (opts envCli)

  runRIO () (runClientCommand co) `catch` 
    (\(e :: SomeException) -> do 
      hPrint stderr $ show e
      exitWith $ ExitFailure 1
    )
