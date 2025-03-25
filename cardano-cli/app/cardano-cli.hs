{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

#if !defined(mingw32_HOST_OS)
#define UNIX
#endif

import           Cardano.Api
import           Cardano.CLI.Environment (getCliVisibilityLevel, getEnvCli)
import           Cardano.CLI.Option (opts, pref)
import           Cardano.CLI.Run (renderClientCommandError, runClientCommand)
import           Cardano.CLI.TopHandler
import qualified Cardano.Crypto.Init as Crypto

import           Control.Monad.Trans.Except.Exit (orDie)
import qualified GHC.IO.Encoding as GHC
import qualified Options.Applicative as Opt

#ifdef UNIX
import           Cardano.CLI.OS.Posix
#endif

main :: IO ()
main = toplevelExceptionHandler $ do
  Crypto.cryptoInit

  envCli <- getEnvCli

  visibility <- getCliVisibilityLevel

  GHC.mkTextEncoding "UTF-8" >>= GHC.setLocaleEncoding
#ifdef UNIX
  _ <- setFileCreationMask (otherModes `unionFileModes` groupModes)
#endif
  co <- Opt.customExecParser pref (opts visibility envCli)

  orDie (docToText . renderClientCommandError) $ runClientCommand co
