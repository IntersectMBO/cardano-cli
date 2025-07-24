{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

#if !defined(mingw32_HOST_OS)
#define UNIX
#endif

import           Cardano.CLI.Environment (getEnvCli)
import           Cardano.CLI.Option (opts, pref)
import           Cardano.CLI.Run (runClientCommand)
import           Cardano.Api
import           Cardano.CLI.Compatible.Exception
import           Cardano.CLI.Command
import           Cardano.CLI.Compatible.Command
import           Cardano.CLI.Render
import           Cardano.CLI.EraBased.Command
import           Cardano.CLI.EraIndependent.Address.Command
import           Cardano.CLI.EraIndependent.Hash.Command (renderHashCmds)
import           Cardano.CLI.EraIndependent.Key.Command (renderKeyCmds)
import           Cardano.CLI.EraIndependent.Node.Command (renderNodeCmds)
import           Cardano.CLI.EraBased.Query.Command (renderQueryCmds)
import           Cardano.CLI.Legacy.Command (renderLegacyCommand)
import           Cardano.CLI.EraIndependent.Cip.Command (renderCipFormatCmds)
import           Cardano.CLI.TopHandler
import qualified Cardano.Crypto.Init as Crypto

import qualified GHC.IO.Encoding as GHC
import qualified Options.Applicative as Opt
import           RIO

#ifdef UNIX
import           Cardano.CLI.OS.Posix (setFileCreationMask, groupModes, otherModes, unionFileModes)
#endif

import           System.Console.Terminal.Size (Window(..), size)
import           System.IO (hPutStrLn) 


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

  runRIO co (runClientCommand co) `catch` 
    (\(e :: CustomCliException) -> do 
      let commandText = case co of 
            AnyEraCommand anyEraCommand  -> renderAnyEraCommand anyEraCommand 
            AddressCommand addrCmds  -> renderAddressCmds addrCmds 
            ByronCommand{}  -> "" 
            CompatibleCommands anyCompatCmd -> renderAnyCompatibleCommand  anyCompatCmd
            HashCmds hashCmds  -> renderHashCmds hashCmds 
            KeyCommands keyCmds  -> renderKeyCmds keyCmds 
            NodeCommands nodeCmds  -> renderNodeCmds nodeCmds 
            QueryCommands queryCmds  -> renderQueryCmds queryCmds 
            LegacyCmds legacyCmds  -> renderLegacyCommand legacyCmds 
            CipFormatCmds cipFmtCmds  -> renderCipFormatCmds cipFmtCmds 
            CliPingCommand{}  -> ""
            CliDebugCmds{} -> "" 
            Help{} -> "" 
            DisplayVersion -> "" 

      hPutStrLn stderr $ docToString $ renderAnyCmdError commandText prettyException e

      exitWith $ ExitFailure 1
    )
