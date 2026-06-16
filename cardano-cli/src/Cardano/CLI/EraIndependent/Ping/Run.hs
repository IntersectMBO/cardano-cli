{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.CLI.EraIndependent.Ping.Run
  ( runPingCmd
  )
where

import Cardano.Api

import Cardano.CLI.Compatible.Exception
import Cardano.CLI.EraIndependent.Ping.Command
import Cardano.Network.Ping qualified as CNP

import Data.IP qualified as IP
import Text.Read (readMaybe)

newtype PingClientCmdError = PingClientMisconfigurationError String
  deriving Show

instance Error PingClientCmdError where
  prettyError = renderPingClientCmdError

renderPingClientCmdError :: PingClientCmdError -> Doc ann
renderPingClientCmdError (PingClientMisconfigurationError err) = pretty err

runPingCmd :: PingCmd -> CIO e ()
runPingCmd cmd
  | Just err <- getConfigurationError cmd =
      throwCliError (PingClientMisconfigurationError err)
  | otherwise =
      -- TODO(network): CNP.pingClients does its own output and exit handling, maybe we want to expose that?
      liftIO (CNP.pingClients (toPingOpts cmd) [toAddress cmd])

toPingOpts :: PingCmd -> CNP.PingOpts
toPingOpts cmd =
  CNP.PingOpts
    { CNP.pingOptsCount = pingCmdCount cmd
    , CNP.pingOptsMagic = CNP.NetworkMagic (pingCmdMagic cmd)
    , CNP.pingOptsJson = if pingCmdJson cmd then CNP.AsJSON else CNP.AsText
    , CNP.pingOptsQuiet = pingCmdQuiet cmd
    , CNP.pingOptsMode =
        if pingOptsGetTip cmd
          then CNP.TipMode
          else
            if pingOptsHandshakeQuery cmd
              then CNP.QueryMode
              else CNP.PingMode
    , -- cardano-cli has no flags for these yet, so use network's own defaults.
      CNP.pingOptsSRVPrefix = "_cardano._tcp"
    , CNP.pingOptsColor = CNP.ColorAuto
    }

toAddress :: PingCmd -> CNP.Address (CNP.Unresolved CNP.SRVOrFilePathUnresolved)
toAddress cmd =
  case pingCmdEndPoint cmd of
    UnixSockEndPoint path -> CNP.FilePathOrDomain path
    -- TODO(network): we could export a parseAddress :: String -> Either String (Address ...)
    HostEndPoint host ->
      case (readMaybe host :: Maybe IP.IP, readMaybe (pingCmdPort cmd) :: Maybe Word) of
        (Just ip, Just port) -> CNP.IP ip port
        _ -> CNP.FilePathOrDomain (host <> ":" <> pingCmdPort cmd)
