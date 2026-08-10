{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraIndependent.Ping.Run
  ( PingClientCmdError (..)
  , renderPingClientCmdError
  , runPingCmd
  )
where

import Cardano.Api

import Cardano.CLI.Compatible.Exception
import Cardano.CLI.EraIndependent.Ping.Command
import Cardano.Network.Ping qualified as CNP

import Control.Exception (SomeException)
import Data.List qualified as List
import Network.Socket (AddrInfo)

data PingClientCmdError
  = PingClientCmdError [(AddrInfo, SomeException)]
  | PingClientMisconfigurationError String
  deriving Show

instance Error PingClientCmdError where
  prettyError = renderPingClientCmdError

runPingCmd :: PingCmd -> CIO e ()
runPingCmd options
  | Just err <- getConfigurationError options =
      throwCliError $ PingClientMisconfigurationError err
runPingCmd options = liftIO $ CNP.pingClients opts addrs
 where
  opts =
    CNP.PingOpts
      { CNP.pingOptsCount = pingCmdCount options
      , CNP.pingOptsMagic = CNP.NetworkMagic (pingCmdMagic options)
      , CNP.pingOptsJson = if pingCmdJson options then CNP.AsJSON else CNP.AsText
      , CNP.pingOptsQuiet = pingCmdQuiet options
      , CNP.pingOptsMode =
          if pingOptsHandshakeQuery options
            then CNP.QueryMode
            else if pingOptsGetTip options then CNP.TipMode else CNP.PingMode
      , CNP.pingOptsSRVPrefix = "_cardano._tcp"
      , CNP.pingOptsColor = CNP.ColorAuto
      , CNP.pingOptsHashType = CNP.FullHash
      }
  addrs = case pingCmdEndPoint options of
    HostEndPoint host -> [CNP.mkAddress (host ++ ":" ++ pingCmdPort options)]
    UnixSockEndPoint fname -> [CNP.mkAddress fname]

renderPingClientCmdError :: PingClientCmdError -> Doc ann
renderPingClientCmdError = \case
  PingClientCmdError es -> mconcat $ List.intersperse "\n" $ pshow <$> es
  PingClientMisconfigurationError err -> pretty err
