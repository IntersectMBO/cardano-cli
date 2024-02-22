{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

{- HLINT ignore "Move brackets to avoid $" -}

module Cardano.CLI.Run.Ping
  ( PingCmd (..),
    PingClientCmdError (..),
    renderPingClientCmdError,
    runPingCmd,
    parsePingCmd,
  )
where

import Cardano.Api.Pretty
import qualified Cardano.Network.Ping as CNP
import Control.Applicative ((<|>))
import Control.Concurrent.Class.MonadSTM.Strict (StrictTMVar)
import qualified Control.Concurrent.Class.MonadSTM.Strict as STM
import Control.Exception (SomeException)
import Control.Monad (forM, unless)
import Control.Monad.Class.MonadAsync (MonadAsync (async, wait, waitCatch))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Except.Extra (left)
import Control.Tracer (Tracer (..))
import Data.List (foldl')
import qualified Data.List as L
import qualified Data.List as List
import Data.Word (Word32)
import Network.Socket (AddrInfo)
import qualified Network.Socket as Socket
import qualified Options.Applicative as Opt
import qualified Prettyprinter as PP
import qualified System.Exit as IO
import qualified System.IO as IO

newtype PingClientCmdError = PingClientCmdError [(AddrInfo, SomeException)]

data EndPoint = HostEndPoint String | UnixSockEndPoint String deriving (Eq, Show)

data PingCmd = PingFullCmd FullData | PingHandshakeOnlyCmd HandshakeOnlyData
  deriving (Eq, Show)

data HandshakeOnlyData = HandshakeOnlyData
  { handshakeCmdEndPoint :: !EndPoint,
    handshakeCmdPort :: !String,
    handshakeCmdMagic :: !Word32,
    handshakeCmdJson :: !Bool,
    handshakeCmdQuiet :: !Bool,
    handshakeOptsHandshakeQuery :: !Bool
  }
  deriving (Eq, Show)

data FullData = FullData
  { pingCmdCount :: !Word32,
    pingCmdHost :: !String,
    pingCmdPort :: !String,
    pingCmdMagic :: !Word32,
    pingCmdJson :: !Bool,
    pingCmdQuiet :: !Bool,
    pingOptsHandshakeQuery :: !Bool
  }
  deriving (Eq, Show)

pingClient :: Tracer IO CNP.LogMsg -> Tracer IO String -> PingCmd -> [CNP.NodeVersion] -> AddrInfo -> IO ()
pingClient stdout stderr cmd = CNP.pingClient stdout stderr opts
  where
    opts = case cmd of
      PingFullCmd options ->
        CNP.PingOpts
          { CNP.pingOptsQuiet = pingCmdQuiet options,
            CNP.pingOptsJson = pingCmdJson options,
            CNP.pingOptsCount = pingCmdCount options,
            CNP.pingOptsHost = Just $ pingCmdHost options,
            CNP.pingOptsUnixSock = Nothing,
            CNP.pingOptsPort = pingCmdPort options,
            CNP.pingOptsMagic = pingCmdMagic options,
            CNP.pingOptsHandshakeQuery = pingOptsHandshakeQuery options
          }
      PingHandshakeOnlyCmd options ->
        CNP.PingOpts
          { CNP.pingOptsQuiet = handshakeCmdQuiet options,
            CNP.pingOptsJson = handshakeCmdJson options,
            CNP.pingOptsCount = 0,
            CNP.pingOptsHost = host,
            CNP.pingOptsUnixSock = unixsock,
            CNP.pingOptsPort = handshakeCmdPort options,
            CNP.pingOptsMagic = handshakeCmdMagic options,
            CNP.pingOptsHandshakeQuery = handshakeOptsHandshakeQuery options
          }
        where
          (host, unixsock) = case handshakeCmdEndPoint options of
            HostEndPoint h -> (Just h, Nothing)
            UnixSockEndPoint u -> (Nothing, Just u)

runPingCmd :: PingCmd -> ExceptT PingClientCmdError IO ()
runPingCmd cmd = do
  let hints = Socket.defaultHints {Socket.addrSocketType = Socket.Stream}

  msgQueue <- liftIO STM.newEmptyTMVarIO

  -- 'addresses' are all the endpoints to connect to and 'versions' are the node protocol versions
  -- to ping with.
  (addresses, versions) <- case endpoint of
    HostEndPoint host -> do
      addrs <- liftIO $ Socket.getAddrInfo (Just hints) (Just host) (Just host)
      return (addrs, CNP.supportedNodeToNodeVersions magic)
    UnixSockEndPoint fname -> do
      let addr =
            Socket.AddrInfo
              []
              Socket.AF_UNIX
              Socket.Stream
              Socket.defaultProtocol
              (Socket.SockAddrUnix fname)
              Nothing
      return ([addr], CNP.supportedNodeToClientVersions magic)

  -- Logger async thread handle
  laid <- liftIO . async $ CNP.logger msgQueue json handshakeQuery
  -- Ping client thread handles
  caids <- forM addresses $ liftIO . async . pingClient (Tracer $ doLog msgQueue) (Tracer doErrLog) cmd versions
  res <- L.zip addresses <$> mapM (liftIO . waitCatch) caids
  liftIO $ doLog msgQueue CNP.LogEnd
  liftIO $ wait laid

  -- Collect errors 'es' from failed pings and 'addrs' from successful pings.
  let (es, addrs) = foldl' partition ([], []) res

  -- Report any errors
  case (es, addrs) of
    ([], _) -> liftIO IO.exitSuccess
    (_, []) -> left $ PingClientCmdError es
    (_, _) -> do
      unless quiet $ mapM_ (liftIO . IO.hPrint IO.stderr) es
      liftIO IO.exitSuccess
  where
    (magic, quiet, json, handshakeQuery, endpoint) = case cmd of
      PingFullCmd opts -> (pingCmdMagic opts, pingCmdQuiet opts, pingCmdJson opts, pingOptsHandshakeQuery opts, HostEndPoint $ pingCmdHost opts)
      PingHandshakeOnlyCmd opts -> (handshakeCmdMagic opts, handshakeCmdQuiet opts, handshakeCmdJson opts, handshakeOptsHandshakeQuery opts, handshakeCmdEndPoint opts)
    partition ::
      ([(AddrInfo, SomeException)], [AddrInfo]) ->
      (AddrInfo, Either SomeException ()) ->
      ([(AddrInfo, SomeException)], [AddrInfo])
    partition (es, as) (a, Left e) = ((a, e) : es, as)
    partition (es, as) (a, Right _) = (es, a : as)

    doLog :: StrictTMVar IO CNP.LogMsg -> CNP.LogMsg -> IO ()
    doLog msgQueue msg = STM.atomically $ STM.putTMVar msgQueue msg

    doErrLog :: String -> IO ()
    doErrLog = IO.hPutStrLn IO.stderr

renderPingClientCmdError :: PingClientCmdError -> Doc ann
renderPingClientCmdError = \case
  PingClientCmdError es -> mconcat $ List.intersperse "\n" $ pshow <$> es

parsePingCmd :: Opt.Parser PingCmd
parsePingCmd =
  Opt.hsubparser $
    mconcat
      [ Opt.metavar "ping",
        Opt.command "ping" $
          Opt.info pPing $
            Opt.progDescDoc $
              Just $
                mconcat
                  [ PP.pretty @String "Ping a cardano node either using node-to-node or node-to-client protocol. ",
                    PP.pretty @String "It negotiates a handshake and keeps sending keep alive messages."
                  ]
      ]

pHost :: Bool -> Opt.Parser String
pHost internal =
  Opt.strOption $
    mconcat
      [ Opt.long "host",
        Opt.short 'h',
        Opt.metavar "HOST",
        Opt.help "Hostname/IP, e.g. relay.iohk.example.",
        if internal then Opt.internal else mempty
      ]

pUnixSocket :: Opt.Parser EndPoint
pUnixSocket =
  fmap
    UnixSockEndPoint
    ( Opt.strOption $
        mconcat
          [ Opt.long "unixsock",
            Opt.short 'u',
            Opt.metavar "SOCKET",
            Opt.help $
              mconcat
                [ "Unix socket, e.g. file.socket. ",
                  "Requires the --hanshake-only flag.  "
                ]
          ]
    )

pEndpoint :: Opt.Parser EndPoint
pEndpoint = (HostEndPoint <$> pHost True) <|> pUnixSocket

pPingNoHandshakeOnly :: Opt.Parser FullData
pPingNoHandshakeOnly =
  FullData
    <$> ( Opt.option Opt.auto $
            mconcat
              [ Opt.long "count",
                Opt.short 'c',
                Opt.metavar "COUNT",
                Opt.help $
                  mconcat
                    [ "Stop after sending count requests and receiving count responses.  ",
                      "If this option is not specified, ping will operate until interrupted.  "
                    ],
                Opt.value maxBound
              ]
        )
    <*> pHost False
    <*> ( Opt.strOption $
            mconcat
              [ Opt.long "port",
                Opt.short 'p',
                Opt.metavar "PORT",
                Opt.help "Port number, e.g. 1234.",
                Opt.value "3001"
              ]
        )
    <*> ( Opt.option Opt.auto $
            mconcat
              [ Opt.long "magic",
                Opt.short 'm',
                Opt.metavar "MAGIC",
                Opt.help "Network magic.",
                Opt.value CNP.mainnetMagic
              ]
        )
    <*> ( Opt.switch $
            mconcat
              [ Opt.long "json",
                Opt.short 'j',
                Opt.help "JSON output flag."
              ]
        )
    <*> ( Opt.switch $
            mconcat
              [ Opt.long "quiet",
                Opt.short 'q',
                Opt.help "Quiet flag, CSV/JSON only output"
              ]
        )
    <*> ( Opt.switch $
            mconcat
              [ Opt.long "query-versions",
                Opt.short 'Q',
                Opt.help $
                  mconcat
                    [ "Query the supported protocol versions using the handshake protocol and terminate the connection. ",
                      "(deprecated; use under --handshake-only instead)."
                    ]
              ]
        )

pPingHandshakeOnly :: Opt.Parser HandshakeOnlyData
pPingHandshakeOnly =
  HandshakeOnlyData
    <$> pEndpoint
    <*> ( Opt.strOption $
            mconcat
              [ Opt.long "port",
                Opt.short 'p',
                Opt.metavar "PORT",
                Opt.help "Port number, e.g. 1234.",
                Opt.value "3001"
              ]
        )
    <*> ( Opt.option Opt.auto $
            mconcat
              [ Opt.long "magic",
                Opt.short 'm',
                Opt.metavar "MAGIC",
                Opt.help "Network magic.",
                Opt.value CNP.mainnetMagic
              ]
        )
    <*> ( Opt.switch $
            mconcat
              [ Opt.long "json",
                Opt.short 'j',
                Opt.help "JSON output flag."
              ]
        )
    <*> ( Opt.switch $
            mconcat
              [ Opt.long "quiet",
                Opt.short 'q',
                Opt.help "Quiet flag, CSV/JSON only output"
              ]
        )
    <*> ( Opt.switch $
            mconcat
              [ Opt.long "query-versions",
                Opt.short 'Q',
                Opt.help "Query the supported protocol versions using the handshake protocol and terminate the connection. "
              ]
        )

pPing :: Opt.Parser PingCmd
pPing =
  (PingFullCmd <$> pPingNoHandshakeOnly)
    <|> ( Opt.subparser $
            Opt.command "handshake-only" $
              Opt.info (PingHandshakeOnlyCmd <$> pPingHandshakeOnly) $
                Opt.progDesc "Perform only the handshake process without sending a ping."
        )
