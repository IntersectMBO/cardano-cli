{-# LANGUAGE TypeApplications #-}

{- HLINT ignore "Move brackets to avoid $" -}

module Cardano.CLI.Options.Ping
  ( parsePingCmd
  ) where

import           Cardano.CLI.Commands.Ping
import qualified Cardano.Network.Ping as CNP

import           Control.Applicative ((<|>))
import qualified Options.Applicative as Opt
import qualified Prettyprinter as PP

parsePingCmd :: Opt.Parser PingCmd
parsePingCmd = Opt.hsubparser $ mconcat
  [ Opt.metavar "ping"
  , Opt.command "ping" $ Opt.info pPing $ Opt.progDescDoc $ Just $ mconcat
    [ PP.pretty @String "Ping a cardano node either using node-to-node or node-to-client protocol. "
    , PP.pretty @String "It negotiates a handshake and keeps sending keep alive messages."
    ]
  ]

pHost :: Opt.Parser String
pHost =
  Opt.strOption $ mconcat
    [ Opt.long "host"
    , Opt.short 'h'
    , Opt.metavar "HOST"
    , Opt.help "Hostname/IP, e.g. relay.iohk.example."
    ]

pUnixSocket :: Opt.Parser String
pUnixSocket =
  Opt.strOption $ mconcat
    [ Opt.long "unixsock"
    , Opt.short 'u'
    , Opt.metavar "SOCKET"
    , Opt.help "Unix socket, e.g. file.socket."
    ]

pEndPoint :: Opt.Parser EndPoint
pEndPoint = fmap HostEndPoint pHost <|> fmap UnixSockEndPoint pUnixSocket

pPing :: Opt.Parser PingCmd
pPing = PingCmd
  <$> ( Opt.option Opt.auto $ mconcat
        [ Opt.long "count"
        , Opt.short 'c'
        , Opt.metavar "COUNT"
        , Opt.help $ mconcat
          [ "Stop after sending count requests and receiving count responses.  "
          , "If this option is not specified, ping will operate until interrupted.  "
          ]
        , Opt.value maxBound
        ]
      )
  <*> pEndPoint
  <*> ( Opt.strOption $ mconcat
        [ Opt.long "port"
        , Opt.short 'p'
        , Opt.metavar "PORT"
        , Opt.help "Port number, e.g. 1234."
        , Opt.value "3001"
        ]
      )
  <*> ( Opt.option Opt.auto $ mconcat
        [ Opt.long "magic"
        , Opt.short 'm'
        , Opt.metavar "MAGIC"
        , Opt.help "Network magic."
        , Opt.value CNP.mainnetMagic
        ]
      )
  <*> ( Opt.switch $ mconcat
        [ Opt.long "json"
        , Opt.short 'j'
        , Opt.help "JSON output flag."
        ]
      )
  <*> ( Opt.switch $ mconcat
        [ Opt.long "quiet"
        , Opt.short 'q'
        , Opt.help "Quiet flag, CSV/JSON only output"
        ]
      )
  <*> ( Opt.switch $ mconcat
        [ Opt.long "query-versions"
        , Opt.short 'Q'
        , Opt.help "Query the supported protocol versions using the handshake protocol and terminate the connection."
        ]
      )
