
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

{- HLINT ignore "Move brackets to avoid $" -}

module Cardano.CLI.Run.HandShake
  ( HandShakeCmd(..)
  , parseHandShakeCmd
  , toPingCmd
  ) where


import           Cardano.CLI.Run.Ping (PingCmd (..))
import qualified Cardano.CLI.Run.Ping as Ping
import qualified Cardano.Network.Ping as CNP

import           Control.Applicative ((<|>))
import           Data.Word (Word32)
import qualified Options.Applicative as Opt
import qualified Prettyprinter as PP

data HandShakeCmd = HandShakeCmd
  { endPoint        :: !Ping.EndPoint
  , port            :: !String
  , magic           :: !Word32
  , json            :: !Bool
  , quiet           :: !Bool
  , query           :: !Bool
  } deriving (Eq, Show)

toPingCmd :: HandShakeCmd -> PingCmd
toPingCmd (HandShakeCmd {endPoint, port, magic, json, quiet, query}) =
  PingCmd {
      pingCmdCount = 0
    , pingCmdEndPoint = endPoint
    , pingCmdPort = port
    , pingCmdMagic = magic
    , pingCmdJson = json
    , pingCmdQuiet = quiet
    , pingOptsHandshakeQuery = query
  }

parseHandShakeCmd :: Opt.Parser HandShakeCmd
parseHandShakeCmd = Opt.hsubparser $ mconcat
  [ Opt.metavar "handshake"
  , Opt.command "handshake"
      $ Opt.info pHandShake $ Opt.progDescDoc
      $ Just $ PP.pretty @String "Negotiates a handshake and prints the negotiated version"
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

pEndPoint :: Opt.Parser Ping.EndPoint
pEndPoint = fmap Ping.HostEndPoint pHost <|> fmap Ping.UnixSockEndPoint pUnixSocket

pHandShake :: Opt.Parser HandShakeCmd
pHandShake = HandShakeCmd
  <$> pEndPoint
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
        , Opt.help "Don't print network statistics, only print negotiated version (default) or supported versions (-Q)."
        ]
      )
  <*> ( Opt.switch $ mconcat
        [ Opt.long "query-versions"
        , Opt.short 'Q'
        , Opt.help "Instead of printing the negotiated version, print all queried versions."
        ]
      )
