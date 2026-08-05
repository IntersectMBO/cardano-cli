{-# LANGUAGE TypeApplications #-}

module Cardano.CLI.EraIndependent.Ping.Option
  ( parsePingCmd
  )
where

import Cardano.CLI.Command (ClientCommand (CliPingCommand))
import Cardano.CLI.EraIndependent.Ping.Command
import Cardano.Network.Ping qualified as Ping

import Control.Applicative
import Options.Applicative qualified as Opt
import Prettyprinter qualified as PP

parsePingCmd :: Opt.Mod Opt.CommandFields ClientCommand
parsePingCmd =
  Opt.command "ping" $
    Opt.info (CliPingCommand <$> pPing <**> Opt.helper) $
      Opt.progDescDoc $
        Just $
          mconcat
            [ PP.pretty @String "Ping a cardano node either using node-to-node or node-to-client protocol. "
            , PP.pretty @String "It negotiates a handshake and keeps sending keep alive messages."
            ]

pPing :: Opt.Parser PingCmd
pPing = uncurry PingCmd <$> Ping.cmdlineParser
