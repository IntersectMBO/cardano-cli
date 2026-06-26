{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes     #-}

module Cardano.CLI.EraIndependent.Ping.Run
  ( runPingCmd
  )
where

import Cardano.Api

import Cardano.CLI.Compatible.Exception (CIO)
import Cardano.CLI.EraIndependent.Ping.Command
import Cardano.Network.Ping qualified as Ping


runPingCmd :: PingCmd -> CIO e ()
runPingCmd PingCmd { pingOpts, pingAddresses } =
    liftIO $ Ping.pingClients pingOpts pingAddresses
