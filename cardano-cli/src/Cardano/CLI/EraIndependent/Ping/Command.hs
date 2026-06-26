module Cardano.CLI.EraIndependent.Ping.Command
  ( PingCmd (..)
  , Address
  )
where

import Cardano.Network.Ping

data PingCmd = PingCmd { 
    pingOpts      :: PingOpts,
    pingAddresses :: [Address (Unresolved SRVOrFilePathUnresolved)]
  }
