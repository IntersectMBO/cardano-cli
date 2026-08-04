module Cardano.CLI.Type.Error.StakeAddressNetworkIdMismatchError
  ( StakeAddressNetworkIdMismatchError (..)
  )
where

import Cardano.Api
import Cardano.Api.Ledger qualified as L

data StakeAddressNetworkIdMismatchError = StakeAddressNetworkIdMismatchError
  { stakeAddress :: !StakeAddress
  , cliNetworkId :: !NetworkId
  }
  deriving Show

instance Error StakeAddressNetworkIdMismatchError where
  prettyError (StakeAddressNetworkIdMismatchError sAddr@(StakeAddress addrNetwork _) netId) =
    mconcat
      [ "The stake address "
      , pretty (serialiseAddress sAddr)
      , " is "
      , renderNetwork addrNetwork
      , ", but the command was given "
      , renderNetworkId netId
      , ".\n"
      , "Check the network options (--mainnet, --testnet-magic, or the "
      , "CARDANO_NODE_NETWORK_ID environment variable), or use a stake address "
      , "for the expected network."
      ]
   where
    renderNetwork :: L.Network -> Doc ann
    renderNetwork L.Mainnet = "a mainnet stake address"
    renderNetwork L.Testnet = "a testnet stake address"

    renderNetworkId :: NetworkId -> Doc ann
    renderNetworkId Mainnet = "mainnet"
    renderNetworkId (Testnet (NetworkMagic magic)) =
      "a testnet with network magic " <> pretty magic
