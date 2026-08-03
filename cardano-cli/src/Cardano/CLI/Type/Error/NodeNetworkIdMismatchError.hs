module Cardano.CLI.Type.Error.NodeNetworkIdMismatchError
  ( NodeNetworkIdMismatchError (..)
  )
where

import Cardano.Api

data NodeNetworkIdMismatchError = NodeNetworkIdMismatchError
  { cliNetworkId :: !NetworkId
  , nodeNetworkId :: !NetworkId
  }
  deriving Show

instance Error NodeNetworkIdMismatchError where
  prettyError (NodeNetworkIdMismatchError cli node) =
    mconcat
      [ "The network id given to the command does not match the network of the node: "
      , "the command was given "
      , renderNetworkId cli
      , ", but the node is on "
      , renderNetworkId node
      , ".\n"
      , "Specify "
      , renderNetworkIdFlag node
      , " (or set the CARDANO_NODE_NETWORK_ID environment variable accordingly), "
      , "or connect to a node on the expected network."
      ]
   where
    renderNetworkId :: NetworkId -> Doc ann
    renderNetworkId Mainnet = "mainnet"
    renderNetworkId (Testnet (NetworkMagic magic)) =
      "a testnet with network magic " <> pretty magic

    renderNetworkIdFlag :: NetworkId -> Doc ann
    renderNetworkIdFlag Mainnet = "--mainnet"
    renderNetworkIdFlag (Testnet (NetworkMagic magic)) =
      "--testnet-magic " <> pretty magic
