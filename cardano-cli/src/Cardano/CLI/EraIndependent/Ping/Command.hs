module Cardano.CLI.EraIndependent.Ping.Command
  ( EndPoint (..)
  , PingCmd (..)
  , getConfigurationError
  )
where

import Data.Word

data EndPoint
  = HostEndPoint String
  | UnixSockEndPoint String
  deriving (Eq, Show)

data PingCmd = PingCmd
  { pingCmdCount :: !Word32
  , pingCmdEndPoint :: !EndPoint
  , pingCmdPort :: !String
  , pingCmdMagic :: !Word32
  , pingCmdJson :: !Bool
  , pingCmdQuiet :: !Bool
  , pingOptsHandshakeQuery :: !Bool
  , pingOptsGetTip :: !Bool
  }
  deriving (Eq, Show)

getConfigurationError :: PingCmd -> Maybe String
getConfigurationError
  PingCmd
    { pingCmdEndPoint = endPoint
    , pingOptsGetTip = getTip
    , pingOptsHandshakeQuery = query
    } =
    case endPoint of
      UnixSockEndPoint{}
        | query || getTip -> Nothing
        | otherwise -> Just "Unix sockets only support queries for available versions or a tip."
      HostEndPoint{} -> Nothing
