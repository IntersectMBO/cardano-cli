module Cardano.CLI.Commands.Ping
  ( EndPoint(..)
  , PingCmd(..)
  ) where

import           Data.Word

data EndPoint =
      HostEndPoint String
    | UnixSockEndPoint String
  deriving (Eq, Show)

data PingCmd = PingCmd
  { pingCmdCount           :: !Word32
  , pingCmdEndPoint        :: !EndPoint
  , pingCmdPort            :: !String
  , pingCmdMagic           :: !Word32
  , pingCmdJson            :: !Bool
  , pingCmdQuiet           :: !Bool
  , pingOptsHandshakeQuery :: !Bool
  } deriving (Eq, Show)
