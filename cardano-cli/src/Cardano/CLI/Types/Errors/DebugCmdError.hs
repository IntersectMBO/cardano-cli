{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}

module Cardano.CLI.Types.Errors.DebugCmdError
  ( DebugCmdError (..)
  )
where

import           Cardano.Api

import           GHC.Generics (Generic)

data DebugCmdError
  = DebugCmdFailed
  | DebugCmdTextEnvCddlError !(FileError TextEnvelopeCddlError)
  | DebugCmdWriteFileError !(FileError ())
  deriving (Show, Generic)
