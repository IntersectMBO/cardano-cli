{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraIndependent.Hash.Command
  ( HashCmds (..)
  , HashGoal (..)
  , HashAnchorDataCmdArgs (..)
  , HashScriptCmdArgs (..)
  , AnchorDataHashSource (..)
  , renderHashCmds
  )
where

import Cardano.Api
import Cardano.Api.Ledger qualified as L

import Cardano.CLI.Type.Common

import Data.Text (Text)

data HashCmds
  = HashAnchorDataCmd !HashAnchorDataCmdArgs
  | HashScriptCmd !HashScriptCmdArgs
  | HashGenesisFile !GenesisFile

data HashGoal hash
  = -- | The hash is written to stdout
    HashToStdout
  | -- | The hash to check against
    CheckHash !hash
  | -- | The output file to which the hash is written
    HashToFile !(File () Out)
  deriving Show

data HashAnchorDataCmdArgs
  = HashAnchorDataCmdArgs
  { toHash :: !AnchorDataHashSource
  , hashGoal :: !(HashGoal (L.SafeHash L.AnchorData))
  }
  deriving Show

data AnchorDataHashSource
  = AnchorDataHashSourceBinaryFile (File ProposalBinary In)
  | AnchorDataHashSourceTextFile (File ProposalText In)
  | AnchorDataHashSourceText Text
  | AnchorDataHashSourceURL L.Url
  deriving Show

data HashScriptCmdArgs
  = HashScriptCmdArgs
  { toHash :: !ScriptFile
  , mOutFile :: !(Maybe (File () Out))
  -- ^ The output file to which the hash is written
  }
  deriving Show

renderHashCmds :: HashCmds -> Text
renderHashCmds = \case
  HashAnchorDataCmd{} -> "hash anchor-data"
  HashScriptCmd{} -> "hash script"
  HashGenesisFile{} -> "hash genesis-file"
