{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Commands.Hash
  ( HashCmds (..)
  , HashAnchorDataCmdArgs (..)
  , HashScriptCmdArgs (..)
  , AnchorDataHashSource (..)
  , renderHashCmds
  )
where

import           Cardano.Api

import           Cardano.CLI.Types.Common

import           Data.Text (Text)

data HashCmds
  = HashAnchorDataCmd !HashAnchorDataCmdArgs
  | HashScriptCmd !HashScriptCmdArgs

data HashAnchorDataCmdArgs
  = HashAnchorDataCmdArgs
  { toHash :: !AnchorDataHashSource
  , mOutFile :: !(Maybe (File () Out))
  -- ^ The output file to which the hash is written
  }
  deriving Show

data AnchorDataHashSource
  = AnchorDataHashSourceBinaryFile (File ProposalBinary In)
  | AnchorDataHashSourceTextFile (File ProposalText In)
  | AnchorDataHashSourceText Text
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
