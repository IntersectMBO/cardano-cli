{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Commands.Hash
  ( HashCmds (..)
  , HashGoal (..)
  , HashAnchorDataCmdArgs (..)
  , HashScriptCmdArgs (..)
  , AnchorDataHashSource (..)
  , renderHashCmds
  )
where

import           Cardano.Api
import qualified Cardano.Api.Ledger as L

import           Cardano.CLI.Types.Common

import           Data.Text (Text)

data HashCmds
  = HashAnchorDataCmd !HashAnchorDataCmdArgs
  | HashScriptCmd !HashScriptCmdArgs

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
  , hashGoal :: !(HashGoal (L.SafeHash L.StandardCrypto L.AnchorData))
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
