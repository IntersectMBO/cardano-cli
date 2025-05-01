{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraIndependent.Node.Command
  ( NodeCmds (..)
  , renderNodeCmds
  , NodeKeyGenColdCmdArgs (..)
  , NodeKeyGenKESCmdArgs (..)
  , NodeKeyGenVRFCmdArgs (..)
  , NodeKeyHashVRFCmdArgs (..)
  , NodeNewCounterCmdArgs (..)
  , NodeIssueOpCertCmdArgs (..)
  )
where

import Cardano.Api.Shelley

import Cardano.CLI.Type.Common
import Cardano.CLI.Type.Key

import Data.Text (Text)
import Vary (Vary)

data NodeCmds
  = NodeKeyGenColdCmd !NodeKeyGenColdCmdArgs
  | NodeKeyGenKESCmd !NodeKeyGenKESCmdArgs
  | NodeKeyGenVRFCmd !NodeKeyGenVRFCmdArgs
  | NodeKeyHashVRFCmd !NodeKeyHashVRFCmdArgs
  | NodeNewCounterCmd !NodeNewCounterCmdArgs
  | NodeIssueOpCertCmd !NodeIssueOpCertCmdArgs
  deriving Show

data NodeKeyGenColdCmdArgs
  = NodeKeyGenColdCmdArgs
  { keyOutputFormat :: !(Vary [FormatBech32, FormatTextEnvelope])
  , vkeyFile :: !(VerificationKeyFile Out)
  , skeyFile :: !(SigningKeyFile Out)
  , operationalCertificateIssueCounter :: !(OpCertCounterFile Out)
  }
  deriving Show

data NodeKeyGenKESCmdArgs
  = NodeKeyGenKESCmdArgs
  { keyOutputFormat :: !(Vary [FormatBech32, FormatTextEnvelope])
  , vkeyFile :: !(VerificationKeyFile Out)
  , skeyFile :: !(SigningKeyFile Out)
  }
  deriving Show

data NodeKeyGenVRFCmdArgs
  = NodeKeyGenVRFCmdArgs
  { keyOutputFormat :: !(Vary [FormatBech32, FormatTextEnvelope])
  , vkeyFile :: !(VerificationKeyFile Out)
  , skeyFile :: !(SigningKeyFile Out)
  }
  deriving Show

data NodeKeyHashVRFCmdArgs
  = NodeKeyHashVRFCmdArgs
  { vkeySource :: !(VerificationKeyOrFile VrfKey)
  , mOutFile :: !(Maybe (File () Out))
  }
  deriving Show

data NodeNewCounterCmdArgs
  = NodeNewCounterCmdArgs
  { coldVkeyFile :: !ColdVerificationKeyOrFile
  , counter :: !Word
  , mOutFile :: !(OpCertCounterFile InOut)
  }
  deriving Show

data NodeIssueOpCertCmdArgs
  = NodeIssueOpCertCmdArgs
  { kesVkeySource :: !(VerificationKeyOrFile KesKey)
  -- ^ The hot KES verification key.
  , poolSkeyFile :: !(SigningKeyFile In)
  -- ^ The cold signing key.
  , operationalCertificateCounterFile :: !(OpCertCounterFile InOut)
  -- ^ Counter that establishes the precedence of the operational certificate.
  , kesPeriod :: !KESPeriod
  -- ^ Start of the validity period for this certificate.
  , outFile :: !(File () Out)
  }
  deriving Show

renderNodeCmds :: NodeCmds -> Text
renderNodeCmds = \case
  NodeKeyGenColdCmd{} -> "node key-gen"
  NodeKeyGenKESCmd{} -> "node key-gen-KES"
  NodeKeyGenVRFCmd{} -> "node key-gen-VRF"
  NodeKeyHashVRFCmd{} -> "node key-hash-VRF"
  NodeNewCounterCmd{} -> "node new-counter"
  NodeIssueOpCertCmd{} -> "node issue-op-cert"
