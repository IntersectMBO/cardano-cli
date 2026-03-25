{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraIndependent.Node.Command
  ( NodeCmds (..)
  , renderNodeCmds
  , NodeKeyGenColdCmdArgs (..)
  , NodeKeyGenKESCmdArgs (..)
  , NodeKeyGenVRFCmdArgs (..)
  , NodeKeyGenBLSCmdArgs (..)
  , NodeKeyHashVRFCmdArgs (..)
  , NodeKeyHashBLSCmdArgs (..)
  , NodeIssuePopBLSCmdArgs (..)
  , NodeNewCounterCmdArgs (..)
  , NodeIssueOpCertCmdArgs (..)
  , NodeIssueLeiosOpCertCmdArgs (..)
  )
where

import Cardano.Api

import Cardano.CLI.Type.Common
import Cardano.CLI.Type.Key

import Vary (Vary)

data NodeCmds
  = NodeKeyGenColdCmd !NodeKeyGenColdCmdArgs
  | NodeKeyGenKESCmd !NodeKeyGenKESCmdArgs
  | NodeKeyGenVRFCmd !NodeKeyGenVRFCmdArgs
  | NodeKeyGenBLSCmd !NodeKeyGenBLSCmdArgs
  | NodeKeyHashVRFCmd !NodeKeyHashVRFCmdArgs
  | NodeKeyHashBLSCmd !NodeKeyHashBLSCmdArgs
  | NodeIssuePopBLSCmd !NodeIssuePopBLSCmdArgs
  | NodeNewCounterCmd !NodeNewCounterCmdArgs
  | NodeIssueOpCertCmd !NodeIssueOpCertCmdArgs
  | NodeIssueLeiosOpCertCmd !NodeIssueLeiosOpCertCmdArgs
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

data NodeKeyGenBLSCmdArgs
  = NodeKeyGenBLSCmdArgs
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

data NodeKeyHashBLSCmdArgs
  = NodeKeyHashBLSCmdArgs
  { vkeySource :: !(VerificationKeyOrFile BlsKey)
  , mOutFile :: !(Maybe (File () Out))
  }
  deriving Show

data NodeIssuePopBLSCmdArgs
  = NodeIssuePopBLSCmdArgs
  { blsSkeyFile :: !(SigningKeyFile In)
  -- ^ The BLS signing key.
  , outFile :: !(File () Out)
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

data NodeIssueLeiosOpCertCmdArgs
  = NodeIssueLeiosOpCertCmdArgs
  { kesVkeySource :: !(VerificationKeyOrFile KesKey)
  -- ^ The hot KES verification key.
  , poolSkeyFile :: !(SigningKeyFile In)
  -- ^ The cold signing key.
  , operationalCertificateCounterFile :: !(OpCertCounterFile InOut)
  -- ^ Counter that establishes the precedence of the operational certificate.
  , kesPeriod :: !KESPeriod
  -- ^ Start of the validity period for this certificate.
  , blsVkeySource :: !(VerificationKeyOrFile BlsKey)
  -- ^ The BLS verification key.
  , blsPossessionProofFile :: !(File BlsPossessionProof In)
  -- ^ The BLS possession proof file.
  , outFile :: !(File () Out)
  }
  deriving Show

renderNodeCmds :: NodeCmds -> Text
renderNodeCmds = \case
  NodeKeyGenColdCmd{} -> "node key-gen"
  NodeKeyGenKESCmd{} -> "node key-gen-KES"
  NodeKeyGenVRFCmd{} -> "node key-gen-VRF"
  NodeKeyGenBLSCmd{} -> "node key-gen-BLS"
  NodeKeyHashVRFCmd{} -> "node key-hash-VRF"
  NodeKeyHashBLSCmd{} -> "node key-hash-BLS"
  NodeIssuePopBLSCmd{} -> "node issue-pop-BLS"
  NodeNewCounterCmd{} -> "node new-counter"
  NodeIssueOpCertCmd{} -> "node issue-op-cert"
  NodeIssueLeiosOpCertCmd{} -> "node issue-leios-op-cert"
