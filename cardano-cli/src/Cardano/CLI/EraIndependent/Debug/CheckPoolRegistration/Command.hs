{-# LANGUAGE DataKinds #-}

module Cardano.CLI.EraIndependent.Debug.CheckPoolRegistration.Command
  ( CheckPoolRegistrationCmdArgs (..)
  )
where

import Cardano.Api

import Cardano.CLI.Type.Common

import Vary (Vary)

-- | Argument for the 'debug check-pool-registration' command.
--
-- There is deliberately no @--stake-pool-id@: the certificate carries the pool
-- id, so it is the only input beyond the node connection options. The era is
-- taken from the node rather than from a flag.
data CheckPoolRegistrationCmdArgs = CheckPoolRegistrationCmdArgs
  { nodeConnInfo :: !LocalNodeConnectInfo
  , poolRegistrationCertFile :: !(File () In)
  -- ^ The built, not yet submitted, stake pool registration certificate.
  , outputFormat :: !(Vary [FormatJson, FormatText, FormatYaml])
  , mOutFile :: !(Maybe (File () Out))
  }
  deriving Show
