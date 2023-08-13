
module Cardano.CLI.Types.Errors.GovernanceCommitteeError
  ( GovernanceCommitteeError(..)
  ) where

import           Cardano.Api

data GovernanceCommitteeError
  = GovernanceCommitteeCmdWriteFileError (FileError ())
  | GovernanceCommitteeCmdTextEnvReadFileError (FileError TextEnvelopeError)
