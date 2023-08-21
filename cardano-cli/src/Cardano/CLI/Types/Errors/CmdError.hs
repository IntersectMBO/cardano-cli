{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Types.Errors.CmdError
  ( CmdError(..)
  ) where

import           Cardano.Api

import           Cardano.CLI.Types.Errors.EraBasedDelegationError
import           Cardano.CLI.Types.Errors.EraBasedRegistrationError
import           Cardano.CLI.Types.Errors.GovernanceActionsError
import           Cardano.CLI.Types.Errors.GovernanceCmdError
import           Cardano.CLI.Types.Errors.GovernanceCommitteeError
import           Cardano.CLI.Types.Errors.GovernanceVoteCmdError

data CmdError
  = CmdEraBasedRegistrationError !EraBasedRegistrationError
  | CmdEraDelegationError        !EraBasedDelegationError
  | CmdGovernanceActionError     !GovernanceActionsError
  | CmdGovernanceCmdError        !GovernanceCmdError
  | CmdGovernanceCommitteeError  !GovernanceCommitteeError
  | CmdGovernanceVoteError       !GovernanceVoteCmdError
  deriving Show

instance Error CmdError where
  displayError = \case
    CmdGovernanceCmdError e -> displayError e
    CmdEraDelegationError e -> displayError e
    CmdEraBasedRegistrationError e -> displayError e
    CmdGovernanceVoteError e -> displayError e
    CmdGovernanceCommitteeError e -> displayError e
    CmdGovernanceActionError e -> displayError e
