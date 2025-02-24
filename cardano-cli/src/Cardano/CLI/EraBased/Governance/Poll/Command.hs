{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.Governance.Poll.Command
  ( GovernancePollCmds (..)
  , renderGovernancePollCmds
  , GovernanceCreatePollCmdArgs (..)
  , GovernanceAnswerPollCmdArgs (..)
  , GovernanceVerifyPollCmdArgs (..)
  )
where

import Cardano.Api
import Cardano.Api.Shelley

import Data.Text (Text)

data GovernancePollCmds era
  = GovernanceCreatePoll !(GovernanceCreatePollCmdArgs era)
  | GovernanceAnswerPoll !(GovernanceAnswerPollCmdArgs era)
  | GovernanceVerifyPoll !(GovernanceVerifyPollCmdArgs era)

-- | Create a SPO poll
data GovernanceCreatePollCmdArgs era
  = GovernanceCreatePollCmdArgs
  { eon :: !(BabbageEraOnwards era)
  , prompt :: !Text
  , choices :: ![Text]
  , nonce :: !(Maybe Word)
  , outFile :: !(File GovernancePoll Out)
  }
  deriving (Eq, Show)

-- | Answer a SPO poll
data GovernanceAnswerPollCmdArgs era
  = GovernanceAnswerPollCmdArgs
  { eon :: !(BabbageEraOnwards era)
  , pollFile :: !(File GovernancePoll In)
  , answerIndex :: !(Maybe Word)
  , mOutFile :: !(Maybe (File () Out))
  }
  deriving (Eq, Show)

-- | Verify answer to a given SPO poll
data GovernanceVerifyPollCmdArgs era
  = GovernanceVerifyPollCmdArgs
  { eon :: !(BabbageEraOnwards era)
  , pollFile :: !(File GovernancePoll In)
  , txFile :: !(File (Tx ()) In)
  , mOutFile :: !(Maybe (File () Out))
  }
  deriving (Eq, Show)

renderGovernancePollCmds
  :: ()
  => GovernancePollCmds era
  -> Text
renderGovernancePollCmds = \case
  GovernanceCreatePoll{} -> "governance create-poll"
  GovernanceAnswerPoll{} -> "governance answer-poll"
  GovernanceVerifyPoll{} -> "governance verify-poll"
