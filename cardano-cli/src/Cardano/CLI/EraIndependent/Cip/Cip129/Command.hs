module Cardano.CLI.EraIndependent.Cip.Cip129.Command
  ( Cip129 (..)
  , renderCip129Command
  )
where

import Cardano.CLI.EraIndependent.Cip.Common

import Data.Text (Text)

data Cip129
  = Cip129DRep Input Output
  | Cip129CommitteeHotKey Input Output
  | Cip129CommitteeColdKey Input Output
  | Cip129GovernanceAction Input Output

renderCip129Command :: Cip129 -> Text
renderCip129Command (Cip129DRep{}) = "cip-129 drep"
renderCip129Command (Cip129CommitteeHotKey{}) = "cip-129 committee-hot-key"
renderCip129Command (Cip129CommitteeColdKey{}) = "cip-129 committee-cold-key"
renderCip129Command (Cip129GovernanceAction{}) = "cip-129 governance-action-id"
