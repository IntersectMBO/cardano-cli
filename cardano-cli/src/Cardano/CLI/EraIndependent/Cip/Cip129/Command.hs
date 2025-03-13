module Cardano.CLI.EraIndependent.Cip.Cip129.Command
  ( Cip129(..)
  )
where

import Cardano.CLI.EraIndependent.Cip.Common

data Cip129 
    = Cip129DRep Input Output
    | Cip129CommitteeHotKey Input Output
    | Cip129CommitteeColdKey Input Output
    | Cip129GovernanceAction Input Output
