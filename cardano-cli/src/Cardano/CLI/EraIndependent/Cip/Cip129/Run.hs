module Cardano.CLI.EraIndependent.Cip.Cip129.Run
  ( runCip129
  )
where

import Cardano.CLI.EraIndependent.Cip.Cip129.Command

runCip129 :: Cip129 -> IO ()
runCip129 (Cip129DRep inp out) = undefined 
runCip129 (Cip129CommitteeHotKey inp out) = undefined
runCip129 (Cip129CommitteeColdKey inp out) = undefined
runCip129 (Cip129GovernanceAction inp out) = undefined