
module Cardano.CLI.EraIndependent.Cip.Run
  ( runCipFormat
  )
where

import Cardano.CLI.EraIndependent.Cip.Command
import  Cardano.CLI.EraIndependent.Cip.Cip129.Run

runCipFormat :: CipFormatCmds -> IO ()
runCipFormat (Cip129 cip129) = runCip129 cip129