{-# LANGUAGE RankNTypes #-}

module Cardano.CLI.EraIndependent.Cip.Run
  ( runCipFormat
  )
where

import Cardano.CLI.Compatible.Exception
import Cardano.CLI.EraIndependent.Cip.Cip129.Run
import Cardano.CLI.EraIndependent.Cip.Command

runCipFormat :: CipFormatCmds -> CIO e ()
runCipFormat (Cip129 cip129) = runCip129 cip129
