
module Cardano.CLI.EraIndependent.Cip.Command
  ( -- * Supported CIPs
    Cip129(..)

  , CipFormatCmds(..)

  )
where
import Cardano.CLI.EraIndependent.Cip.Cip129.Command

newtype CipFormatCmds
    = Cip129 Cip129


