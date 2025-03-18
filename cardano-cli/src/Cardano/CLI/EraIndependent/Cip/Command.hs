module Cardano.CLI.EraIndependent.Cip.Command
  ( -- * Supported CIPs
    Cip129 (..)
  , CipFormatCmds (..)
  , renderCipFormatCmds
  )
where

import Cardano.CLI.EraIndependent.Cip.Cip129.Command

import Data.Text (Text)

newtype CipFormatCmds
  = Cip129 Cip129

renderCipFormatCmds :: CipFormatCmds -> Text
renderCipFormatCmds (Cip129 cip129) = "cip-format" <> renderCip129Command cip129
