module Cardano.CLI.EraIndependent.Cip.Option
  ( parseCipCmd
  )
where

import Cardano.CLI.Command (ClientCommand (CipFormatCmds))
import Cardano.CLI.EraIndependent.Cip.Cip129.Option
import Cardano.CLI.EraIndependent.Cip.Command
import Cardano.CLI.Parser

import Options.Applicative qualified as Opt

parseCipCmd :: Opt.Parser ClientCommand
parseCipCmd =
  Opt.hsubparser $
    commandWithMetavar "cip-format" $
      Opt.info (CipFormatCmds <$> pCipFormat) $
        Opt.progDesc $
          mconcat
            [ "Group of commands related to CIP changes."
            ]

pCipFormat :: Opt.Parser CipFormatCmds
pCipFormat =
  Opt.hsubparser $
    commandWithMetavar "cip-129" $
      Opt.info pCip129 $
        Opt.progDesc $
          mconcat
            [ "Modified binary encoding of drep keys, constitutional committee cold and hot keys, governance actions. "
            , "https://github.com/cardano-foundation/CIPs/tree/master/CIP-0129"
            ]
