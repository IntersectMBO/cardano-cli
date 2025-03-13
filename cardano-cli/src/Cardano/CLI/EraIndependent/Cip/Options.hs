{-# LANGUAGE TypeApplications #-}

module Cardano.CLI.EraIndependent.Cip.Options
  ( parseCipCmd
  )
where

import Cardano.CLI.Command (ClientCommand (CipFormatCmds))
import Options.Applicative qualified as Opt
import Prettyprinter qualified as PP
import Cardano.CLI.EraIndependent.Cip.Command
import Cardano.CLI.EraIndependent.Cip.Cip129.Options

import Cardano.CLI.Parser

parseCipCmd :: Opt.Mod Opt.CommandFields ClientCommand
parseCipCmd =
  Opt.command "cip-format" $
    Opt.info (CipFormatCmds <$> pCipFormat) $
      Opt.progDescDoc $
        Just $
          mconcat
            [ PP.pretty @String "Convert a hex or bech32 ledger cddl encoded thing to a CIP format."
            ]

pCipFormat :: Opt.Parser CipFormatCmds
pCipFormat =  
    subParser "cip-129" $
        Opt.info pCip129 $
          Opt.progDesc $
            mconcat
              [ "Modified binary encoding of drep keys, constitutional committee cold and hot keys, governance actions."
              , "https://github.com/cardano-foundation/CIPs/tree/master/CIP-0129"
              ]




