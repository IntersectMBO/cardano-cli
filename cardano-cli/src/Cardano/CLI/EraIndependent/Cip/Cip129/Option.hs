module Cardano.CLI.EraIndependent.Cip.Cip129.Option
  ( pCip129
  )
where

import Cardano.CLI.EraIndependent.Cip.Command
import Cardano.CLI.EraIndependent.Cip.Common
import Cardano.CLI.Parser

import Data.Foldable
import Options.Applicative qualified as Opt

-- Add sub parsers
pCip129 :: Opt.Parser CipFormatCmds
pCip129 =
  Cip129
    <$> asum
      [ Opt.hsubparser $
          commandWithMetavar "drep" $
            Opt.info pCip129Drep $
              Opt.progDesc "Convert drep verification key to the cip-129 compliant format"
      , Opt.hsubparser $
          commandWithMetavar "committee-hot-key" $
            Opt.info pCip129CommitteeHotKey $
              Opt.progDesc "Convert committee hot key to the cip-129 compliant format"
      , Opt.hsubparser $
          commandWithMetavar "committee-cold-key" $
            Opt.info pCip129CommitteeColdKey $
              Opt.progDesc "Convert committee cold key to the cip-129 compliant format"
      , Opt.hsubparser $
          commandWithMetavar "governance-action-id" $
            Opt.info pCip129GovernanceAction $
              Opt.progDesc "Convert governance action id to the cip-129 compliant format"
      ]

pCip129Drep :: Opt.Parser Cip129
pCip129Drep =
  Cip129DRep
    <$> pInput
    <*> pOutput
 where
  pInput =
    asum
      [ pInputFile "drep-file" "Input hex/bech32/text envelope drep file"
      , pInputHexText "drep-hex" "HEX" "Input hex encoded drep"
      , pInputBech32Text "drep-bech32" "BECH32" "Input bech32 encoded drep"
      ]

pCip129CommitteeHotKey :: Opt.Parser Cip129
pCip129CommitteeHotKey =
  Cip129CommitteeHotKey
    <$> pInput
    <*> pOutput
 where
  pInput =
    asum
      [ pInputFile "committee-hot-key-file" "Input hex/bech32/text envelope committee hot key file"
      , pInputHexText "committee-hot-key-hex" "HEX" "Input hex encoded committee hot key"
      , pInputBech32Text "committee-hot-key-bech32" "BECH32" "Input bech32 encoded committee hot key"
      ]

pCip129CommitteeColdKey :: Opt.Parser Cip129
pCip129CommitteeColdKey =
  Cip129CommitteeColdKey
    <$> pInput
    <*> pOutput
 where
  pInput =
    asum
      [ pInputFile "committee-cold-key-file" "Input hex/bech32/text envelope committee cold key file"
      , pInputHexText "committee-cold-key-hex" "HEX" "Input hex encoded committee cold key"
      , pInputBech32Text "committee-cold-key-bech32" "BECH32" "Input bech32 encoded committee cold key"
      ]

pCip129GovernanceAction :: Opt.Parser Cip129
pCip129GovernanceAction =
  Cip129GovernanceAction
    <$> pInput
    <*> pOutput
 where
  pInput =
    asum
      [ pInputFile "governance-action-file" "Input hex/bech32/text envelope governance action file"
      , pInputHexText "governance-action-hex" "HEX" "Input hex encoded governance action"
      , pInputBech32Text "governance-action-bech32" "BECH32" "Input bech32 encoded governance action"
      ]

pOutput :: Opt.Parser Output
pOutput =
  asum
    [ pOutputFile "output-file" "Output file"
    , pOutputText "output-text" "Output text"
    ]
