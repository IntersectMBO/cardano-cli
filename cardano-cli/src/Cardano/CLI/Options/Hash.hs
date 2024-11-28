{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Cardano.CLI.Options.Hash
  ( pHashCmds
  )
where

import qualified Cardano.Api.Ledger as L

import qualified Cardano.CLI.Commands.Hash as Cmd
import           Cardano.CLI.EraBased.Options.Common
import           Cardano.CLI.Parser

import           Data.Foldable
import           Options.Applicative
import qualified Options.Applicative as Opt

pHashCmds :: Parser Cmd.HashCmds
pHashCmds =
  subParser "hash" $
    Opt.info
      (asum [pHashAnchorDataCmd, pHashScriptCmd, pHashGenesisHashCmd])
      ( Opt.progDesc $
          mconcat
            [ "Compute the hash to pass to the various --*-hash arguments of commands."
            ]
      )

pHashAnchorDataCmd :: Parser Cmd.HashCmds
pHashAnchorDataCmd = do
  subParser "anchor-data"
    $ Opt.info
      ( fmap
          Cmd.HashAnchorDataCmd
          ( Cmd.HashAnchorDataCmdArgs
              <$> pAnchorDataHashSource
              <*> pHashGoal
          )
      )
    $ Opt.progDesc "Compute the hash of some anchor data (to then pass it to other commands)."

pHashGoal :: Parser (Cmd.HashGoal (L.SafeHash L.StandardCrypto L.AnchorData))
pHashGoal =
  asum
    [ Cmd.CheckHash <$> pExpectedAnchorDataHash
    , Cmd.HashToFile <$> pOutputFile
    ]
    <|> pure Cmd.HashToStdout

pAnchorDataHashSource :: Parser Cmd.AnchorDataHashSource
pAnchorDataHashSource =
  asum
    [ Cmd.AnchorDataHashSourceText
        <$> Opt.strOption
          ( mconcat
              [ Opt.long "text"
              , Opt.metavar "TEXT"
              , Opt.help "Text to hash as UTF-8"
              ]
          )
    , Cmd.AnchorDataHashSourceBinaryFile
        <$> pFileInDirection "file-binary" "Binary file to hash"
    , Cmd.AnchorDataHashSourceTextFile
        <$> pFileInDirection "file-text" "Text file to hash"
    , Cmd.AnchorDataHashSourceURL
        <$> pUrl "url" "A URL to the file to hash (HTTP(S) and IPFS only)"
    ]

pHashScriptCmd :: Parser Cmd.HashCmds
pHashScriptCmd = do
  subParser "script"
    $ Opt.info
      ( fmap
          Cmd.HashScriptCmd
          ( Cmd.HashScriptCmdArgs
              <$> pScript
              <*> optional pOutputFile
          )
      )
    $ Opt.progDesc "Compute the hash of a script (to then pass it to other commands)."

pHashGenesisHashCmd :: Parser Cmd.HashCmds
pHashGenesisHashCmd =
  subParser "genesis-file" $
    Opt.info pGenesisHash $
      Opt.progDesc "Compute the hash of a genesis file."

pGenesisHash :: Parser Cmd.HashCmds
pGenesisHash =
  Cmd.HashGenesisFile <$> pGenesisFile "The genesis file."
