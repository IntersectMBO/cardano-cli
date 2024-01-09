{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Cardano.CLI.EraBased.Options.Governance.Hash
  (
    pGovernanceHashCmds,
  ) where

import           Cardano.Api

import qualified Cardano.CLI.EraBased.Commands.Governance.Hash as Cmd
import           Cardano.CLI.EraBased.Options.Common

import           Data.Foldable
import           Options.Applicative
import qualified Options.Applicative as Opt

pGovernanceHashCmds
  :: CardanoEra era
  -> Maybe (Parser (Cmd.GovernanceHashCmds era))
pGovernanceHashCmds era =
  subInfoParser "hash"
    ( Opt.progDesc
        $ mconcat
          [ "Compute the hash to pass to the various --*-hash arguments of governance commands."
          ]
    )
    [ pGovernanceHashAnchorDataCmd era
    , pGovernanceHashScriptCmd era
    ]

pGovernanceHashAnchorDataCmd :: ()
  => CardanoEra era
  -> Maybe (Parser (Cmd.GovernanceHashCmds era))
pGovernanceHashAnchorDataCmd era = do
  eon <- forEraMaybeEon era
  return
    $ subParser "anchor-data"
    $ Opt.info
        ( fmap Cmd.GovernanceHashAnchorDataCmd
            (Cmd.GovernanceHashAnchorDataCmdArgs eon
               <$> pGovernanceAnchorDataHashSource
               <*> optional pOutputFile))
    $ Opt.progDesc "Compute the hash of some anchor data."

pGovernanceAnchorDataHashSource :: Parser Cmd.GovernanceAnchorDataHashSource
pGovernanceAnchorDataHashSource =
  asum
    [
      Cmd.GovernanceAnchorDataHashSourceText
        <$> Opt.strOption
              ( mconcat
                [ Opt.long "text"
                , Opt.metavar "TEXT"
                , Opt.help "Text to hash as UTF-8"
                ]
              )
    , Cmd.GovernanceAnchorDataHashSourceBinaryFile
        <$> pFileInDirection "file-binary" "Binary file to hash"
    , Cmd.GovernanceAnchorDataHashSourceTextFile
        <$> pFileInDirection "file-text" "Text file to hash"
    ]

pGovernanceHashScriptCmd :: ()
  => CardanoEra era
  -> Maybe (Parser (Cmd.GovernanceHashCmds era))
pGovernanceHashScriptCmd era = do
  eon <- forEraMaybeEon era
  return
    $ subParser "script"
    $ Opt.info
        ( fmap Cmd.GovernanceHashScriptCmd
            (Cmd.GovernanceHashScriptCmdArgs eon
               <$> pScript
               <*> optional pOutputFile))
    $ Opt.progDesc "Compute the hash of a script."
