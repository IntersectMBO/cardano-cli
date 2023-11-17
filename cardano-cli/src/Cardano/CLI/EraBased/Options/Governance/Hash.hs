{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Cardano.CLI.EraBased.Options.Governance.Hash
  (
    pGovernanceHashCmds
  ) where

import           Cardano.Api

import           Cardano.CLI.EraBased.Commands.Governance.Hash
                   (GovernanceHashCmdArgs (GovernanceHashCmdArgs))
import qualified Cardano.CLI.EraBased.Commands.Governance.Hash as Cmd
import           Cardano.CLI.EraBased.Options.Common

import           Data.Foldable
import           Options.Applicative
import qualified Options.Applicative as Opt

pGovernanceHashCmds :: ()
  => CardanoEra era
  -> Maybe (Parser (Cmd.GovernanceHashCmds era))
pGovernanceHashCmds era = do
  eon <- forEraMaybeEon era
  return
    $ subParser "hash"
    $ Opt.info
        ( fmap Cmd.GovernanceHashCmd
            (GovernanceHashCmdArgs eon
               <$> pGovernanceHashSource
               <*> optional pOutputFile))
    $ Opt.progDesc "Compute the hash to pass to the various --*-hash arguments of governance commands."

pGovernanceHashSource :: Parser Cmd.GovernanceHashSource
pGovernanceHashSource =
  asum
    [
      Cmd.GovernanceHashSourceText
        <$> Opt.strOption
              ( mconcat
                [ Opt.long "text"
                , Opt.metavar "TEXT"
                , Opt.help "Text to hash as UTF-8"
                ]
              )
    , Cmd.GovernanceHashSourceBinaryFile
        <$> pFileInDirection "file-binary" "Binary file to hash"
    , Cmd.GovernanceHashSourceTextFile
        <$> pFileInDirection "file-text" "Text file to hash"
    ]
