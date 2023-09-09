{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Legacy.Run.TextView
  ( runLegacyTextViewCmds
  ) where

import           Cardano.Api

import           Cardano.CLI.EraBased.Run.TextView
import           Cardano.CLI.Legacy.Commands.TextView
import           Cardano.CLI.Types.Errors.TextViewFileError

import           Control.Monad.Trans.Except (ExceptT)

runLegacyTextViewCmds :: LegacyTextViewCmds -> ExceptT TextViewFileError IO ()
runLegacyTextViewCmds = \case
  TextViewInfo fpath mOutfile -> runLegacyTextViewInfoCmd fpath mOutfile

runLegacyTextViewInfoCmd :: ()
  => FilePath
  -> Maybe (File () Out)
  -> ExceptT TextViewFileError IO ()
runLegacyTextViewInfoCmd = runTextViewInfoCmd
