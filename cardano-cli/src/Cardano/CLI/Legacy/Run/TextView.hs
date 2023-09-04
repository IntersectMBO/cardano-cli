{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Legacy.Run.TextView
  ( runLegacyTextViewCmds
  ) where

import           Cardano.Api

import           Cardano.CLI.EraBased.Run.TextView
import           Cardano.CLI.Legacy.Commands.TextView
import           Cardano.CLI.Types.Errors.ShelleyTextViewFileError

import           Control.Monad.Trans.Except (ExceptT)

runLegacyTextViewCmds :: LegacyTextViewCmds -> ExceptT ShelleyTextViewFileError IO ()
runLegacyTextViewCmds = \case
  TextViewInfo fpath mOutfile -> runLegacyTextViewInfoCmd fpath mOutfile

runLegacyTextViewInfoCmd :: ()
  => FilePath
  -> Maybe (File () Out)
  -> ExceptT ShelleyTextViewFileError IO ()
runLegacyTextViewInfoCmd = runTextViewInfoCmd
