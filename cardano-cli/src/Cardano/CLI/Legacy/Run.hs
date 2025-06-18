{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.CLI.Legacy.Run
  ( runLegacyCmds
  )
where

import Cardano.CLI.Compatible.Exception
import Cardano.CLI.Legacy.Genesis.Run
import Cardano.CLI.Legacy.Option

runLegacyCmds :: LegacyCmds -> CIO e ()
runLegacyCmds = \case
  LegacyGenesisCmds cmd -> runLegacyGenesisCmds cmd
