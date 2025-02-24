{-# LANGUAGE DataKinds #-}

module Cardano.CLI.EraIndependent.Debug.TransactionView.Command where

import Cardano.CLI.Type.Common

data TransactionViewCmdArgs = TransactionViewCmdArgs
  { outputFormat :: !ViewOutputFormat
  , mOutFile :: !(Maybe (File () Out))
  , inputTxBodyOrTxFile :: !InputTxBodyOrTxFile
  }
  deriving Show
