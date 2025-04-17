{-# LANGUAGE DataKinds #-}

module Cardano.CLI.EraIndependent.Debug.TransactionView.Command where

import Cardano.CLI.Type.Common

import Vary (Vary)

data TransactionViewCmdArgs = TransactionViewCmdArgs
  { outputFormat :: !(Vary [FormatJson, FormatYaml])
  , mOutFile :: !(Maybe (File () Out))
  , inputTxBodyOrTxFile :: !InputTxBodyOrTxFile
  }
  deriving Show
