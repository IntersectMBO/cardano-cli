{-# LANGUAGE DataKinds #-}

module Cardano.CLI.Commands.Debug.TransactionView where

import           Cardano.CLI.Types.Common

data TransactionViewCmdArgs = TransactionViewCmdArgs
  { outputFormat :: !ViewOutputFormat
  , mOutFile :: !(Maybe (File () Out))
  , inputTxBodyOrTxFile :: !InputTxBodyOrTxFile
  }
  deriving Show
