{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Cardano.CLI.Commands.Debug.Transaction.Echo
  ( TransactionEchoCmdArgs (..)
  )
where

import           Cardano.Api.Shelley

import           Cardano.CLI.Types.Common

data TransactionEchoCmdArgs = TransactionEchoCmdArgs
  { txOrTxBodyFile :: !InputTxBodyOrTxFile
  , outTxFile :: !(TxFile Out)
  }
  deriving Show
