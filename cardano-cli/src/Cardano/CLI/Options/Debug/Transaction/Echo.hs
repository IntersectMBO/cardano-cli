{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.Options.Debug.Transaction.Echo
  ( pDebugTransactionEcho
  )
where

import           Cardano.CLI.Commands.Debug.Transaction
import           Cardano.CLI.Commands.Debug.Transaction.Echo
import           Cardano.CLI.EraBased.Options.Common

import           Options.Applicative hiding (help, str)

pDebugTransactionEcho :: Parser DebugTransactionCmds
pDebugTransactionEcho =
  fmap DebugTransactionEchoCmd $
    TransactionEchoCmdArgs
      <$> pInputTxOrTxBodyFile
      <*> pTxFileOut
