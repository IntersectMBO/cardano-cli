{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.Command
  ( AnyEraCommand (..)
  , Cmds (..)
  , renderAnyEraCommand
  , renderCmds
  )
where

import Cardano.Api (ShelleyBasedEra (..))

import Cardano.CLI.EraBased.Genesis.Command
import Cardano.CLI.EraBased.Governance.Option (GovernanceCmds, renderGovernanceCmds)
import Cardano.CLI.EraBased.Query.Command
import Cardano.CLI.EraBased.StakeAddress.Command
import Cardano.CLI.EraBased.StakePool.Command
import Cardano.CLI.EraBased.TextView.Command
import Cardano.CLI.EraBased.Transaction.Command
import Cardano.CLI.EraIndependent.Address.Command
import Cardano.CLI.EraIndependent.Key.Command
import Cardano.CLI.EraIndependent.Node.Command

import Data.Text (Text)
import Data.Typeable (Typeable)

data AnyEraCommand where
  AnyEraCommandOf :: Typeable era => ShelleyBasedEra era -> Cmds era -> AnyEraCommand

renderAnyEraCommand :: AnyEraCommand -> Text
renderAnyEraCommand = \case
  AnyEraCommandOf _ cmd -> renderCmds cmd

data Cmds era
  = AddressCmds AddressCmds
  | KeyCmds KeyCmds
  | GenesisCmds (GenesisCmds era)
  | GovernanceCmds (GovernanceCmds era)
  | NodeCmds NodeCmds
  | QueryCmds (QueryCmds era)
  | StakeAddressCmds (StakeAddressCmds era)
  | StakePoolCmds (StakePoolCmds era)
  | TextViewCmds (TextViewCmds era)
  | TransactionCmds (TransactionCmds era)

renderCmds :: Cmds era -> Text
renderCmds = \case
  AddressCmds cmd ->
    renderAddressCmds cmd
  KeyCmds cmd ->
    renderKeyCmds cmd
  GenesisCmds cmd ->
    renderGenesisCmds cmd
  GovernanceCmds cmd ->
    renderGovernanceCmds cmd
  NodeCmds cmd ->
    renderNodeCmds cmd
  QueryCmds cmd ->
    renderQueryCmds cmd
  StakeAddressCmds cmd ->
    renderStakeAddressCmds cmd
  StakePoolCmds cmd ->
    renderStakePoolCmds cmd
  TextViewCmds cmd ->
    renderTextViewCmds cmd
  TransactionCmds cmd ->
    renderTransactionCmds cmd
