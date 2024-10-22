{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.Commands.TopLevelCommands
  ( AnyEraCommand (..)
  , Cmds (..)
  , renderAnyEraCommand
  , renderCmds
  )
where

import           Cardano.Api (ShelleyBasedEra (..))

import           Cardano.CLI.Commands.Address
import           Cardano.CLI.Commands.Key
import           Cardano.CLI.Commands.Node
import           Cardano.CLI.EraBased.Commands.Genesis
import           Cardano.CLI.EraBased.Commands.Query
import           Cardano.CLI.EraBased.Commands.StakeAddress
import           Cardano.CLI.EraBased.Commands.StakePool
import           Cardano.CLI.EraBased.Commands.TextView
import           Cardano.CLI.EraBased.Commands.Transaction
import           Cardano.CLI.EraBased.Options.Governance (GovernanceCmds, renderGovernanceCmds)

import           Data.Text (Text)
import           Data.Typeable (Typeable)

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
