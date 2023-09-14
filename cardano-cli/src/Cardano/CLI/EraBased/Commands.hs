{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.Commands
  ( AnyEraCommand (..)
  , Cmds (..)
  , renderAnyEraCommand
  , renderCmds
  , pAnyEraCommand
  , pCmds
  ) where

import           Cardano.Api (CardanoEra (..), ShelleyBasedEra (..))

import           Cardano.CLI.Environment
import           Cardano.CLI.EraBased.Commands.Address
import           Cardano.CLI.EraBased.Commands.Genesis
import           Cardano.CLI.EraBased.Commands.Key
import           Cardano.CLI.EraBased.Commands.Node
import           Cardano.CLI.EraBased.Commands.Query
import           Cardano.CLI.EraBased.Commands.StakeAddress
import           Cardano.CLI.EraBased.Commands.StakePool
import           Cardano.CLI.EraBased.Commands.TextView
import           Cardano.CLI.EraBased.Commands.Transaction
import           Cardano.CLI.EraBased.Options.Address
import           Cardano.CLI.EraBased.Options.Common
import           Cardano.CLI.EraBased.Options.Genesis
import           Cardano.CLI.EraBased.Options.Governance
import           Cardano.CLI.EraBased.Options.Key
import           Cardano.CLI.EraBased.Options.Node
import           Cardano.CLI.EraBased.Options.Query
import           Cardano.CLI.EraBased.Options.StakeAddress
import           Cardano.CLI.EraBased.Options.StakePool
import           Cardano.CLI.EraBased.Options.TextView
import           Cardano.CLI.EraBased.Options.Transaction

import           Data.Foldable
import           Data.Maybe
import           Data.Text (Text)
import           Options.Applicative (Parser)
import qualified Options.Applicative as Opt

data AnyEraCommand where
  AnyEraCommandOf :: ShelleyBasedEra era -> Cmds era -> AnyEraCommand

renderAnyEraCommand :: AnyEraCommand -> Text
renderAnyEraCommand = \case
  AnyEraCommandOf _ cmd -> renderCmds cmd

data Cmds era
  = AddressCmds         (AddressCmds era)
  | KeyCmds             (KeyCmds era)
  | GenesisCmds         (GenesisCmds era)
  | GovernanceCmds      (GovernanceCmds era)
  | NodeCmds            (NodeCmds era)
  | QueryCmds           (QueryCmds era)
  | StakeAddressCmds    (StakeAddressCmds era)
  | StakePoolCmds       (StakePoolCmds era)
  | TextViewCmds        (TextViewCmds era)
  | TransactionCmds     (TransactionCmds era)

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

pAnyEraCommand :: EnvCli -> Parser AnyEraCommand
pAnyEraCommand envCli =
  asum
    [ -- Note, byron is ommitted because there is already a legacy command group for it.

      subParser "shelley"
        $ Opt.info (AnyEraCommandOf ShelleyBasedEraShelley <$> pCmds ShelleyEra envCli)
        $ Opt.progDesc "Shelley era commands"
    , subParser "allegra"
        $ Opt.info (AnyEraCommandOf ShelleyBasedEraAllegra <$> pCmds AllegraEra envCli)
        $ Opt.progDesc "Allegra era commands"
    , subParser "mary"
        $ Opt.info (AnyEraCommandOf ShelleyBasedEraMary <$> pCmds MaryEra envCli)
        $ Opt.progDesc "Mary era commands"
    , subParser "alonzo"
        $ Opt.info (AnyEraCommandOf ShelleyBasedEraAlonzo <$> pCmds AlonzoEra envCli)
        $ Opt.progDesc "Alonzo era commands"
    , subParser "babbage"
        $ Opt.info (AnyEraCommandOf ShelleyBasedEraBabbage <$> pCmds BabbageEra envCli)
        $ Opt.progDesc "Babbage era commands"
    , subParser "conway"
        $ Opt.info (AnyEraCommandOf ShelleyBasedEraConway <$> pCmds ConwayEra envCli)
        $ Opt.progDesc "Conway era commands"

    , subParser "latest"
        $ Opt.info (AnyEraCommandOf ShelleyBasedEraBabbage <$> pCmds BabbageEra envCli)
        $ Opt.progDesc "Latest era commands (Babbage)"
    ]

pCmds :: CardanoEra era -> EnvCli -> Parser (Cmds era)
pCmds era envCli =
  asum $ catMaybes
    [ fmap AddressCmds      <$> pAddressCmds era envCli
    , fmap KeyCmds          <$> pKeyCmds
    , fmap GenesisCmds      <$> pGenesisCmds envCli
    , fmap GovernanceCmds   <$> pGovernanceCmds era envCli
    , fmap NodeCmds         <$> pNodeCmds
    , fmap QueryCmds        <$> pQueryCmds envCli
    , fmap StakeAddressCmds <$> pStakeAddressCmds era envCli
    , fmap StakePoolCmds    <$> pStakePoolCmds era envCli
    , fmap TextViewCmds     <$> pTextViewCmds
    , fmap TransactionCmds  <$> pTransactionCmds era envCli
    ]
