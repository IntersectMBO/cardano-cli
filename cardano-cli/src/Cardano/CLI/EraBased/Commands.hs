{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.Commands
  ( AnyEraCommand (..)
  , Cmds (..)
  , renderAnyEraCommand
  , renderCmds
  , pAnyEraCommand
  , pCmds
  )
where

import           Cardano.Api (ShelleyBasedEra (..), toCardanoEra)

import           Cardano.CLI.Commands.Key
import           Cardano.CLI.Commands.Node
import           Cardano.CLI.Environment
import           Cardano.CLI.EraBased.Commands.Address
import           Cardano.CLI.EraBased.Commands.Genesis
import           Cardano.CLI.EraBased.Commands.Query
import           Cardano.CLI.EraBased.Commands.StakeAddress
import           Cardano.CLI.EraBased.Commands.StakePool hiding (sbe)
import           Cardano.CLI.EraBased.Commands.TextView
import           Cardano.CLI.EraBased.Commands.Transaction
import           Cardano.CLI.EraBased.Options.Address
import           Cardano.CLI.EraBased.Options.Common
import           Cardano.CLI.EraBased.Options.Genesis
import           Cardano.CLI.EraBased.Options.Governance
import           Cardano.CLI.EraBased.Options.Query
import           Cardano.CLI.EraBased.Options.StakeAddress
import           Cardano.CLI.EraBased.Options.StakePool
import           Cardano.CLI.EraBased.Options.TextView
import           Cardano.CLI.EraBased.Options.Transaction
import           Cardano.CLI.Options.Key
import           Cardano.CLI.Options.Node

import           Data.Foldable
import           Data.Maybe
import           Data.Text (Text)
import           Data.Typeable (Typeable)
import           Options.Applicative (Parser)
import qualified Options.Applicative as Opt

data AnyEraCommand where
  AnyEraCommandOf :: Typeable era => ShelleyBasedEra era -> Cmds era -> AnyEraCommand

renderAnyEraCommand :: AnyEraCommand -> Text
renderAnyEraCommand = \case
  AnyEraCommandOf _ cmd -> renderCmds cmd

data Cmds era
  = AddressCmds (AddressCmds era)
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

pAnyEraCommand :: EnvCli -> Parser AnyEraCommand
pAnyEraCommand envCli =
  asum
    [ -- Note, byron is ommitted because there is already a legacy command group for it.

      subParser "shelley" $
        Opt.info (AnyEraCommandOf ShelleyBasedEraShelley <$> pCmds ShelleyBasedEraShelley envCli) $
          Opt.progDesc ("Shelley era commands" <> deprecationText)
    , subParser "allegra" $
        Opt.info (AnyEraCommandOf ShelleyBasedEraAllegra <$> pCmds ShelleyBasedEraAllegra envCli) $
          Opt.progDesc ("Allegra era commands" <> deprecationText)
    , subParser "mary" $
        Opt.info (AnyEraCommandOf ShelleyBasedEraMary <$> pCmds ShelleyBasedEraMary envCli) $
          Opt.progDesc ("Mary era commands" <> deprecationText)
    , subParser "alonzo" $
        Opt.info (AnyEraCommandOf ShelleyBasedEraAlonzo <$> pCmds ShelleyBasedEraAlonzo envCli) $
          Opt.progDesc ("Alonzo era commands" <> deprecationText)
    , subParser "babbage" $
        Opt.info (AnyEraCommandOf ShelleyBasedEraBabbage <$> pCmds ShelleyBasedEraBabbage envCli) $
          Opt.progDesc ("Babbage era commands" <> deprecationText)
    , subParser "conway" $
        Opt.info (AnyEraCommandOf ShelleyBasedEraConway <$> pCmds ShelleyBasedEraConway envCli) $
          Opt.progDesc "Conway era commands"
    , subParser "latest" $
        Opt.info (AnyEraCommandOf ShelleyBasedEraConway <$> pCmds ShelleyBasedEraConway envCli) $
          Opt.progDesc "Latest era commands (Conway)"
    ]

pCmds :: ShelleyBasedEra era -> EnvCli -> Parser (Cmds era)
pCmds sbe' envCli = do
  let cEra = toCardanoEra sbe'
  asum $
    catMaybes
      [ fmap AddressCmds <$> pAddressCmds cEra envCli
      , Just (KeyCmds <$> pKeyCmds)
      , fmap GenesisCmds <$> pGenesisCmds cEra envCli
      , fmap GovernanceCmds <$> pGovernanceCmds cEra
      , Just (NodeCmds <$> pNodeCmds)
      , fmap QueryCmds <$> pQueryCmds cEra envCli
      , fmap StakeAddressCmds <$> pStakeAddressCmds cEra envCli
      , fmap StakePoolCmds <$> pStakePoolCmds cEra envCli
      , fmap TextViewCmds <$> pTextViewCmds
      , fmap TransactionCmds <$> pTransactionCmds sbe' envCli
      ]
