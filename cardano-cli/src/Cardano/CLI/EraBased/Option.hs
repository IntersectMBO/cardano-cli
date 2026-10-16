{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.CLI.EraBased.Option
  ( pCmds
  , pAnyEraCommand
  , MainnetEra
  )
where

import Cardano.Api.Experimental
import Cardano.Api.Pretty (prettyShow)

import Cardano.CLI.Environment
import Cardano.CLI.EraBased.Command
import Cardano.CLI.EraBased.Genesis.Option
import Cardano.CLI.EraBased.Governance.Option (pGovernanceCmds)
import Cardano.CLI.EraBased.Query.Option
import Cardano.CLI.EraBased.StakeAddress.Option
import Cardano.CLI.EraBased.StakePool.Option
import Cardano.CLI.EraBased.TextView.Option
import Cardano.CLI.EraBased.Transaction.Option
import Cardano.CLI.EraBased.Transaction.SubTransaction.Option (pSubTransactionCmds)
import Cardano.CLI.EraIndependent.Address.Option
import Cardano.CLI.EraIndependent.Key.Option
import Cardano.CLI.EraIndependent.Node.Option
import Cardano.CLI.Parser

import Data.Foldable
import Data.Maybe
import Options.Applicative (Parser)
import Options.Applicative qualified as Opt

-- | The era running on mainnet, used by the latest, top-level node and top-level query commands.
type MainnetEra = ConwayEra

pCmds :: forall era. IsEra era => EnvCli -> Parser (Cmds era)
pCmds envCli = do
  asum $
    catMaybes
      [ Just (AddressCmds <$> pAddressCmds envCli)
      , Just (KeyCmds <$> pKeyCmds)
      , fmap GenesisCmds <$> pGenesisCmds envCli
      , fmap GovernanceCmds <$> pGovernanceCmds
      , Just (NodeCmds <$> pNodeCmds @era)
      , fmap QueryCmds <$> pQueryCmds envCli
      , fmap StakeAddressCmds <$> pStakeAddressCmds envCli
      , fmap StakePoolCmds <$> pStakePoolCmds envCli
      , fmap SubTransactionCmds <$> pSubTransactionCmds @era
      , fmap TextViewCmds <$> pTextViewCmds
      , fmap TransactionCmds <$> pTransactionCmds envCli
      ]

pAnyEraCommand :: EnvCli -> Parser AnyEraCommand
pAnyEraCommand envCli =
  asum
    [ Opt.hsubparser $
        commandWithMetavar "conway" $
          Opt.info (AnyEraCommandOf ConwayEra <$> pCmds @ConwayEra envCli) $
            Opt.progDesc "Conway era commands"
    , Opt.hsubparser $
        commandWithMetavar "dijkstra" $
          Opt.info (AnyEraCommandOf DijkstraEra <$> pCmds @DijkstraEra envCli) $
            Opt.progDesc "Dijkstra era commands"
    , Opt.hsubparser $
        commandWithMetavar "latest" $
          Opt.info (AnyEraCommandOf (useEra @MainnetEra) <$> pCmds @MainnetEra envCli) $
            Opt.progDesc $
              "Latest era commands (" <> prettyShow (useEra @MainnetEra) <> ")"
    ]
