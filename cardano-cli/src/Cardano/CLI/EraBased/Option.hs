{-# LANGUAGE RankNTypes #-}

module Cardano.CLI.EraBased.Option
  ( pCmds
  , pAnyEraCommand
  )
where

import Cardano.Api (ShelleyBasedEra (..))

import Cardano.CLI.Environment
import Cardano.CLI.EraBased.Command
import Cardano.CLI.EraBased.Common.Option
import Cardano.CLI.EraBased.Genesis.Option
import Cardano.CLI.EraBased.Governance.Option (pGovernanceCmds)
import Cardano.CLI.EraBased.Query.Option
import Cardano.CLI.EraBased.StakeAddress.Option
import Cardano.CLI.EraBased.StakePool.Option
import Cardano.CLI.EraBased.TextView.Option
import Cardano.CLI.EraBased.Transaction.Option
import Cardano.CLI.EraIndependent.Address.Option
import Cardano.CLI.EraIndependent.Key.Option
import Cardano.CLI.EraIndependent.Node.Option
import Cardano.CLI.Parser

import Data.Foldable
import Data.Maybe
import Options.Applicative (Mod, Parser)
import Options.Applicative qualified as Opt

pCmds :: ShelleyBasedEra era -> EnvCli -> (forall a b. Mod a b) -> Parser (Cmds era)
pCmds era envCli mods = do
  asum $
    catMaybes
      [ Just (AddressCmds <$> pAddressCmds envCli mods)
      , Just (KeyCmds <$> pKeyCmds mods)
      , fmap GenesisCmds <$> pGenesisCmds era envCli
      , fmap GovernanceCmds <$> pGovernanceCmds era
      , Just (NodeCmds <$> pNodeCmds mods)
      , fmap QueryCmds <$> pQueryCmds era envCli
      , fmap StakeAddressCmds <$> pStakeAddressCmds era envCli
      , fmap StakePoolCmds <$> pStakePoolCmds era envCli
      , fmap TextViewCmds <$> pTextViewCmds
      , fmap TransactionCmds <$> pTransactionCmds era envCli
      ]

pAnyEraCommand :: EnvCli -> Parser AnyEraCommand
pAnyEraCommand envCli =
  asum
    [ -- Note, byron is ommitted because there is already a legacy command group for it.

      Opt.hsubparser $
        mconcat
          [ commandWithMetavar "shelley" $
              Opt.info (AnyEraCommandOf ShelleyBasedEraShelley <$> pCmds ShelleyBasedEraShelley envCli mempty) $
                Opt.progDesc ("Shelley era commands" <> deprecationText)
          , Opt.internal
          ]
    , Opt.hsubparser $
        mconcat
          [ commandWithMetavar "allegra" $
              Opt.info (AnyEraCommandOf ShelleyBasedEraAllegra <$> pCmds ShelleyBasedEraAllegra envCli mempty) $
                Opt.progDesc ("Allegra era commands" <> deprecationText)
          , Opt.internal
          ]
    , Opt.hsubparser $
        mconcat
          [ commandWithMetavar "mary" $
              Opt.info (AnyEraCommandOf ShelleyBasedEraMary <$> pCmds ShelleyBasedEraMary envCli mempty) $
                Opt.progDesc ("Mary era commands" <> deprecationText)
          , Opt.internal
          ]
    , Opt.hsubparser $
        mconcat
          [ commandWithMetavar "alonzo" $
              Opt.info (AnyEraCommandOf ShelleyBasedEraAlonzo <$> pCmds ShelleyBasedEraAlonzo envCli mempty) $
                Opt.progDesc ("Alonzo era commands" <> deprecationText)
          , Opt.internal
          ]
    , Opt.hsubparser $
        mconcat
          [ commandWithMetavar "babbage" $
              Opt.info (AnyEraCommandOf ShelleyBasedEraBabbage <$> pCmds ShelleyBasedEraBabbage envCli mempty) $
                Opt.progDesc ("Babbage era commands" <> deprecationText)
          , Opt.internal
          ]
    , Opt.hsubparser $
        commandWithMetavar "conway" $
          Opt.info (AnyEraCommandOf ShelleyBasedEraConway <$> pCmds ShelleyBasedEraConway envCli mempty) $
            Opt.progDesc "Conway era commands"
    , Opt.hsubparser $
        commandWithMetavar "latest" $
          Opt.info (AnyEraCommandOf ShelleyBasedEraConway <$> pCmds ShelleyBasedEraConway envCli mempty) $
            Opt.progDesc "Latest era commands (Conway)"
    ]
