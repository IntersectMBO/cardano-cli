{-# LANGUAGE TypeApplications #-}

module Cardano.CLI.EraBased.Option
  ( pCmds
  , pAnyEraCommand
  )
where

import Cardano.Api (Convert (..))
import Cardano.Api.Experimental

import Cardano.CLI.Environment
import Cardano.CLI.EraBased.Command
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
import Options.Applicative (Parser)
import Options.Applicative qualified as Opt

pCmds :: IsEra era => EnvCli -> Parser (Cmds era)
pCmds envCli = do
  asum $
    catMaybes
      [ Just (AddressCmds <$> pAddressCmds envCli)
      , Just (KeyCmds <$> pKeyCmds)
      , fmap GenesisCmds <$> pGenesisCmds (convert useEra) envCli
      , fmap GovernanceCmds <$> pGovernanceCmds
      , Just (NodeCmds <$> pNodeCmds)
      , fmap QueryCmds <$> pQueryCmds envCli
      , fmap StakeAddressCmds <$> pStakeAddressCmds envCli
      , fmap StakePoolCmds <$> pStakePoolCmds (convert useEra) envCli
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
        commandWithMetavar "latest" $
          Opt.info (AnyEraCommandOf ConwayEra <$> pCmds @ConwayEra envCli) $
            Opt.progDesc "Latest era commands (Conway)"
    ]
