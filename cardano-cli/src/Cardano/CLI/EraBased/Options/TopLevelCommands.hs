module Cardano.CLI.EraBased.Options.TopLevelCommands
  ( pCmds
  )
where

import Cardano.Api (ShelleyBasedEra (..))

import Cardano.CLI.Environment
import Cardano.CLI.EraBased.Commands.TopLevelCommands
import Cardano.CLI.EraBased.Options.Genesis
import Cardano.CLI.EraBased.Options.Governance (pGovernanceCmds)
import Cardano.CLI.EraBased.Options.Query
import Cardano.CLI.EraBased.Options.StakeAddress
import Cardano.CLI.EraBased.Options.StakePool
import Cardano.CLI.EraBased.Options.TextView
import Cardano.CLI.EraBased.Options.Transaction
import Cardano.CLI.Options.Address
import Cardano.CLI.Options.Key
import Cardano.CLI.Options.Node

import Data.Foldable
import Data.Maybe
import Options.Applicative (Parser)

pCmds :: ShelleyBasedEra era -> EnvCli -> Parser (Cmds era)
pCmds era envCli = do
  asum $
    catMaybes
      [ Just (AddressCmds <$> pAddressCmds envCli)
      , Just (KeyCmds <$> pKeyCmds)
      , fmap GenesisCmds <$> pGenesisCmds era envCli
      , fmap GovernanceCmds <$> pGovernanceCmds era
      , Just (NodeCmds <$> pNodeCmds)
      , fmap QueryCmds <$> pQueryCmds era envCli
      , fmap StakeAddressCmds <$> pStakeAddressCmds era envCli
      , fmap StakePoolCmds <$> pStakePoolCmds era envCli
      , fmap TextViewCmds <$> pTextViewCmds
      , fmap TransactionCmds <$> pTransactionCmds era envCli
      ]
