{-# LANGUAGE DataKinds #-}

module Cardano.CLI.Compatible.Governance.Option
  ( pCompatibleGovernanceCmds
  )
where

import Cardano.Api

import Cardano.CLI.Compatible.Governance.Command
import Cardano.CLI.EraBased.Governance.Option

import Data.Foldable
import Data.Maybe
import Options.Applicative

pCompatibleGovernanceCmds :: ShelleyBasedEra era -> Parser (CompatibleGovernanceCmds era)
pCompatibleGovernanceCmds sbe =
  asum $ catMaybes [fmap CreateCompatibleProtocolUpdateCmd <$> pGovernanceCmds sbe]
