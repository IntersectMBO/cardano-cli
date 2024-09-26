{-# LANGUAGE GADTs #-}
{-
This module is concerned with providing backwards compatible cli commands for our internal
testing needs. The intention is to restrict as much as possible which functionality we maintain backwards
compatibility for.
-}

module Cardano.CLI.Compatible.Commands
  ( 
  )
where

import Cardano.Api 
import Options.Applicative
import           Cardano.CLI.Environment

data AnyCompatibleCommand where
  AnyCompatibleCommand :: ShelleyBasedEra era -> CompatibleCommand era -> AnyCompatibleCommand

data CompatibleCommand era 
 = CompatibleSimpleSignedTransaction
 | CompatibleCreateProtocolUpdate


pAnyCompatibleCommand :: EnvCli -> Parser AnyCompatibleCommand
pAnyCompatibleCommand = undefined 


pCompatibleCommand :: ShelleyBasedEra era -> EnvCli -> Parser (CompatibleCommand era)
pCompatibleCommand era env = undefined