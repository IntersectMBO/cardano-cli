{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Run.EraBased
  ( AnyEraCmdError(..)
  , runAnyEraCommand
  , runEraBasedCommand
  , runEraBasedGovernanceCmd

  , renderAnyEraCmdError
  ) where

import           Cardano.CLI.Commands.EraBased
import           Cardano.CLI.Options.EraBased.Governance
import           Cardano.CLI.Types.Era

import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Except.Extra (firstExceptT)
import           Data.Text (Text)

newtype AnyEraCmdError
  = AnyEraCmdGenericError ()

renderAnyEraCmdError :: AnyEraCmdError -> Text
renderAnyEraCmdError = \case
  AnyEraCmdGenericError () -> "Generic any era command error"

runAnyEraCommand :: AnyEraCommand -> ExceptT AnyEraCmdError IO ()
runAnyEraCommand = \case
  AnyEraCommandOf _ cmd -> firstExceptT AnyEraCmdGenericError $ runEraBasedCommand cmd

runEraBasedCommand :: EraBasedCommand era -> ExceptT () IO ()
runEraBasedCommand = \case
  EraBasedGovernanceCmd cmd -> runEraBasedGovernanceCmd cmd

runEraBasedGovernanceCmd :: EraBasedGovernanceCmd era -> ExceptT () IO ()
runEraBasedGovernanceCmd = \case
  EraBasedGovernancePreConwayCmd w ->
    runEraBasedGovernancePreConwayCmd w
  EraBasedGovernancePostConwayCmd w ->
    runEraBasedGovernancePostConwayCmd w

runEraBasedGovernancePreConwayCmd
  :: ShelleyToBabbageEra era
  -> ExceptT () IO ()
runEraBasedGovernancePreConwayCmd _w = pure ()

runEraBasedGovernancePostConwayCmd
  :: ConwayEraOnwards era
  -> ExceptT () IO ()
runEraBasedGovernancePostConwayCmd _w = pure ()
