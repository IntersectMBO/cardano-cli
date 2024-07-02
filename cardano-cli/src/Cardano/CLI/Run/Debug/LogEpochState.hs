{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.Run.Debug.LogEpochState
  ( runLogEpochStateCmd
  ) where

import           Cardano.Api
import qualified Cardano.Api as Api

import           Cardano.CLI.Commands.Debug.LogEpochState
import           Cardano.CLI.Orphans ()

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified System.IO as IO

runLogEpochStateCmd
  :: LogEpochStateCmdArgs
  -> IO ()
runLogEpochStateCmd
    LogEpochStateCmdArgs
      { nodeSocketPath
      , configurationFile
      , outputFilePath = File outputFilePath
      } = do
  LBS.appendFile outputFilePath ""

  result <- runExceptT $ foldEpochState
    AlonzoEraOnwardsConway -- FIXME: get this from somewhere, prolly query the node beforehand for an era
    configurationFile
    nodeSocketPath
    Api.QuickValidation
    (EpochNo maxBound)
    ()
    (\(AnyNewEpochState sbe nes) _ _ -> do
        liftIO $ LBS.appendFile outputFilePath
          $ shelleyBasedEraConstraints sbe (Aeson.encode nes) <> "\n"
        pure ConditionNotMet
    )

  case result of
    Right _ -> pure ()
    Left e -> IO.hPutStrLn IO.stderr $ "Error: " <> show e
