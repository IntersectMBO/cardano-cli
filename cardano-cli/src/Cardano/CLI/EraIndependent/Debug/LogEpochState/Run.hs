{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.EraIndependent.Debug.LogEpochState.Run
  ( runLogEpochStateCmd
  )
where

import Cardano.Api
import Cardano.Api qualified as Api

import Cardano.CLI.EraIndependent.Debug.LogEpochState.Command
import Cardano.CLI.Orphan ()

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import System.IO qualified as IO

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

    result <-
      runExceptT $
        foldEpochState
          configurationFile
          nodeSocketPath
          Api.QuickValidation
          (EpochNo maxBound)
          ()
          ( \(AnyNewEpochState sbe nes) _ _ -> do
              liftIO $
                LBS.appendFile outputFilePath $
                  shelleyBasedEraConstraints sbe (Aeson.encode nes) <> "\n"
              pure ConditionNotMet
          )

    case result of
      Right _ -> pure ()
      Left e -> IO.hPutStrLn IO.stderr $ "Error: " <> show e
