{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.CLI.Polysemy
  ( cardanoCliPath,
    execCardanoCli,
    execCardanoCli_,
    execDetailCardanoCli,
    localWorkspace,
  ) where

import           HaskellWorks.Polysemy
import           HaskellWorks.Polysemy.Error.Types
import           HaskellWorks.Polysemy.Hedgehog
import           HaskellWorks.Polysemy.Hedgehog.Process
import           HaskellWorks.Polysemy.Prelude
import           HaskellWorks.Polysemy.System.Process
import           Polysemy ()

cardanoCliPath :: FilePath
cardanoCliPath = "cardano-cli"

-- | Execute cardano-cli via the command line.
--
-- Waits for the process to finish and returns the stdout.
execCardanoCli :: ()
  => HasCallStack
  => Member (Embed IO) r
  => Member Hedgehog r
  => Member Log r
  => [String]
  -- ^ Arguments to the CLI command
  -> Sem r String
  -- ^ Captured stdout
execCardanoCli args = withFrozenCallStack $
  execFlexOk "cardano-cli" "CARDANO_CLI" args
    & trapFail @GenericError
    & trapFail @IOException

execCardanoCli_ :: ()
  => HasCallStack
  => Member (Embed IO) r
  => Member Hedgehog r
  => Member Log r
  => [String]
  -- ^ Arguments to the CLI command
  -> Sem r ()
execCardanoCli_ args = withFrozenCallStack $
  void $ execCardanoCli args

-- | Execute cardano-cli via the command line, expecting it to fail.
--
-- Waits for the process to finish and returns the exit code, stdout and stderr.
execDetailCardanoCli :: ()
  => HasCallStack
  => Member (Embed IO) r
  => Member Hedgehog r
  => Member Log r
  => [String]
  -- ^ Arguments to the CLI command
  -> Sem r (ExitCode, String, String)
  -- ^ Captured stdout
execDetailCardanoCli arguments = withFrozenCallStack $
  execDetailFlex defaultExecConfig "cardano-cli" "CARDANO_CLI" arguments
    & trapFail @GenericError
    & trapFail @IOException

localWorkspace ::  ()
  => Member Hedgehog r
  => Member Log r
  => Member (Embed IO) r
  => Sem
        ( Reader Workspace
        : Reader ProjectRoot
        : Reader PackagePath
        : Resource
        : r)
        ()
  -> Sem r ()
localWorkspace f = do
  cabalProjectDir <- findCabalProjectDir "."

  f & moduleWorkspace "cardano-cli"
    & runReader (ProjectRoot cabalProjectDir)
    & runReader (PackagePath "cardano-cli")
    & runResource
