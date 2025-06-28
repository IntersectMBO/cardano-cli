-- | Tests of the command "key verification-key", in particular tests of
-- https://github.com/IntersectMBO/cardano-cli/issues/651
module Test.Cli.VerificationKey where

import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import GHC.Stack (HasCallStack)
import GHC.Stack qualified as GHC

import Test.Cardano.CLI.Aeson (assertEqualModuloDesc)
import Test.Cardano.CLI.Util
import Test.Cardano.CLI.Workspace

import Hedgehog (MonadTest, Property)
import Hedgehog.Extras.Test.Base qualified as H

-- | Execute me with:
-- @cabal test cardano-cli-test --test-options '-p "/verification key drep/"'@
hprop_verification_key_drep :: Property
hprop_verification_key_drep =
  watchdogProp . propertyOnce . moduleWorkspace2 "tmp" $ runOne ["drep", "key-gen"]

-- | Execute me with:
-- @cabal test cardano-cli-test --test-options '-p "/verification key committee hot/"'@
hprop_verification_key_committee_hot :: Property
hprop_verification_key_committee_hot =
  watchdogProp . propertyOnce . moduleWorkspace2 "tmp" $ runOne ["committee", "key-gen-hot"]

-- | Execute me with:
-- @cabal test cardano-cli-test --test-options '-p "/verification key committee cold/"'@
hprop_verification_key_committee_cold :: Property
hprop_verification_key_committee_cold =
  watchdogProp . propertyOnce . moduleWorkspace2 "tmp" $ runOne ["committee", "key-gen-cold"]

runOne
  :: ()
  => (HasCallStack, MonadCatch m, MonadIO m, MonadTest m)
  => [String]
  -- ^ The piece of the command to generate the keys
  -> FilePath
  -- ^ The temporary directory, i.e. where the test is allowed to generate files
  -> m ()
runOne cmd tempDir = GHC.withFrozenCallStack $ do
  verificationKeyFile <- noteTempFile tempDir "gen.vkey"
  signingKeyFile <- noteTempFile tempDir "gen.skey"
  verificationKeyFileOut <- noteTempFile tempDir "vkey.out"

  H.noteM_ $
    execCardanoCLI $
      ["conway", "governance"]
        ++ cmd
        ++ [ "--verification-key-file"
           , verificationKeyFile
           , "--signing-key-file"
           , signingKeyFile
           ]

  H.noteM_ $
    execCardanoCLI
      [ "conway"
      , "key"
      , "verification-key"
      , "--signing-key-file"
      , signingKeyFile
      , "--verification-key-file"
      , verificationKeyFileOut
      ]

  assertEqualModuloDesc verificationKeyFile verificationKeyFileOut
