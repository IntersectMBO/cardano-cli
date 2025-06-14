{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.CLI.Util
  ( assertDirectoryMissing
  , checkTxCddlFormat
  , checkTextEnvelopeFormat
  , equivalence
  , execCardanoCLI
  , execCardanoCLIWithEnvVars
  , execDetailCardanoCLI
  , execDetailConfigCardanoCLI
  , tryExecCardanoCLI
  , propertyOnce
  , withSnd
  , noteInputFile
  , noteTempFile
  , redactJsonField
  )
where

import Cardano.Api

import Cardano.CLI.Read

import Control.Monad (when)
import Control.Monad.Catch hiding (bracket_)
import Control.Monad.Morph (hoist)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Data.Aeson qualified as Aeson
import Data.Aeson.Encode.Pretty qualified as Aeson
import Data.Aeson.Key qualified as Aeson
import Data.Aeson.KeyMap qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.Function ((&))
import Data.List qualified as List
import Data.Monoid (Last (..))
import Data.Text (Text)
import GHC.IO.Exception (ExitCode (..))
import GHC.Stack (CallStack, HasCallStack)
import GHC.Stack qualified as GHC
import System.Directory qualified as IO
import System.Environment qualified as IO
import System.Exit qualified as IO
import System.FilePath (isAbsolute, takeDirectory, (</>))
import System.IO.Unsafe qualified as IO
import System.Process (CreateProcess)
import System.Process qualified as IO

import Hedgehog qualified as H
import Hedgehog.Extras (ExecConfig)
import Hedgehog.Extras qualified as H
import Hedgehog.Extras.Test (ExecConfig (..))
import Hedgehog.Internal.Property (Diff, MonadTest, liftTest, mkTest)
import Hedgehog.Internal.Property qualified as H
import Hedgehog.Internal.Show (ValueDiff (ValueSame), mkValue, showPretty, valueDiff)
import Hedgehog.Internal.Source (getCaller)

-- | Execute cardano-cli via the command line.
--
-- Waits for the process to finish and returns the stdout.
execCardanoCLI
  :: (MonadTest m, MonadCatch m, MonadIO m, HasCallStack)
  => [String]
  -- ^ Arguments to the CLI command
  -> m String
  -- ^ Captured stdout
execCardanoCLI = GHC.withFrozenCallStack $ H.execFlex "cardano-cli" "CARDANO_CLI"

-- | Execute cardano-cli via the command line but set
-- environment variables. Fails if the process returns a non-zero exit code.
--
-- Waits for the process to finish and returns the stdout.
execCardanoCLIWithEnvVars
  :: (MonadTest m, MonadCatch m, MonadIO m, HasCallStack)
  => [(String, String)]
  -- ^ Environment variables to set
  -> [String]
  -- ^ Arguments to the CLI command
  -> m String
execCardanoCLIWithEnvVars envVars args = GHC.withFrozenCallStack $ do
  env <- H.evalIO IO.getEnvironment
  result <-
    execDetailConfigCardanoCLI
      H.defaultExecConfig
        { H.execConfigEnv = Last $ Just (envVars ++ env)
        }
      args
  case result of
    (ExitFailure _, _, stderr) -> do
      H.note_ stderr
      H.failure
    (ExitSuccess, stdout, _) -> return stdout

-- | Execute cardano-cli via the command line, expecting it to fail.
--
-- Waits for the process to finish and returns the exit code, stdout and stderr.
execDetailCardanoCLI
  :: (MonadTest m, MonadCatch m, MonadIO m, HasCallStack)
  => [String]
  -- ^ Arguments to the CLI command
  -> m (IO.ExitCode, String, String)
  -- ^ exit code, stdout, stderr
execDetailCardanoCLI params = GHC.withFrozenCallStack $ execDetailConfigCardanoCLI H.defaultExecConfig params

-- | Execute cardano-cli via the command line, expecting it to fail, and accepting custom config.
--
-- Waits for the process to finish and returns the exit code, stdout and stderr.
execDetailConfigCardanoCLI
  :: (MonadTest m, MonadCatch m, MonadIO m, HasCallStack)
  => ExecConfig
  -- ^ Configuration for the execution
  -> [String]
  -- ^ Arguments to the CLI command
  -> m (IO.ExitCode, String, String)
  -- ^ Exit code, stdout, stderr
execDetailConfigCardanoCLI cfg = GHC.withFrozenCallStack $ execDetailFlex cfg "cardano-cli" "CARDANO_CLI"

procFlex'
  :: (MonadTest m, MonadCatch m, MonadIO m, HasCallStack)
  => ExecConfig
  -> String
  -- ^ Cabal package name corresponding to the executable
  -> String
  -- ^ Environment variable pointing to the binary to run
  -> [String]
  -- ^ Arguments to the CLI command
  -> m CreateProcess
  -- ^ Captured stdout
procFlex' execConfig pkg binaryEnv arguments = GHC.withFrozenCallStack . H.evalM $ do
  bin <- H.binFlex pkg binaryEnv
  return
    (IO.proc bin arguments)
      { IO.env = getLast $ execConfigEnv execConfig
      , IO.cwd = getLast $ execConfigCwd execConfig
      }

execDetailFlex
  :: (MonadTest m, MonadCatch m, MonadIO m, HasCallStack)
  => ExecConfig
  -> String
  -> String
  -> [String]
  -> m (IO.ExitCode, String, String)
-- ^ exit code, stdout, stderr
execDetailFlex execConfig pkgBin envBin arguments = GHC.withFrozenCallStack $ do
  cp <- procFlex' execConfig pkgBin envBin arguments
  H.annotate . ("Command: " <>) $ case IO.cmdspec cp of
    IO.ShellCommand cmd -> cmd
    IO.RawCommand cmd args -> cmd <> " " <> List.unwords args
  H.evalIO $ IO.readCreateProcessWithExitCode cp ""

tryExecCardanoCLI
  :: (MonadCatch m, MonadIO m, HasCallStack)
  => [String]
  -- ^ Arguments to the CLI command
  -> H.PropertyT m (Either H.Failure String)
  -- ^ Captured stdout, or error in case of failures
tryExecCardanoCLI args =
  GHC.withFrozenCallStack (H.execFlex "cardano-cli" "CARDANO_CLI") args
    & H.unPropertyT
    & H.unTest
    & runExceptT
    & lift
    & H.TestT
    & H.PropertyT

-- | Checks that the 'tvType' and 'tvDescription' are equivalent between two files.
checkTextEnvelopeFormat
  :: (MonadTest m, MonadIO m, HasCallStack)
  => TextEnvelopeType
  -> FilePath
  -> FilePath
  -> m ()
checkTextEnvelopeFormat tve reference created = GHC.withFrozenCallStack $ do
  eRefTextEnvelope <- H.evalIO $ readTextEnvelopeOfTypeFromFile tve reference
  refTextEnvelope <- handleTextEnvelope eRefTextEnvelope

  eCreatedTextEnvelope <- H.evalIO $ readTextEnvelopeOfTypeFromFile tve created
  createdTextEnvelope <- handleTextEnvelope eCreatedTextEnvelope

  typeTitleEquivalence refTextEnvelope createdTextEnvelope
 where
  handleTextEnvelope
    :: MonadTest m
    => Either (FileError TextEnvelopeError) TextEnvelope
    -> m TextEnvelope
  handleTextEnvelope = \case
    Right refTextEnvelope ->
      return refTextEnvelope
    Left fileErr ->
      failWithCustom GHC.callStack Nothing . (docToString . prettyError) $ fileErr

  typeTitleEquivalence :: (MonadTest m, HasCallStack) => TextEnvelope -> TextEnvelope -> m ()
  typeTitleEquivalence
    (TextEnvelope refType refTitle _)
    (TextEnvelope createdType createdTitle _) = GHC.withFrozenCallStack $ do
      equivalence refType createdType
      equivalence refTitle createdTitle

checkTxCddlFormat
  :: (MonadTest m, MonadIO m, HasCallStack)
  => FilePath
  -- ^ Reference/golden file
  -> FilePath
  -- ^ Newly created file
  -> m ()
checkTxCddlFormat referencePath createdPath = do
  fileExists <- liftIO $ IO.doesFileExist referencePath

  if fileExists
    then do
      reference <- H.evalIO $ fileOrPipe referencePath
      created <- H.evalIO $ fileOrPipe createdPath
      r <- H.evalIO $ readCddlTx reference
      c <- H.evalIO $ readCddlTx created
      r H.=== c
    else
      if createFiles
        then do
          -- CREATE_GOLDEN_FILES is set, so we create any golden files that don't
          -- already exist.
          H.note_ $ "Creating golden file " <> referencePath
          H.createDirectoryIfMissing_ (takeDirectory referencePath)
          H.readFile createdPath >>= H.writeFile referencePath
        else do
          H.note_ $
            mconcat
              [ "Golden file " <> referencePath
              , " does not exist.  To create, run with CREATE_GOLDEN_FILES=1"
              ]
          H.failure

-- | Whether the test should create the golden files if the file does ont exist.
createFiles :: Bool
createFiles = IO.unsafePerformIO $ do
  value <- IO.lookupEnv "CREATE_GOLDEN_FILES"
  return $ value == Just "1"

-- | Asserts that the given directory is missing.
assertDirectoryMissing :: (MonadTest m, MonadIO m, HasCallStack) => FilePath -> m ()
assertDirectoryMissing dir = GHC.withFrozenCallStack $ do
  exists <- H.evalIO $ IO.doesDirectoryExist dir
  when exists $ H.failWithCustom GHC.callStack Nothing (dir <> " should not have been created.")

--------------------------------------------------------------------------------
-- Helpers, Error rendering & Clean up
--------------------------------------------------------------------------------

cardanoCliPath :: FilePath
cardanoCliPath = "cardano-cli"

-- | Return the input file path after annotating it relative to the project root directory
noteInputFile :: (MonadTest m, HasCallStack) => FilePath -> m FilePath
noteInputFile filePath = GHC.withFrozenCallStack $ do
  if isAbsolute filePath
    then H.annotate filePath
    else H.annotate $ cardanoCliPath </> filePath
  return filePath

-- | Return the test file path after annotating it relative to the project root directory
noteTempFile :: (MonadTest m, HasCallStack) => FilePath -> FilePath -> m FilePath
noteTempFile tempDir filePath = GHC.withFrozenCallStack $ do
  if isAbsolute filePath
    then H.note filePath
    else do
      let tempWithFilePath = tempDir </> filePath
      if isAbsolute tempWithFilePath
        then H.note tempWithFilePath
        else do
          H.annotate $ cardanoCliPath </> tempWithFilePath
          return tempWithFilePath

-- | Return the supply value with the result of the supplied function as a tuple
withSnd :: (a -> b) -> a -> (a, b)
withSnd f a = (a, f a)

-- These were lifted from hedgehog and slightly modified

propertyOnce :: H.PropertyT (ResourceT IO) () -> H.Property
propertyOnce = H.withTests 1 . H.withShrinks 0 . H.property . hoist runResourceT

-- | Check for equivalence between two types and perform a file cleanup on failure.
equivalence
  :: (MonadTest m, Eq a, Show a, HasCallStack)
  => a
  -> a
  -> m ()
equivalence x y = do
  ok <- H.eval (x == y)
  if ok
    then H.success
    else failDiffCustom GHC.callStack x y

-- | Takes a 'CallStack' so the error can be rendered at the appropriate call site.
failWithCustom :: MonadTest m => CallStack -> Maybe Diff -> String -> m a
failWithCustom cs mdiff msg =
  liftTest $ mkTest (Left $ H.Failure (getCaller cs) msg mdiff, mempty)

-- | Fails with an error that shows the difference between two values.
failDiffCustom :: (MonadTest m, Show a) => CallStack -> a -> a -> m ()
failDiffCustom cS x y =
  case valueDiff <$> mkValue x <*> mkValue y of
    Nothing ->
      GHC.withFrozenCallStack $
        failWithCustom cS Nothing $
          Prelude.unlines
            [ "Failed"
            , "━━ lhs ━━"
            , showPretty x
            , "━━ rhs ━━"
            , showPretty y
            ]
    Just vdiff@(ValueSame _) ->
      GHC.withFrozenCallStack $
        failWithCustom
          cS
          ( Just $
              H.Diff "━━━ Failed (" "" "no differences" "" ") ━━━" vdiff
          )
          ""
    Just vdiff ->
      GHC.withFrozenCallStack $
        failWithCustom
          cS
          ( Just $
              H.Diff "━━━ Failed (" "- lhs" ") (" "+ rhs" ") ━━━" vdiff
          )
          ""

redactJsonField
  :: ()
  => MonadTest m
  => MonadIO m
  => HasCallStack
  => Text
  -> Text
  -> FilePath
  -> FilePath
  -> m ()
redactJsonField fieldName replacement sourceFilePath targetFilePath = GHC.withFrozenCallStack $ do
  contents <- H.evalIO $ LBS.readFile sourceFilePath
  case Aeson.eitherDecode contents :: Either String Aeson.Value of
    Left err -> failWithCustom GHC.callStack Nothing err
    Right json -> do
      redactedJson <- case json of
        Aeson.Object obj ->
          pure $ Aeson.Object $ flip Aeson.mapWithKey obj $ \k v ->
            if k == Aeson.fromText fieldName
              then Aeson.String replacement
              else v
        v -> pure v
      H.evalIO $ LBS.writeFile targetFilePath (Aeson.encodePretty redactedJson)
