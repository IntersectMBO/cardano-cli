{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- HLINT ignore "Redundant id" -}

module Test.Golden.Help
  ( hprop_golden_HelpAll
  , test_golden_HelpCmds
  )
where

import Prelude hiding (lines)

import Control.Monad
import Data.Char qualified as Char
import Data.List (nub)
import Data.List qualified as List
import Data.Maybe (maybeToList)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Exts (IsList (..))
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process.Extra (readProcess)
import Text.Regex (Regex, mkRegex, subRegex)

import Test.Cardano.CLI.Util (execCardanoCLI, propertyOnce)
import Test.Cardano.CLI.Util qualified as H

import Hedgehog (Property)
import Hedgehog qualified as H
import Hedgehog.Extras.Stock.OS (isWin32)
import Hedgehog.Extras.Test qualified as H
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

ansiRegex :: Regex
ansiRegex = mkRegex "\\[[0-9]+m"

filterAnsi :: String -> String
filterAnsi line = subRegex ansiRegex stripped ""
 where
  stripped = filter (/= '\ESC') line

{- HLINT ignore "Use camelCase" -}

extractCmd :: Text -> [Text]
extractCmd =
  id
    . takeWhile nonSwitch
    . Text.split Char.isSpace
    . Text.strip
 where
  nonSwitch :: Text -> Bool
  nonSwitch s =
    case Text.unpack (Text.take 1 s) of
      (c : _) -> Char.isAlpha c
      [] -> False

-- | Test that converting a @cardano-address@ Byron signing key yields the
-- expected result.
hprop_golden_HelpAll :: Property
hprop_golden_HelpAll =
  propertyOnce . H.moduleWorkspace "help" $ \_ -> do
    -- These tests are not run on Windows because the cardano-cli usage
    -- output is slightly different on Windows.  For example it uses
    -- "cardano-cli.exe" instead of "cardano-cli".
    unless isWin32 $ do
      helpFp <- H.note "test/cardano-cli-golden/files/golden/help.cli"

      help <-
        filterAnsi
          <$> execCardanoCLI
            [ "help"
            ]

      H.diffVsGoldenFile help helpFp

-- | Return the string with the prefix dropped if the prefix is present, otherwise return Nothing.
selectAndDropPrefix :: Text -> Text -> Maybe Text
selectAndDropPrefix prefix text =
  if Text.isPrefixOf prefix text
    then Just $ Text.drop (Text.length prefix) text
    else Nothing

-- | If the command invocation has a metavar with screaming snake case in the last position, remove it
stripMetavar :: Text -> Text
stripMetavar =
  Text.unwords
    . removeMetavar
    . Text.words
 where
  removeMetavar =
    \case
      [] -> []
      [w] -- remove metavar from the last position
        | isScreamingSnakeCase w -> []
        | otherwise -> [w]
      w : ws -> w : removeMetavar ws

  isScreamingSnakeCase :: Text -> Bool
  isScreamingSnakeCase = all (\c -> Char.isUpperCase c || c == '_') . toList

selectCmd :: Text -> Maybe Text
selectCmd = fmap stripMetavar . selectAndDropPrefix "Usage: cardano-cli "

test_golden_HelpCmds :: IO TestTree
test_golden_HelpCmds =
  -- These tests are not run on Windows because the cardano-cli usage
  -- output is slightly different on Windows. For example it uses
  -- "cardano-cli.exe" instead of "cardano-cli".
  if isWin32
    then return $ testGroup "help-commands" []
    else do
      helpText <-
        filterAnsi
          <$> readProcess
            "cardano-cli"
            [ "help"
            ]
            ""

      let lines = Text.lines (Text.pack helpText)
          usages = [] : nub (List.filter (not . null) (fmap extractCmd $ maybeToList . selectCmd =<< lines))

      return $
        testGroup
          "help-commands"
          [ testProperty
              (subPath usage)
              ( propertyOnce . H.moduleWorkspace "help-commands" $ \_ -> do
                  H.noteShow_ usage
                  let expectedCmdHelpFp =
                        "test/cardano-cli-golden/files/golden" </> subPath usage

                  (exitCode, stdout, stderr) <- H.execDetailCardanoCLI (Text.unpack <$> usage <> ["--help"])
                  let cmdHelp = filterAnsi stdout

                  case exitCode of
                    ExitSuccess ->
                      H.diffVsGoldenFile cmdHelp expectedCmdHelpFp
                    ExitFailure _ -> do
                      H.note_ "Failed to generate correct help text"
                      H.noteShow_ exitCode
                      H.note_ $ filterAnsi stderr
                      H.note_ cmdHelp -- stdout
                      H.failure
              )
          | usage <- usages
          ]
 where
  subPath :: [Text] -> FilePath
  subPath [] =
    -- This is the case where the usage is empty (just calling `cardano-cli` without parameters),
    -- which results in the main help output. We store that in a file named "base_help.cli".
    -- We need to make an exception because otherwise the file would be named ".cli" and would
    -- be invisible. We also put it outside of the "help" directory to avoid potential clashes
    -- with other files.
    "base_help.cli"
  subPath usage =
    -- For all other cases, we store the help output in a file named after the command sequence
    -- separated by underscores, under a directory named "help".
    "help" </> Text.unpack (Text.intercalate "_" usage) <> ".cli"
