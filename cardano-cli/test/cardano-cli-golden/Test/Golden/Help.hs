{-# LANGUAGE OverloadedStrings #-}

{- HLINT ignore "Redundant id" -}

module Test.Golden.Help
  ( hprop_golden_HelpAll
  , hprop_golden_HelpCmds
  )
where

import Prelude hiding (lines)

import Control.Monad (forM_, unless, (<=<))
import Data.Char qualified as Char
import Data.List qualified as List
import Data.Maybe (maybeToList)
import Data.Text (Text)
import Data.Text qualified as Text
import System.FilePath ((</>))
import Text.Regex (Regex, mkRegex, subRegex)

import Test.Cardano.CLI.Util (execCardanoCLIWithEnvVars, propertyOnce)
import Test.Cardano.CLI.Util qualified as H

import Hedgehog (Property)
import Hedgehog.Extras.Stock.OS (isWin32)
import Hedgehog.Extras.Test.Base qualified as H
import Hedgehog.Extras.Test.Golden qualified as H

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
          <$> execCardanoCLIWithEnvVars
            [ ("CARDANO_CLI_VISIBILITY_LEVEL", "internal")
            ]
            [ "help"
            ]

      H.diffVsGoldenFile help helpFp

third :: (a, b, c) -> c
third (_, _, c) = c

-- | Return the string with the prefix dropped if the prefix is present, otherwise return Nothing.
selectAndDropPrefix :: Text -> Text -> Maybe Text
selectAndDropPrefix prefix text =
  if Text.isPrefixOf prefix text
    then Just $ Text.drop (Text.length prefix) text
    else Nothing

deselectSuffix :: Text -> Text -> Maybe Text
deselectSuffix suffix text =
  if Text.isSuffixOf suffix text
    then Nothing
    else Just text

selectCmd :: Text -> Maybe Text
selectCmd = selectAndDropPrefix "Usage: cardano-cli " <=< deselectSuffix " COMMAND"

hprop_golden_HelpCmds :: Property
hprop_golden_HelpCmds =
  propertyOnce . H.moduleWorkspace "help-commands" $ \_ -> do
    -- These tests are not run on Windows because the cardano-cli usage
    -- output is slightly different on Windows.  For example it uses
    -- "cardano-cli.exe" instead of "cardano-cli".
    unless isWin32 $ do
      help <-
        filterAnsi
          <$> execCardanoCLIWithEnvVars
            [ ("CARDANO_CLI_VISIBILITY_LEVEL", "internal")
            ]
            [ "help"
            ]

      let lines = Text.lines (Text.pack help)
      let usages = [] : List.filter (not . null) (fmap extractCmd $ maybeToList . selectCmd =<< lines)

      forM_ usages $ \usage -> do
        H.noteShow_ usage
        let expectedCmdHelpFp =
              "test/cardano-cli-golden/files/golden" </> subPath usage

        cmdHelp <- filterAnsi . third <$> H.execDetailCardanoCLI (fmap Text.unpack usage)

        H.diffVsGoldenFile cmdHelp expectedCmdHelpFp
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
