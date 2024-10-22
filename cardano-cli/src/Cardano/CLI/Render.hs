{- HLINT ignore "Redundant id" -}

module Cardano.CLI.Render
  ( customRenderHelp
  , renderAnyCmdError
  )
where

import           Cardano.Api (textShow)

import           Data.Text (Text)
import qualified Data.Text as T
import           Options.Applicative
import           Options.Applicative.Help.Ann
import           Options.Applicative.Help.Types (helpText, renderHelp)
import           Prettyprinter
import           Prettyprinter.Render.Util.SimpleDocTree
import qualified System.Environment as IO
import qualified System.IO.Unsafe as IO

cliHelpTraceEnabled :: Bool
cliHelpTraceEnabled = IO.unsafePerformIO $ do
  mValue <- IO.lookupEnv "CLI_HELP_TRACE"
  return $ mValue == Just "1"
{-# NOINLINE cliHelpTraceEnabled #-}

-- | Convert a help text to 'String'.  When the CLI_HELP_TRACE environment variable is set
-- to '1', the output will be in HTML so that it can be viewed in a browser where developer
-- tools can be used to inspect tracing that aids in describing the structure of the output
-- document.
customRenderHelp :: Int -> ParserHelp -> String
customRenderHelp =
  if cliHelpTraceEnabled
    then customRenderHelpAsHtml
    else customRenderHelpAsAnsi

customRenderHelpAsHtml :: Int -> ParserHelp -> String
customRenderHelpAsHtml cols =
  T.unpack
    . wrapper
    . renderSimplyDecorated id renderElement
    . treeForm
    . layoutSmart (LayoutOptions (AvailablePerLine cols 1.0))
    . helpText
 where
  renderElement :: Ann -> Text -> Text
  renderElement ann x =
    if cliHelpTraceEnabled
      then case ann of
        AnnTrace _ name -> "<span name=" <> textShow name <> ">" <> x <> "</span>"
        AnnStyle _ -> x
      else x
  wrapper =
    if cliHelpTraceEnabled
      then
        id
          . ("<html>\n" <>)
          . ("<body>\n" <>)
          . ("<pre>\n" <>)
          . (<> "\n</html>")
          . (<> "\n</body>")
          . (<> "\n</pre>")
      else id

customRenderHelpAsAnsi :: Int -> ParserHelp -> String
customRenderHelpAsAnsi = renderHelp

renderAnyCmdError :: Text -> (a -> Doc ann) -> a -> Doc ann
renderAnyCmdError cmdText renderer shelCliCmdErr =
  mconcat
    [ "Command failed: "
    , pretty cmdText
    , "  Error: "
    , renderer shelCliCmdErr
    ]
