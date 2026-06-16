module Cardano.CLI.Render
  ( customRenderHelp
  , renderAnyCmdError
  )
where

import Data.Text (Text)
import Options.Applicative.Help.Types (ParserHelp, renderHelp)
import Prettyprinter

customRenderHelp :: Int -> ParserHelp -> String
customRenderHelp = renderHelp

renderAnyCmdError :: Text -> (a -> Doc ann) -> a -> Doc ann
renderAnyCmdError cmdText renderer shelCliCmdErr =
  mconcat
    [ "Command failed: "
    , pretty cmdText
    , "\nError: "
    , renderer shelCliCmdErr
    ]
