{-# LANGUAGE DataKinds #-}

module Cardano.CLI.EraIndependent.Cip.Common
  ( -- * Input related 
    Input (..)

  , pInputFile
  , pInputHexText
  , pInputBech32Text
  
    -- * Output related 
  , Output (..)
  , pOutputFile
  , pOutputText
  )
  where 


import Options.Applicative qualified as Opt
import Cardano.Api 
import Cardano.CLI.EraBased.Common.Option hiding (pOutputFile)
import qualified Data.Text as Text
import Data.Text (Text)


data Input 
    = InputFile (File () In)
    | InputHexText Text
    | InputBech32Text Text

pInputFile :: String -> String -> Opt.Parser Input
pInputFile optName desc = 
  InputFile <$> pFileInDirection optName desc

pInputHexText :: String -> String -> String -> Opt.Parser Input
pInputHexText optName metavar help = 
   fmap (InputHexText . Text.pack) $
    Opt.strOption $
      mconcat
        [ Opt.long optName
        , Opt.metavar metavar
        , Opt.help help
        ]

pInputBech32Text :: String -> String -> String -> Opt.Parser Input
pInputBech32Text optName metavar help = 
   fmap (InputBech32Text . Text.pack) $
    Opt.strOption $
      mconcat
        [ Opt.long optName
        , Opt.metavar metavar
        , Opt.help help
        ]


data Output 
    = OutputFile (File () Out)
    | OutputText Text 

pOutputFile :: String -> String -> Opt.Parser Output
pOutputFile optName desc = 
    OutputFile <$> pFileOutDirection optName desc


pOutputText :: String -> String -> String -> Opt.Parser Output
pOutputText optName metavar help = 
   fmap (OutputText . Text.pack) $
    Opt.strOption $
      mconcat
        [ Opt.long optName
        , Opt.metavar metavar
        , Opt.help help
        ]