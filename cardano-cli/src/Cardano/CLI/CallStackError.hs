module Cardano.CLI.CallStackError where

import Cardano.Api

import Data.Bifunctor
import Data.Text qualified as Text
import Prettyprinter
import Prettyprinter.Render.Text

-- * Example error definitions *

data ErrorA
  = BadErrorA
  | ErrorAWrapper ActualErrorThrown
  deriving Show

instance Error ErrorA where
  prettyError BadErrorA = "A Bad Error A has occurred!"
  prettyError (ErrorAWrapper e) = "A nested error of type A Wrapper has occurred, these are the details: " <> prettyError e

data ActualErrorThrown = ActualErrorThrown
  deriving Show

instance Error ActualErrorThrown where
  prettyError ActualErrorThrown = "An Actual Error Thrown!"

-- * Example API *

currentApiExampleFunction :: Either ErrorA ()
currentApiExampleFunction =
  let err = ErrorAWrapper ActualErrorThrown
   in Left err

-- * Example 1 *

updatedApiExampleFunction :: Either ErrorA ()
updatedApiExampleFunction = first ErrorAWrapper errorThrowingFunction

errorThrowingFunction :: Either ActualErrorThrown ()
errorThrowingFunction = Left ActualErrorThrown

runExample :: IO ()
runExample = putStrLn $
  case updatedApiExampleFunction of
    Left e -> Text.unpack . renderStrict . layoutPretty defaultLayoutOptions $ prettyError e
    Right _ -> "No error"

-- * Example 2 *

updatedApiExampleFunction2 :: Either ErrorA ()
updatedApiExampleFunction2 = errorThrowingFunction2

errorThrowingFunction2 :: Either ErrorA ()
errorThrowingFunction2 = Left BadErrorA

runExample2 :: IO ()
runExample2 = putStrLn $
  case updatedApiExampleFunction2 of
    Left e -> Text.unpack . renderStrict . layoutPretty defaultLayoutOptions $ prettyError e
    Right _ -> "No error"

-- * Main function to run all examples *

runAllExamples :: IO ()
runAllExamples = do
  putStrLn "Example 1: Return the prioritized callstack"
  runExample
  putStrLn "\nExample 2: Return the prioritized callstack"
  runExample2
