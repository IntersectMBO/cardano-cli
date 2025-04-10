module Cardano.CLI.CallStackError where

import Cardano.Api

import Data.Bifunctor
import Data.Text qualified as Text
import GHC.Stack
import Prettyprinter
import Prettyprinter.Render.Text

-- Example error with nested error ActualErrorThrown
data ErrorA
  = BadErrorA
  | ErrorAWrapper ActualErrorThrown

data ActualErrorThrown = ActualErrorThrown deriving Show

-- No Callstack
currentApiExampleFunction :: Either ErrorA ()
currentApiExampleFunction =
  let err = ErrorAWrapper ActualErrorThrown
   in Left err

-- Updated api with callstack

data WithCallStack e
  = PrioritizedCallStack e CallStack -- When we want only the innermost call stack
  deriving Show

class Show e => ModifiedError e where
  prettyErrorModified :: WithCallStack e -> Doc ann

data ErrorAWithCallStack
  = BadErrorA' -- Stays the same
  | TopLevelErrorWithCallStack (WithCallStack ActualErrorThrown) -- We must wrap nested errors with WithCallStack
  deriving Show

instance ModifiedError ErrorAWithCallStack where
  prettyErrorModified (PrioritizedCallStack e cs) =
    case e of
      BadErrorA' -> vsep ["BadErrorA", pretty (prettyCallStack cs)] -- No further callstacks so we render everything here
      TopLevelErrorWithCallStack (p@PrioritizedCallStack{}) -> prettyErrorModified p

instance ModifiedError ActualErrorThrown where
  prettyErrorModified (PrioritizedCallStack e cs) =
    case e of
      ActualErrorThrown -> vsep ["ActualErrorThrown:", indent 2 ((pretty (prettyCallStack cs)))] -- no further call stacks, so we don't call prettyErrorModified

-- Examples

updatedApiExampleFunction :: HasCallStack => Either (WithCallStack ErrorAWithCallStack) ()
updatedApiExampleFunction = first (wrapErrorWithAllPrioritizedCallStack TopLevelErrorWithCallStack) errorThrowingFunction

errorThrowingFunction :: HasCallStack => Either (WithCallStack ActualErrorThrown) ()
errorThrowingFunction = Left $ PrioritizedCallStack (ActualErrorThrown) callStack

runExample :: IO ()
runExample = putStrLn $
  case updatedApiExampleFunction of
    Left e -> Text.unpack . renderStrict . layoutPretty defaultLayoutOptions $ prettyErrorModified e
    Right _ -> "No error"

-- Example 2

updatedApiExampleFunction2 :: HasCallStack => Either (WithCallStack ErrorAWithCallStack) ()
updatedApiExampleFunction2 = errorThrowingFunction2

errorThrowingFunction2 :: HasCallStack => Either (WithCallStack ErrorAWithCallStack) ()
errorThrowingFunction2 = Left $ PrioritizedCallStack (BadErrorA') callStack

runExample2 :: IO ()
runExample2 = putStrLn $
  case updatedApiExampleFunction2 of
    Left e -> Text.unpack . renderStrict . layoutPretty defaultLayoutOptions $ prettyErrorModified e
    Right _ -> "No error"

runAllExamples :: IO ()
runAllExamples = do
  putStrLn "Example 1: Return the prioritized callstack"
  runExample
  putStrLn "\nExample 2: Return the prioritized callstack"
  runExample2

wrapErrorWithAllPrioritizedCallStack :: HasCallStack => (a -> e) -> a -> WithCallStack e
wrapErrorWithAllPrioritizedCallStack wrapper errorToBeWrapped =
  PrioritizedCallStack (wrapper errorToBeWrapped) callStack
