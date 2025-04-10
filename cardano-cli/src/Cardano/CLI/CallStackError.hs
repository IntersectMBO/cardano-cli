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
  let err = ErrorAWrapper (ActualErrorThrown)
   in Left err

-- Updated api with callstack

data WithCallStack e
  = AllCallStacks e CallStack --- When we want all of the call stacks
  | PrioritizedCallStack e CallStack -- When we want only the innermost call stack
  deriving Show

class Show e => ModifiedError e where
  prettyErrorModified :: WithCallStack e -> Doc ann

data ErrorAWithCallStack
  = BadErrorA' -- Stays the same
  | TopLevelErrorWithCallStack (WithCallStack ActualErrorThrown) -- We must wrap nested errors with WithCallStack
  deriving Show

instance ModifiedError ErrorAWithCallStack where
  prettyErrorModified (AllCallStacks e cs) =
    case e of
      BadErrorA' ->
        vsep
          [ "BadErrorA"
          , pretty (prettyCallStack cs)
          ]
      TopLevelErrorWithCallStack s@AllCallStacks{} ->
        -- We render the callstack to this point and then recurse with prettyErrorModified
        vsep
          [ "TopLevelErrorWithCallStack"
          , pretty (prettyCallStack cs)
          , indent 1 $ prettyErrorModified s
          ]
      -- Only render the inner most callstack (PrioritizedCallStack)
      TopLevelErrorWithCallStack (p@PrioritizedCallStack{}) -> prettyErrorModified p
  prettyErrorModified (PrioritizedCallStack e cs) =
    case e of
      BadErrorA' -> vsep ["BadErrorA", pretty (prettyCallStack cs)]
      TopLevelErrorWithCallStack (AllCallStacks{}) -> mempty -- Programmer must not nest AllCallStacks within a PrioritizedCallStack.
      TopLevelErrorWithCallStack (p@PrioritizedCallStack{}) -> prettyErrorModified p -- We render the prioritized callstack only

instance ModifiedError ActualErrorThrown where
  prettyErrorModified (AllCallStacks e cs) =
    case e of
      ActualErrorThrown -> vsep ["ActualErrorThrown:", indent 2 ((pretty (prettyCallStack cs)))] -- no further call stacks, so we don't call prettyErrorModified
  prettyErrorModified (PrioritizedCallStack e cs) =
    case e of
      ActualErrorThrown -> vsep ["ActualErrorThrown:", indent 2 ((pretty (prettyCallStack cs)))] -- no further call stacks, so we don't call prettyErrorModified

-- Examples

updatedApiExampleFunction :: HasCallStack => Either (WithCallStack ErrorAWithCallStack) ()
updatedApiExampleFunction = first (appendWithAllCallStack TopLevelErrorWithCallStack) errorThrowingFunction

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

-- Example 3 - return all call stacks

updatedApiExampleFunction3 :: HasCallStack => Either (WithCallStack ErrorAWithCallStack) ()
updatedApiExampleFunction3 = first (appendWithAllCallStack TopLevelErrorWithCallStack) errorThrowingFunction3

errorThrowingFunction3 :: HasCallStack => Either (WithCallStack ActualErrorThrown) ()
errorThrowingFunction3 = Left $ AllCallStacks (ActualErrorThrown) callStack

runExample3 :: IO ()
runExample3 = putStrLn $
  case updatedApiExampleFunction3 of
    Left e -> Text.unpack . renderStrict . layoutPretty defaultLayoutOptions $ prettyErrorModified e
    Right _ -> "No error"

runAllExamples :: IO ()
runAllExamples = do
  putStrLn "Example 1:"
  runExample
  putStrLn "\nExample 2:"
  runExample2
  putStrLn "\nExample 3:"
  runExample3

appendWithAllCallStack :: HasCallStack => (a -> e) -> a -> WithCallStack e
appendWithAllCallStack wrapper errorToBeWrapped =
  AllCallStacks (wrapper errorToBeWrapped) callStack
