{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE InstanceSigs #-}

module Cardano.CLI.CallStackError where

import Cardano.Api hiding (Error, prettyError)

import Data.Bifunctor
import Data.Text qualified as Text
import GHC.Stack (CallStack, HasCallStack, callStack, prettyCallStack)
import Prettyprinter
import Prettyprinter.Render.Text

-- * New definitions *

class ErrorContent e where
  prettyErrorContent :: e -> Doc ann

data Content = forall e. ErrorContent e => Content e

data Cause = forall c. Error c => Cause c

class Error e where
  getErrorContent :: e -> Content
  getErrorCallStack :: e -> CallStack
  getCause :: e -> Maybe Cause

prettyErrorWithoutStack :: Error e => e -> Doc ann
prettyErrorWithoutStack e =
  case getErrorContent e of
    Content content -> prettyErrorContent content

prettyError :: Error e => e -> Doc ann
prettyError e =
  vsep
    [ prettyErrorWithoutStack e
    , indent 2 $ pretty $ prettyCallStack (getErrorCallStack e)
    , ""
    , case getCause e of
        Nothing -> mempty
        Just (Cause cause) ->
          vsep
            [ "Caused by:"
            , indent 2 $ prettyError cause
            ]
    ]

data ErrorWithStack e
  = ErrorContent e => RootErrorWithStack e CallStack
  | forall c. (ErrorContent e, Error c) => CausedErrorWithStack e CallStack c

mkError :: (ErrorContent e, HasCallStack) => e -> ErrorWithStack e
mkError e = RootErrorWithStack e callStack

wrapError
  :: HasCallStack => ErrorContent e1 => (e2 -> e1) -> ErrorWithStack e2 -> ErrorWithStack e1
wrapError f se@(RootErrorWithStack e _) = CausedErrorWithStack (f e) callStack se
wrapError f se@(CausedErrorWithStack e _ _) = CausedErrorWithStack (f e) callStack se

instance ErrorContent e => Error (ErrorWithStack e) where
  getErrorContent :: ErrorWithStack e -> Content
  getErrorContent (RootErrorWithStack e _) = Content e
  getErrorContent (CausedErrorWithStack e _ _) = Content e

  getErrorCallStack :: ErrorWithStack e -> CallStack
  getErrorCallStack (RootErrorWithStack _ cs) = cs
  getErrorCallStack (CausedErrorWithStack _ cs _) = cs

  getCause :: ErrorWithStack e -> Maybe Cause
  getCause (RootErrorWithStack _ _) = Nothing
  getCause (CausedErrorWithStack _ _ c) = Just $ Cause c

-- * Example error definitions *

data ErrorA
  = BadErrorA
  | ErrorAWrapper ActualErrorThrown
  deriving Show

instance ErrorContent ErrorA where
  prettyErrorContent BadErrorA = "A Bad Error A has occurred!"
  prettyErrorContent (ErrorAWrapper e) = "A nested error of type A Wrapper has occurred, these are the details: " <> prettyErrorContent e

data ActualErrorThrown = ActualErrorThrown
  deriving Show

instance ErrorContent ActualErrorThrown where
  prettyErrorContent ActualErrorThrown = "An Actual Error Thrown!"

-- * Example API *

currentApiExampleFunction :: Either (ErrorWithStack ErrorA) ()
currentApiExampleFunction =
  let err = ErrorAWrapper ActualErrorThrown
   in Left (mkError err)

-- * Example 1 *

updatedApiExampleFunction :: Either (ErrorWithStack ErrorA) ()
updatedApiExampleFunction = first (wrapError ErrorAWrapper) errorThrowingFunction

errorThrowingFunction :: Either (ErrorWithStack ActualErrorThrown) ()
errorThrowingFunction = Left (mkError ActualErrorThrown)

runExample :: IO ()
runExample = putStrLn $
  case updatedApiExampleFunction of
    Left e -> Text.unpack . renderStrict . layoutPretty defaultLayoutOptions $ prettyError e
    Right _ -> "No error"

-- * Example 2 *

updatedApiExampleFunction2 :: Either (ErrorWithStack ErrorA) ()
updatedApiExampleFunction2 = errorThrowingFunction2

errorThrowingFunction2 :: Either (ErrorWithStack ErrorA) ()
errorThrowingFunction2 = Left (mkError BadErrorA)

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
