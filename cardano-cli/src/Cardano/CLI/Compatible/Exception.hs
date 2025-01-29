{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.CLI.Compatible.Exception
  ( CustomCliException (..)
  )
where

import           Cardano.Api

import           GHC.Stack

import           RIO

data CustomCliException where
  CustomCliException
    :: (Show error, Typeable error, Error error, HasCallStack)
    => error -> CustomCliException

deriving instance Show CustomCliException

instance Exception CustomCliException where
  displayException (CustomCliException e) =
    unlines
      [ show (prettyError e)
      , prettyCallStack callStack
      ]
