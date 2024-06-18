{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.CLI.Polysemy
  ( ByronKeyFailure(..),
    ByronKeyFormat(..),
    SigningKeyFile,
    File(..),
    FileDirection(..),
    readByronSigningKey
  ) where

import           Cardano.Api.Byron (SomeByronSigningKey (..))

import           Cardano.CLI.Byron.Key (ByronKeyFailure (..))
import qualified Cardano.CLI.Byron.Key as Cli
import           Cardano.CLI.Types.Common

import           Control.Monad.Except (runExceptT)

import           HaskellWorks.Polysemy
import           HaskellWorks.Polysemy.Prelude
import           Polysemy ()

readByronSigningKey :: ()
  => Member (Error ByronKeyFailure) r
  => Member (Embed IO) r
  => ByronKeyFormat
  -> SigningKeyFile In
  -> Sem r SomeByronSigningKey
readByronSigningKey bKeyFormat fp =
  (embed $ runExceptT $ Cli.readByronSigningKey bKeyFormat fp)
    & onLeftM throw
