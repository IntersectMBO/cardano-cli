{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.Byron.Delegation
  ( serialiseDelegationCert
  , serialiseByronWitness
  )
where

import Cardano.Api.Byron hiding (delegateVK)
import Cardano.Api.Serialise.Raw

import Cardano.Prelude (canonicalEncodePretty)

import Prelude hiding ((.))

import Control.Category
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LB

serialiseDelegationCert :: Certificate -> ByteString
serialiseDelegationCert = LB.toStrict . canonicalEncodePretty

serialiseByronWitness :: SomeByronSigningKey -> ByteString
serialiseByronWitness sk =
  case sk of
    AByronSigningKeyLegacy bSkey -> serialiseToRawBytes bSkey
    AByronSigningKey legBKey -> serialiseToRawBytes legBKey
