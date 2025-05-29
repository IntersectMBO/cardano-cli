{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Type.TxFeature
  ( TxFeature (..)
  , renderFeature
  )
where

import Data.Text (Text)

-- | An enumeration of era-dependent features where we have to check that it
-- is permissible to use this feature in this era.
data TxFeature
  = TxFeatureMultiAssetOutputs
  | TxFeatureInlineDatums
  deriving Show

renderFeature :: TxFeature -> Text
renderFeature = \case
  TxFeatureMultiAssetOutputs -> "Multi-Asset outputs"
  TxFeatureInlineDatums -> "Inline datums"
