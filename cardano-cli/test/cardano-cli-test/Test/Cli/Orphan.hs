{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cli.Orphan where

import Cardano.Api.Experimental qualified as Exp
import Cardano.Api.Ledger qualified as L

import Cardano.CLI.Orphan ()
import Cardano.Ledger.Api.State.Query qualified as L

import Data.Aeson (eitherDecode, encode)

import Test.Gen.Cardano.CLI.StakePool (genQueryResultStakePoolState)

import Hedgehog (Property, forAll)
import Hedgehog qualified as H

-- | The era the pool state is generated in.
type TestEra = Exp.DijkstraEra

-- | Decoding the JSON of a stake pool state gives back the same state.
-- Execute me with:
-- @cabal test cardano-cli-test --test-options '-p "/query result stake pool state json round trip/"'@
hprop_query_result_stake_pool_state_json_round_trip :: Property
hprop_query_result_stake_pool_state_json_round_trip = H.property $ do
  poolState <- forAll $ genQueryResultStakePoolState @TestEra
  coverPresence "bls key" $ L.qrspsBlsKey poolState
  coverPresence "metadata" $ L.qrspsMetadata poolState
  coverPresence "delegators" $ L.qrspsDelegators poolState
  H.cover 4 "delegators present and empty" $
    fmap null (L.strictMaybeToMaybe $ L.qrspsDelegators poolState) == Just True
  H.cover 4 "delegators present and non-empty" $
    fmap null (L.strictMaybeToMaybe $ L.qrspsDelegators poolState) == Just False
  coverPresence "retiring" $ L.qrspsRetiring poolState
  coverPresence "future params" $ L.qrspsFutureStakePoolParams poolState
  H.tripping poolState encode eitherDecode

coverPresence :: H.MonadTest m => H.LabelName -> L.StrictMaybe a -> m ()
coverPresence label value = do
  H.cover 10 (label <> " present") $ isPresent value
  H.cover 10 (label <> " absent") . not $ isPresent value
 where
  isPresent = \case
    L.SJust _ -> True
    L.SNothing -> False
