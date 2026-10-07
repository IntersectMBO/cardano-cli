{-# LANGUAGE OverloadedStrings #-}

module Test.Cli.Query.PoolState where

import Cardano.Api
import Cardano.Api.Experimental qualified as Exp
import Cardano.Api.Ledger qualified as L

import Cardano.CLI.Type.Common (mkPoolStates)
import Cardano.Keys qualified as Keys
import Cardano.Ledger.Api.State.Query qualified as Query
import Cardano.Ledger.State qualified as L

import Data.Aeson qualified as Aeson
import Data.Map.Strict qualified as Map
import Lens.Micro ((^?))
import Lens.Micro.Aeson qualified as Aeson

import Test.Gen.Cardano.Api.Typed (genStakeAddress, genVerificationKeyHash)

import Test.Cardano.CLI.Util (propertyOnce, watchdogProp)

import Hedgehog (Property, (===))
import Hedgehog qualified as H

hprop_pool_state_preserves_schema_and_unknown_bls_registration :: Property
hprop_pool_state_preserves_schema_and_unknown_bls_registration = watchdogProp . propertyOnce $ do
  operator <- H.forAll $ genVerificationKeyHash AsStakePoolKey
  VrfKeyHash vrf <- H.forAll $ genVerificationKeyHash AsVrfKey
  account <- H.forAll genStakeAddress
  let params :: L.StakePoolParams (Exp.LedgerEra DijkstraEra)
      params =
        L.StakePoolParams
          { L.sppId = unStakePoolKeyHash operator
          , L.sppVrf = L.toVRFVerKeyHash vrf
          , L.sppBlsKey = L.SNothing
          , L.sppPledge = L.Coin 123
          , L.sppCost = L.Coin 456
          , L.sppMargin = minBound
          , L.sppAccountAddress = toShelleyStakeAddr account
          , L.sppOwners = mempty
          , L.sppRelays = mempty
          , L.sppMetadata = L.SNothing
          }
      futureParams pp = pp{L.sppCost = L.Coin 789}
      retiring = EpochNo 77
      deposit = L.Coin 500000000
      queried
        :: L.StakePoolParams (Exp.LedgerEra DijkstraEra)
        -> Map.Map (L.KeyHash L.StakePool) L.Coin
        -> PoolState DijkstraEra
      queried pp poolDeposits =
        PoolState $
          Query.QueryPoolStateResult
            { Query.qpsrStakePoolParams = Map.singleton (L.sppId pp) pp
            , Query.qpsrFutureStakePoolParams = Map.singleton (L.sppId pp) (futureParams pp)
            , Query.qpsrRetiring = Map.singleton (L.sppId pp) retiring
            , Query.qpsrDeposits = poolDeposits
            }
      projection pp poolDeposits =
        H.evalMaybe $ Map.lookup (L.sppId pp) $ mkPoolStates (queried pp poolDeposits)
      deposits = Map.singleton (L.sppId params) deposit
  compactDeposit <- H.evalMaybe $ L.toCompact deposit
  ordinary <- projection params deposits
  Aeson.toJSON ordinary
    === Aeson.object
      [ "poolParams" Aeson..= L.mkStakePoolState (EpochNo 88) compactDeposit mempty params
      , "futurePoolParams"
          Aeson..= L.mkStakePoolState (EpochNo 89) compactDeposit mempty (futureParams params)
      , "retiring" Aeson..= Just retiring
      ]
  missing <- projection params Map.empty
  Aeson.toJSON missing
    === Aeson.object
      [ "poolParams" Aeson..= Aeson.Null
      , "futurePoolParams" Aeson..= Aeson.Null
      , "retiring" Aeson..= Just retiring
      ]

  signingKey <- liftIO $ Keys.generateSigningKey Keys.AsBlsKey
  let keyJson =
        Aeson.object
          [ "blsPubKey" Aeson..= Keys.serialiseToRawBytesHexText (Keys.getVerificationKey signingKey)
          , "blsPossessionProof"
              Aeson..= Keys.serialiseToRawBytesHexText (Keys.createBlsPossessionProof signingKey)
          ]
  blsKey <- H.evalEither $ Aeson.eitherDecode $ Aeson.encode keyJson
  withBls <- projection params{L.sppBlsKey = L.SJust blsKey} deposits
  let json = Aeson.toJSON withBls
  mapM_
    ( \field -> do
        json ^? Aeson.key field . Aeson.key "spsBlsKey" . Aeson.key "bksKey" === Just keyJson
        json ^? Aeson.key field . Aeson.key "spsBlsKey" . Aeson.key "bksRegisteredIn" === Just Aeson.Null
    )
    ["poolParams", "futurePoolParams"]
