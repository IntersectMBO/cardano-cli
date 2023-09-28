{-# LANGUAGE NumericUnderscores #-}

module Test.Cli.JSON
  ( hprop_json_roundtrip_delegations_and_rewards
  , hprop_roundtrip_kes_period_info_output_JSON
  ) where

import Cardano.Api.Shelley

import Cardano.CLI.Types.Common
import Cardano.CLI.Types.Output (QueryKesPeriodInfoOutput (..), createOpCertIntervalInfo)

import Data.Aeson
import Data.Map.Strict qualified as Map
import Data.Time
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Word (Word64)

import Test.Gen.Cardano.Api.Typed
  ( genLovelace
  , genSlotNo
  , genStakeAddress
  , genVerificationKeyHash
  )

import Hedgehog (Gen, Property, forAll, property, tripping)
import Hedgehog.Gen as Gen
import Hedgehog.Range as Range

-- TODO: Move to cardano-api
hprop_json_roundtrip_delegations_and_rewards :: Property
hprop_json_roundtrip_delegations_and_rewards =
  property $ do
    dAndG <- forAll genDelegationsAndRewards
    tripping dAndG encode eitherDecode

genDelegationsAndRewards :: Gen DelegationsAndRewards
genDelegationsAndRewards = do
  let r = Range.constant 0 3
  sAddrs <- Gen.list r genStakeAddress
  sLovelace <- Gen.list r genLovelace
  let delegMapAmt = Map.fromList $ zip sAddrs sLovelace
  poolIDs <- Gen.list r genPoolId
  let delegMapPool = Map.fromList $ zip sAddrs poolIDs
  return $ DelegationsAndRewards (delegMapAmt, delegMapPool)

genOpCertIntervalInformation :: Gen OpCertIntervalInformation
genOpCertIntervalInformation = do
  createOpCertIntervalInfo
    <$> (CurrentKesPeriod <$> genWord64)
    <*> (OpCertStartingKesPeriod <$> genWord64)
    <*> (OpCertEndingKesPeriod <$> genWord64)
    <*> Gen.maybe (SlotsTillKesKeyExpiry <$> genSlotNo)

genPoolId :: Gen (Hash StakePoolKey)
genPoolId = genVerificationKeyHash AsStakePoolKey

genWord64 :: Gen Word64
genWord64 = Gen.word64 Range.constantBounded

genUTCTime :: Gen UTCTime
genUTCTime = do
  t <- Gen.int64 Range.constantBounded
  pure . posixSecondsToUTCTime $ fromIntegral t / 1_000_000

genKesPeriodInfoOutput :: Gen QueryKesPeriodInfoOutput
genKesPeriodInfoOutput =
  QueryKesPeriodInfoOutput
    <$> genOpCertIntervalInformation
    <*> Gen.maybe genUTCTime
    <*> Gen.maybe (OpCertNodeStateCounter <$> genWord64)
    <*> (OpCertOnDiskCounter <$> genWord64)
    <*> genWord64
    <*> genWord64

hprop_roundtrip_kes_period_info_output_JSON :: Property
hprop_roundtrip_kes_period_info_output_JSON = property $ do
  kesPeriodOutput <- forAll genKesPeriodInfoOutput
  tripping kesPeriodOutput encode eitherDecode
