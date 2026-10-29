{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Gen.Cardano.CLI.StakePool
  ( genStakePoolParams
  , genQueryResultStakePoolState
  )
where

import Cardano.Api
import Cardano.Api.Experimental qualified as Exp
import Cardano.Api.Ledger qualified as L

import Cardano.Ledger.Api.State.Query qualified as L
import Cardano.Ledger.BaseTypes (Port (..), textToDns)
import Cardano.Ledger.State qualified as L

import Data.Array.Byte (ByteArray)
import GHC.Exts (fromList)

import Test.Gen.Cardano.Api.Typed
  ( genCertificate
  , genEpochNo
  , genLovelace
  , genSigningKey
  , genStakeCredential
  )

import Hedgehog (Gen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

-- | Parameters from the ledger generator, with a BLS key that is present or absent, a 32 byte
-- metadata hash and relays of every kind that needs no IP address added.
genStakePoolParams
  :: forall era
   . Exp.IsEra era
  => Gen (L.StakePoolParams (Exp.LedgerEra era))
genStakePoolParams = Exp.obtainCommonConstraints (Exp.useEra @era) $ do
  baseParams <-
    Gen.just $
      (\(Exp.Certificate cert) -> L.getRegPoolTxCert cert)
        <$> genCertificate (convert $ Exp.useEra @era)
  blsKey <- Gen.maybe $ toLedgerBlsKey <$> genSigningKey AsBlsKey
  metadata <- traverse genMetadataHash $ L.sppMetadata baseParams
  dnsName <- textToDns 64 "relay.example.com"
  extraRelays <- Gen.subsequence $ extraRelayCandidates dnsName
  pure
    baseParams
      { L.sppBlsKey = maybe L.SNothing L.SJust blsKey
      , L.sppMetadata = metadata
      , L.sppRelays = L.sppRelays baseParams <> fromList extraRelays
      }
 where
  genMetadataHash poolMetadata = do
    hashBytes <- Gen.list (Range.singleton 32) $ Gen.word8 Range.constantBounded
    pure poolMetadata{L.pmHash = fromList hashBytes :: ByteArray}

  extraRelayCandidates :: L.DnsName -> [L.StakePoolRelay]
  extraRelayCandidates dnsName =
    [ L.SingleHostAddr L.SNothing L.SNothing L.SNothing
    , L.SingleHostName (L.SJust $ Port 3001) dnsName
    , L.MultiHostName dnsName
    ]

-- | A pool state with every optional field present or absent, and parameters of a future
-- re-registration from 'genStakePoolParams'.
genQueryResultStakePoolState
  :: forall era
   . Exp.IsEra era
  => Gen (L.QueryResultStakePoolState (Exp.LedgerEra era))
genQueryResultStakePoolState = do
  params <- genStakePoolParams @era
  blsKey <- genOptional $ L.BlsKeyState . toLedgerBlsKey <$> genSigningKey AsBlsKey <*> genEpochNo
  delegators <-
    genOptional . Gen.set (Range.linear 0 5) $ toShelleyStakeCredential <$> genStakeCredential
  retiring <- genOptional genEpochNo
  futureParams <- genOptional $ genStakePoolParams @era
  deposit <- genLovelace
  numDelegators <- Gen.int $ Range.linear 0 1000
  let L.AccountAddress _ accountId = L.sppAccountAddress params
  pure
    L.QueryResultStakePoolState
      { L.qrspsVrf = L.sppVrf params
      , L.qrspsBlsKey = L.maybeToStrictMaybe blsKey
      , L.qrspsPledge = L.sppPledge params
      , L.qrspsCost = L.sppCost params
      , L.qrspsMargin = L.sppMargin params
      , L.qrspsAccountId = accountId
      , L.qrspsOwners = L.sppOwners params
      , L.qrspsRelays = L.sppRelays params
      , L.qrspsMetadata = L.sppMetadata params
      , L.qrspsDeposit = deposit
      , L.qrspsNumDelegators = numDelegators
      , L.qrspsDelegators = L.maybeToStrictMaybe delegators
      , L.qrspsRetiring = L.maybeToStrictMaybe retiring
      , L.qrspsFutureStakePoolParams = L.maybeToStrictMaybe futureParams
      }

-- | Absent and present values equally often, unlike 'Gen.maybe'.
genOptional :: Gen a -> Gen (Maybe a)
genOptional gen = Gen.choice [pure Nothing, Just <$> gen]
