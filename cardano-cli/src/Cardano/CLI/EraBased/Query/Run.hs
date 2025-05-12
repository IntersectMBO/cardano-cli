{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

{- HLINT ignore "Redundant id" -}

module Cardano.CLI.EraBased.Query.Run
  ( runQueryCmds
  , runQueryKesPeriodInfoCmd
  , runQueryLeadershipScheduleCmd
  , runQueryLedgerStateCmd
  , runQueryLedgerPeerSnapshot
  , runQueryPoolStateCmd
  , runQueryProtocolParametersCmd
  , runQueryProtocolStateCmd
  , runQuerySlotNumberCmd
  , runQueryStakeAddressInfoCmd
  , runQueryStakeDistributionCmd
  , runQueryStakePoolsCmd
  , runQueryStakeSnapshotCmd
  , runQueryTipCmd
  , runQueryTxMempoolCmd
  , runQueryUTxOCmd
  , DelegationsAndRewards (..)
  , renderQueryCmdError
  , renderOpCertIntervalInformation
  , percentage
  )
where

import Cardano.Api hiding (QueryInShelleyBasedEra (..))
import Cardano.Api qualified as Api
import Cardano.Api.Consensus qualified as Consensus
import Cardano.Api.Ledger (StandardCrypto, strictMaybeToMaybe)
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Network (LedgerPeerSnapshot, Serialised (..))
import Cardano.Api.Network qualified as Consensus
import Cardano.Api.Shelley hiding (QueryInShelleyBasedEra (..))

import Cardano.Binary qualified as CBOR
import Cardano.CLI.EraBased.Genesis.Internal.Common
import Cardano.CLI.EraBased.Query.Command qualified as Cmd
import Cardano.CLI.Helper
import Cardano.CLI.Json.Encode qualified as Json
import Cardano.CLI.Read
  ( getHashFromStakePoolKeyHashSource
  )
import Cardano.CLI.Type.Common
import Cardano.CLI.Type.Error.NodeEraMismatchError
import Cardano.CLI.Type.Error.QueryCmdError
import Cardano.CLI.Type.Key
import Cardano.CLI.Type.Output (QueryDRepStateOutput (..))
import Cardano.CLI.Type.Output qualified as O
import Cardano.Crypto.Hash (hashToBytesAsHex)
import Cardano.Ledger.Api.State.Query qualified as L
import Cardano.Slotting.EpochInfo (EpochInfo (..), epochInfoSlotToUTCTime, hoistEpochInfo)
import Cardano.Slotting.Time (RelativeTime (..), toRelativeTime)

import Control.Monad (forM, join)
import Data.Aeson as Aeson
import Data.Aeson qualified as A
import Data.Aeson.Encode.Pretty qualified as Aeson
import Data.Bifunctor (Bifunctor (..))
import Data.ByteString.Base16.Lazy qualified as Base16
import Data.ByteString.Lazy qualified as BS
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Coerce (coerce)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Proxy (Proxy (..))
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.IO qualified as T
import Data.Text.Lazy.IO qualified as LT
import Data.Time.Clock
import GHC.Exts (IsList (..))
import GHC.Generics
import Lens.Micro ((^.))
import Numeric (showEFloat)
import Prettyprinter
import Prettyprinter.Render.Terminal (AnsiStyle)
import System.IO qualified as IO
import Text.Printf (printf)
import Vary

runQueryCmds :: Cmd.QueryCmds era -> ExceptT QueryCmdError IO ()
runQueryCmds = \case
  Cmd.QueryLeadershipScheduleCmd args -> runQueryLeadershipScheduleCmd args
  Cmd.QueryProtocolParametersCmd args -> runQueryProtocolParametersCmd args
  Cmd.QueryTipCmd args -> runQueryTipCmd args
  Cmd.QueryStakePoolsCmd args -> runQueryStakePoolsCmd args
  Cmd.QueryStakeDistributionCmd args -> runQueryStakeDistributionCmd args
  Cmd.QueryStakeAddressInfoCmd args -> runQueryStakeAddressInfoCmd args
  Cmd.QueryLedgerStateCmd args -> runQueryLedgerStateCmd args
  Cmd.QueryLedgerPeerSnapshotCmd args -> runQueryLedgerPeerSnapshot args
  Cmd.QueryStakeSnapshotCmd args -> runQueryStakeSnapshotCmd args
  Cmd.QueryProtocolStateCmd args -> runQueryProtocolStateCmd args
  Cmd.QueryUTxOCmd args -> runQueryUTxOCmd args
  Cmd.QueryKesPeriodInfoCmd args -> runQueryKesPeriodInfoCmd args
  Cmd.QueryPoolStateCmd args -> runQueryPoolStateCmd args
  Cmd.QueryTxMempoolCmd args -> runQueryTxMempoolCmd args
  Cmd.QuerySlotNumberCmd args -> runQuerySlotNumberCmd args
  Cmd.QueryRefScriptSizeCmd args -> runQueryRefScriptSizeCmd args
  Cmd.QueryConstitutionCmd args -> runQueryConstitution args
  Cmd.QueryGovStateCmd args -> runQueryGovState args
  Cmd.QueryRatifyStateCmd args -> runQueryRatifyState args
  Cmd.QueryFuturePParamsCmd args -> runQueryFuturePParams args
  Cmd.QueryDRepStateCmd args -> runQueryDRepState args
  Cmd.QueryDRepStakeDistributionCmd args -> runQueryDRepStakeDistribution args
  Cmd.QuerySPOStakeDistributionCmd args -> runQuerySPOStakeDistribution args
  Cmd.QueryCommitteeMembersStateCmd args -> runQueryCommitteeMembersState args
  Cmd.QueryTreasuryValueCmd args -> runQueryTreasuryValue args
  Cmd.QueryProposalsCmd args -> runQueryProposals args
  Cmd.QueryStakePoolDefaultVoteCmd args -> runQueryStakePoolDefaultVote args
  Cmd.QueryEraHistoryCmd args -> runQueryEraHistoryCmd args

runQueryProtocolParametersCmd
  :: ()
  => Cmd.QueryProtocolParametersCmdArgs
  -> ExceptT QueryCmdError IO ()
runQueryProtocolParametersCmd
  Cmd.QueryProtocolParametersCmdArgs
    { Cmd.nodeConnInfo
    , Cmd.mOutFile
    } = do
    AnyCardanoEra era <- firstExceptT QueryCmdAcquireFailure $ determineEra nodeConnInfo
    sbe <- forEraInEon @ShelleyBasedEra era (left QueryCmdByronEra) pure

    let qInMode = QueryInEra $ QueryInShelleyBasedEra sbe Api.QueryProtocolParameters

    pparams <-
      executeQueryAnyMode nodeConnInfo qInMode
        & modifyError QueryCmdConvenienceError

    let output = shelleyBasedEraConstraints sbe $ Json.encodeJson pparams

    firstExceptT QueryCmdWriteFileError
      . newExceptT
      $ writeLazyByteStringOutput mOutFile output

-- | Calculate the percentage sync rendered as text: @min 1 (tipTime/nowTime)@
percentage
  :: RelativeTime
  -- ^ @tolerance@.  If @b - a < tolerance@, then 100% is reported.  This even if we are @tolerance@ seconds
  -- behind, we are still considered fully synced.
  -> RelativeTime
  -- ^ @tipTime@ The time of the most recently synced block.
  -> RelativeTime
  -- ^ @nowTime@ The time of the tip of the block chain to which we need to sync.
  -> Text
percentage tolerance a b = Text.pack (printf "%.2f" pc)
 where
  -- All calculations are in seconds (Integer)
  t = relativeTimeSeconds tolerance
  -- Plus 1 to prevent division by zero.  The 's' prefix stands for strictly-positive.
  sa = relativeTimeSeconds a + 1
  sb = relativeTimeSeconds b + 1
  -- Fast forward the 'nowTime` by the tolerance, but don't let the result exceed the tip time.
  ua = min (sa + t) sb
  ub = sb
  -- Final percentage to render as text.
  pc = (fromIntegral ua / fromIntegral ub) * 100.0 :: Double

  relativeTimeSeconds :: RelativeTime -> Integer
  relativeTimeSeconds (RelativeTime dt) = floor (nominalDiffTimeToSeconds dt)

-- | Query the chain tip via the chain sync protocol.
--
-- This is a fallback query to support older versions of node to client protocol.
queryChainTipViaChainSync :: MonadIO m => LocalNodeConnectInfo -> m ChainTip
queryChainTipViaChainSync localNodeConnInfo = do
  liftIO . T.hPutStrLn IO.stderr $
    "Warning: Local header state query unavailable. Falling back to chain sync query"
  liftIO $ getLocalChainTip localNodeConnInfo

runQueryTipCmd
  :: ()
  => Cmd.QueryTipCmdArgs
  -> ExceptT QueryCmdError IO ()
runQueryTipCmd
  ( Cmd.QueryTipCmdArgs
      { Cmd.commons =
        Cmd.QueryCommons
          { Cmd.nodeConnInfo
          , Cmd.target
          }
      , Cmd.mOutFile
      }
    ) = do
    eLocalState <- ExceptT $
      fmap sequence $
        executeLocalStateQueryExpr nodeConnInfo target $
          runExceptT $ do
            era <- lift queryCurrentEra & onLeft (left . QueryCmdUnsupportedNtcVersion)
            eraHistory <- lift queryEraHistory & onLeft (left . QueryCmdUnsupportedNtcVersion)
            mChainBlockNo <- lift queryChainBlockNo & onLeft (left . QueryCmdUnsupportedNtcVersion) & fmap Just
            mChainPoint <- lift queryChainPoint & onLeft (left . QueryCmdUnsupportedNtcVersion) & fmap Just
            mSystemStart <- lift querySystemStart & onLeft (left . QueryCmdUnsupportedNtcVersion) & fmap Just

            return
              O.QueryTipLocalState
                { O.era = era
                , O.eraHistory = eraHistory
                , O.mSystemStart = mSystemStart
                , O.mChainTip = makeChainTip <$> mChainBlockNo <*> mChainPoint
                }

    mLocalState <- hushM (first QueryCmdAcquireFailure eLocalState) $ \e ->
      liftIO . LT.hPutStrLn IO.stderr $
        docToLazyText $
          "Warning: Local state unavailable: " <> renderQueryCmdError e

    chainTip <-
      pure (mLocalState >>= O.mChainTip)
        -- The chain tip is unavailable via local state query because we are connecting with an older
        -- node to client protocol so we use chain sync instead which necessitates another connection.
        -- At some point when we can stop supporting the older node to client protocols, this fallback
        -- can be removed.
        & onNothing (queryChainTipViaChainSync nodeConnInfo)

    let tipSlotNo :: SlotNo = case chainTip of
          ChainTipAtGenesis -> 0
          ChainTip slotNo _ _ -> slotNo

    localStateOutput <- forM mLocalState $ \localState -> do
      case slotToEpoch tipSlotNo (O.eraHistory localState) of
        Left e -> do
          liftIO . LT.hPutStrLn IO.stderr $
            docToLazyText $
              "Warning: Epoch unavailable: " <> renderQueryCmdError (QueryCmdPastHorizon e)
          return $
            O.QueryTipLocalStateOutput
              { O.localStateChainTip = chainTip
              , O.mEra = Nothing
              , O.mEpoch = Nothing
              , O.mSyncProgress = Nothing
              , O.mSlotInEpoch = Nothing
              , O.mSlotsToEpochEnd = Nothing
              }
        Right (epochNo, SlotsInEpoch slotsInEpoch, SlotsToEpochEnd slotsToEpochEnd) -> do
          syncProgressResult <- runExceptT $ do
            systemStart <-
              fmap getSystemStart (O.mSystemStart localState) & hoistMaybe QueryCmdSystemStartUnavailable
            nowSeconds <- toRelativeTime (SystemStart systemStart) <$> liftIO getCurrentTime
            tipTimeResult <-
              getProgress tipSlotNo (O.eraHistory localState) & bimap QueryCmdPastHorizon fst & except

            let tolerance = RelativeTime (secondsToNominalDiffTime 600)

            return $ percentage tolerance tipTimeResult nowSeconds

          mSyncProgress <- hushM syncProgressResult $ \e -> do
            liftIO . LT.hPutStrLn IO.stderr $
              docToLazyText $
                "Warning: Sync progress unavailable: " <> renderQueryCmdError e

          return $
            O.QueryTipLocalStateOutput
              { O.localStateChainTip = chainTip
              , O.mEra = Just (O.era localState)
              , O.mEpoch = Just epochNo
              , O.mSlotInEpoch = Just slotsInEpoch
              , O.mSlotsToEpochEnd = Just slotsToEpochEnd
              , O.mSyncProgress = mSyncProgress
              }

    let output = Json.encodeJson localStateOutput

    firstExceptT QueryCmdWriteFileError
      . newExceptT
      $ writeLazyByteStringOutput mOutFile output

-- | Query the UTxO, filtered by a given set of addresses, from a Shelley node
-- via the local state query protocol.
runQueryUTxOCmd
  :: ()
  => Cmd.QueryUTxOCmdArgs
  -> ExceptT QueryCmdError IO ()
runQueryUTxOCmd
  ( Cmd.QueryUTxOCmdArgs
      { Cmd.commons =
        Cmd.QueryCommons
          { Cmd.nodeConnInfo
          , Cmd.target
          }
      , Cmd.queryFilter
      , Cmd.format
      , Cmd.mOutFile
      }
    ) = do
    join $
      lift
        ( executeLocalStateQueryExpr nodeConnInfo target $ runExceptT $ do
            AnyCardanoEra era <- easyRunQueryCurrentEra

            sbe <-
              requireShelleyBasedEra era
                & onNothing (left QueryCmdByronEra)

            utxo <- easyRunQuery (queryUtxo sbe queryFilter)

            pure $ do
              writeFilteredUTxOs sbe format mOutFile utxo
        )
        & onLeft (left . QueryCmdAcquireFailure)
        & onLeft left

runQueryKesPeriodInfoCmd
  :: ()
  => Cmd.QueryKesPeriodInfoCmdArgs
  -> ExceptT QueryCmdError IO ()
runQueryKesPeriodInfoCmd
  Cmd.QueryKesPeriodInfoCmdArgs
    { Cmd.commons =
      Cmd.QueryCommons
        { Cmd.nodeConnInfo
        , Cmd.target
        }
    , Cmd.nodeOpCertFp
    , Cmd.mOutFile
    } = do
    opCert <-
      lift (readFileTextEnvelope AsOperationalCertificate nodeOpCertFp)
        & onLeft (left . QueryCmdOpCertCounterReadError)

    join $
      lift
        ( executeLocalStateQueryExpr nodeConnInfo target $ runExceptT $ do
            AnyCardanoEra era <- easyRunQueryCurrentEra

            sbe <-
              requireShelleyBasedEra era
                & onNothing (left QueryCmdByronEra)

            -- We check that the KES period specified in the operational certificate is correct
            -- based on the KES period defined in the genesis parameters and the current slot number
            gParams <- easyRunQuery (queryGenesisParameters sbe)

            eraHistory <- easyRunQueryEraHistory

            let eInfo = toTentativeEpochInfo eraHistory

            -- We get the operational certificate counter from the protocol state and check that
            -- it is equivalent to what we have on disk.
            ptclState <- easyRunQuery (queryProtocolState sbe)

            pure $ do
              chainTip <- liftIO $ getLocalChainTip nodeConnInfo

              let curKesPeriod = currentKesPeriod chainTip gParams
                  oCertStartKesPeriod = opCertStartingKesPeriod opCert
                  oCertEndKesPeriod = opCertEndKesPeriod gParams opCert
                  opCertIntervalInformation = opCertIntervalInfo gParams chainTip curKesPeriod oCertStartKesPeriod oCertEndKesPeriod

              (onDiskC, stateC) <- shelleyBasedEraConstraints sbe $ opCertOnDiskAndStateCounters ptclState opCert

              let counterInformation = opCertNodeAndOnDiskCounters onDiskC stateC

              -- Always render diagnostic information
              liftIO . putStrLn $
                docToString $
                  renderOpCertIntervalInformation (unFile nodeOpCertFp) opCertIntervalInformation
              liftIO . putStrLn $
                docToString $
                  renderOpCertNodeAndOnDiskCounterInformation (unFile nodeOpCertFp) counterInformation

              let qKesInfoOutput = createQueryKesPeriodInfoOutput opCertIntervalInformation counterInformation eInfo gParams
                  output = Json.encodeJson qKesInfoOutput

              firstExceptT QueryCmdWriteFileError
                . newExceptT
                $ writeLazyByteStringOutput mOutFile output
        )
        & onLeft (left . QueryCmdAcquireFailure)
        & onLeft left
   where
    currentKesPeriod :: ChainTip -> GenesisParameters era -> CurrentKesPeriod
    currentKesPeriod ChainTipAtGenesis _ = CurrentKesPeriod 0
    currentKesPeriod (ChainTip currSlot _ _) gParams =
      let slotsPerKesPeriod = fromIntegral $ protocolParamSlotsPerKESPeriod gParams
       in CurrentKesPeriod $ unSlotNo currSlot `div` slotsPerKesPeriod

    opCertStartingKesPeriod :: OperationalCertificate -> OpCertStartingKesPeriod
    opCertStartingKesPeriod = OpCertStartingKesPeriod . fromIntegral . getKesPeriod

    opCertEndKesPeriod :: GenesisParameters era -> OperationalCertificate -> OpCertEndingKesPeriod
    opCertEndKesPeriod gParams oCert =
      let OpCertStartingKesPeriod start = opCertStartingKesPeriod oCert
          maxKesEvo = fromIntegral $ protocolParamMaxKESEvolutions gParams
       in OpCertEndingKesPeriod $ start + maxKesEvo

    -- See OCERT rule in Shelley Spec: https://hydra.iohk.io/job/Cardano/cardano-ledger-specs/shelleyLedgerSpec/latest/download-by-type/doc-pdf/ledger-spec
    opCertIntervalInfo
      :: GenesisParameters era
      -> ChainTip
      -> CurrentKesPeriod
      -> OpCertStartingKesPeriod
      -> OpCertEndingKesPeriod
      -> OpCertIntervalInformation
    opCertIntervalInfo gParams currSlot' c s e@(OpCertEndingKesPeriod oCertEnd) =
      let cSlot = case currSlot' of
            (ChainTip cSlotN _ _) -> unSlotNo cSlotN
            ChainTipAtGenesis -> 0
          slotsTillExp =
            SlotsTillKesKeyExpiry . SlotNo $
              (oCertEnd * fromIntegral (protocolParamSlotsPerKESPeriod gParams)) - cSlot
       in O.createOpCertIntervalInfo c s e (Just slotsTillExp)

    opCertNodeAndOnDiskCounters
      :: OpCertOnDiskCounter
      -> Maybe OpCertNodeStateCounter
      -> OpCertNodeAndOnDiskCounterInformation
    opCertNodeAndOnDiskCounters o@(OpCertOnDiskCounter odc) (Just n@(OpCertNodeStateCounter nsc))
      | odc < nsc = OpCertOnDiskCounterBehindNodeState o n
      | odc > nsc + 1 = OpCertOnDiskCounterTooFarAheadOfNodeState o n
      | odc == nsc + 1 = OpCertOnDiskCounterAheadOfNodeState o n
      | otherwise = OpCertOnDiskCounterEqualToNodeState o n
    opCertNodeAndOnDiskCounters o Nothing = OpCertNoBlocksMintedYet o

    opCertExpiryUtcTime
      :: Tentative (EpochInfo (Either Text))
      -> GenesisParameters era
      -> OpCertEndingKesPeriod
      -> Maybe UTCTime
    opCertExpiryUtcTime eInfo gParams (OpCertEndingKesPeriod oCertExpiryKesPeriod) =
      let time =
            epochInfoSlotToUTCTime
              (tentative eInfo)
              (SystemStart $ protocolParamSystemStart gParams)
              (fromIntegral $ oCertExpiryKesPeriod * fromIntegral (protocolParamSlotsPerKESPeriod gParams))
       in case time of
            Left _ -> Nothing
            Right t -> Just t

    renderOpCertNodeAndOnDiskCounterInformation
      :: FilePath -> OpCertNodeAndOnDiskCounterInformation -> Doc AnsiStyle
    renderOpCertNodeAndOnDiskCounterInformation opCertFile = \case
      OpCertOnDiskCounterEqualToNodeState _ _ ->
        green "✓"
          <+> hang
            0
            ( vsep
                [ "The operational certificate counter agrees with the node protocol state counter"
                ]
            )
      OpCertOnDiskCounterAheadOfNodeState _ _ ->
        green "✓"
          <+> hang
            0
            ( vsep
                [ "The operational certificate counter ahead of the node protocol state counter by 1"
                ]
            )
      OpCertOnDiskCounterTooFarAheadOfNodeState onDiskC nodeStateC ->
        red "✗"
          <+> hang
            0
            ( vsep
                [ "The operational certificate counter too far ahead of the node protocol state counter in the operational certificate at: "
                    <> pretty opCertFile
                , "On disk operational certificate counter: " <> pretty (unOpCertOnDiskCounter onDiskC)
                , "Protocol state counter: " <> pretty (unOpCertNodeStateCounter nodeStateC)
                ]
            )
      OpCertOnDiskCounterBehindNodeState onDiskC nodeStateC ->
        red "✗"
          <+> hang
            0
            ( vsep
                [ "The protocol state counter is greater than the counter in the operational certificate at: "
                    <> pretty opCertFile
                , "On disk operational certificate counter: " <> pretty (unOpCertOnDiskCounter onDiskC)
                , "Protocol state counter: " <> pretty (unOpCertNodeStateCounter nodeStateC)
                ]
            )
      OpCertNoBlocksMintedYet (OpCertOnDiskCounter onDiskC) ->
        red "✗"
          <+> hang
            0
            ( vsep
                [ "No blocks minted so far with the operational certificate at: " <> pretty opCertFile
                , "On disk operational certificate counter: " <> pretty onDiskC
                ]
            )

    createQueryKesPeriodInfoOutput
      :: OpCertIntervalInformation
      -> OpCertNodeAndOnDiskCounterInformation
      -> Tentative (EpochInfo (Either Text))
      -> GenesisParameters era
      -> O.QueryKesPeriodInfoOutput
    createQueryKesPeriodInfoOutput oCertIntervalInfo oCertCounterInfo eInfo gParams =
      let (e, mStillExp) = case oCertIntervalInfo of
            OpCertWithinInterval _ end _ sTillExp -> (end, Just sTillExp)
            OpCertStartingKesPeriodIsInTheFuture _ end _ -> (end, Nothing)
            OpCertExpired _ end _ -> (end, Nothing)
            OpCertSomeOtherError _ end _ -> (end, Nothing)
          (onDiskCounter, mNodeCounter) = case oCertCounterInfo of
            OpCertOnDiskCounterEqualToNodeState d n -> (d, Just n)
            OpCertOnDiskCounterAheadOfNodeState d n -> (d, Just n)
            OpCertOnDiskCounterTooFarAheadOfNodeState d n -> (d, Just n)
            OpCertOnDiskCounterBehindNodeState d n -> (d, Just n)
            OpCertNoBlocksMintedYet d -> (d, Nothing)
       in O.QueryKesPeriodInfoOutput
            { O.qKesOpCertIntervalInformation = oCertIntervalInfo
            , O.qKesInfoNodeStateOperationalCertNo = mNodeCounter
            , O.qKesInfoOnDiskOperationalCertNo = onDiskCounter
            , O.qKesInfoMaxKesKeyEvolutions = fromIntegral $ protocolParamMaxKESEvolutions gParams
            , O.qKesInfoSlotsPerKesPeriod = fromIntegral $ protocolParamSlotsPerKESPeriod gParams
            , O.qKesInfoKesKeyExpiry =
                case mStillExp of
                  Just _ -> opCertExpiryUtcTime eInfo gParams e
                  Nothing -> Nothing
            }

    -- We get the operational certificate counter from the protocol state and check that
    -- it is equivalent to what we have on disk.
    opCertOnDiskAndStateCounters
      :: forall era
       . ()
      => Consensus.PraosProtocolSupportsNode (ConsensusProtocol era)
      => FromCBOR (Consensus.ChainDepState (ConsensusProtocol era))
      => ProtocolState era
      -> OperationalCertificate
      -> ExceptT QueryCmdError IO (OpCertOnDiskCounter, Maybe OpCertNodeStateCounter)
    opCertOnDiskAndStateCounters ptclState opCert@(OperationalCertificate _ stakePoolVKey) = do
      let onDiskOpCertCount = fromIntegral $ getOpCertCount opCert

      chainDepState <-
        pure (decodeProtocolState ptclState)
          & onLeft (left . QueryCmdProtocolStateDecodeFailure)

      -- We need the stake pool id to determine what the counter of our SPO
      -- should be.
      let opCertCounterMap = Consensus.getOpCertCounters (Proxy @(ConsensusProtocol era)) chainDepState
          StakePoolKeyHash blockIssuerHash =
            verificationKeyHash stakePoolVKey

      case Map.lookup (coerce blockIssuerHash) opCertCounterMap of
        -- Operational certificate exists in the protocol state
        -- so our ondisk op cert counter must be greater than or
        -- equal to what is in the node state.
        Just ptclStateCounter -> return (OpCertOnDiskCounter onDiskOpCertCount, Just $ OpCertNodeStateCounter ptclStateCounter)
        Nothing -> return (OpCertOnDiskCounter onDiskOpCertCount, Nothing)

renderOpCertIntervalInformation :: FilePath -> OpCertIntervalInformation -> Doc AnsiStyle
renderOpCertIntervalInformation opCertFile opCertInfo = case opCertInfo of
  OpCertWithinInterval _start _end _current _stillExp ->
    green "✓"
      <+> hang
        0
        ( vsep
            [ "Operational certificate's KES period is within the correct KES period interval"
            ]
        )
  OpCertStartingKesPeriodIsInTheFuture
    (OpCertStartingKesPeriod start)
    (OpCertEndingKesPeriod end)
    (CurrentKesPeriod current) ->
      red "✗"
        <+> hang
          0
          ( vsep
              [ "Node operational certificate at: "
                  <> pretty opCertFile
                  <> " has an incorrectly specified starting KES period. "
              , "Current KES period: " <> pretty current
              , "Operational certificate's starting KES period: " <> pretty start
              , "Operational certificate's expiry KES period: " <> pretty end
              ]
          )
  OpCertExpired _ (OpCertEndingKesPeriod end) (CurrentKesPeriod current) ->
    red "✗"
      <+> hang
        0
        ( vsep
            [ "Node operational certificate at: " <> pretty opCertFile <> " has expired. "
            , "Current KES period: " <> pretty current
            , "Operational certificate's expiry KES period: " <> pretty end
            ]
        )
  OpCertSomeOtherError
    (OpCertStartingKesPeriod start)
    (OpCertEndingKesPeriod end)
    (CurrentKesPeriod current) ->
      red "✗"
        <+> hang
          0
          ( vsep
              [ "An unknown error occurred with operational certificate at: " <> pretty opCertFile
              , "Current KES period: " <> pretty current
              , "Operational certificate's starting KES period: " <> pretty start
              , "Operational certificate's expiry KES period: " <> pretty end
              ]
          )

-- | Query the current and future parameters for a stake pool, including the retirement date.
-- Any of these may be empty (in which case a null will be displayed).
runQueryPoolStateCmd
  :: ()
  => Cmd.QueryPoolStateCmdArgs
  -> ExceptT QueryCmdError IO ()
runQueryPoolStateCmd
  Cmd.QueryPoolStateCmdArgs
    { Cmd.commons =
      Cmd.QueryCommons
        { Cmd.nodeConnInfo
        , Cmd.target
        }
    , Cmd.allOrOnlyPoolIds
    , Cmd.mOutFile
    } = do
    join $
      lift
        ( executeLocalStateQueryExpr nodeConnInfo target $ runExceptT $ do
            AnyCardanoEra era <- easyRunQueryCurrentEra

            sbe <-
              requireShelleyBasedEra era
                & onNothing (left QueryCmdByronEra)

            beo <- requireEon BabbageEra era

            let poolFilter = case allOrOnlyPoolIds of
                  All -> Nothing
                  Only poolIds -> Just $ fromList poolIds

            result <- easyRunQuery (queryPoolState beo poolFilter)

            pure $ shelleyBasedEraConstraints sbe (writePoolState mOutFile) result
        )
        & onLeft (left . QueryCmdAcquireFailure)
        & onLeft left

-- | Query the local mempool state
runQueryTxMempoolCmd
  :: ()
  => Cmd.QueryTxMempoolCmdArgs
  -> ExceptT QueryCmdError IO ()
runQueryTxMempoolCmd
  Cmd.QueryTxMempoolCmdArgs
    { Cmd.nodeConnInfo
    , Cmd.query
    , Cmd.mOutFile
    } = do
    localQuery <- case query of
      TxMempoolQueryTxExists tx -> do
        AnyCardanoEra era <-
          determineEra nodeConnInfo
            & modifyError QueryCmdAcquireFailure
        pure $ LocalTxMonitoringQueryTx $ TxIdInMode era tx
      TxMempoolQueryNextTx -> pure LocalTxMonitoringSendNextTx
      TxMempoolQueryInfo -> pure LocalTxMonitoringMempoolInformation

    result <- liftIO $ queryTxMonitoringLocal nodeConnInfo localQuery

    let output = Json.encodeJson result

    firstExceptT QueryCmdWriteFileError
      . newExceptT
      $ writeLazyByteStringOutput mOutFile output

runQuerySlotNumberCmd
  :: ()
  => Cmd.QuerySlotNumberCmdArgs
  -> ExceptT QueryCmdError IO ()
runQuerySlotNumberCmd
  Cmd.QuerySlotNumberCmdArgs
    { Cmd.commons =
      Cmd.QueryCommons
        { Cmd.nodeConnInfo
        , Cmd.target
        }
    , Cmd.utcTime
    } = do
    SlotNo slotNo <- utcTimeToSlotNo nodeConnInfo target utcTime
    liftIO . putStr $ show slotNo

runQueryRefScriptSizeCmd
  :: ()
  => Cmd.QueryRefScriptSizeCmdArgs
  -> ExceptT QueryCmdError IO ()
runQueryRefScriptSizeCmd
  Cmd.QueryRefScriptSizeCmdArgs
    { Cmd.commons =
      Cmd.QueryCommons
        { Cmd.nodeConnInfo
        , Cmd.target
        }
    , Cmd.transactionInputs
    , Cmd.format
    , Cmd.mOutFile
    } = do
    join $
      lift
        ( executeLocalStateQueryExpr nodeConnInfo target $ runExceptT $ do
            AnyCardanoEra era <- easyRunQueryCurrentEra

            sbe <-
              requireShelleyBasedEra era
                & onNothing (left QueryCmdByronEra)

            beo <- requireEon BabbageEra era

            utxo <- easyRunQuery (queryUtxo sbe $ QueryUTxOByTxIn transactionInputs)

            pure $
              writeFormattedOutput format mOutFile $
                RefInputScriptSize $
                  getReferenceInputsSizeForTxIds beo (toLedgerUTxO sbe utxo) transactionInputs
        )
        & onLeft (left . QueryCmdAcquireFailure)
        & onLeft left

newtype RefInputScriptSize = RefInputScriptSize {refInputScriptSize :: Int}
  deriving Generic
  deriving anyclass ToJSON

instance Pretty RefInputScriptSize where
  pretty (RefInputScriptSize s) = "Reference inputs scripts size is" <+> pretty s <+> "bytes."

-- | Obtain stake snapshot information for a pool, plus information about the total active stake.
-- This information can be used for leader slot calculation, for example, and has been requested by SPOs.
-- Obtaining the information directly is significantly more time and memory efficient than using a full ledger state dump.
runQueryStakeSnapshotCmd
  :: ()
  => Cmd.QueryStakeSnapshotCmdArgs
  -> ExceptT QueryCmdError IO ()
runQueryStakeSnapshotCmd
  Cmd.QueryStakeSnapshotCmdArgs
    { Cmd.commons =
      Cmd.QueryCommons
        { Cmd.nodeConnInfo
        , Cmd.target
        }
    , Cmd.allOrOnlyPoolIds
    , Cmd.mOutFile
    } = do
    join $
      lift
        ( executeLocalStateQueryExpr nodeConnInfo target $ runExceptT $ do
            AnyCardanoEra era <- easyRunQueryCurrentEra

            sbe <-
              requireShelleyBasedEra era
                & onNothing (left QueryCmdByronEra)

            let poolFilter = case allOrOnlyPoolIds of
                  All -> Nothing
                  Only poolIds -> Just $ fromList poolIds

            beo <- requireEon BabbageEra era

            result <- easyRunQuery (queryStakeSnapshot beo poolFilter)

            pure $ shelleyBasedEraConstraints sbe (writeStakeSnapshots mOutFile) result
        )
        & onLeft (left . QueryCmdAcquireFailure)
        & onLeft left

runQueryLedgerStateCmd
  :: ()
  => Cmd.QueryLedgerStateCmdArgs
  -> ExceptT QueryCmdError IO ()
runQueryLedgerStateCmd
  ( Cmd.QueryLedgerStateCmdArgs
      { Cmd.commons =
        Cmd.QueryCommons
          { Cmd.nodeConnInfo
          , Cmd.target
          }
      , Cmd.outputFormat
      , Cmd.mOutFile
      }
    ) = do
    contents <-
      join $
        lift
          ( executeLocalStateQueryExpr nodeConnInfo target $ runExceptT $ do
              AnyCardanoEra era <- easyRunQueryCurrentEra

              sbe <-
                requireShelleyBasedEra era
                  & onNothing (left QueryCmdByronEra)

              serialisedDebugLedgerState <- easyRunQuery (queryDebugLedgerState sbe)

              pure $
                shelleyBasedEraConstraints sbe $
                  outputFormat
                    & ( id
                          . Vary.on (\FormatJson -> ledgerStateAsJsonByteString serialisedDebugLedgerState)
                          . Vary.on (\FormatText -> ledgerStateAsTextByteString serialisedDebugLedgerState)
                          $ Vary.exhaustiveCase
                      )
          )
          & onLeft (left . QueryCmdAcquireFailure)
          & onLeft left

    firstExceptT QueryCmdWriteFileError
      . newExceptT
      $ writeLazyByteStringOutput mOutFile contents

ledgerStateAsJsonByteString
  :: IsShelleyBasedEra era
  => SerialisedDebugLedgerState era
  -> ExceptT QueryCmdError IO LBS.ByteString
ledgerStateAsJsonByteString serialisedDebugLedgerState =
  case decodeDebugLedgerState serialisedDebugLedgerState of
    Left (bs, _decoderError) -> firstExceptT QueryCmdHelpersError $ cborToTextByteString bs
    Right decodededgerState -> pure $ Aeson.encode decodededgerState <> "\n"

ledgerStateAsTextByteString
  :: Applicative f
  => SerialisedDebugLedgerState era -> f LBS.ByteString
ledgerStateAsTextByteString serialisedDebugLedgerState =
  let SerialisedDebugLedgerState serLedgerState = serialisedDebugLedgerState
   in pure $ unSerialised serLedgerState

runQueryLedgerPeerSnapshot
  :: ()
  => Cmd.QueryLedgerPeerSnapshotCmdArgs
  -> ExceptT QueryCmdError IO ()
runQueryLedgerPeerSnapshot
  Cmd.QueryLedgerPeerSnapshotCmdArgs
    { Cmd.commons =
      Cmd.QueryCommons
        { Cmd.nodeConnInfo
        , Cmd.target
        }
    , Cmd.outputFormat
    , Cmd.mOutFile
    } = do
    result <-
      join $
        lift
          ( executeLocalStateQueryExpr nodeConnInfo target $ runExceptT $ do
              AnyCardanoEra era <-
                lift queryCurrentEra
                  & onLeft (left . QueryCmdUnsupportedNtcVersion)

              sbe <-
                requireShelleyBasedEra era
                  & onNothing (left QueryCmdByronEra)

              result <- easyRunQuery (queryLedgerPeerSnapshot sbe)

              pure $
                shelleyBasedEraConstraints sbe $
                  case decodeBigLedgerPeerSnapshot result of
                    Left (bs, _decoderError) -> pure $ Left bs
                    Right snapshot -> pure $ Right snapshot
          )
          & onLeft (left . QueryCmdAcquireFailure)
          & onLeft left

    case result of
      Left (bs :: LBS.ByteString) -> do
        firstExceptT QueryCmdHelpersError $ pPrintCBOR bs
      Right (snapshot :: LedgerPeerSnapshot) -> do
        let output =
              outputFormat
                & ( id
                      . Vary.on (\FormatJson -> Json.encodeJson)
                      . Vary.on (\FormatYaml -> Json.encodeYaml)
                      $ Vary.exhaustiveCase
                  )
                $ snapshot

        firstExceptT QueryCmdWriteFileError
          . newExceptT
          $ writeLazyByteStringOutput mOutFile output

runQueryProtocolStateCmd
  :: ()
  => Cmd.QueryProtocolStateCmdArgs
  -> ExceptT QueryCmdError IO ()
runQueryProtocolStateCmd
  ( Cmd.QueryProtocolStateCmdArgs
      { Cmd.commons =
        Cmd.QueryCommons
          { Cmd.nodeConnInfo
          , Cmd.target
          }
      , Cmd.mOutFile
      }
    ) = do
    join $
      lift
        ( executeLocalStateQueryExpr nodeConnInfo target $ runExceptT $ do
            AnyCardanoEra era <- easyRunQueryCurrentEra

            sbe <-
              requireShelleyBasedEra era
                & onNothing (left QueryCmdByronEra)

            result <- easyRunQuery (queryProtocolState sbe)

            pure $ shelleyBasedEraConstraints sbe $ writeProtocolState sbe mOutFile result
        )
        & onLeft (left . QueryCmdAcquireFailure)
        & onLeft left

-- | Query the current delegations and reward accounts, filtered by a given
-- set of addresses, from a Shelley node via the local state query protocol.
runQueryStakeAddressInfoCmd
  :: ()
  => Cmd.QueryStakeAddressInfoCmdArgs
  -> ExceptT QueryCmdError IO ()
runQueryStakeAddressInfoCmd
  cmd@Cmd.QueryStakeAddressInfoCmdArgs
    { Cmd.commons =
      Cmd.QueryCommons
        { Cmd.nodeConnInfo
        , Cmd.target
        }
    , Cmd.mOutFile
    } = do
    AnyCardanoEra era <-
      firstExceptT
        QueryCmdAcquireFailure
        (newExceptT $ executeLocalStateQueryExpr nodeConnInfo target queryCurrentEra)
        & onLeft (left . QueryCmdUnsupportedNtcVersion)
    sbe <- requireShelleyBasedEra era & onNothing (left QueryCmdByronEra)

    said <- callQueryStakeAddressInfoCmd cmd

    writeStakeAddressInfo sbe said mOutFile

-- | Container for data returned by 'callQueryStakeAddressInfoCmd' where:
data StakeAddressInfoData = StakeAddressInfoData
  { rewards :: DelegationsAndRewards
  -- ^ Rewards: map of stake addresses to pool ID and rewards balance.
  , deposits :: Map StakeAddress Lovelace
  -- ^ Deposits: the stake address registration deposit.
  , gaDeposits :: Map L.GovActionId Lovelace
  -- ^ Gov Action Deposits: map of governance actions and their deposits associated
  --   with the reward account. Empty if not used in governance actions.
  , delegatees :: Map StakeAddress L.DRep
  -- ^ Delegatees: map of stake addresses and their vote delegation preference.
  }

callQueryStakeAddressInfoCmd
  :: ()
  => Cmd.QueryStakeAddressInfoCmdArgs
  -> ExceptT QueryCmdError IO StakeAddressInfoData
callQueryStakeAddressInfoCmd
  Cmd.QueryStakeAddressInfoCmdArgs
    { Cmd.commons =
      Cmd.QueryCommons
        { Cmd.nodeConnInfo = nodeConnInfo@LocalNodeConnectInfo{localNodeNetworkId = networkId}
        , Cmd.target
        }
    , Cmd.addr = StakeAddress _ addr
    } =
    do
      lift $ executeLocalStateQueryExpr nodeConnInfo target $ runExceptT $ do
        AnyCardanoEra era <- easyRunQueryCurrentEra

        sbe <-
          requireShelleyBasedEra era
            & onNothing (left QueryCmdByronEra)

        let stakeAddr = Set.singleton $ fromShelleyStakeCredential addr

        (stakeRewardAccountBalances, stakePools) <-
          easyRunQuery (queryStakeAddresses sbe stakeAddr networkId)

        beo <- requireEon BabbageEra era

        stakeDelegDeposits <- easyRunQuery (queryStakeDelegDeposits beo stakeAddr)

        (stakeVoteDelegatees, gaDeposits) <-
          monoidForEraInEonA
            (toCardanoEra beo)
            ( \ceo -> do
                stakeVoteDelegatees <- easyRunQuery (queryStakeVoteDelegatees ceo stakeAddr)

                govActionStates :: (Seq.Seq (L.GovActionState (ShelleyLedgerEra era))) <-
                  easyRunQuery $ queryProposals ceo Set.empty

                let gaDeposits =
                      conwayEraOnwardsConstraints ceo $
                        Map.fromList
                          [ (L.gasId gas, L.pProcDeposit proc)
                          | gas <- toList govActionStates
                          , let proc = L.gasProposalProcedure gas
                          , let rewardAccount = L.pProcReturnAddr proc
                                stakeCredential :: Api.StakeCredential = fromShelleyStakeCredential $ L.raCredential rewardAccount
                          , stakeCredential == fromShelleyStakeCredential addr
                          ]

                return (stakeVoteDelegatees, gaDeposits)
            )

        pure $
          StakeAddressInfoData
            (DelegationsAndRewards (stakeRewardAccountBalances, stakePools))
            (Map.mapKeys (makeStakeAddress networkId) stakeDelegDeposits)
            gaDeposits
            (Map.mapKeys (makeStakeAddress networkId) stakeVoteDelegatees)
      & onLeft (left . QueryCmdAcquireFailure)
      & onLeft left

-- -------------------------------------------------------------------------------------------------

writeStakeAddressInfo
  :: ShelleyBasedEra era
  -> StakeAddressInfoData
  -> Maybe (File () Out)
  -> ExceptT QueryCmdError IO ()
writeStakeAddressInfo
  sbe
  ( StakeAddressInfoData
      { rewards = DelegationsAndRewards (stakeAccountBalances, stakePools)
      , deposits = stakeDelegDeposits
      , gaDeposits = gaDeposits
      , delegatees = voteDelegatees
      }
    )
  mOutFile = do
    let output = Json.encodeJson $ jsonInfo sbe

    firstExceptT QueryCmdWriteFileError
      . newExceptT
      $ writeLazyByteStringOutput mOutFile output
   where
    jsonInfo :: ShelleyBasedEra era -> [Aeson.Value]
    jsonInfo =
      caseShelleyToBabbageOrConwayEraOnwards
        ( const $
            map
              ( \(addr, mBalance, mPoolId, _mDRep, mDeposit) ->
                  Aeson.object
                    [ "address" .= addr
                    , "delegation" .= mPoolId
                    , "rewardAccountBalance" .= mBalance
                    , "delegationDeposit" .= mDeposit
                    ]
              )
              merged
        )
        ( const $
            map
              ( \(addr, mBalance, mPoolId, mDRep, mDeposit) ->
                  Aeson.object
                    [ "address" .= addr
                    , "stakeDelegation" .= mPoolId
                    , "voteDelegation" .= fmap friendlyDRep mDRep
                    , "rewardAccountBalance" .= mBalance
                    , "stakeRegistrationDeposit" .= mDeposit
                    , "govActionDeposits" .= gaDeposits
                    ]
              )
              merged
        )

    friendlyDRep :: L.DRep -> Text
    friendlyDRep L.DRepAlwaysAbstain = "alwaysAbstain"
    friendlyDRep L.DRepAlwaysNoConfidence = "alwaysNoConfidence"
    friendlyDRep (L.DRepCredential cred) =
      L.credToText cred -- this will pring "keyHash-..." or "scriptHash-...", depending on the type of credential
    merged
      :: [(StakeAddress, Maybe Lovelace, Maybe PoolId, Maybe L.DRep, Maybe Lovelace)]
    merged =
      [ (addr, mBalance, mPoolId, mDRep, mDeposit)
      | addr <-
          toList
            ( Set.unions
                [ Map.keysSet stakeAccountBalances
                , Map.keysSet stakePools
                , Map.keysSet stakeDelegDeposits
                , Map.keysSet voteDelegatees
                ]
            )
      , let mBalance = Map.lookup addr stakeAccountBalances
            mPoolId = Map.lookup addr stakePools
            mDeposit = Map.lookup addr stakeDelegDeposits
            mDRep = Map.lookup addr voteDelegatees
      ]

writeStakeSnapshots
  :: forall era
   . Maybe (File () Out)
  -> SerialisedStakeSnapshots era
  -> ExceptT QueryCmdError IO ()
writeStakeSnapshots mOutFile qState = do
  StakeSnapshot snapshot <-
    pure (decodeStakeSnapshot qState)
      & onLeft (left . QueryCmdStakeSnapshotDecodeError)

  let output = Json.encodeJson snapshot

  firstExceptT QueryCmdWriteFileError
    . newExceptT
    $ writeLazyByteStringOutput mOutFile output

-- | This function obtains the pool parameters, equivalent to the following jq query on the output of query ledger-state
--   .nesEs.esLState.lsDPState.dpsPState.psStakePoolParams.<pool_id>
writePoolState
  :: forall era ledgerera
   . ()
  => ShelleyLedgerEra era ~ ledgerera
  => L.Era ledgerera
  => Maybe (File () Out)
  -> SerialisedPoolState era
  -> ExceptT QueryCmdError IO ()
writePoolState mOutFile serialisedCurrentEpochState = do
  PoolState poolState <-
    pure (decodePoolState serialisedCurrentEpochState)
      & onLeft (left . QueryCmdPoolStateDecodeError)

  let hks :: [L.KeyHash L.StakePool]
      hks =
        toList $
          Map.keysSet (L.psStakePoolParams poolState)
            <> Map.keysSet (L.psFutureStakePoolParams poolState)
            <> Map.keysSet (L.psRetiring poolState)

  let poolStates :: Map (L.KeyHash 'L.StakePool) (Params StandardCrypto)
      poolStates =
        fromList $
          hks
            <&> ( \hk ->
                    ( hk
                    , Params
                        { poolParameters = Map.lookup hk (L.psStakePoolParams poolState)
                        , futurePoolParameters = Map.lookup hk (L.psFutureStakePoolParams poolState)
                        , retiringEpoch = Map.lookup hk (L.psRetiring poolState)
                        }
                    )
                )

  let output = Json.encodeJson poolStates

  firstExceptT QueryCmdWriteFileError
    . newExceptT
    $ writeLazyByteStringOutput mOutFile output

writeProtocolState
  :: ShelleyBasedEra era
  -> Maybe (File () Out)
  -> ProtocolState era
  -> ExceptT QueryCmdError IO ()
writeProtocolState sbe mOutFile ps@(ProtocolState pstate) =
  case sbe of
    ShelleyBasedEraShelley ->
      case mOutFile of
        Nothing -> decodePState ps
        Just (File fpath) -> writePState fpath pstate
    ShelleyBasedEraAllegra ->
      case mOutFile of
        Nothing -> decodePState ps
        Just (File fpath) -> writePState fpath pstate
    ShelleyBasedEraMary ->
      case mOutFile of
        Nothing -> decodePState ps
        Just (File fpath) -> writePState fpath pstate
    ShelleyBasedEraAlonzo ->
      case mOutFile of
        Nothing -> decodePState ps
        Just (File fpath) -> writePState fpath pstate
    ShelleyBasedEraBabbage ->
      case mOutFile of
        Nothing -> decodePState ps
        Just (File fpath) -> writePState fpath pstate
    ShelleyBasedEraConway ->
      case mOutFile of
        Nothing -> decodePState ps
        Just (File fpath) -> writePState fpath pstate
 where
  writePState fpath pstate' =
    handleIOExceptT (QueryCmdWriteFileError . FileIOError fpath)
      . LBS.writeFile fpath
      $ unSerialised pstate'
  decodePState ps' =
    case decodeProtocolState ps' of
      Left (bs, _) -> firstExceptT QueryCmdHelpersError $ pPrintCBOR bs
      Right chainDepstate -> liftIO . LBS.putStrLn $ Aeson.encodePretty chainDepstate

writeFilteredUTxOs
  :: Api.ShelleyBasedEra era
  -> Vary [FormatCbor, FormatJson, FormatText]
  -> Maybe (File () Out)
  -> UTxO era
  -> ExceptT QueryCmdError IO ()
writeFilteredUTxOs sbe format mOutFile utxo = do
  let output =
        shelleyBasedEraConstraints sbe $
          format
            & ( id
                  . Vary.on (\FormatCbor -> Base16.encode . CBOR.serialize $ toLedgerUTxO sbe utxo)
                  . Vary.on (\FormatJson -> Json.encodeJson utxo)
                  . Vary.on (\FormatText -> strictTextToLazyBytestring $ filteredUTxOsToText sbe utxo)
                  $ Vary.exhaustiveCase
              )

  firstExceptT QueryCmdWriteFileError
    . newExceptT
    $ writeLazyByteStringOutput mOutFile output

filteredUTxOsToText :: Api.ShelleyBasedEra era -> UTxO era -> Text
filteredUTxOsToText sbe (UTxO utxo) = do
  mconcat
    [ Text.unlines [title, Text.replicate (Text.length title + 2) "-"]
    , Text.unlines $ case sbe of
        ShelleyBasedEraShelley ->
          map (utxoToText sbe) $ toList utxo
        ShelleyBasedEraAllegra ->
          map (utxoToText sbe) $ toList utxo
        ShelleyBasedEraMary ->
          map (utxoToText sbe) $ toList utxo
        ShelleyBasedEraAlonzo ->
          map (utxoToText sbe) $ toList utxo
        ShelleyBasedEraBabbage ->
          map (utxoToText sbe) $ toList utxo
        ShelleyBasedEraConway ->
          map (utxoToText sbe) $ toList utxo
    ]
 where
  title :: Text
  title =
    "                           TxHash                                 TxIx        Amount"

utxoToText
  :: Api.ShelleyBasedEra era
  -> (TxIn, TxOut CtxUTxO era)
  -> Text
utxoToText sbe txInOutTuple =
  case sbe of
    ShelleyBasedEraShelley ->
      let (TxIn (TxId txhash) (TxIx index), TxOut _ value _ _) = txInOutTuple
       in mconcat
            [ Text.decodeLatin1 (hashToBytesAsHex txhash)
            , textShowN 6 index
            , "        " <> printableValue value
            ]
    ShelleyBasedEraAllegra ->
      let (TxIn (TxId txhash) (TxIx index), TxOut _ value _ _) = txInOutTuple
       in mconcat
            [ Text.decodeLatin1 (hashToBytesAsHex txhash)
            , textShowN 6 index
            , "        " <> printableValue value
            ]
    ShelleyBasedEraMary ->
      let (TxIn (TxId txhash) (TxIx index), TxOut _ value _ _) = txInOutTuple
       in mconcat
            [ Text.decodeLatin1 (hashToBytesAsHex txhash)
            , textShowN 6 index
            , "        " <> printableValue value
            ]
    ShelleyBasedEraAlonzo ->
      let (TxIn (TxId txhash) (TxIx index), TxOut _ value mDatum _) = txInOutTuple
       in mconcat
            [ Text.decodeLatin1 (hashToBytesAsHex txhash)
            , textShowN 6 index
            , "        " <> printableValue value <> " + " <> Text.pack (show mDatum)
            ]
    ShelleyBasedEraBabbage ->
      let (TxIn (TxId txhash) (TxIx index), TxOut _ value mDatum _) = txInOutTuple
       in mconcat
            [ Text.decodeLatin1 (hashToBytesAsHex txhash)
            , textShowN 6 index
            , "        " <> printableValue value <> " + " <> Text.pack (show mDatum)
            ]
    ShelleyBasedEraConway ->
      let (TxIn (TxId txhash) (TxIx index), TxOut _ value mDatum _) = txInOutTuple
       in mconcat
            [ Text.decodeLatin1 (hashToBytesAsHex txhash)
            , textShowN 6 index
            , "        " <> printableValue value <> " + " <> Text.pack (show mDatum)
            ]
 where
  textShowN :: Show a => Int -> a -> Text
  textShowN len x =
    let str = show x
        slen = length str
     in Text.pack $ replicate (max 1 (len - slen)) ' ' ++ str

  printableValue :: TxOutValue era -> Text
  printableValue = \case
    TxOutValueByron (L.Coin i) -> Text.pack $ show i
    TxOutValueShelleyBased sbe2 val -> renderValue $ Api.fromLedgerValue sbe2 val

runQueryStakePoolsCmd
  :: ()
  => Cmd.QueryStakePoolsCmdArgs
  -> ExceptT QueryCmdError IO ()
runQueryStakePoolsCmd
  Cmd.QueryStakePoolsCmdArgs
    { Cmd.commons =
      Cmd.QueryCommons
        { Cmd.nodeConnInfo
        , Cmd.target
        }
    , Cmd.format
    , Cmd.mOutFile
    } = do
    join $
      lift
        ( executeLocalStateQueryExpr nodeConnInfo target $ runExceptT @QueryCmdError $ do
            AnyCardanoEra era <- easyRunQueryCurrentEra

            sbe <-
              requireShelleyBasedEra era
                & onNothing (left QueryCmdByronEra)

            poolIds <- easyRunQuery (queryStakePools sbe)

            pure $ writeStakePools format mOutFile poolIds
        )
        & onLeft (left . QueryCmdAcquireFailure)
        & onLeft left

-- TODO: replace with writeFormattedOutput
writeStakePools
  :: Vary [FormatJson, FormatText]
  -> Maybe (File () Out)
  -> Set PoolId
  -> ExceptT QueryCmdError IO ()
writeStakePools format mOutFile stakePools = do
  let output =
        format
          & ( id
                . Vary.on (\FormatJson -> writeJson)
                . Vary.on (\FormatText -> writeText)
                $ Vary.exhaustiveCase
            )

  firstExceptT QueryCmdWriteFileError
    . newExceptT
    $ writeLazyByteStringOutput mOutFile output
 where
  writeJson =
    Aeson.encodePretty stakePools
  writeText =
    LBS.unlines $
      map (strictTextToLazyBytestring . serialiseToBech32) $
        toList stakePools

writeFormattedOutput
  :: MonadIOTransError QueryCmdError t m
  => ToJSON a
  => Pretty a
  => Vary [FormatJson, FormatText]
  -> Maybe (File b Out)
  -> a
  -> t m ()
writeFormattedOutput format mOutFile value = do
  let output =
        format
          & ( id
                . Vary.on (\FormatJson -> Json.encodeJson value)
                . Vary.on (\FormatText -> fromString . docToString $ pretty value)
                $ Vary.exhaustiveCase
            )

  modifyError QueryCmdWriteFileError
    . hoistIOEither
    $ writeLazyByteStringOutput mOutFile output

runQueryStakeDistributionCmd
  :: ()
  => Cmd.QueryStakeDistributionCmdArgs
  -> ExceptT QueryCmdError IO ()
runQueryStakeDistributionCmd
  Cmd.QueryStakeDistributionCmdArgs
    { Cmd.commons =
      Cmd.QueryCommons
        { Cmd.nodeConnInfo
        , Cmd.target
        }
    , Cmd.format
    , Cmd.mOutFile
    } = do
    join $
      lift
        ( executeLocalStateQueryExpr nodeConnInfo target $ runExceptT $ do
            AnyCardanoEra era <- easyRunQueryCurrentEra

            sbe <-
              requireShelleyBasedEra era
                & onNothing (left QueryCmdByronEra)

            result <- easyRunQuery (queryStakeDistribution sbe)

            pure $ do
              writeStakeDistribution format mOutFile result
        )
        & onLeft (left . QueryCmdAcquireFailure)
        & onLeft left

writeStakeDistribution
  :: Vary [FormatJson, FormatText]
  -> Maybe (File () Out)
  -> Map PoolId Rational
  -> ExceptT QueryCmdError IO ()
writeStakeDistribution format mOutFile stakeDistrib = do
  let output =
        format
          & ( id
                . Vary.on (\FormatJson -> Json.encodeJson stakeDistrib)
                . Vary.on (\FormatText -> strictTextToLazyBytestring stakeDistributionText)
                $ Vary.exhaustiveCase
            )

  firstExceptT QueryCmdWriteFileError
    . newExceptT
    $ writeLazyByteStringOutput mOutFile output
 where
  stakeDistributionText =
    Text.unlines $
      [ title
      , Text.replicate (Text.length title + 2) "-"
      ]
        ++ [showStakeDistr poolId stakeFraction | (poolId, stakeFraction) <- toList stakeDistrib]
   where
    title :: Text
    title =
      "                           PoolId                                 Stake frac"
    showStakeDistr :: PoolId -> Rational -> Text
    showStakeDistr poolId stakeFraction =
      mconcat
        [ serialiseToBech32 poolId
        , "   "
        , Text.pack $ showEFloat (Just 3) (fromRational stakeFraction :: Double) ""
        ]

runQueryLeadershipScheduleCmd
  :: Cmd.QueryLeadershipScheduleCmdArgs
  -> ExceptT QueryCmdError IO ()
runQueryLeadershipScheduleCmd
  Cmd.QueryLeadershipScheduleCmdArgs
    { Cmd.commons =
      Cmd.QueryCommons
        { Cmd.nodeConnInfo
        , Cmd.target
        }
    , Cmd.genesisFp = GenesisFile genFile
    , Cmd.poolColdVerKeyFile
    , Cmd.vrkSkeyFp
    , Cmd.whichSchedule
    , Cmd.format
    , Cmd.mOutFile
    } = do
    poolid <- getHashFromStakePoolKeyHashSource poolColdVerKeyFile

    vrkSkey <-
      modifyError QueryCmdTextEnvelopeReadError . hoistIOEither $
        readFileTextEnvelope (AsSigningKey AsVrfKey) vrkSkeyFp

    shelleyGenesis <-
      modifyError QueryCmdGenesisReadError $
        decodeShelleyGenesisFile genFile

    join $
      lift
        ( executeLocalStateQueryExpr nodeConnInfo target $ runExceptT $ do
            AnyCardanoEra era <- easyRunQueryCurrentEra

            sbe <-
              requireShelleyBasedEra era
                & onNothing (left QueryCmdByronEra)

            pparams <- easyRunQuery (queryProtocolParameters sbe)
            ptclState <- easyRunQuery (queryProtocolState sbe)
            eraHistory <- easyRunQueryEraHistory

            let eInfo = toEpochInfo eraHistory

            curentEpoch <- easyRunQuery (queryEpoch sbe)

            case whichSchedule of
              CurrentEpoch -> do
                beo <- requireEon BabbageEra era

                serCurrentEpochState <-
                  easyRunQuery (queryPoolDistribution beo (Just (Set.singleton poolid)))

                pure $ do
                  schedule <-
                    firstExceptT QueryCmdLeaderShipError $
                      hoistEither $
                        shelleyBasedEraConstraints sbe $
                          currentEpochEligibleLeadershipSlots
                            sbe
                            shelleyGenesis
                            eInfo
                            pparams
                            ptclState
                            poolid
                            vrkSkey
                            serCurrentEpochState
                            curentEpoch

                  writeSchedule mOutFile eInfo shelleyGenesis schedule
              NextEpoch -> do
                serCurrentEpochState <- easyRunQuery (queryCurrentEpochState sbe)

                pure $ do
                  tip <- liftIO $ getLocalChainTip nodeConnInfo

                  schedule <-
                    firstExceptT QueryCmdLeaderShipError $
                      hoistEither $
                        shelleyBasedEraConstraints sbe $
                          nextEpochEligibleLeadershipSlots
                            sbe
                            shelleyGenesis
                            serCurrentEpochState
                            ptclState
                            poolid
                            vrkSkey
                            pparams
                            eInfo
                            (tip, curentEpoch)

                  writeSchedule mOutFile eInfo shelleyGenesis schedule
        )
        & onLeft (left . QueryCmdAcquireFailure)
        & onLeft left
   where
    writeSchedule mOutFile' eInfo shelleyGenesis schedule = do
      let start = SystemStart $ sgSystemStart shelleyGenesis
          output =
            format
              & ( id
                    . Vary.on (\FormatJson -> Json.encodeJson $ leadershipScheduleToJson schedule eInfo start)
                    . Vary.on (\FormatText -> strictTextToLazyBytestring $ leadershipScheduleToText schedule eInfo start)
                    . Vary.on (\FormatYaml -> Json.encodeYaml $ leadershipScheduleToJson schedule eInfo start)
                    $ Vary.exhaustiveCase
                )

      firstExceptT QueryCmdWriteFileError
        . newExceptT
        $ writeLazyByteStringOutput mOutFile' output

    leadershipScheduleToText
      :: Set SlotNo
      -> EpochInfo (Either Text)
      -> SystemStart
      -> Text
    leadershipScheduleToText leadershipSlots eInfo sStart = do
      Text.unlines $
        title
          : Text.replicate (Text.length title + 2) "-"
          : [showLeadershipSlot slot eInfo sStart | slot <- toList leadershipSlots]
     where
      title :: Text
      title =
        "     SlotNo                          UTC Time              "

      showLeadershipSlot
        :: SlotNo
        -> EpochInfo (Either Text)
        -> SystemStart
        -> Text
      showLeadershipSlot lSlot@(SlotNo sn) eInfo' sStart' =
        case epochInfoSlotToUTCTime eInfo' sStart' lSlot of
          Right slotTime ->
            mconcat
              [ "     "
              , Text.pack $ show sn
              , "                   "
              , Text.pack $ show slotTime
              ]
          Left err ->
            mconcat
              [ "     "
              , Text.pack $ show sn
              , "                   "
              , err
              ]
    leadershipScheduleToJson
      :: Set SlotNo
      -> EpochInfo (Either Text)
      -> SystemStart
      -> [Aeson.Value]
    leadershipScheduleToJson leadershipSlots eInfo sStart =
      showLeadershipSlot <$> List.sort (toList leadershipSlots)
     where
      showLeadershipSlot :: SlotNo -> Aeson.Value
      showLeadershipSlot lSlot@(SlotNo sn) =
        case epochInfoSlotToUTCTime eInfo sStart lSlot of
          Right slotTime ->
            Aeson.object
              [ "slotNumber" Aeson..= sn
              , "slotTime" Aeson..= slotTime
              ]
          Left err ->
            Aeson.object
              [ "slotNumber" Aeson..= sn
              , "error" Aeson..= Text.unpack err
              ]

runQueryConstitution
  :: Cmd.QueryNoArgCmdArgs era
  -> ExceptT QueryCmdError IO ()
runQueryConstitution
  Cmd.QueryNoArgCmdArgs
    { Cmd.eon
    , Cmd.commons =
      Cmd.QueryCommons
        { Cmd.nodeConnInfo
        , Cmd.target
        }
    , Cmd.mOutFile
    } = conwayEraOnwardsConstraints eon $ do
    constitution <- runQuery nodeConnInfo target $ queryConstitution eon
    writeOutput mOutFile constitution

runQueryGovState
  :: Cmd.QueryNoArgCmdArgs era
  -> ExceptT QueryCmdError IO ()
runQueryGovState
  Cmd.QueryNoArgCmdArgs
    { Cmd.eon
    , Cmd.commons =
      Cmd.QueryCommons
        { Cmd.nodeConnInfo
        , Cmd.target
        }
    , Cmd.mOutFile
    } = conwayEraOnwardsConstraints eon $ do
    govState <- runQuery nodeConnInfo target $ queryGovState eon
    writeOutput mOutFile govState

runQueryRatifyState
  :: Cmd.QueryNoArgCmdArgs era
  -> ExceptT QueryCmdError IO ()
runQueryRatifyState
  Cmd.QueryNoArgCmdArgs
    { Cmd.eon
    , Cmd.commons =
      Cmd.QueryCommons
        { Cmd.nodeConnInfo
        , Cmd.target
        }
    , Cmd.mOutFile
    } = conwayEraOnwardsConstraints eon $ do
    ratifyState <- runQuery nodeConnInfo target $ queryRatifyState eon
    writeOutput mOutFile ratifyState

runQueryFuturePParams
  :: Cmd.QueryNoArgCmdArgs era
  -> ExceptT QueryCmdError IO ()
runQueryFuturePParams
  Cmd.QueryNoArgCmdArgs
    { Cmd.eon
    , Cmd.commons =
      Cmd.QueryCommons
        { Cmd.nodeConnInfo
        , Cmd.target
        }
    , Cmd.mOutFile
    } = conwayEraOnwardsConstraints eon $ do
    futurePParams <- runQuery nodeConnInfo target $ queryFuturePParams eon
    writeOutput mOutFile futurePParams

runQueryDRepState
  :: Cmd.QueryDRepStateCmdArgs era
  -> ExceptT QueryCmdError IO ()
runQueryDRepState
  Cmd.QueryDRepStateCmdArgs
    { Cmd.eon
    , Cmd.drepHashSources = drepHashSources'
    , Cmd.includeStake
    , Cmd.commons =
      Cmd.QueryCommons
        { Cmd.nodeConnInfo
        , Cmd.target
        }
    , Cmd.mOutFile
    } = conwayEraOnwardsConstraints eon $ do
    let drepHashSources = case drepHashSources' of All -> []; Only l -> l
    drepCreds <- modifyError QueryCmdDRepKeyError $ mapM readDRepCredential drepHashSources

    drepState <- runQuery nodeConnInfo target $ queryDRepState eon $ fromList drepCreds

    drepStakeDistribution <-
      case includeStake of
        Cmd.WithStake ->
          runQuery nodeConnInfo target $
            queryDRepStakeDistribution eon (fromList $ L.DRepCredential <$> drepCreds)
        Cmd.NoStake -> return mempty

    let assocs :: [(L.Credential L.DRepRole, L.DRepState)] = Map.assocs drepState
        toWrite = toDRepStateOutput drepStakeDistribution <$> assocs

    writeOutput mOutFile toWrite
   where
    toDRepStateOutput
      :: ()
      => Map L.DRep Lovelace
      -> (L.Credential L.DRepRole, L.DRepState)
      -> QueryDRepStateOutput
    toDRepStateOutput stakeDistr (cred, ds) =
      QueryDRepStateOutput
        cred
        (ds ^. L.drepExpiryL)
        (strictMaybeToMaybe $ ds ^. L.drepAnchorL)
        (ds ^. L.drepDepositL)
        includeStake
        (Map.lookup (L.DRepCredential cred) stakeDistr)

runQueryDRepStakeDistribution
  :: Cmd.QueryDRepStakeDistributionCmdArgs era
  -> ExceptT QueryCmdError IO ()
runQueryDRepStakeDistribution
  Cmd.QueryDRepStakeDistributionCmdArgs
    { Cmd.eon
    , Cmd.commons =
      Cmd.QueryCommons
        { Cmd.nodeConnInfo
        , Cmd.target
        }
    , Cmd.drepHashSources = drepHashSources'
    , Cmd.mOutFile
    } = conwayEraOnwardsConstraints eon $ do
    let drepFromSource =
          fmap L.DRepCredential
            . firstExceptT QueryCmdDRepKeyError
            . readDRepCredential
        drepHashSources = case drepHashSources' of
          All -> []
          Only l -> l
    dreps <- fromList <$> mapM drepFromSource drepHashSources

    drepStakeDistribution <- runQuery nodeConnInfo target $ queryDRepStakeDistribution eon dreps
    writeOutput mOutFile $
      Map.assocs drepStakeDistribution

runQuerySPOStakeDistribution
  :: Cmd.QuerySPOStakeDistributionCmdArgs era
  -> ExceptT QueryCmdError IO ()
runQuerySPOStakeDistribution
  Cmd.QuerySPOStakeDistributionCmdArgs
    { Cmd.eon
    , Cmd.commons =
      commons@Cmd.QueryCommons
        { Cmd.nodeConnInfo = nodeConnInfo@LocalNodeConnectInfo{localNodeNetworkId = networkId}
        , Cmd.target
        }
    , Cmd.spoHashSources = spoHashSources'
    , Cmd.mOutFile
    } = conwayEraOnwardsConstraints eon $ do
    let spoFromSource = firstExceptT QueryCmdSPOKeyError . readSPOCredential
        spoHashSources = case spoHashSources' of
          All -> []
          Only l -> l

    spos <- fromList <$> mapM spoFromSource spoHashSources

    let beo = convert eon

    spoStakeDistribution :: Map (L.KeyHash L.StakePool) L.Coin <-
      runQuery nodeConnInfo target $ querySPOStakeDistribution eon spos
    let poolIds :: Set (Hash StakePoolKey) = Set.fromList $ map StakePoolKeyHash $ Map.keys spoStakeDistribution

    serialisedPoolState :: SerialisedPoolState era <-
      runQuery nodeConnInfo target $ queryPoolState beo (Just poolIds)

    PoolState (poolState :: L.PState (ShelleyLedgerEra era)) <-
      pure (decodePoolState serialisedPoolState)
        & onLeft (left . QueryCmdPoolStateDecodeError)

    let addressesAndRewards
          :: Map
               StakeAddress
               (L.KeyHash L.StakePool) =
            Map.fromList
              [ ( makeStakeAddress networkId . fromShelleyStakeCredential . L.raCredential . L.ppRewardAccount $ addr
                , keyHash
                )
              | (keyHash, addr) <- Map.toList $ L.psStakePoolParams poolState
              ]

        mkQueryStakeAddressInfoCmdArgs addr =
          Cmd.QueryStakeAddressInfoCmdArgs
            { Cmd.commons = commons
            , addr
            , mOutFile -- unused anyway. TODO tighten this by removing the field.
            }

    spoToDelegatee <-
      Map.fromList . concat
        <$> traverse
          ( \stakeAddr -> do
              info <- callQueryStakeAddressInfoCmd $ mkQueryStakeAddressInfoCmdArgs stakeAddr
              return $
                [ (spo, delegatee)
                | (Just spo, delegatee) <-
                    map (first (`Map.lookup` addressesAndRewards)) $ Map.toList $ delegatees info
                ]
          )
          (Map.keys addressesAndRewards)

    let toWrite =
          [ ( spo
            , coin
            , Map.lookup spo spoToDelegatee
            )
          | (spo, coin) <- Map.assocs spoStakeDistribution
          ]

    writeOutput mOutFile toWrite

runQueryCommitteeMembersState
  :: Cmd.QueryCommitteeMembersStateCmdArgs era
  -> ExceptT QueryCmdError IO ()
runQueryCommitteeMembersState
  Cmd.QueryCommitteeMembersStateCmdArgs
    { Cmd.eon
    , Cmd.commons =
      Cmd.QueryCommons
        { Cmd.nodeConnInfo
        , Cmd.target
        }
    , Cmd.committeeColdKeys = coldCredKeys
    , Cmd.committeeHotKeys = hotCredKeys
    , Cmd.memberStatuses = memberStatuses
    , Cmd.mOutFile
    } = conwayEraOnwardsConstraints eon $ do
    let coldKeysFromVerKeyHashOrFile =
          modifyError QueryCmdCommitteeColdKeyError
            . readVerificationKeyOrHashOrFileOrScriptHash AsCommitteeColdKey unCommitteeColdKeyHash
    coldKeys <- fromList <$> mapM coldKeysFromVerKeyHashOrFile coldCredKeys

    let hotKeysFromVerKeyHashOrFile =
          modifyError QueryCmdCommitteeHotKeyError
            . readVerificationKeyOrHashOrFileOrScriptHash AsCommitteeHotKey unCommitteeHotKeyHash
    hotKeys <- fromList <$> mapM hotKeysFromVerKeyHashOrFile hotCredKeys

    committeeState <-
      runQuery nodeConnInfo target $
        queryCommitteeMembersState eon coldKeys hotKeys (fromList memberStatuses)
    writeOutput mOutFile $ A.toJSON committeeState

runQueryTreasuryValue
  :: Cmd.QueryTreasuryValueCmdArgs era
  -> ExceptT QueryCmdError IO ()
runQueryTreasuryValue
  Cmd.QueryTreasuryValueCmdArgs
    { Cmd.eon
    , Cmd.commons =
      Cmd.QueryCommons
        { Cmd.nodeConnInfo
        , Cmd.target
        }
    , Cmd.mOutFile
    } = conwayEraOnwardsConstraints eon $ do
    L.AccountState (L.Coin treasury) _reserves <-
      runQuery nodeConnInfo target $ queryAccountState eon

    let output = LBS.pack $ show treasury

    firstExceptT QueryCmdWriteFileError
      . newExceptT
      $ writeLazyByteStringOutput mOutFile output

runQueryProposals
  :: Cmd.QueryProposalsCmdArgs era
  -> ExceptT QueryCmdError IO ()
runQueryProposals
  Cmd.QueryProposalsCmdArgs
    { Cmd.eon
    , Cmd.commons =
      Cmd.QueryCommons
        { Cmd.nodeConnInfo
        , Cmd.target
        }
    , Cmd.govActionIds = govActionIds'
    , Cmd.mOutFile
    } = conwayEraOnwardsConstraints eon $ do
    let govActionIds = case govActionIds' of
          All -> []
          Only l -> l

    govActionStates :: (Seq.Seq (L.GovActionState (ShelleyLedgerEra era))) <-
      runQuery nodeConnInfo target $ queryProposals eon $ Set.fromList govActionIds

    writeOutput mOutFile govActionStates

runQueryEraHistoryCmd :: Cmd.QueryEraHistoryCmdArgs -> ExceptT QueryCmdError IO ()
runQueryEraHistoryCmd
  Cmd.QueryEraHistoryCmdArgs
    { Cmd.commons =
      Cmd.QueryCommons
        { Cmd.nodeConnInfo
        , Cmd.target
        }
    , Cmd.mOutFile
    } = do
    eraHistory <-
      lift
        ( executeLocalStateQueryExpr nodeConnInfo target $
            runExceptT $
              lift queryEraHistory & onLeft (left . QueryCmdUnsupportedNtcVersion)
        )
        & onLeft (left . QueryCmdAcquireFailure)
        & onLeft left

    let output = textEnvelopeToJSON Nothing eraHistory

    firstExceptT QueryCmdWriteFileError
      . newExceptT
      $ writeLazyByteStringOutput mOutFile output

runQueryStakePoolDefaultVote
  :: Cmd.QueryStakePoolDefaultVoteCmdArgs era
  -> ExceptT QueryCmdError IO ()
runQueryStakePoolDefaultVote
  Cmd.QueryStakePoolDefaultVoteCmdArgs
    { Cmd.eon
    , Cmd.commons =
      Cmd.QueryCommons
        { Cmd.nodeConnInfo
        , Cmd.target
        }
    , Cmd.spoHashSources
    , Cmd.mOutFile
    } = conwayEraOnwardsConstraints eon $ do
    let spoFromSource = firstExceptT QueryCmdSPOKeyError . readSPOCredential
    spo <- spoFromSource spoHashSources

    defVote :: L.DefaultVote <-
      runQuery nodeConnInfo target $ queryStakePoolDefaultVote eon spo

    let output = Aeson.encode defVote

    firstExceptT QueryCmdWriteFileError
      . newExceptT
      $ writeLazyByteStringOutput mOutFile output

runQuery
  :: LocalNodeConnectInfo
  -> Consensus.Target ChainPoint
  -> LocalStateQueryExpr
       BlockInMode
       ChainPoint
       QueryInMode
       ()
       IO
       ( Either
           UnsupportedNtcVersionError
           (Either Consensus.EraMismatch a)
       )
  -> ExceptT QueryCmdError IO a
runQuery localNodeConnInfo target query =
  firstExceptT
    QueryCmdAcquireFailure
    (newExceptT $ executeLocalStateQueryExpr localNodeConnInfo target query)
    & onLeft (left . QueryCmdUnsupportedNtcVersion)
    & onLeft (left . QueryCmdEraMismatch)

writeOutput
  :: ToJSON b
  => Maybe (File a Out)
  -> b
  -> ExceptT QueryCmdError IO ()
writeOutput mOutFile content = case mOutFile of
  Nothing -> liftIO . LBS.putStrLn . Aeson.encodePretty $ content
  Just (File f) ->
    handleIOExceptT (QueryCmdWriteFileError . FileIOError f) $
      LBS.writeFile f (Aeson.encodePretty content)

-- Helpers

toEpochInfo :: EraHistory -> EpochInfo (Either Text)
toEpochInfo (EraHistory interpreter) =
  hoistEpochInfo (first (Text.pack . show) . runExcept) $
    Consensus.interpreterToEpochInfo interpreter

-- | A value that is tentative or produces a tentative value if used.  These values
-- are considered accurate only if some future event such as a hard fork does not
-- render them invalid.
newtype Tentative a = Tentative {tentative :: a} deriving (Eq, Show)

-- | Get an Epoch Info that computes tentative values.  The values computed are
-- tentative because it uses an interpreter that is extended past the horizon.
-- This interpreter will compute accurate values into the future as long as a
-- a hard fork does not happen in the intervening time.  Those values are thus
-- "tentative" because they can change in the event of a hard fork.
toTentativeEpochInfo :: EraHistory -> Tentative (EpochInfo (Either Text))
toTentativeEpochInfo (EraHistory interpreter) =
  Tentative $
    hoistEpochInfo (first (Text.pack . show) . runExcept) $
      Consensus.interpreterToEpochInfo (Consensus.unsafeExtendSafeZone interpreter)

-- | Get slot number for timestamp, or an error if the UTC timestamp is before 'SystemStart' or after N+1 era
utcTimeToSlotNo
  :: LocalNodeConnectInfo
  -> Consensus.Target ChainPoint
  -> UTCTime
  -> ExceptT QueryCmdError IO SlotNo
utcTimeToSlotNo localNodeConnInfo target utcTime =
  lift
    ( executeLocalStateQueryExpr localNodeConnInfo target $ runExceptT $ do
        systemStart <- easyRunQuerySystemStart
        eraHistory <- easyRunQueryEraHistory

        let relTime = toRelativeTime systemStart utcTime

        pure (Api.getSlotForRelativeTime relTime eraHistory)
          & onLeft (left . QueryCmdPastHorizon)
    )
    & onLeft (left . QueryCmdAcquireFailure)
    & onLeft left

requireEon
  :: forall eon era minEra m
   . (Eon eon, Monad m)
  => CardanoEra minEra
  -- ^ minimal required era i.e. for 'ConwayEraOnwards' eon it's 'Conway'
  -> CardanoEra era
  -- ^ node era
  -> ExceptT QueryCmdError m (eon era)
-- TODO: implement 'Bounded' for `Some eon` and remove 'minEra'
requireEon minEra era =
  hoistMaybe
    (mkEraMismatchError NodeEraMismatchError{nodeEra = era, era = minEra})
    (forEraMaybeEon era)

strictTextToLazyBytestring :: Text -> LBS.ByteString
strictTextToLazyBytestring t = BS.fromChunks [Text.encodeUtf8 t]

easyRunQueryCurrentEra
  :: ExceptT QueryCmdError (LocalStateQueryExpr block point QueryInMode r IO) AnyCardanoEra
easyRunQueryCurrentEra = lift queryCurrentEra & onLeft (left . QueryCmdUnsupportedNtcVersion)

easyRunQueryEraHistory
  :: ExceptT QueryCmdError (LocalStateQueryExpr block point QueryInMode r IO) EraHistory
easyRunQueryEraHistory = lift queryEraHistory & onLeft (left . QueryCmdUnsupportedNtcVersion)

easyRunQuerySystemStart
  :: ExceptT QueryCmdError (LocalStateQueryExpr block point QueryInMode r IO) SystemStart
easyRunQuerySystemStart = lift querySystemStart & onLeft (left . QueryCmdUnsupportedNtcVersion)

easyRunQuery
  :: ()
  => Monad m
  => m (Either UnsupportedNtcVersionError (Either Consensus.EraMismatch a))
  -> ExceptT QueryCmdError m a
easyRunQuery q =
  lift q
    & onLeft (left . QueryCmdUnsupportedNtcVersion)
    & onLeft (left . QueryCmdEraMismatch)
