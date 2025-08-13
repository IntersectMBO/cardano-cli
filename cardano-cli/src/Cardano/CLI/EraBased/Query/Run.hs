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
import Cardano.Api.Experimental (obtainCommonConstraints)
import Cardano.Api.Experimental qualified as Exp
import Cardano.Api.Ledger (strictMaybeToMaybe)
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Network qualified as Consensus

import Cardano.Binary qualified as CBOR
import Cardano.CLI.Compatible.Exception
import Cardano.CLI.Compatible.Json.Friendly (friendlyDRep)
import Cardano.CLI.EraBased.Genesis.Internal.Common
import Cardano.CLI.EraBased.Query.Command qualified as Cmd
import Cardano.CLI.Helper
import Cardano.CLI.Json.Encode qualified as Json
import Cardano.CLI.Read
  ( getHashFromStakePoolKeyHashSource
  )
import Cardano.CLI.Type.Common
import Cardano.CLI.Type.Error.QueryCmdError
import Cardano.CLI.Type.Key
  ( readDRepCredential
  , readSPOCredential
  , readVerificationKeyOrHashOrFileOrScriptHash
  )
import Cardano.CLI.Type.Output (QueryDRepStateOutput (..))
import Cardano.CLI.Type.Output qualified as O
import Cardano.Crypto.Hash (hashToBytesAsHex)
import Cardano.Ledger.Api.State.Query qualified as L
import Cardano.Ledger.Conway.State (ChainAccountState (..))
import Cardano.Slotting.EpochInfo (EpochInfo (..), epochInfoSlotToUTCTime, hoistEpochInfo)
import Cardano.Slotting.Time (RelativeTime (..), toRelativeTime)
import Ouroboros.Consensus.Cardano.Block (CardanoBlock, StandardCrypto)
import Ouroboros.Consensus.HardFork.Combinator.NetworkVersion
import Ouroboros.Consensus.Node.NetworkProtocolVersion
import Ouroboros.Consensus.Shelley.Ledger.NetworkProtocolVersion

import RIO hiding (toList)

import Control.Monad.Morph
import Data.Aeson as Aeson
import Data.ByteString.Base16.Lazy qualified as Base16
import Data.ByteString.Lazy qualified as BS
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Coerce (coerce)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.SOP.Index
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.IO qualified as T
import Data.Text.Lazy.IO qualified as LT
import Data.Time.Clock
import GHC.Exts (IsList (..))
import Numeric (showEFloat)
import Prettyprinter
import Prettyprinter.Render.Terminal (AnsiStyle)
import System.IO qualified as IO
import Text.Printf (printf)
import Vary

runQueryCmds :: Cmd.QueryCmds era -> CIO e ()
runQueryCmds = \case
  Cmd.QueryCommitteeMembersStateCmd args -> runQueryCommitteeMembersState args
  Cmd.QueryConstitutionCmd args -> runQueryConstitution args
  Cmd.QueryDRepStakeDistributionCmd args -> runQueryDRepStakeDistribution args
  Cmd.QueryDRepStateCmd args -> runQueryDRepState args
  Cmd.QueryEraHistoryCmd args -> runQueryEraHistoryCmd args
  Cmd.QueryFuturePParamsCmd args -> runQueryFuturePParams args
  Cmd.QueryGovStateCmd args -> runQueryGovState args
  Cmd.QueryKesPeriodInfoCmd args -> runQueryKesPeriodInfoCmd args
  Cmd.QueryLeadershipScheduleCmd args -> runQueryLeadershipScheduleCmd args
  Cmd.QueryLedgerPeerSnapshotCmd args -> runQueryLedgerPeerSnapshot args
  Cmd.QueryLedgerStateCmd args -> runQueryLedgerStateCmd args
  Cmd.QueryPoolStateCmd args -> runQueryPoolStateCmd args
  Cmd.QueryProposalsCmd args -> runQueryProposals args
  Cmd.QueryProtocolParametersCmd args -> runQueryProtocolParametersCmd args
  Cmd.QueryProtocolStateCmd args -> runQueryProtocolStateCmd args
  Cmd.QueryRatifyStateCmd args -> runQueryRatifyState args
  Cmd.QueryRefScriptSizeCmd args -> runQueryRefScriptSizeCmd args
  Cmd.QuerySlotNumberCmd args -> runQuerySlotNumberCmd args
  Cmd.QuerySPOStakeDistributionCmd args -> runQuerySPOStakeDistribution args
  Cmd.QueryStakeAddressInfoCmd args -> runQueryStakeAddressInfoCmd args
  Cmd.QueryStakeDistributionCmd args -> runQueryStakeDistributionCmd args
  Cmd.QueryStakePoolDefaultVoteCmd args -> runQueryStakePoolDefaultVote args
  Cmd.QueryStakePoolsCmd args -> runQueryStakePoolsCmd args
  Cmd.QueryStakeSnapshotCmd args -> runQueryStakeSnapshotCmd args
  Cmd.QueryTipCmd args -> runQueryTipCmd args
  Cmd.QueryTreasuryValueCmd args -> runQueryTreasuryValue args
  Cmd.QueryTxMempoolCmd args -> runQueryTxMempoolCmd args
  Cmd.QueryUTxOCmd args -> runQueryUTxOCmd args

runQueryProtocolParametersCmd
  :: ()
  => Cmd.QueryProtocolParametersCmdArgs
  -> CIO e ()
runQueryProtocolParametersCmd
  Cmd.QueryProtocolParametersCmdArgs
    { Cmd.nodeConnInfo
    , Cmd.outputFormat
    , Cmd.mOutFile
    } = do
    anyCEra@(AnyCardanoEra cEra) <- fromExceptTCli $ determineEra nodeConnInfo
    case forEraInEonMaybe cEra id of
      Nothing -> throwCliError $ QueryCmdEraNotSupported anyCEra
      Just sbe -> do
        let qInMode = QueryInEra $ QueryInShelleyBasedEra sbe Api.QueryProtocolParameters

        pparams <-
          fromExceptTCli $
            executeQueryAnyMode nodeConnInfo qInMode

        let output =
              shelleyBasedEraConstraints sbe
                $ outputFormat
                  & ( id
                        . Vary.on (\FormatJson -> Json.encodeJson)
                        . Vary.on (\FormatYaml -> Json.encodeYaml)
                        $ Vary.exhaustiveCase
                    )
                $ pparams

        fromEitherIOCli @(FileError ()) $
          writeLazyByteStringOutput mOutFile output

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
  -> CIO e ()
runQueryTipCmd
  ( Cmd.QueryTipCmdArgs
      { Cmd.commons =
        Cmd.QueryCommons
          { Cmd.nodeConnInfo
          , Cmd.target
          }
      , Cmd.outputFormat
      , Cmd.mOutFile
      }
    ) = do
    eLocalState <- fromEitherIOCli $
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
      case mLocalState >>= O.mChainTip of
        Nothing -> queryChainTipViaChainSync nodeConnInfo
        Just tip -> pure tip
    -- The chain tip is unavailable via local state query because we are connecting with an older
    -- node to client protocol so we use chain sync instead which necessitates another connection.
    -- At some point when we can stop supporting the older node to client protocols, this fallback
    -- can be removed.

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

    let output =
          outputFormat
            & ( id
                  . Vary.on (\FormatJson -> Json.encodeJson)
                  . Vary.on (\FormatYaml -> Json.encodeYaml)
                  $ Vary.exhaustiveCase
              )
            $ localStateOutput

    fromEitherIOCli @(FileError ()) $
      writeLazyByteStringOutput mOutFile output

-- | Query the UTxO, filtered by a given set of addresses, from a Shelley node
-- via the local state query protocol.
runQueryUTxOCmd
  :: ()
  => Cmd.QueryUTxOCmdArgs
  -> CIO e ()
runQueryUTxOCmd
  ( Cmd.QueryUTxOCmdArgs
      { Cmd.commons =
        Cmd.QueryCommons
          { Cmd.nodeConnInfo
          , Cmd.target
          }
      , Cmd.queryFilter
      , Cmd.outputFormat
      , Cmd.mOutFile
      }
    ) = do
    fromEitherIOCli
      ( executeLocalStateQueryExpr nodeConnInfo target $ runExceptT $ do
          anyCEra@(AnyCardanoEra cEra) <- easyRunQueryCurrentEra

          case forEraInEonMaybe cEra id of
            Nothing -> throwCliError $ QueryCmdEraNotSupported anyCEra
            Just sbe -> do
              utxo <- easyRunQuery (queryUtxo sbe queryFilter)
              hoist liftIO $ writeFilteredUTxOs sbe outputFormat mOutFile utxo
      )
      & fromEitherCIOCli

runQueryKesPeriodInfoCmd
  :: ()
  => Cmd.QueryKesPeriodInfoCmdArgs
  -> CIO e ()
runQueryKesPeriodInfoCmd
  Cmd.QueryKesPeriodInfoCmdArgs
    { Cmd.commons =
      Cmd.QueryCommons
        { Cmd.nodeConnInfo
        , Cmd.target
        }
    , Cmd.nodeOpCertFp
    , Cmd.outputFormat
    , Cmd.mOutFile
    } = do
    opCert <-
      fromEitherIOCli $ readFileTextEnvelope nodeOpCertFp

    output <-
      fromEitherIOCli
        ( executeLocalStateQueryExpr nodeConnInfo target $ runExceptT $ do
            AnyCardanoEra cEra <- easyRunQueryCurrentEra

            era <- hoist liftIO $ supportedEra cEra
            let sbe = convert era
            -- We check that the KES period specified in the operational certificate is correct
            -- based on the KES period defined in the genesis parameters and the current slot number
            gParams <- easyRunQuery (queryGenesisParameters sbe)

            eraHistory <- easyRunQueryEraHistory

            let eInfo = toTentativeEpochInfo eraHistory

            -- We get the operational certificate counter from the protocol state and check that
            -- it is equivalent to what we have on disk.
            ptclState <- easyRunQuery (queryProtocolState sbe)

            chainTip <- liftIO $ getLocalChainTip nodeConnInfo

            let curKesPeriod = currentKesPeriod chainTip gParams
                oCertStartKesPeriod = opCertStartingKesPeriod opCert
                oCertEndKesPeriod = opCertEndKesPeriod gParams opCert
                opCertIntervalInformation = opCertIntervalInfo gParams chainTip curKesPeriod oCertStartKesPeriod oCertEndKesPeriod

            (onDiskC, stateC) <-
              Exp.obtainCommonConstraints era $
                hoist liftIO $
                  opCertOnDiskAndStateCounters ptclState opCert

            let counterInformation = opCertNodeAndOnDiskCounters onDiskC stateC

            -- Always render diagnostic information
            liftIO . putStrLn $
              docToString $
                renderOpCertIntervalInformation (unFile nodeOpCertFp) opCertIntervalInformation

            liftIO . putStrLn $
              docToString $
                renderOpCertNodeAndOnDiskCounterInformation (unFile nodeOpCertFp) counterInformation

            let qKesInfoOutput = createQueryKesPeriodInfoOutput opCertIntervalInformation counterInformation eInfo gParams

            return
              $ outputFormat
                & ( id
                      . Vary.on (\FormatJson -> Json.encodeJson)
                      . Vary.on (\FormatYaml -> Json.encodeYaml)
                      $ Vary.exhaustiveCase
                  )
              $ qKesInfoOutput
        )
        & fromEitherCIOCli

    fromEitherIOCli @(FileError ()) $
      writeLazyByteStringOutput mOutFile output
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
  -> CIO e ()
runQueryPoolStateCmd
  Cmd.QueryPoolStateCmdArgs
    { Cmd.commons =
      Cmd.QueryCommons
        { Cmd.nodeConnInfo
        , Cmd.target
        }
    , Cmd.allOrOnlyPoolIds
    , Cmd.outputFormat
    , Cmd.mOutFile
    } = do
    fromEitherIOCli
      ( executeLocalStateQueryExpr nodeConnInfo target $ runExceptT $ do
          AnyCardanoEra cEra <- easyRunQueryCurrentEra

          era <- hoist liftIO $ supportedEra cEra

          let beo = convert era
              poolFilter = case allOrOnlyPoolIds of
                All -> Nothing
                Only poolIds -> Just $ fromList poolIds

          result <- easyRunQuery (queryPoolState beo poolFilter)
          hoist liftIO $ writePoolState era outputFormat mOutFile result
      )
      & fromEitherCIOCli

-- | Query the local mempool state
runQueryTxMempoolCmd
  :: ()
  => Cmd.QueryTxMempoolCmdArgs
  -> CIO e ()
runQueryTxMempoolCmd
  Cmd.QueryTxMempoolCmdArgs
    { Cmd.nodeConnInfo
    , Cmd.query
    , Cmd.outputFormat
    , Cmd.mOutFile
    } = do
    localQuery <- case query of
      TxMempoolQueryTxExists tx -> do
        AnyCardanoEra era <-
          fromExceptTCli $ determineEra nodeConnInfo
        pure $ LocalTxMonitoringQueryTx $ TxIdInMode era tx
      TxMempoolQueryNextTx -> pure LocalTxMonitoringSendNextTx
      TxMempoolQueryInfo -> pure LocalTxMonitoringMempoolInformation

    result <- liftIO $ queryTxMonitoringLocal nodeConnInfo localQuery

    let output =
          outputFormat
            & ( id
                  . Vary.on (\FormatJson -> Json.encodeJson)
                  . Vary.on (\FormatYaml -> Json.encodeYaml)
                  $ Vary.exhaustiveCase
              )
            $ result

    fromEitherIOCli @(FileError ()) $
      writeLazyByteStringOutput mOutFile output

runQuerySlotNumberCmd
  :: ()
  => Cmd.QuerySlotNumberCmdArgs
  -> CIO e ()
runQuerySlotNumberCmd
  Cmd.QuerySlotNumberCmdArgs
    { Cmd.commons =
      Cmd.QueryCommons
        { Cmd.nodeConnInfo
        , Cmd.target
        }
    , Cmd.utcTime
    } = do
    SlotNo slotNo <- fromExceptTCli $ utcTimeToSlotNo nodeConnInfo target utcTime
    liftIO . putStr $ show slotNo

runQueryRefScriptSizeCmd
  :: forall e
   . Cmd.QueryRefScriptSizeCmdArgs
  -> CIO e ()
runQueryRefScriptSizeCmd
  Cmd.QueryRefScriptSizeCmdArgs
    { Cmd.commons =
      Cmd.QueryCommons
        { Cmd.nodeConnInfo
        , Cmd.target
        }
    , Cmd.transactionInputs
    , Cmd.outputFormat
    , Cmd.mOutFile
    } = do
    r <- fromEitherIOCli $ executeLocalStateQueryExpr nodeConnInfo target $ runExceptT $ do
      AnyCardanoEra cEra <- easyRunQueryCurrentEra

      era <- hoist liftIO $ supportedEra cEra

      let beo = convert era
          sbe = convert era

      utxo <- easyRunQuery (queryUtxo sbe $ QueryUTxOByTxIn transactionInputs)

      newExceptT $
        runRIO () $
          catch
            ( fmap Right $
                writeFormattedOutput outputFormat mOutFile $
                  RefInputScriptSize $
                    getReferenceInputsSizeForTxIds beo (toLedgerUTxO sbe utxo) transactionInputs
            )
            (pure . Left . QueryCmdWriteFileError)
    fromEitherCli r

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
  -> CIO e ()
runQueryStakeSnapshotCmd
  Cmd.QueryStakeSnapshotCmdArgs
    { Cmd.commons =
      Cmd.QueryCommons
        { Cmd.nodeConnInfo
        , Cmd.target
        }
    , Cmd.allOrOnlyPoolIds
    , Cmd.outputFormat
    , Cmd.mOutFile
    } = do
    fromEitherIOCli
      ( executeLocalStateQueryExpr nodeConnInfo target $ runExceptT $ do
          AnyCardanoEra cEra <- easyRunQueryCurrentEra

          era <- hoist liftIO $ supportedEra cEra

          let poolFilter = case allOrOnlyPoolIds of
                All -> Nothing
                Only poolIds -> Just $ fromList poolIds

          let beo = convert era

          result <- easyRunQuery (queryStakeSnapshot beo poolFilter)

          hoist liftIO $ obtainCommonConstraints era (writeStakeSnapshots outputFormat mOutFile) result
      )
      & fromEitherCIOCli

runQueryLedgerStateCmd
  :: ()
  => Cmd.QueryLedgerStateCmdArgs
  -> CIO e ()
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
    output <-
      fromEitherIOCli
        ( executeLocalStateQueryExpr nodeConnInfo target $ runExceptT $ do
            AnyCardanoEra cEra <- easyRunQueryCurrentEra

            era <- hoist liftIO $ supportedEra cEra
            let sbe = convert era
            serialisedDebugLedgerState <- easyRunQuery (queryDebugLedgerState sbe)

            hoist liftIO $
              obtainCommonConstraints era $
                outputFormat
                  & ( id
                        . Vary.on (\FormatJson -> ledgerStateAsJsonByteString serialisedDebugLedgerState)
                        . Vary.on (\FormatText -> ledgerStateAsTextByteString serialisedDebugLedgerState)
                        . Vary.on (\FormatYaml -> ledgerStateAsYamlByteString serialisedDebugLedgerState)
                        $ Vary.exhaustiveCase
                    )
        )
        & fromEitherCIOCli
    fromEitherIOCli @(FileError ()) $
      writeLazyByteStringOutput mOutFile output

ledgerStateAsJsonByteString
  :: IsShelleyBasedEra era
  => SerialisedDebugLedgerState era
  -> ExceptT QueryCmdError IO LBS.ByteString
ledgerStateAsJsonByteString serialisedDebugLedgerState =
  case decodeDebugLedgerState serialisedDebugLedgerState of
    Left (bs, _decoderError) ->
      newExceptT $
        runRIO () $
          catch
            (Right <$> cborToTextByteString bs)
            (pure . Left . QueryBackwardCompatibleError "query ledger-state")
    Right decodededgerState -> pure $ Json.encodeJson decodededgerState <> "\n"

ledgerStateAsTextByteString
  :: Applicative f
  => SerialisedDebugLedgerState era -> f LBS.ByteString
ledgerStateAsTextByteString serialisedDebugLedgerState =
  let SerialisedDebugLedgerState serLedgerState = serialisedDebugLedgerState
   in pure $ unSerialised serLedgerState

ledgerStateAsYamlByteString
  :: IsShelleyBasedEra era
  => SerialisedDebugLedgerState era
  -> ExceptT QueryCmdError IO LBS.ByteString
ledgerStateAsYamlByteString serialisedDebugLedgerState =
  case decodeDebugLedgerState serialisedDebugLedgerState of
    Left (bs, _decoderError) ->
      newExceptT $
        runRIO () $
          catch
            (Right <$> cborToTextByteString bs)
            (pure . Left . QueryBackwardCompatibleError "query ledger-state")
    Right decodededgerState -> pure $ Json.encodeYaml decodededgerState

runQueryLedgerPeerSnapshot
  :: ()
  => Cmd.QueryLedgerPeerSnapshotCmdArgs
  -> CIO e ()
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
      fromEitherIOCli
        ( executeLocalStateQueryExprWithVersion nodeConnInfo target $ \globalNtcVersion -> runExceptT $ do
            AnyCardanoEra cEra <-
              lift queryCurrentEra
                & onLeft (left . QueryCmdUnsupportedNtcVersion)

            era <- hoist liftIO $ supportedEra cEra
            let sbe = convert era

            result <- easyRunQuery (queryLedgerPeerSnapshot sbe)

            shelleyNtcVersion <- hoistEither $ getShelleyNodeToClientVersion era globalNtcVersion

            hoist liftIO $
              obtainCommonConstraints era $
                case decodeBigLedgerPeerSnapshot shelleyNtcVersion result of
                  Left (bs, _decoderError) -> pure $ Left bs
                  Right snapshot -> pure $ Right snapshot
        )
        & fromEitherCIOCli

    case result of
      Left (bs :: LBS.ByteString) -> do
        fromExceptTCli $ pPrintCBOR bs
      Right (snapshot :: LedgerPeerSnapshot) -> do
        let output =
              outputFormat
                & ( id
                      . Vary.on (\FormatJson -> Json.encodeJson)
                      . Vary.on (\FormatYaml -> Json.encodeYaml)
                      $ Vary.exhaustiveCase
                  )
                $ snapshot

        fromEitherIOCli @(FileError ()) $
          writeLazyByteStringOutput mOutFile output

runQueryProtocolStateCmd
  :: ()
  => Cmd.QueryProtocolStateCmdArgs
  -> CIO e ()
runQueryProtocolStateCmd
  ( Cmd.QueryProtocolStateCmdArgs
      { Cmd.commons =
        Cmd.QueryCommons
          { Cmd.nodeConnInfo
          , Cmd.target
          }
      , Cmd.outputFormat
      , Cmd.mOutFile
      }
    ) = do
    () <-
      fromEitherIOCli
        ( executeLocalStateQueryExpr nodeConnInfo target $ runExceptT $ do
            anyE@(AnyCardanoEra cEra) <- easyRunQueryCurrentEra

            era <-
              pure (forEraMaybeEon cEra)
                & onNothing (left $ QueryCmdEraNotSupported anyE)

            ps <- easyRunQuery (queryProtocolState (convert era))

            output <-
              hoist liftIO
                $ Exp.obtainCommonConstraints era
                $ outputFormat
                  & ( id
                        . Vary.on (\FormatCborBin -> protocolStateToCborBinary)
                        . Vary.on (\FormatCborHex -> fmap Base16.encode . protocolStateToCborBinary)
                        . Vary.on (\FormatJson -> fmap (Json.encodeJson . toJSON) . protocolStateToChainDepState era)
                        . Vary.on (\FormatYaml -> fmap (Json.encodeYaml . toJSON) . protocolStateToChainDepState era)
                        $ Vary.exhaustiveCase
                    )
                $ ps

            fromEitherIOCli @(FileError ()) $
              writeLazyByteStringOutput mOutFile output
        )
        & fromEitherCIOCli

    pure ()
   where
    protocolStateToChainDepState
      :: Exp.Era era
      -> ProtocolState era
      -> ExceptT QueryCmdError IO (Consensus.ChainDepState (ConsensusProtocol era))
    protocolStateToChainDepState era ps =
      Exp.obtainCommonConstraints era $ do
        pure (decodeProtocolState ps)
          & onLeft (left . QueryCmdProtocolStateDecodeFailure)

    protocolStateToCborBinary
      :: ProtocolState era
      -> ExceptT QueryCmdError IO LBS.ByteString
    protocolStateToCborBinary (ProtocolState pstate) =
      pure $ unSerialised pstate

-- | Query the current delegations and reward accounts, filtered by a given
-- set of addresses, from a Shelley node via the local state query protocol.
runQueryStakeAddressInfoCmd
  :: ()
  => Cmd.QueryStakeAddressInfoCmdArgs
  -> CIO e ()
runQueryStakeAddressInfoCmd
  Cmd.QueryStakeAddressInfoCmdArgs
    { Cmd.commons =
      commons
    , Cmd.addr
    , Cmd.outputFormat
    , Cmd.mOutFile
    } = do
    said <- fromExceptTCli $ getQueryStakeAddressInfo commons addr

    fromExceptTCli $ writeStakeAddressInfo said outputFormat mOutFile

-- | Container for data returned by 'getQueryStakeAddressInfo' where:
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

getQueryStakeAddressInfo
  :: Cmd.QueryCommons
  -> StakeAddress
  -> ExceptT QueryCmdError IO StakeAddressInfoData
getQueryStakeAddressInfo
  Cmd.QueryCommons
    { Cmd.nodeConnInfo = nodeConnInfo@LocalNodeConnectInfo{localNodeNetworkId = networkId}
    , Cmd.target
    }
  (StakeAddress _ addr) =
    do
      lift $ executeLocalStateQueryExpr nodeConnInfo target $ runExceptT $ do
        AnyCardanoEra cEra <- easyRunQueryCurrentEra

        era <- hoist liftIO $ supportedEra cEra

        let stakeAddr = Set.singleton $ fromShelleyStakeCredential addr
            sbe = convert era
            beo = convert era

        (stakeRewardAccountBalances, stakePools) <-
          easyRunQuery (queryStakeAddresses sbe stakeAddr networkId)

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

getShelleyNodeToClientVersion
  :: Exp.Era era -> NodeToClientVersion -> Either QueryCmdError ShelleyNodeToClientVersion
getShelleyNodeToClientVersion era globalNtcVersion =
  case supportedNodeToClientVersions (Proxy @(CardanoBlock StandardCrypto)) Map.! globalNtcVersion of
    HardForkNodeToClientEnabled _ np ->
      case era of
        Exp.ConwayEra ->
          case projectNP conwayIndex np of
            EraNodeToClientDisabled -> Left QueryCmdNodeToClientDisabled
            EraNodeToClientEnabled shelleyNtcVersion -> return shelleyNtcVersion
        Exp.DijkstraEra ->
          case projectNP dijkstraIndex np of
            EraNodeToClientDisabled -> Left QueryCmdNodeToClientDisabled
            EraNodeToClientEnabled shelleyNtcVersion -> return shelleyNtcVersion
    HardForkNodeToClientDisabled _ -> Left QueryCmdNodeToClientDisabled

conwayIndex :: Index (x'1 : x'2 : x'3 : x'4 : x'5 : x'6 : x : xs1) x
conwayIndex = IS (IS (IS (IS (IS (IS IZ)))))

dijkstraIndex :: Index (x'1 : x'2 : x'3 : x'4 : x'5 : x'6 : x'7 : x : xs1) x
dijkstraIndex = IS (IS (IS (IS (IS (IS (IS IZ))))))

writeStakeAddressInfo
  :: StakeAddressInfoData
  -> Vary [FormatJson, FormatYaml]
  -> Maybe (File () Out)
  -> ExceptT QueryCmdError IO ()
writeStakeAddressInfo
  ( StakeAddressInfoData
      { rewards = DelegationsAndRewards (stakeAccountBalances, stakePools)
      , deposits = stakeDelegDeposits
      , gaDeposits = gaDeposits
      , delegatees = voteDelegatees
      }
    )
  outputFormat
  mOutFile = do
    let output =
          outputFormat
            & ( id
                  . Vary.on (\FormatJson -> Json.encodeJson)
                  . Vary.on (\FormatYaml -> Json.encodeYaml)
                  $ Vary.exhaustiveCase
              )
            $ jsonInfo

    firstExceptT QueryCmdWriteFileError
      . newExceptT
      $ writeLazyByteStringOutput mOutFile output
   where
    jsonInfo :: [Aeson.Value]
    jsonInfo =
      map
        ( \(addr, mBalance, mPoolId, mDRep, mDeposit) ->
            Aeson.object
              [ "address" .= addr
              , "stakeDelegation" .= fmap friendlyStake mPoolId
              , "voteDelegation" .= fmap friendlyDRep mDRep
              , "rewardAccountBalance" .= mBalance
              , "stakeRegistrationDeposit" .= mDeposit
              , "govActionDeposits" .= gaDeposits
              ]
        )
        merged

    friendlyStake :: PoolId -> Aeson.Value
    friendlyStake poolId =
      Aeson.object
        [ "stakePoolBech32" .= UsingBech32 poolId
        , "stakePoolHex" .= UsingRawBytesHex poolId
        ]

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
   . Vary [FormatJson, FormatYaml]
  -> Maybe (File () Out)
  -> SerialisedStakeSnapshots era
  -> ExceptT QueryCmdError IO ()
writeStakeSnapshots outputFormat mOutFile qState = do
  StakeSnapshot snapshot <-
    pure (decodeStakeSnapshot qState)
      & onLeft (left . QueryCmdStakeSnapshotDecodeError)

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

-- | This function obtains the pool parameters, equivalent to the following jq query on the output of query ledger-state
--   .nesEs.esLState.lsDPState.dpsPState.psStakePoolParams.<pool_id>
writePoolState
  :: Exp.Era era
  -> Vary [FormatJson, FormatYaml]
  -> Maybe (File () Out)
  -> SerialisedPoolState
  -> ExceptT QueryCmdError IO ()
writePoolState era outputFormat mOutFile serialisedCurrentEpochState = do
  poolState <-
    liftEither . first QueryCmdPoolStateDecodeError $
      decodePoolState (convert era) serialisedCurrentEpochState

  let poolStates = mkPoolStates poolState :: Map (L.KeyHash L.StakePool) PoolParams
      output =
        outputFormat
          & ( id
                . Vary.on (\FormatJson -> Json.encodeJson)
                . Vary.on (\FormatYaml -> Json.encodeYaml)
                $ Vary.exhaustiveCase
            )
          $ poolStates

  firstExceptT QueryCmdWriteFileError
    . newExceptT
    $ writeLazyByteStringOutput mOutFile output

writeFilteredUTxOs
  :: ShelleyBasedEra era
  -> Vary [FormatCborBin, FormatCborHex, FormatJson, FormatText, FormatYaml]
  -> Maybe (File () Out)
  -> UTxO era
  -> ExceptT QueryCmdError IO ()
writeFilteredUTxOs era format mOutFile utxo = do
  let output =
        shelleyBasedEraConstraints era $
          format
            & ( id
                  . Vary.on (\FormatCborBin -> CBOR.serialize $ toLedgerUTxO era utxo)
                  . Vary.on (\FormatCborHex -> Base16.encode . CBOR.serialize $ toLedgerUTxO era utxo)
                  . Vary.on (\FormatJson -> Json.encodeJson utxo)
                  . Vary.on (\FormatText -> strictTextToLazyBytestring $ filteredUTxOsToText utxo)
                  . Vary.on (\FormatYaml -> Json.encodeYaml utxo)
                  $ Vary.exhaustiveCase
              )

  firstExceptT QueryCmdWriteFileError
    . newExceptT
    $ writeLazyByteStringOutput mOutFile output

filteredUTxOsToText :: UTxO era -> Text
filteredUTxOsToText (UTxO utxo) = do
  mconcat
    [ Text.unlines [title, Text.replicate (Text.length title + 2) "-"]
    , Text.unlines $
        map utxoToText $
          toList utxo
    ]
 where
  title :: Text
  title =
    "                           TxHash                                 TxIx        Amount"

utxoToText
  :: (TxIn, TxOut CtxUTxO era)
  -> Text
utxoToText txInOutTuple =
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
  -> CIO e ()
runQueryStakePoolsCmd
  Cmd.QueryStakePoolsCmdArgs
    { Cmd.commons =
      Cmd.QueryCommons
        { Cmd.nodeConnInfo
        , Cmd.target
        }
    , Cmd.outputFormat
    , Cmd.mOutFile
    } = do
    fromEitherIOCli
      ( executeLocalStateQueryExpr nodeConnInfo target $ runExceptT @QueryCmdError $ do
          AnyCardanoEra cEra <- easyRunQueryCurrentEra

          era <- hoist liftIO $ supportedEra cEra
          let sbe = convert era
          poolIds <- easyRunQuery (queryStakePools sbe)

          fromExceptTCli $ writeStakePools outputFormat mOutFile poolIds
      )
      & fromEitherCIOCli

-- TODO: replace with writeFormattedOutput
writeStakePools
  :: Vary [FormatJson, FormatText, FormatYaml]
  -> Maybe (File () Out)
  -> Set PoolId
  -> ExceptT QueryCmdError IO ()
writeStakePools format mOutFile stakePools = do
  let output =
        format
          & ( id
                . Vary.on (\FormatJson -> Json.encodeJson)
                . Vary.on (\FormatText -> encodeText)
                . Vary.on (\FormatYaml -> Json.encodeYaml)
                $ Vary.exhaustiveCase
            )
          $ stakePools

  firstExceptT QueryCmdWriteFileError
    . newExceptT
    $ writeLazyByteStringOutput mOutFile output
 where
  encodeText =
    LBS.unlines
      . map (strictTextToLazyBytestring . serialiseToBech32)
      . toList

writeFormattedOutput
  :: ToJSON a
  => Pretty a
  => Vary [FormatJson, FormatText, FormatYaml]
  -> Maybe (File b Out)
  -> a
  -> CIO e ()
writeFormattedOutput format mOutFile value = do
  let output =
        format
          & ( id
                . Vary.on (\FormatJson -> Json.encodeJson value)
                . Vary.on (\FormatText -> fromString . docToString $ pretty value)
                . Vary.on (\FormatYaml -> Json.encodeYaml value)
                $ Vary.exhaustiveCase
            )

  fromEitherIOCli @(FileError ()) $
    writeLazyByteStringOutput mOutFile output

runQueryStakeDistributionCmd
  :: ()
  => Cmd.QueryStakeDistributionCmdArgs
  -> CIO e ()
runQueryStakeDistributionCmd
  Cmd.QueryStakeDistributionCmdArgs
    { Cmd.commons =
      Cmd.QueryCommons
        { Cmd.nodeConnInfo
        , Cmd.target
        }
    , Cmd.outputFormat
    , Cmd.mOutFile
    } = do
    fromEitherIOCli
      ( executeLocalStateQueryExpr nodeConnInfo target $ runExceptT $ do
          AnyCardanoEra cEra <- easyRunQueryCurrentEra

          era <- hoist liftIO $ supportedEra cEra
          let sbe = convert era
          result <- easyRunQuery (queryStakeDistribution sbe)

          hoist liftIO $
            writeStakeDistribution outputFormat mOutFile result
      )
      & fromEitherCIOCli

writeStakeDistribution
  :: Vary [FormatJson, FormatText, FormatYaml]
  -> Maybe (File () Out)
  -> Map PoolId Rational
  -> ExceptT QueryCmdError IO ()
writeStakeDistribution format mOutFile stakeDistrib = do
  let output =
        format
          & ( id
                . Vary.on (\FormatJson -> Json.encodeJson stakeDistrib)
                . Vary.on (\FormatText -> strictTextToLazyBytestring stakeDistributionText)
                . Vary.on (\FormatYaml -> Json.encodeYaml stakeDistrib)
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
  -> CIO e ()
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
    , Cmd.outputFormat
    , Cmd.mOutFile
    } = do
    poolid <- getHashFromStakePoolKeyHashSource poolColdVerKeyFile

    vrkSkey <-
      fromEitherIOCli $
        readFileTextEnvelope @(SigningKey VrfKey) vrkSkeyFp

    shelleyGenesis <-
      fromExceptTCli $
        decodeShelleyGenesisFile genFile

    fromExceptTCli . join $
      lift
        ( executeLocalStateQueryExpr nodeConnInfo target $ runExceptT $ do
            AnyCardanoEra cEra <- easyRunQueryCurrentEra

            era <- hoist liftIO $ supportedEra cEra
            let sbe = convert era

            pparams <- easyRunQuery (queryProtocolParameters sbe)
            ptclState <- easyRunQuery (queryProtocolState sbe)
            eraHistory <- easyRunQueryEraHistory

            let eInfo = toEpochInfo eraHistory

            curentEpoch <- easyRunQuery (queryEpoch sbe)

            case whichSchedule of
              CurrentEpoch -> do
                let beo = convert era

                serCurrentEpochState <-
                  easyRunQuery (queryPoolDistribution beo (Just (Set.singleton poolid)))

                pure $ do
                  schedule <-
                    firstExceptT QueryCmdLeaderShipError $
                      hoistEither $
                        obtainCommonConstraints era $
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
                        obtainCommonConstraints era $
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
            outputFormat
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
  -> CIO e ()
runQueryConstitution
  Cmd.QueryNoArgCmdArgs
    { Cmd.eon
    , Cmd.commons =
      Cmd.QueryCommons
        { Cmd.nodeConnInfo
        , Cmd.target
        }
    , Cmd.outputFormat
    , Cmd.mOutFile
    } = conwayEraOnwardsConstraints eon $ do
    constitution <- fromExceptTCli $ runQuery nodeConnInfo target $ queryConstitution eon

    let output =
          outputFormat
            & ( id
                  . Vary.on (\FormatJson -> Json.encodeJson)
                  . Vary.on (\FormatYaml -> Json.encodeYaml)
                  $ Vary.exhaustiveCase
              )
            $ constitution

    fromEitherIOCli @(FileError ()) $
      writeLazyByteStringOutput mOutFile output

runQueryGovState
  :: Cmd.QueryNoArgCmdArgs era
  -> CIO e ()
runQueryGovState
  Cmd.QueryNoArgCmdArgs
    { Cmd.eon
    , Cmd.commons =
      Cmd.QueryCommons
        { Cmd.nodeConnInfo
        , Cmd.target
        }
    , Cmd.outputFormat
    , Cmd.mOutFile
    } = conwayEraOnwardsConstraints eon $ do
    govState <- fromExceptTCli $ runQuery nodeConnInfo target $ queryGovState eon

    let output =
          outputFormat
            & ( id
                  . Vary.on (\FormatJson -> Json.encodeJson)
                  . Vary.on (\FormatYaml -> Json.encodeYaml)
                  $ Vary.exhaustiveCase
              )
            $ govState

    fromEitherIOCli @(FileError ()) $
      writeLazyByteStringOutput mOutFile output

runQueryRatifyState
  :: Cmd.QueryNoArgCmdArgs era
  -> CIO e ()
runQueryRatifyState
  Cmd.QueryNoArgCmdArgs
    { Cmd.eon
    , Cmd.commons =
      Cmd.QueryCommons
        { Cmd.nodeConnInfo
        , Cmd.target
        }
    , Cmd.outputFormat
    , Cmd.mOutFile
    } = conwayEraOnwardsConstraints eon $ do
    ratifyState <- fromExceptTCli $ runQuery nodeConnInfo target $ queryRatifyState eon

    let output =
          outputFormat
            & ( id
                  . Vary.on (\FormatJson -> Json.encodeJson)
                  . Vary.on (\FormatYaml -> Json.encodeYaml)
                  $ Vary.exhaustiveCase
              )
            $ ratifyState

    fromEitherIOCli @(FileError ()) $
      writeLazyByteStringOutput mOutFile output

runQueryFuturePParams
  :: Cmd.QueryNoArgCmdArgs era
  -> CIO e ()
runQueryFuturePParams
  Cmd.QueryNoArgCmdArgs
    { Cmd.eon
    , Cmd.commons =
      Cmd.QueryCommons
        { Cmd.nodeConnInfo
        , Cmd.target
        }
    , Cmd.outputFormat
    , Cmd.mOutFile
    } = conwayEraOnwardsConstraints eon $ do
    futurePParams <- fromExceptTCli $ runQuery nodeConnInfo target $ queryFuturePParams eon

    let output =
          outputFormat
            & ( id
                  . Vary.on (\FormatJson -> Json.encodeJson)
                  . Vary.on (\FormatYaml -> Json.encodeYaml)
                  $ Vary.exhaustiveCase
              )
            $ futurePParams

    fromEitherIOCli @(FileError ()) $
      writeLazyByteStringOutput mOutFile output

runQueryDRepState
  :: Cmd.QueryDRepStateCmdArgs era
  -> CIO e ()
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
    , Cmd.outputFormat
    , Cmd.mOutFile
    } = conwayEraOnwardsConstraints eon $ do
    let drepHashSources = case drepHashSources' of All -> []; Only l -> l
    drepCreds <- mapM readDRepCredential drepHashSources

    drepState <- fromExceptTCli $ runQuery nodeConnInfo target $ queryDRepState eon $ fromList drepCreds

    drepStakeDistribution <-
      case includeStake of
        Cmd.WithStake ->
          fromExceptTCli $
            runQuery nodeConnInfo target $
              queryDRepStakeDistribution eon (fromList $ L.DRepCredential <$> drepCreds)
        Cmd.NoStake -> return mempty

    let assocs :: [(L.Credential L.DRepRole, L.DRepState)] = Map.assocs drepState
        drepStateOutputs = toDRepStateOutput drepStakeDistribution <$> assocs

    let output =
          outputFormat
            & ( id
                  . Vary.on (\FormatJson -> Json.encodeJson)
                  . Vary.on (\FormatYaml -> Json.encodeYaml)
                  $ Vary.exhaustiveCase
              )
            $ drepStateOutputs

    fromEitherIOCli @(FileError ()) $
      writeLazyByteStringOutput mOutFile output
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
  -> CIO e ()
runQueryDRepStakeDistribution
  Cmd.QueryDRepStakeDistributionCmdArgs
    { Cmd.eon
    , Cmd.commons =
      Cmd.QueryCommons
        { Cmd.nodeConnInfo
        , Cmd.target
        }
    , Cmd.drepHashSources = drepHashSources'
    , Cmd.outputFormat
    , Cmd.mOutFile
    } = conwayEraOnwardsConstraints eon $ do
    let drepFromSource =
          fmap L.DRepCredential . readDRepCredential
        drepHashSources = case drepHashSources' of
          All -> []
          Only l -> l
    dreps <- fromList <$> mapM drepFromSource drepHashSources

    drepStakeDistribution <-
      fromExceptTCli $ runQuery nodeConnInfo target $ queryDRepStakeDistribution eon dreps

    let output =
          outputFormat
            & ( id
                  . Vary.on (\FormatJson -> Json.encodeJson)
                  . Vary.on (\FormatYaml -> Json.encodeYaml)
                  $ Vary.exhaustiveCase
              )
            $ drepStakeDistribution

    fromEitherIOCli @(FileError ()) $
      writeLazyByteStringOutput mOutFile output

runQuerySPOStakeDistribution
  :: Cmd.QuerySPOStakeDistributionCmdArgs era
  -> CIO e ()
runQuerySPOStakeDistribution
  Cmd.QuerySPOStakeDistributionCmdArgs
    { Cmd.eon
    , Cmd.commons =
      Cmd.QueryCommons
        { Cmd.nodeConnInfo
        , Cmd.target
        }
    , Cmd.spoHashSources = spoHashSources'
    , Cmd.outputFormat
    , Cmd.mOutFile
    } = conwayEraOnwardsConstraints eon $ do
    let spoFromSource = readSPOCredential
        spoHashSources = case spoHashSources' of
          All -> []
          Only l -> l

    spos <- fromList <$> mapM spoFromSource spoHashSources

    let beo = convert eon

    spoStakeDistribution :: Map (L.KeyHash L.StakePool) L.Coin <-
      fromExceptTCli $ runQuery nodeConnInfo target $ querySPOStakeDistribution eon spos

    let poolIds :: Set (Hash StakePoolKey) = Set.fromList $ map StakePoolKeyHash $ Map.keys spoStakeDistribution

    serialisedPoolState <-
      fromExceptTCli $ runQuery nodeConnInfo target $ queryPoolState beo (Just poolIds)

    PoolState poolStateResult <-
      fromEitherCli $ decodePoolState (convert eon) serialisedPoolState

    let spoToRewardCred :: Map (L.KeyHash L.StakePool) (L.Credential 'L.Staking)
        spoToRewardCred =
          Map.map
            (L.raCredential . L.ppRewardAccount)
            (L.qpsrStakePoolParams poolStateResult)

        allRewardCreds :: Set StakeCredential
        allRewardCreds = Set.fromList $ map fromShelleyStakeCredential $ Map.elems spoToRewardCred

    rewardCredToDRep <-
      fromExceptTCli $ runQuery nodeConnInfo target $ queryStakeVoteDelegatees eon allRewardCreds

    let spoToDelegatee :: Map (L.KeyHash L.StakePool) L.DRep
        spoToDelegatee =
          Map.mapMaybe
            (\rewardCred -> Map.lookup (fromShelleyStakeCredential rewardCred) rewardCredToDRep)
            spoToRewardCred

    let json =
          [ ( spo
            , coin
            , Map.lookup spo spoToDelegatee
            )
          | (spo, coin) <- Map.assocs spoStakeDistribution
          ]

        output =
          outputFormat
            & ( id
                  . Vary.on (\FormatJson -> Json.encodeJson)
                  . Vary.on (\FormatYaml -> Json.encodeYaml)
                  $ Vary.exhaustiveCase
              )
            $ json

    fromEitherIOCli @(FileError ()) $
      writeLazyByteStringOutput mOutFile output

runQueryCommitteeMembersState
  :: Cmd.QueryCommitteeMembersStateCmdArgs era
  -> CIO e ()
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
    , Cmd.outputFormat
    , Cmd.mOutFile
    } = conwayEraOnwardsConstraints eon $ do
    let coldKeysFromVerKeyHashOrFile =
          readVerificationKeyOrHashOrFileOrScriptHash unCommitteeColdKeyHash
    coldKeys <- fromList <$> mapM coldKeysFromVerKeyHashOrFile coldCredKeys

    let hotKeysFromVerKeyHashOrFile =
          readVerificationKeyOrHashOrFileOrScriptHash unCommitteeHotKeyHash
    hotKeys <- fromList <$> mapM hotKeysFromVerKeyHashOrFile hotCredKeys

    committeeState <-
      fromExceptTCli $
        runQuery nodeConnInfo target $
          queryCommitteeMembersState eon coldKeys hotKeys (fromList memberStatuses)

    let output =
          outputFormat
            & ( id
                  . Vary.on (\FormatJson -> Json.encodeJson)
                  . Vary.on (\FormatYaml -> Json.encodeYaml)
                  $ Vary.exhaustiveCase
              )
            $ committeeState

    fromEitherIOCli @(FileError ()) $
      writeLazyByteStringOutput mOutFile output

runQueryTreasuryValue
  :: Cmd.QueryTreasuryValueCmdArgs era
  -> CIO e ()
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
    chainAccountState <-
      fromExceptTCli $
        runQuery nodeConnInfo target $
          queryAccountState eon

    let (L.Coin treasury) = casTreasury chainAccountState
        output = LBS.pack $ show treasury

    fromEitherCIOCli @(FileError ()) $
      writeLazyByteStringOutput mOutFile output

runQueryProposals
  :: Cmd.QueryProposalsCmdArgs era
  -> CIO e ()
runQueryProposals
  Cmd.QueryProposalsCmdArgs
    { Cmd.eon
    , Cmd.commons =
      Cmd.QueryCommons
        { Cmd.nodeConnInfo
        , Cmd.target
        }
    , Cmd.govActionIds = govActionIds'
    , Cmd.outputFormat
    , Cmd.mOutFile
    } = conwayEraOnwardsConstraints eon $ do
    let govActionIds = case govActionIds' of
          All -> []
          Only l -> l

    govActionStates :: (Seq.Seq (L.GovActionState (ShelleyLedgerEra era))) <-
      fromExceptTCli $ runQuery nodeConnInfo target $ queryProposals eon $ Set.fromList govActionIds

    let output =
          outputFormat
            & ( id
                  . Vary.on (\FormatJson -> Json.encodeJson)
                  . Vary.on (\FormatYaml -> Json.encodeYaml)
                  $ Vary.exhaustiveCase
              )
            $ govActionStates

    fromEitherIOCli @(FileError ()) $
      writeLazyByteStringOutput mOutFile output

runQueryEraHistoryCmd :: Cmd.QueryEraHistoryCmdArgs -> CIO e ()
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
      fromEitherIOCli
        ( executeLocalStateQueryExpr nodeConnInfo target $
            runExceptT $
              lift queryEraHistory & onLeft (left . QueryCmdUnsupportedNtcVersion)
        )
        & fromEitherCIOCli

    let output = textEnvelopeToJSON Nothing eraHistory

    fromEitherIOCli @(FileError ()) $
      writeLazyByteStringOutput mOutFile output

runQueryStakePoolDefaultVote
  :: Cmd.QueryStakePoolDefaultVoteCmdArgs era
  -> CIO e ()
runQueryStakePoolDefaultVote
  Cmd.QueryStakePoolDefaultVoteCmdArgs
    { Cmd.eon
    , Cmd.commons =
      Cmd.QueryCommons
        { Cmd.nodeConnInfo
        , Cmd.target
        }
    , Cmd.spoHashSources
    , Cmd.outputFormat
    , Cmd.mOutFile
    } = conwayEraOnwardsConstraints eon $ do
    let spoFromSource = readSPOCredential
    spo <- spoFromSource spoHashSources

    defVote :: L.DefaultVote <-
      fromExceptTCli $
        runQuery nodeConnInfo target $
          queryStakePoolDefaultVote eon spo

    let output =
          outputFormat
            & ( id
                  . Vary.on (\FormatJson -> Json.encodeJson)
                  . Vary.on (\FormatYaml -> Json.encodeYaml)
                  $ Vary.exhaustiveCase
              )
            $ defVote

    fromEitherIOCli @(FileError ()) $
      writeLazyByteStringOutput mOutFile output

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

supportedEra :: Typeable era => CardanoEra era -> ExceptT QueryCmdError IO (Exp.Era era)
supportedEra cEra =
  pure (forEraMaybeEon cEra)
    & onNothing (left $ QueryCmdEraNotSupported (AnyCardanoEra cEra))
