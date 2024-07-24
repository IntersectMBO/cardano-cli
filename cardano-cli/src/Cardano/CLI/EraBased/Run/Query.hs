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

module Cardano.CLI.EraBased.Run.Query
  ( runQueryCmds
  , runQueryConstitutionHashCmd
  , runQueryKesPeriodInfoCmd
  , runQueryLeadershipScheduleCmd
  , runQueryLedgerStateCmd
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
  , newOutputFormat
  , renderQueryCmdError
  , renderOpCertIntervalInformation
  , percentage
  )
where

import           Cardano.Api hiding (QueryInShelleyBasedEra (..))
import qualified Cardano.Api as Api
import           Cardano.Api.Byron hiding (QueryInShelleyBasedEra (..))
import qualified Cardano.Api.Ledger as L
import           Cardano.Api.Shelley hiding (QueryInShelleyBasedEra (..))

import qualified Cardano.CLI.EraBased.Commands.Query as Cmd
import           Cardano.CLI.EraBased.Run.Genesis.Common
import           Cardano.CLI.Helpers
import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Errors.NodeEraMismatchError
import           Cardano.CLI.Types.Errors.QueryCmdError
import           Cardano.CLI.Types.Errors.QueryCmdLocalStateQueryError
import           Cardano.CLI.Types.Key
import qualified Cardano.CLI.Types.Output as O
import           Cardano.Crypto.Hash (hashToBytesAsHex)
import qualified Cardano.Crypto.Hash.Blake2b as Blake2b
import qualified Cardano.Ledger.Shelley.LedgerState as L
import           Cardano.Slotting.EpochInfo (EpochInfo (..), epochInfoSlotToUTCTime, hoistEpochInfo)
import           Ouroboros.Consensus.BlockchainTime.WallClock.Types (RelativeTime (..),
                   toRelativeTime)
import qualified Ouroboros.Consensus.Cardano.Block as Consensus
import qualified Ouroboros.Consensus.HardFork.History as Consensus
import qualified Ouroboros.Consensus.Protocol.Abstract as Consensus
import qualified Ouroboros.Consensus.Protocol.Praos.Common as Consensus
import           Ouroboros.Consensus.Protocol.TPraos (StandardCrypto)
import           Ouroboros.Network.Block (Serialised (..))
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Type as Consensus

import           Control.Monad (forM, forM_, join)
import           Data.Aeson as Aeson
import qualified Data.Aeson as A
import           Data.Aeson.Encode.Pretty (encodePretty)
import           Data.Bifunctor (Bifunctor (..))
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Coerce (coerce)
import           Data.Function ((&))
import           Data.Functor ((<&>))
import qualified Data.List as List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Proxy (Proxy (..))
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as LT
import           Data.Time.Clock
import           GHC.Generics
import           Lens.Micro ((^.))
import           Numeric (showEFloat)
import           Prettyprinter
import           Prettyprinter.Render.Terminal (AnsiStyle)
import qualified System.IO as IO
import           Text.Printf (printf)

runQueryCmds :: Cmd.QueryCmds era -> ExceptT QueryCmdError IO ()
runQueryCmds = \case
  Cmd.QueryLeadershipScheduleCmd args -> runQueryLeadershipScheduleCmd args
  Cmd.QueryProtocolParametersCmd args -> runQueryProtocolParametersCmd args
  Cmd.QueryConstitutionHashCmd args -> runQueryConstitutionHashCmd args
  Cmd.QueryTipCmd args -> runQueryTipCmd args
  Cmd.QueryStakePoolsCmd args -> runQueryStakePoolsCmd args
  Cmd.QueryStakeDistributionCmd args -> runQueryStakeDistributionCmd args
  Cmd.QueryStakeAddressInfoCmd args -> runQueryStakeAddressInfoCmd args
  Cmd.QueryLedgerStateCmd args -> runQueryLedgerStateCmd args
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
  Cmd.QueryDRepStateCmd args -> runQueryDRepState args
  Cmd.QueryDRepStakeDistributionCmd args -> runQueryDRepStakeDistribution args
  Cmd.QueryCommitteeMembersStateCmd args -> runQueryCommitteeMembersState args
  Cmd.QueryTreasuryValueCmd args -> runQueryTreasuryValue args

runQueryConstitutionHashCmd
  :: ()
  => Cmd.QueryConstitutionHashCmdArgs
  -> ExceptT QueryCmdError IO ()
runQueryConstitutionHashCmd
  Cmd.QueryConstitutionHashCmdArgs
    { Cmd.nodeSocketPath
    , Cmd.consensusModeParams
    , Cmd.networkId
    , Cmd.target
    , Cmd.mOutFile
    } = do
    let localNodeConnInfo = LocalNodeConnectInfo consensusModeParams networkId nodeSocketPath

    result <- liftIO $ executeLocalStateQueryExpr localNodeConnInfo target $ runExceptT $ do
      AnyCardanoEra era <- lift queryCurrentEra & onLeft (left . QueryCmdUnsupportedNtcVersion)

      sbe <-
        requireShelleyBasedEra era
          & onNothing (left QueryCmdByronEra)

      lift (shelleyBasedEraConstraints sbe (queryConstitutionHash sbe))
        & onLeft (left . QueryCmdUnsupportedNtcVersion)
        & onLeft (left . QueryCmdEraMismatch)

    writeConstitutionHash mOutFile =<< except (join (first QueryCmdAcquireFailure result))
   where
    writeConstitutionHash
      :: Maybe (File () Out)
      -> L.SafeHash L.StandardCrypto L.AnchorData
      -> ExceptT QueryCmdError IO ()
    writeConstitutionHash mOutFile' cHash =
      firstExceptT QueryCmdWriteFileError . newExceptT $
        writeLazyByteStringOutput mOutFile' $
          encodePretty cHash

runQueryProtocolParametersCmd
  :: ()
  => Cmd.QueryProtocolParametersCmdArgs
  -> ExceptT QueryCmdError IO ()
runQueryProtocolParametersCmd
  Cmd.QueryProtocolParametersCmdArgs
    { Cmd.nodeSocketPath
    , Cmd.consensusModeParams
    , Cmd.networkId
    , Cmd.mOutFile
    } = do
    let localNodeConnInfo = LocalNodeConnectInfo consensusModeParams networkId nodeSocketPath
    AnyCardanoEra era <- firstExceptT QueryCmdAcquireFailure $ determineEra localNodeConnInfo
    sbe <- forEraInEon @ShelleyBasedEra era (left QueryCmdByronEra) pure
    let qInMode = QueryInEra $ QueryInShelleyBasedEra sbe Api.QueryProtocolParameters
    pp <-
      executeQueryAnyMode localNodeConnInfo qInMode
        & modifyError QueryCmdConvenienceError
    writeProtocolParameters sbe mOutFile pp
   where
    writeProtocolParameters
      :: ShelleyBasedEra era
      -> Maybe (File () Out)
      -> L.PParams (ShelleyLedgerEra era)
      -> ExceptT QueryCmdError IO ()
    writeProtocolParameters sbe mOutFile' pparams =
      firstExceptT QueryCmdWriteFileError . newExceptT $
        writeLazyByteStringOutput mOutFile' $
          shelleyBasedEraConstraints sbe $
            encodePretty pparams

-- | Calculate the percentage sync rendered as text.
percentage
  :: RelativeTime
  -- ^ 'tolerance'.  If 'b' - 'a' < 'tolerance', then 100% is reported.  This even if we are 'tolerance' seconds
  -- behind, we are still considered fully synced.
  -> RelativeTime
  -- ^ 'nowTime'.  The time of the most recently synced block.
  -> RelativeTime
  -- ^ 'tipTime'.  The time of the tip of the block chain to which we need to sync.
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
  pc = id @Double (fromIntegral ua / fromIntegral ub) * 100.0

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
  Cmd.QueryTipCmdArgs
    { Cmd.nodeSocketPath
    , Cmd.consensusModeParams
    , Cmd.networkId
    , Cmd.target
    , Cmd.mOutFile
    } = do
    let localNodeConnInfo = LocalNodeConnectInfo consensusModeParams networkId nodeSocketPath

    eLocalState <- ExceptT $
      fmap sequence $
        executeLocalStateQueryExpr localNodeConnInfo target $
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
        & onNothing (queryChainTipViaChainSync localNodeConnInfo)

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

            return $ percentage tolerance nowSeconds tipTimeResult

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

    firstExceptT QueryCmdWriteFileError . newExceptT $
      writeLazyByteStringOutput mOutFile $
        encodePretty localStateOutput

-- | Query the UTxO, filtered by a given set of addresses, from a Shelley node
-- via the local state query protocol.
runQueryUTxOCmd
  :: ()
  => Cmd.QueryUTxOCmdArgs
  -> ExceptT QueryCmdError IO ()
runQueryUTxOCmd
  Cmd.QueryUTxOCmdArgs
    { Cmd.nodeSocketPath
    , Cmd.consensusModeParams
    , Cmd.queryFilter
    , Cmd.networkId
    , Cmd.target
    , Cmd.format
    , Cmd.mOutFile
    } = do
    let localNodeConnInfo = LocalNodeConnectInfo consensusModeParams networkId nodeSocketPath

    join $
      lift
        ( executeLocalStateQueryExpr localNodeConnInfo target $ runExceptT $ do
            AnyCardanoEra era <-
              lift queryCurrentEra
                & onLeft (left . QueryCmdUnsupportedNtcVersion)

            sbe <-
              requireShelleyBasedEra era
                & onNothing (left QueryCmdByronEra)

            utxo <-
              lift (queryUtxo sbe queryFilter)
                & onLeft (left . QueryCmdUnsupportedNtcVersion)
                & onLeft (left . QueryCmdLocalStateQueryError . EraMismatchError)

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
    { Cmd.nodeSocketPath
    , Cmd.consensusModeParams
    , Cmd.networkId
    , Cmd.nodeOpCertFp
    , Cmd.target
    , Cmd.mOutFile
    } = do
    opCert <-
      lift (readFileTextEnvelope AsOperationalCertificate nodeOpCertFp)
        & onLeft (left . QueryCmdOpCertCounterReadError)

    let localNodeConnInfo = LocalNodeConnectInfo consensusModeParams networkId nodeSocketPath

    join $
      lift
        ( executeLocalStateQueryExpr localNodeConnInfo target $ runExceptT $ do
            AnyCardanoEra era <-
              lift queryCurrentEra
                & onLeft (left . QueryCmdUnsupportedNtcVersion)

            sbe <-
              requireShelleyBasedEra era
                & onNothing (left QueryCmdByronEra)

            -- We check that the KES period specified in the operational certificate is correct
            -- based on the KES period defined in the genesis parameters and the current slot number
            gParams <-
              lift (queryGenesisParameters sbe)
                & onLeft (left . QueryCmdUnsupportedNtcVersion)
                & onLeft (left . QueryCmdLocalStateQueryError . EraMismatchError)

            eraHistory <-
              lift queryEraHistory
                & onLeft (left . QueryCmdUnsupportedNtcVersion)

            let eInfo = toTentativeEpochInfo eraHistory

            -- We get the operational certificate counter from the protocol state and check that
            -- it is equivalent to what we have on disk.
            ptclState <-
              lift (queryProtocolState sbe)
                & onLeft (left . QueryCmdUnsupportedNtcVersion)
                & onLeft (left . QueryCmdLocalStateQueryError . EraMismatchError)

            pure $ do
              chainTip <- liftIO $ getLocalChainTip localNodeConnInfo

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
                  kesPeriodInfoJSON = encodePretty qKesInfoOutput

              liftIO $ LBS.putStrLn kesPeriodInfoJSON
              forM_
                mOutFile
                ( \(File oFp) ->
                    handleIOExceptT (QueryCmdWriteFileError . FileIOError oFp) $
                      LBS.writeFile oFp kesPeriodInfoJSON
                )
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
      => L.ADDRHASH (Consensus.PraosProtocolSupportsNodeCrypto (ConsensusProtocol era)) ~ Blake2b.Blake2b_224
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
          StakePoolKeyHash blockIssuerHash = verificationKeyHash stakePoolVKey

      case Map.lookup (coerce blockIssuerHash) opCertCounterMap of
        -- Operational certificate exists in the protocol state
        -- so our ondisk op cert counter must be greater than or
        -- equal to what is in the node state
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
    { Cmd.nodeSocketPath
    , Cmd.consensusModeParams
    , Cmd.networkId
    , Cmd.allOrOnlyPoolIds
    , Cmd.target
    , Cmd.mOutFile
    } = do
    let localNodeConnInfo = LocalNodeConnectInfo consensusModeParams networkId nodeSocketPath

    join $
      lift
        ( executeLocalStateQueryExpr localNodeConnInfo target $ runExceptT $ do
            AnyCardanoEra era <-
              lift queryCurrentEra
                & onLeft (left . QueryCmdUnsupportedNtcVersion)

            sbe <-
              requireShelleyBasedEra era
                & onNothing (left QueryCmdByronEra)

            beo <- requireEon BabbageEra era

            let poolFilter = case allOrOnlyPoolIds of
                  All -> Nothing
                  Only poolIds -> Just $ Set.fromList poolIds

            result <-
              lift (queryPoolState beo poolFilter)
                & onLeft (left . QueryCmdUnsupportedNtcVersion)
                & onLeft (left . QueryCmdLocalStateQueryError . EraMismatchError)

            pure $ do
              shelleyBasedEraConstraints sbe (writePoolState mOutFile) result
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
    { Cmd.nodeSocketPath
    , Cmd.consensusModeParams
    , Cmd.networkId
    , Cmd.query
    , Cmd.mOutFile
    } = do
    let localNodeConnInfo = LocalNodeConnectInfo consensusModeParams networkId nodeSocketPath

    localQuery <- case query of
      TxMempoolQueryTxExists tx -> do
        AnyCardanoEra era <-
          determineEra localNodeConnInfo
            & modifyError QueryCmdAcquireFailure
        pure $ LocalTxMonitoringQueryTx $ TxIdInMode era tx
      TxMempoolQueryNextTx -> pure LocalTxMonitoringSendNextTx
      TxMempoolQueryInfo -> pure LocalTxMonitoringMempoolInformation

    result <- liftIO $ queryTxMonitoringLocal localNodeConnInfo localQuery
    firstExceptT QueryCmdWriteFileError . newExceptT $
      writeLazyByteStringOutput mOutFile $
        encodePretty result

runQuerySlotNumberCmd
  :: ()
  => Cmd.QuerySlotNumberCmdArgs
  -> ExceptT QueryCmdError IO ()
runQuerySlotNumberCmd
  Cmd.QuerySlotNumberCmdArgs
    { Cmd.nodeSocketPath
    , Cmd.consensusModeParams
    , Cmd.networkId
    , Cmd.utcTime
    , Cmd.target
    } = do
    SlotNo slotNo <- utcTimeToSlotNo nodeSocketPath consensusModeParams networkId target utcTime
    liftIO . putStr $ show slotNo

runQueryRefScriptSizeCmd
  :: ()
  => Cmd.QueryRefScriptSizeCmdArgs
  -> ExceptT QueryCmdError IO ()
runQueryRefScriptSizeCmd
  Cmd.QueryRefScriptSizeCmdArgs
    { Cmd.nodeSocketPath
    , Cmd.consensusModeParams
    , Cmd.transactionInputs
    , Cmd.networkId
    , Cmd.target
    , Cmd.format
    , Cmd.mOutFile
    } = do
    let localNodeConnInfo = LocalNodeConnectInfo consensusModeParams networkId nodeSocketPath

    join $
      lift
        ( executeLocalStateQueryExpr localNodeConnInfo target $ runExceptT $ do
            AnyCardanoEra era <-
              lift queryCurrentEra
                & onLeft (left . QueryCmdUnsupportedNtcVersion)

            sbe <-
              requireShelleyBasedEra era
                & onNothing (left QueryCmdByronEra)

            beo <- requireEon BabbageEra era

            utxo <-
              lift (queryUtxo sbe $ QueryUTxOByTxIn transactionInputs)
                & onLeft (left . QueryCmdUnsupportedNtcVersion)
                & onLeft (left . QueryCmdLocalStateQueryError . EraMismatchError)

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
    { Cmd.nodeSocketPath
    , Cmd.consensusModeParams
    , Cmd.networkId
    , Cmd.allOrOnlyPoolIds
    , Cmd.target
    , Cmd.mOutFile
    } = do
    let localNodeConnInfo = LocalNodeConnectInfo consensusModeParams networkId nodeSocketPath

    join $
      lift
        ( executeLocalStateQueryExpr localNodeConnInfo target $ runExceptT $ do
            AnyCardanoEra era <-
              lift queryCurrentEra
                & onLeft (left . QueryCmdUnsupportedNtcVersion)

            sbe <-
              requireShelleyBasedEra era
                & onNothing (left QueryCmdByronEra)

            let poolFilter = case allOrOnlyPoolIds of
                  All -> Nothing
                  Only poolIds -> Just $ Set.fromList poolIds

            beo <- requireEon BabbageEra era

            result <-
              lift (queryStakeSnapshot beo poolFilter)
                & onLeft (left . QueryCmdUnsupportedNtcVersion)
                & onLeft (left . QueryCmdLocalStateQueryError . EraMismatchError)

            pure $ do
              shelleyBasedEraConstraints sbe (writeStakeSnapshots mOutFile) result
        )
        & onLeft (left . QueryCmdAcquireFailure)
        & onLeft left

runQueryLedgerStateCmd
  :: ()
  => Cmd.QueryLedgerStateCmdArgs
  -> ExceptT QueryCmdError IO ()
runQueryLedgerStateCmd
  Cmd.QueryLedgerStateCmdArgs
    { Cmd.nodeSocketPath
    , Cmd.consensusModeParams
    , Cmd.networkId
    , Cmd.target
    , Cmd.mOutFile
    } = do
    let localNodeConnInfo = LocalNodeConnectInfo consensusModeParams networkId nodeSocketPath

    join $
      lift
        ( executeLocalStateQueryExpr localNodeConnInfo target $ runExceptT $ do
            AnyCardanoEra era <-
              lift queryCurrentEra
                & onLeft (left . QueryCmdUnsupportedNtcVersion)

            sbe <-
              requireShelleyBasedEra era
                & onNothing (left QueryCmdByronEra)

            result <-
              lift (queryDebugLedgerState sbe)
                & onLeft (left . QueryCmdUnsupportedNtcVersion)
                & onLeft (left . QueryCmdLocalStateQueryError . EraMismatchError)

            pure $ do
              shelleyBasedEraConstraints sbe (writeLedgerState mOutFile) result
        )
        & onLeft (left . QueryCmdAcquireFailure)
        & onLeft left

runQueryProtocolStateCmd
  :: ()
  => Cmd.QueryProtocolStateCmdArgs
  -> ExceptT QueryCmdError IO ()
runQueryProtocolStateCmd
  Cmd.QueryProtocolStateCmdArgs
    { Cmd.nodeSocketPath
    , Cmd.consensusModeParams
    , Cmd.networkId
    , Cmd.target
    , Cmd.mOutFile
    } = do
    let localNodeConnInfo = LocalNodeConnectInfo consensusModeParams networkId nodeSocketPath

    join $
      lift
        ( executeLocalStateQueryExpr localNodeConnInfo target $ runExceptT $ do
            AnyCardanoEra era <-
              lift queryCurrentEra
                & onLeft (left . QueryCmdUnsupportedNtcVersion)

            sbe <-
              requireShelleyBasedEra era
                & onNothing (left QueryCmdByronEra)

            result <-
              lift (queryProtocolState sbe)
                & onLeft (left . QueryCmdUnsupportedNtcVersion)
                & onLeft (left . QueryCmdLocalStateQueryError . EraMismatchError)

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
  Cmd.QueryStakeAddressInfoCmdArgs
    { Cmd.nodeSocketPath
    , Cmd.consensusModeParams
    , Cmd.addr = StakeAddress _ addr
    , Cmd.networkId
    , Cmd.target
    , Cmd.mOutFile
    } = do
    let localNodeConnInfo = LocalNodeConnectInfo consensusModeParams networkId nodeSocketPath

    join $
      lift
        ( executeLocalStateQueryExpr localNodeConnInfo target $ runExceptT $ do
            AnyCardanoEra era <-
              lift queryCurrentEra
                & onLeft (left . QueryCmdUnsupportedNtcVersion)

            sbe <-
              requireShelleyBasedEra era
                & onNothing (left QueryCmdByronEra)

            let stakeAddr = Set.singleton $ fromShelleyStakeCredential addr

            (stakeRewardAccountBalances, stakePools) <-
              lift (queryStakeAddresses sbe stakeAddr networkId)
                & onLeft (left . QueryCmdUnsupportedNtcVersion)
                & onLeft (left . QueryCmdLocalStateQueryError . EraMismatchError)

            beo <- requireEon BabbageEra era

            stakeDelegDeposits <-
              lift (queryStakeDelegDeposits beo stakeAddr)
                & onLeft (left . QueryCmdUnsupportedNtcVersion)
                & onLeft (left . QueryCmdLocalStateQueryError . EraMismatchError)

            stakeVoteDelegatees <- monoidForEraInEonA era $ \ceo ->
              lift (queryStakeVoteDelegatees ceo stakeAddr)
                & onLeft (left . QueryCmdUnsupportedNtcVersion)
                & onLeft (left . QueryCmdLocalStateQueryError . EraMismatchError)

            return $ do
              writeStakeAddressInfo
                sbe
                mOutFile
                (DelegationsAndRewards (stakeRewardAccountBalances, stakePools))
                (Map.mapKeys (makeStakeAddress networkId) stakeDelegDeposits)
                (Map.mapKeys (makeStakeAddress networkId) stakeVoteDelegatees)
        )
        & onLeft (left . QueryCmdAcquireFailure)
        & onLeft left

-- -------------------------------------------------------------------------------------------------

writeStakeAddressInfo
  :: ShelleyBasedEra era
  -> Maybe (File () Out)
  -> DelegationsAndRewards
  -> Map StakeAddress L.Coin
  -- ^ deposits
  -> Map StakeAddress (L.DRep L.StandardCrypto)
  -- ^ vote delegatees
  -> ExceptT QueryCmdError IO ()
writeStakeAddressInfo
  sbe
  mOutFile
  (DelegationsAndRewards (stakeAccountBalances, stakePools))
  stakeDelegDeposits
  voteDelegatees =
    firstExceptT QueryCmdWriteFileError . newExceptT $
      writeLazyByteStringOutput mOutFile (encodePretty $ jsonInfo sbe)
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
                    , "delegationDeposit" .= mDeposit
                    ]
              )
              merged
        )

    friendlyDRep :: L.DRep L.StandardCrypto -> Text
    friendlyDRep L.DRepAlwaysAbstain = "alwaysAbstain"
    friendlyDRep L.DRepAlwaysNoConfidence = "alwaysNoConfidence"
    friendlyDRep (L.DRepCredential cred) =
      L.credToText cred -- this will pring "keyHash-..." or "scriptHash-...", depending on the type of credential
    merged
      :: [(StakeAddress, Maybe L.Coin, Maybe PoolId, Maybe (L.DRep L.StandardCrypto), Maybe L.Coin)]
    merged =
      [ (addr, mBalance, mPoolId, mDRep, mDeposit)
      | addr <-
          Set.toList
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

writeLedgerState
  :: forall era ledgerera
   . ShelleyLedgerEra era ~ ledgerera
  => ToJSON (DebugLedgerState era)
  => FromCBOR (DebugLedgerState era)
  => Maybe (File () Out)
  -> SerialisedDebugLedgerState era
  -> ExceptT QueryCmdError IO ()
writeLedgerState mOutFile qState@(SerialisedDebugLedgerState serLedgerState) =
  case mOutFile of
    Nothing ->
      case decodeDebugLedgerState qState of
        Left (bs, _decoderError) -> firstExceptT QueryCmdHelpersError $ pPrintCBOR bs
        Right ledgerState -> liftIO . LBS.putStrLn $ Aeson.encode ledgerState
    Just (File fpath) ->
      handleIOExceptT (QueryCmdWriteFileError . FileIOError fpath) $
        LBS.writeFile fpath $
          unSerialised serLedgerState

writeStakeSnapshots
  :: forall era ledgerera
   . ()
  => ShelleyLedgerEra era ~ ledgerera
  => L.EraCrypto ledgerera ~ StandardCrypto
  => Maybe (File () Out)
  -> SerialisedStakeSnapshots era
  -> ExceptT QueryCmdError IO ()
writeStakeSnapshots mOutFile qState = do
  StakeSnapshot snapshot <-
    pure (decodeStakeSnapshot qState)
      & onLeft (left . QueryCmdStakeSnapshotDecodeError)

  -- Calculate the three pool and active stake values for the given pool
  liftIO . maybe LBS.putStrLn (LBS.writeFile . unFile) mOutFile $ encodePretty snapshot

-- | This function obtains the pool parameters, equivalent to the following jq query on the output of query ledger-state
--   .nesEs.esLState.lsDPState.dpsPState.psStakePoolParams.<pool_id>
writePoolState
  :: forall era ledgerera
   . ()
  => ShelleyLedgerEra era ~ ledgerera
  => L.EraCrypto ledgerera ~ StandardCrypto
  => L.Era ledgerera
  => Maybe (File () Out)
  -> SerialisedPoolState era
  -> ExceptT QueryCmdError IO ()
writePoolState mOutFile serialisedCurrentEpochState = do
  PoolState poolState <-
    pure (decodePoolState serialisedCurrentEpochState)
      & onLeft (left . QueryCmdPoolStateDecodeError)

  let hks =
        Set.toList $
          Set.fromList $
            Map.keys (L.psStakePoolParams poolState)
              <> Map.keys (L.psFutureStakePoolParams poolState)
              <> Map.keys (L.psRetiring poolState)

  let poolStates :: Map (L.KeyHash 'L.StakePool StandardCrypto) (Params StandardCrypto)
      poolStates =
        Map.fromList $
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

  firstExceptT QueryCmdWriteFileError . newExceptT $
    writeLazyByteStringOutput mOutFile $
      encodePretty poolStates

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
      Right chainDepstate -> liftIO . LBS.putStrLn $ encodePretty chainDepstate

writeFilteredUTxOs
  :: Api.ShelleyBasedEra era
  -> Maybe OutputFormatJsonOrText
  -> Maybe (File () Out)
  -> UTxO era
  -> ExceptT QueryCmdError IO ()
writeFilteredUTxOs sbe format mOutFile utxo =
  shelleyBasedEraConstraints sbe
    $ firstExceptT QueryCmdWriteFileError
      . newExceptT
      . writeLazyByteStringOutput mOutFile
    $ case newOutputFormat format mOutFile of
      OutputFormatJson -> encodePretty utxo
      OutputFormatText -> strictTextToLazyBytestring $ filteredUTxOsToText sbe utxo

filteredUTxOsToText :: Api.ShelleyBasedEra era -> UTxO era -> Text
filteredUTxOsToText sbe (UTxO utxo) = do
  mconcat
    [ Text.unlines [title, Text.replicate (Text.length title + 2) "-"]
    , Text.unlines $ case sbe of
        ShelleyBasedEraShelley ->
          map (utxoToText sbe) $ Map.toList utxo
        ShelleyBasedEraAllegra ->
          map (utxoToText sbe) $ Map.toList utxo
        ShelleyBasedEraMary ->
          map (utxoToText sbe) $ Map.toList utxo
        ShelleyBasedEraAlonzo ->
          map (utxoToText sbe) $ Map.toList utxo
        ShelleyBasedEraBabbage ->
          map (utxoToText sbe) $ Map.toList utxo
        ShelleyBasedEraConway ->
          map (utxoToText sbe) $ Map.toList utxo
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
    { Cmd.nodeSocketPath
    , Cmd.consensusModeParams
    , Cmd.networkId
    , Cmd.target
    , Cmd.format
    , Cmd.mOutFile
    } = do
    let localNodeConnInfo = LocalNodeConnectInfo consensusModeParams networkId nodeSocketPath

    join $
      lift
        ( executeLocalStateQueryExpr localNodeConnInfo target $ runExceptT @QueryCmdError $ do
            AnyCardanoEra era <- lift queryCurrentEra & onLeft (left . QueryCmdUnsupportedNtcVersion)

            sbe <-
              requireShelleyBasedEra era
                & onNothing (left QueryCmdByronEra)

            poolIds <-
              lift (queryStakePools sbe)
                & onLeft (left . QueryCmdUnsupportedNtcVersion)
                & onLeft (left . QueryCmdEraMismatch)

            pure $ writeStakePools (newOutputFormat format mOutFile) mOutFile poolIds
        )
        & onLeft (left . QueryCmdAcquireFailure)
        & onLeft left

-- TODO: replace with writeFormattedOutput
writeStakePools
  :: OutputFormatJsonOrText
  -> Maybe (File () Out)
  -> Set PoolId
  -> ExceptT QueryCmdError IO ()
writeStakePools format mOutFile stakePools =
  firstExceptT QueryCmdWriteFileError . newExceptT $
    writeLazyByteStringOutput mOutFile toWrite
 where
  toWrite :: LBS.ByteString =
    case format of
      OutputFormatText ->
        LBS.unlines $
          map (strictTextToLazyBytestring . serialiseToBech32) $
            Set.toList stakePools
      OutputFormatJson ->
        encodePretty stakePools

writeFormattedOutput
  :: MonadIOTransError QueryCmdError t m
  => ToJSON a
  => Pretty a
  => Maybe OutputFormatJsonOrText
  -> Maybe (File b Out)
  -> a
  -> t m ()
writeFormattedOutput mFormat mOutFile value =
  modifyError QueryCmdWriteFileError . hoistIOEither $
    writeLazyByteStringOutput mOutFile toWrite
 where
  toWrite :: LBS.ByteString =
    case newOutputFormat mFormat mOutFile of
      OutputFormatText -> fromString . docToString $ pretty value
      OutputFormatJson -> encodePretty value

runQueryStakeDistributionCmd
  :: ()
  => Cmd.QueryStakeDistributionCmdArgs
  -> ExceptT QueryCmdError IO ()
runQueryStakeDistributionCmd
  Cmd.QueryStakeDistributionCmdArgs
    { Cmd.nodeSocketPath
    , Cmd.consensusModeParams
    , Cmd.networkId
    , Cmd.format
    , Cmd.target
    , Cmd.mOutFile
    } = do
    let localNodeConnInfo = LocalNodeConnectInfo consensusModeParams networkId nodeSocketPath

    join $
      lift
        ( executeLocalStateQueryExpr localNodeConnInfo target $ runExceptT $ do
            AnyCardanoEra era <-
              lift queryCurrentEra
                & onLeft (left . QueryCmdUnsupportedNtcVersion)

            sbe <-
              requireShelleyBasedEra era
                & onNothing (left QueryCmdByronEra)

            result <-
              lift (queryStakeDistribution sbe)
                & onLeft (left . QueryCmdUnsupportedNtcVersion)
                & onLeft (left . QueryCmdLocalStateQueryError . EraMismatchError)

            pure $ do
              writeStakeDistribution (newOutputFormat format mOutFile) mOutFile result
        )
        & onLeft (left . QueryCmdAcquireFailure)
        & onLeft left

writeStakeDistribution
  :: OutputFormatJsonOrText
  -> Maybe (File () Out)
  -> Map PoolId Rational
  -> ExceptT QueryCmdError IO ()
writeStakeDistribution format mOutFile stakeDistrib =
  firstExceptT QueryCmdWriteFileError . newExceptT $
    writeLazyByteStringOutput mOutFile toWrite
 where
  toWrite :: LBS.ByteString =
    case format of
      OutputFormatJson -> encodePretty stakeDistrib
      OutputFormatText -> strictTextToLazyBytestring stakeDistributionText
  stakeDistributionText =
    Text.unlines $
      [ title
      , Text.replicate (Text.length title + 2) "-"
      ]
        ++ [showStakeDistr poolId stakeFraction | (poolId, stakeFraction) <- Map.toList stakeDistrib]
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
    { Cmd.nodeSocketPath
    , Cmd.consensusModeParams
    , Cmd.networkId
    , Cmd.genesisFp = GenesisFile genFile
    , Cmd.poolColdVerKeyFile
    , Cmd.vrkSkeyFp
    , Cmd.whichSchedule
    , Cmd.target
    , Cmd.format
    , Cmd.mOutFile
    } = do
    let localNodeConnInfo = LocalNodeConnectInfo consensusModeParams networkId nodeSocketPath

    poolid <-
      modifyError QueryCmdTextReadError $
        readVerificationKeyOrHashOrFile AsStakePoolKey poolColdVerKeyFile

    vrkSkey <-
      modifyError QueryCmdTextEnvelopeReadError . hoistIOEither $
        readFileTextEnvelope (AsSigningKey AsVrfKey) vrkSkeyFp

    shelleyGenesis <-
      modifyError QueryCmdGenesisReadError $
        decodeShelleyGenesisFile genFile

    join $
      lift
        ( executeLocalStateQueryExpr localNodeConnInfo target $ runExceptT $ do
            AnyCardanoEra era <-
              lift queryCurrentEra
                & onLeft (left . QueryCmdUnsupportedNtcVersion)

            sbe <-
              requireShelleyBasedEra era
                & onNothing (left QueryCmdByronEra)

            pparams <-
              lift (queryProtocolParameters sbe)
                & onLeft (left . QueryCmdUnsupportedNtcVersion)
                & onLeft (left . QueryCmdLocalStateQueryError . EraMismatchError)

            ptclState <-
              lift (queryProtocolState sbe)
                & onLeft (left . QueryCmdUnsupportedNtcVersion)
                & onLeft (left . QueryCmdLocalStateQueryError . EraMismatchError)

            eraHistory <-
              lift queryEraHistory
                & onLeft (left . QueryCmdUnsupportedNtcVersion)

            let eInfo = toEpochInfo eraHistory

            curentEpoch <-
              lift (queryEpoch sbe)
                & onLeft (left . QueryCmdUnsupportedNtcVersion)
                & onLeft (left . QueryCmdLocalStateQueryError . EraMismatchError)

            case whichSchedule of
              CurrentEpoch -> do
                beo <- requireEon BabbageEra era

                serCurrentEpochState <-
                  lift (queryPoolDistribution beo (Just (Set.singleton poolid)))
                    & onLeft (left . QueryCmdUnsupportedNtcVersion)
                    & onLeft (left . QueryCmdLocalStateQueryError . EraMismatchError)

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
                serCurrentEpochState <-
                  lift (queryCurrentEpochState sbe)
                    & onLeft (left . QueryCmdUnsupportedNtcVersion)
                    & onLeft (left . QueryCmdLocalStateQueryError . EraMismatchError)

                pure $ do
                  tip <- liftIO $ getLocalChainTip localNodeConnInfo

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
    writeSchedule mOutFile' eInfo shelleyGenesis schedule =
      firstExceptT QueryCmdWriteFileError . newExceptT $
        writeLazyByteStringOutput mOutFile' toWrite
     where
      start = SystemStart $ sgSystemStart shelleyGenesis
      toWrite =
        case newOutputFormat format mOutFile' of
          OutputFormatJson ->
            encodePretty $ leadershipScheduleToJson schedule eInfo start
          OutputFormatText ->
            strictTextToLazyBytestring $ leadershipScheduleToText schedule eInfo start

    leadershipScheduleToText
      :: Set SlotNo
      -> EpochInfo (Either Text)
      -> SystemStart
      -> Text
    leadershipScheduleToText leadershipSlots eInfo sStart = do
      Text.unlines $
        title
          : Text.replicate (Text.length title + 2) "-"
          : [showLeadershipSlot slot eInfo sStart | slot <- Set.toList leadershipSlots]
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
      showLeadershipSlot <$> List.sort (Set.toList leadershipSlots)
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
    , Cmd.nodeSocketPath
    , Cmd.consensusModeParams
    , Cmd.networkId
    , Cmd.target
    , Cmd.mOutFile
    } = conwayEraOnwardsConstraints eon $ do
    let localNodeConnInfo = LocalNodeConnectInfo consensusModeParams networkId nodeSocketPath
    constitution <- runQuery localNodeConnInfo target $ queryConstitution eon
    writeOutput mOutFile constitution

runQueryGovState
  :: Cmd.QueryNoArgCmdArgs era
  -> ExceptT QueryCmdError IO ()
runQueryGovState
  Cmd.QueryNoArgCmdArgs
    { Cmd.eon
    , Cmd.nodeSocketPath
    , Cmd.consensusModeParams
    , Cmd.networkId
    , Cmd.target
    , Cmd.mOutFile
    } = conwayEraOnwardsConstraints eon $ do
    let localNodeConnInfo = LocalNodeConnectInfo consensusModeParams networkId nodeSocketPath
    govState <- runQuery localNodeConnInfo target $ queryGovState eon
    writeOutput mOutFile govState

runQueryDRepState
  :: Cmd.QueryDRepStateCmdArgs era
  -> ExceptT QueryCmdError IO ()
runQueryDRepState
  Cmd.QueryDRepStateCmdArgs
    { Cmd.eon
    , Cmd.nodeSocketPath
    , Cmd.consensusModeParams
    , Cmd.networkId
    , Cmd.drepHashSources = drepHashSources'
    , Cmd.includeStake
    , Cmd.target
    , Cmd.mOutFile
    } = conwayEraOnwardsConstraints eon $ do
    let localNodeConnInfo = LocalNodeConnectInfo consensusModeParams networkId nodeSocketPath

    let drepHashSources = case drepHashSources' of All -> []; Only l -> l
    drepCreds <- modifyError QueryCmdDRepKeyError $ mapM readDRepCredential drepHashSources

    drepState <- runQuery localNodeConnInfo target $ queryDRepState eon $ Set.fromList drepCreds

    drepStakeDistribution <-
      case includeStake of
        Cmd.WithStake ->
          runQuery localNodeConnInfo target $
            queryDRepStakeDistribution eon (Set.fromList $ L.DRepCredential <$> drepCreds)
        Cmd.NoStake -> return mempty

    writeOutput mOutFile $
      drepStateToJson drepStakeDistribution <$> Map.assocs drepState
   where
    drepStateToJson
      :: ()
      => ToJSON a
      => Map (L.DRep StandardCrypto) a
      -> (L.Credential L.DRepRole StandardCrypto, L.DRepState StandardCrypto)
      -> (L.Credential L.DRepRole StandardCrypto, A.Value)
    drepStateToJson stakeDistr (cred, ds) =
      ( cred
      , A.object $
          [ "expiry" .= (ds ^. L.drepExpiryL)
          , "anchor" .= (ds ^. L.drepAnchorL)
          , "deposit" .= (ds ^. L.drepDepositL)
          ]
            <> ( case includeStake of
                  Cmd.WithStake -> ["stake" .= Map.lookup (L.DRepCredential cred) stakeDistr]
                  Cmd.NoStake -> []
               )
      )

runQueryDRepStakeDistribution
  :: Cmd.QueryDRepStakeDistributionCmdArgs era
  -> ExceptT QueryCmdError IO ()
runQueryDRepStakeDistribution
  Cmd.QueryDRepStakeDistributionCmdArgs
    { Cmd.eon
    , Cmd.nodeSocketPath
    , Cmd.consensusModeParams
    , Cmd.networkId
    , Cmd.drepHashSources = drepHashSources'
    , Cmd.target
    , Cmd.mOutFile
    } = conwayEraOnwardsConstraints eon $ do
    let localNodeConnInfo = LocalNodeConnectInfo consensusModeParams networkId nodeSocketPath

    let drepFromSource =
          fmap L.DRepCredential
            . firstExceptT QueryCmdDRepKeyError
            . readDRepCredential
        drepHashSources = case drepHashSources' of
          All -> []
          Only l -> l
    dreps <- Set.fromList <$> mapM drepFromSource drepHashSources

    drepStakeDistribution <- runQuery localNodeConnInfo target $ queryDRepStakeDistribution eon dreps
    writeOutput mOutFile $
      Map.assocs drepStakeDistribution

runQueryCommitteeMembersState
  :: Cmd.QueryCommitteeMembersStateCmdArgs era
  -> ExceptT QueryCmdError IO ()
runQueryCommitteeMembersState
  Cmd.QueryCommitteeMembersStateCmdArgs
    { Cmd.eon
    , Cmd.nodeSocketPath
    , Cmd.consensusModeParams
    , Cmd.networkId
    , Cmd.target
    , Cmd.mOutFile
    , Cmd.committeeColdKeys = coldCredKeys
    , Cmd.committeeHotKeys = hotCredKeys
    , Cmd.memberStatuses = memberStatuses
    } = conwayEraOnwardsConstraints eon $ do
    let localNodeConnInfo = LocalNodeConnectInfo consensusModeParams networkId nodeSocketPath

    let coldKeysFromVerKeyHashOrFile =
          modifyError QueryCmdCommitteeColdKeyError
            . readVerificationKeyOrHashOrFileOrScriptHash AsCommitteeColdKey unCommitteeColdKeyHash
    coldKeys <- Set.fromList <$> mapM coldKeysFromVerKeyHashOrFile coldCredKeys

    let hotKeysFromVerKeyHashOrFile =
          modifyError QueryCmdCommitteeHotKeyError
            . readVerificationKeyOrHashOrFileOrScriptHash AsCommitteeHotKey unCommitteeHotKeyHash
    hotKeys <- Set.fromList <$> mapM hotKeysFromVerKeyHashOrFile hotCredKeys

    committeeState <-
      runQuery localNodeConnInfo target $
        queryCommitteeMembersState eon coldKeys hotKeys (Set.fromList memberStatuses)
    writeOutput mOutFile $ A.toJSON committeeState

runQueryTreasuryValue
  :: Cmd.QueryTreasuryValueCmdArgs era
  -> ExceptT QueryCmdError IO ()
runQueryTreasuryValue
  Cmd.QueryTreasuryValueCmdArgs
    { Cmd.eon
    , Cmd.nodeSocketPath
    , Cmd.consensusModeParams
    , Cmd.networkId
    , Cmd.target
    , Cmd.mOutFile
    } = conwayEraOnwardsConstraints eon $ do
    let localNodeConnInfo = LocalNodeConnectInfo consensusModeParams networkId nodeSocketPath

    L.AccountState (L.Coin treasury) _reserves <-
      runQuery localNodeConnInfo target $ queryAccountState eon
    let treasuryString = show treasury
    case mOutFile of
      Nothing ->
        liftIO $ putStrLn treasuryString
      Just outFile ->
        firstExceptT QueryCmdWriteFileError . ExceptT $
          writeLazyByteStringFile outFile $
            LBS.pack treasuryString

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
  Nothing -> liftIO . LBS.putStrLn . encodePretty $ content
  Just (File f) ->
    handleIOExceptT (QueryCmdWriteFileError . FileIOError f) $
      LBS.writeFile f (encodePretty content)

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
  :: SocketPath
  -> ConsensusModeParams
  -> NetworkId
  -> Consensus.Target ChainPoint
  -> UTCTime
  -> ExceptT QueryCmdError IO SlotNo
utcTimeToSlotNo nodeSocketPath consensusModeParams networkId target utcTime = do
  let localNodeConnInfo = LocalNodeConnectInfo consensusModeParams networkId nodeSocketPath

  lift
    ( executeLocalStateQueryExpr localNodeConnInfo target $ runExceptT $ do
        systemStart <-
          lift querySystemStart
            & onLeft (left . QueryCmdUnsupportedNtcVersion)

        eraHistory <-
          lift queryEraHistory
            & onLeft (left . QueryCmdUnsupportedNtcVersion)

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
    ( QueryCmdLocalStateQueryError $
        mkEraMismatchError NodeEraMismatchError{nodeEra = era, era = minEra}
    )
    (forEraMaybeEon era)

-- | The output format to use, for commands with a recently introduced --output-[json,text] flag
-- and that used to have the following default: --out-file implies JSON,
-- output to stdout implied text.
newOutputFormat :: Maybe OutputFormatJsonOrText -> Maybe a -> OutputFormatJsonOrText
newOutputFormat format mOutFile =
  case (format, mOutFile) of
    (Just f, _) -> f -- Take flag from CLI if specified
    (Nothing, Nothing) -> OutputFormatText -- No CLI flag, writing to stdout: write text
    (Nothing, Just _) -> OutputFormatJson -- No CLI flag, writing to a file: write JSON

strictTextToLazyBytestring :: Text -> LBS.ByteString
strictTextToLazyBytestring t = BS.fromChunks [Text.encodeUtf8 t]
