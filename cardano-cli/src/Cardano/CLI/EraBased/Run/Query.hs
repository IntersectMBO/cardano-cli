{-# LANGUAGE DataKinds #-}
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

  , DelegationsAndRewards(..)
  , renderQueryCmdError
  , renderOpCertIntervalInformation
  , percentage
  ) where

import           Cardano.Api hiding (QueryInShelleyBasedEra (..))
import qualified Cardano.Api as Api
import           Cardano.Api.Byron hiding (QueryInShelleyBasedEra (..))
import qualified Cardano.Api.Ledger as L
import qualified Cardano.Api.Ledger as Ledger
import           Cardano.Api.Pretty
import           Cardano.Api.Shelley hiding (QueryInShelleyBasedEra (..))

import qualified Cardano.CLI.EraBased.Commands.Query as Cmd
import           Cardano.CLI.EraBased.Run.CreateTestnetData (readAndDecodeShelleyGenesis)
import           Cardano.CLI.Helpers
import           Cardano.CLI.Read
import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Errors.NodeEraMismatchError
import           Cardano.CLI.Types.Errors.QueryCmdError
import           Cardano.CLI.Types.Errors.QueryCmdLocalStateQueryError
import           Cardano.CLI.Types.Key
import qualified Cardano.CLI.Types.Output as O
import           Cardano.Crypto.Hash (hashToBytesAsHex)
import qualified Cardano.Crypto.Hash.Blake2b as Blake2b
import qualified Cardano.Ledger.BaseTypes as L
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Credential as L
import qualified Cardano.Ledger.Crypto as Crypto
import           Cardano.Ledger.Keys (KeyHash (..), KeyRole (..))
import           Cardano.Ledger.SafeHash (SafeHash)
import           Cardano.Ledger.Shelley.LedgerState
                   (PState (psFutureStakePoolParams, psRetiring, psStakePoolParams))
import qualified Cardano.Ledger.Shelley.LedgerState as SL
import           Cardano.Slotting.EpochInfo (EpochInfo (..), epochInfoSlotToUTCTime, hoistEpochInfo)
import           Ouroboros.Consensus.BlockchainTime.WallClock.Types (RelativeTime (..),
                   toRelativeTime)
import qualified Ouroboros.Consensus.Cardano.Block as Consensus
import qualified Ouroboros.Consensus.HardFork.History as Consensus
import qualified Ouroboros.Consensus.Protocol.Abstract as Consensus
import qualified Ouroboros.Consensus.Protocol.Praos.Common as Consensus
import           Ouroboros.Consensus.Protocol.TPraos (StandardCrypto)
import           Ouroboros.Network.Block (Serialised (..))

import           Control.Monad (forM, forM_, join)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.IO.Unlift (MonadIO (..))
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Except.Extra
import           Data.Aeson as Aeson
import qualified Data.Aeson as A
import           Data.Aeson.Encode.Pretty (encodePretty)
import           Data.Bifunctor (Bifunctor (..))
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
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as T
import qualified Data.Text.IO as Text
import qualified Data.Text.Lazy.IO as LT
import           Data.Time.Clock
import           Lens.Micro ((^.))
import           Numeric (showEFloat)
import           Prettyprinter
import           Prettyprinter.Render.Terminal (AnsiStyle)
import qualified System.IO as IO
import           Text.Printf (printf)

{- HLINT ignore "Move brackets to avoid $" -}
{- HLINT ignore "Redundant flip" -}

runQueryCmds :: Cmd.QueryCmds era -> ExceptT QueryCmdError IO ()
runQueryCmds = \case
  Cmd.QueryLeadershipScheduleCmd    args -> runQueryLeadershipScheduleCmd args
  Cmd.QueryProtocolParametersCmd    args -> runQueryProtocolParametersCmd args
  Cmd.QueryConstitutionHashCmd      args -> runQueryConstitutionHashCmd args
  Cmd.QueryTipCmd                   args -> runQueryTipCmd args
  Cmd.QueryStakePoolsCmd            args -> runQueryStakePoolsCmd args
  Cmd.QueryStakeDistributionCmd     args -> runQueryStakeDistributionCmd args
  Cmd.QueryStakeAddressInfoCmd      args -> runQueryStakeAddressInfoCmd args
  Cmd.QueryLedgerStateCmd           args -> runQueryLedgerStateCmd args
  Cmd.QueryStakeSnapshotCmd         args -> runQueryStakeSnapshotCmd args
  Cmd.QueryProtocolStateCmd         args -> runQueryProtocolStateCmd args
  Cmd.QueryUTxOCmd                  args -> runQueryUTxOCmd args
  Cmd.QueryKesPeriodInfoCmd         args -> runQueryKesPeriodInfoCmd args
  Cmd.QueryPoolStateCmd             args -> runQueryPoolStateCmd args
  Cmd.QueryTxMempoolCmd             args -> runQueryTxMempoolCmd args
  Cmd.QuerySlotNumberCmd            args -> runQuerySlotNumberCmd args
  Cmd.QueryConstitutionCmd          args -> runQueryConstitution args
  Cmd.QueryGovStateCmd              args -> runQueryGovState args
  Cmd.QueryDRepStateCmd             args -> runQueryDRepState args
  Cmd.QueryDRepStakeDistributionCmd args -> runQueryDRepStakeDistribution args
  Cmd.QueryCommitteeMembersStateCmd args -> runQueryCommitteeMembersState args

runQueryConstitutionHashCmd :: ()
  => Cmd.QueryConstitutionHashCmdArgs
  -> ExceptT QueryCmdError IO ()
runQueryConstitutionHashCmd
    Cmd.QueryConstitutionHashCmdArgs
    { Cmd.nodeSocketPath
    , Cmd.consensusModeParams
    , Cmd.networkId
    , Cmd.mOutFile
    } = do
  let localNodeConnInfo = LocalNodeConnectInfo consensusModeParams networkId nodeSocketPath

  result <- liftIO $ executeLocalStateQueryExpr localNodeConnInfo Nothing $ runExceptT $ do
    AnyCardanoEra era <- lift queryCurrentEra & onLeft (left . QueryCmdUnsupportedNtcVersion)

    sbe <- requireShelleyBasedEra era
      & onNothing (left QueryCmdByronEra)

    lift (shelleyBasedEraConstraints sbe (queryConstitutionHash sbe))
      & onLeft (left . QueryCmdUnsupportedNtcVersion)
      & onLeft (left . QueryCmdEraMismatch)

  writeConstitutionHash mOutFile =<< except (join (first QueryCmdAcquireFailure result))
  where
    writeConstitutionHash
      :: Maybe (File () Out)
      -> Maybe (SafeHash StandardCrypto L.AnchorData)
      -> ExceptT QueryCmdError IO ()
    writeConstitutionHash mOutFile' cHash =
      case mOutFile' of
        Nothing -> liftIO $ LBS.putStrLn (encodePretty cHash)
        Just (File fpath) ->
          handleIOExceptT (QueryCmdWriteFileError . FileIOError fpath) $
            LBS.writeFile fpath (encodePretty cHash)

runQueryProtocolParametersCmd :: ()
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
  AnyCardanoEra era <- firstExceptT QueryCmdAcquireFailure $ newExceptT $ determineEra localNodeConnInfo
  sbe <- forEraInEon @ShelleyBasedEra era (left QueryCmdByronEra) pure
  let qInMode = QueryInEra $ QueryInShelleyBasedEra sbe Api.QueryProtocolParameters
  pp <- firstExceptT QueryCmdConvenienceError
          . newExceptT $ executeQueryAnyMode localNodeConnInfo qInMode
  writeProtocolParameters sbe mOutFile pp
  where
    -- TODO: Conway era - use ledger PParams JSON
    writeProtocolParameters
      :: ShelleyBasedEra era
      -> Maybe (File () Out)
      -> Ledger.PParams (ShelleyLedgerEra era)
      -> ExceptT QueryCmdError IO ()
    writeProtocolParameters sbe mOutFile' pparams =
      let apiPParamsJSON = (encodePretty $ fromLedgerPParams sbe pparams)
      in case mOutFile' of
        Nothing -> liftIO $ LBS.putStrLn apiPParamsJSON
        Just (File fpath) ->
          handleIOExceptT (QueryCmdWriteFileError . FileIOError fpath) $
            LBS.writeFile fpath apiPParamsJSON

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
  where -- All calculations are in seconds (Integer)
        t  = relativeTimeSeconds tolerance
        -- Plus 1 to prevent division by zero.  The 's' prefix stands for strictly-positive.
        sa = relativeTimeSeconds a + 1
        sb = relativeTimeSeconds b + 1
        -- Fast forward the 'nowTime` by the tolerance, but don't let the result exceed the tip time.
        ua = min (sa + t) sb
        ub = sb
        -- Final percentage to render as text.
        pc = id @Double (fromIntegral ua / fromIntegral  ub) * 100.0

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

runQueryTipCmd :: ()
  => Cmd.QueryTipCmdArgs
  -> ExceptT QueryCmdError IO ()
runQueryTipCmd
    Cmd.QueryTipCmdArgs
    { Cmd.nodeSocketPath
    , Cmd.consensusModeParams
    , Cmd.networkId
    , Cmd.mOutFile
    } = do
  let localNodeConnInfo = LocalNodeConnectInfo consensusModeParams networkId nodeSocketPath

  eLocalState <- ExceptT $ fmap sequence $
    executeLocalStateQueryExpr localNodeConnInfo Nothing $ runExceptT $ do
      era <- lift queryCurrentEra & onLeft (left . QueryCmdUnsupportedNtcVersion)
      eraHistory <- lift queryEraHistory & onLeft (left . QueryCmdUnsupportedNtcVersion)
      mChainBlockNo <- lift queryChainBlockNo & onLeft (left . QueryCmdUnsupportedNtcVersion) & fmap Just
      mChainPoint <- lift queryChainPoint & onLeft (left . QueryCmdUnsupportedNtcVersion) & fmap Just
      mSystemStart <- lift querySystemStart & onLeft (left . QueryCmdUnsupportedNtcVersion) & fmap Just

      return O.QueryTipLocalState
        { O.era = era
        , O.eraHistory = eraHistory
        , O.mSystemStart = mSystemStart
        , O.mChainTip = makeChainTip <$> mChainBlockNo <*> mChainPoint
        }

  mLocalState <- hushM (first QueryCmdAcquireFailure eLocalState) $ \e ->
    liftIO . LT.hPutStrLn IO.stderr $ docToLazyText $ "Warning: Local state unavailable: " <> renderQueryCmdError e

  chainTip <- pure (mLocalState >>= O.mChainTip)
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
        liftIO . LT.hPutStrLn IO.stderr $ docToLazyText $
          "Warning: Epoch unavailable: " <> renderQueryCmdError (QueryCmdPastHorizon e)
        return $ O.QueryTipLocalStateOutput
          { O.localStateChainTip = chainTip
          , O.mEra = Nothing
          , O.mEpoch = Nothing
          , O.mSyncProgress = Nothing
          , O.mSlotInEpoch = Nothing
          , O.mSlotsToEpochEnd = Nothing
          }

      Right (epochNo, SlotsInEpoch slotsInEpoch, SlotsToEpochEnd slotsToEpochEnd) -> do
        syncProgressResult <- runExceptT $ do
          systemStart <- fmap getSystemStart (O.mSystemStart localState) & hoistMaybe QueryCmdSystemStartUnavailable
          nowSeconds <- toRelativeTime (SystemStart systemStart) <$> liftIO getCurrentTime
          tipTimeResult <- getProgress tipSlotNo (O.eraHistory localState) & bimap QueryCmdPastHorizon fst & except

          let tolerance = RelativeTime (secondsToNominalDiffTime 600)

          return $ flip (percentage tolerance) nowSeconds tipTimeResult

        mSyncProgress <- hushM syncProgressResult $ \e -> do
          liftIO . LT.hPutStrLn IO.stderr $ docToLazyText $ "Warning: Sync progress unavailable: " <> renderQueryCmdError e

        return $ O.QueryTipLocalStateOutput
          { O.localStateChainTip = chainTip
          , O.mEra = Just (O.era localState)
          , O.mEpoch = Just epochNo
          , O.mSlotInEpoch = Just slotsInEpoch
          , O.mSlotsToEpochEnd = Just slotsToEpochEnd
          , O.mSyncProgress = mSyncProgress
          }

  case mOutFile of
    Just (File fpath) -> liftIO $ LBS.writeFile fpath $ encodePretty localStateOutput
    Nothing                 -> liftIO $ LBS.putStrLn        $ encodePretty localStateOutput

-- | Query the UTxO, filtered by a given set of addresses, from a Shelley node
-- via the local state query protocol.
runQueryUTxOCmd :: ()
  => Cmd.QueryUTxOCmdArgs
  -> ExceptT QueryCmdError IO ()
runQueryUTxOCmd
    Cmd.QueryUTxOCmdArgs
    { Cmd.nodeSocketPath
    , Cmd.consensusModeParams
    , Cmd.queryFilter
    , Cmd.networkId
    , Cmd.mOutFile
    } = do
  let localNodeConnInfo = LocalNodeConnectInfo consensusModeParams networkId nodeSocketPath

  join $ lift
    ( executeLocalStateQueryExpr localNodeConnInfo Nothing $ runExceptT $ do
        AnyCardanoEra era <- lift queryCurrentEra
          & onLeft (left . QueryCmdUnsupportedNtcVersion)

        sbe <- requireShelleyBasedEra era
          & onNothing (left QueryCmdByronEra)

        utxo <- lift (queryUtxo sbe queryFilter)
          & onLeft (left . QueryCmdUnsupportedNtcVersion)
          & onLeft (left . QueryCmdLocalStateQueryError . EraMismatchError)

        pure $ do
          writeFilteredUTxOs sbe mOutFile utxo
    )
    & onLeft (left . QueryCmdAcquireFailure)
    & onLeft left

runQueryKesPeriodInfoCmd :: ()
  => Cmd.QueryKesPeriodInfoCmdArgs
  -> ExceptT QueryCmdError IO ()
runQueryKesPeriodInfoCmd
    Cmd.QueryKesPeriodInfoCmdArgs
    { Cmd.nodeSocketPath
    , Cmd.consensusModeParams
    , Cmd.networkId
    , Cmd.nodeOpCertFp
    , Cmd.mOutFile
    } = do
  opCert <- lift (readFileTextEnvelope AsOperationalCertificate nodeOpCertFp)
    & onLeft (left . QueryCmdOpCertCounterReadError)

  let localNodeConnInfo = LocalNodeConnectInfo consensusModeParams networkId nodeSocketPath

  join $ lift
    ( executeLocalStateQueryExpr localNodeConnInfo Nothing $ runExceptT $ do
        AnyCardanoEra era <- lift queryCurrentEra
          & onLeft (left . QueryCmdUnsupportedNtcVersion)

        sbe <- requireShelleyBasedEra era
          & onNothing (left QueryCmdByronEra)

        -- We check that the KES period specified in the operational certificate is correct
        -- based on the KES period defined in the genesis parameters and the current slot number
        gParams <- lift (queryGenesisParameters sbe)
          & onLeft (left . QueryCmdUnsupportedNtcVersion)
          & onLeft (left . QueryCmdLocalStateQueryError . EraMismatchError)

        eraHistory <- lift queryEraHistory
          & onLeft (left . QueryCmdUnsupportedNtcVersion)

        let eInfo = toTentativeEpochInfo eraHistory

        -- We get the operational certificate counter from the protocol state and check that
        -- it is equivalent to what we have on disk.
        ptclState <- lift (queryProtocolState sbe)
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
          liftIO . putStrLn $ docToString $ renderOpCertIntervalInformation (unFile nodeOpCertFp) opCertIntervalInformation
          liftIO . putStrLn $ docToString $ renderOpCertNodeAndOnDiskCounterInformation (unFile nodeOpCertFp) counterInformation

          let qKesInfoOutput = createQueryKesPeriodInfoOutput opCertIntervalInformation counterInformation eInfo gParams
              kesPeriodInfoJSON = encodePretty qKesInfoOutput

          liftIO $ LBS.putStrLn kesPeriodInfoJSON
          forM_ mOutFile (\(File oFp) ->
            handleIOExceptT (QueryCmdWriteFileError . FileIOError oFp)
              $ LBS.writeFile oFp kesPeriodInfoJSON)
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
            slotsTillExp = SlotsTillKesKeyExpiry . SlotNo $ (oCertEnd * fromIntegral (protocolParamSlotsPerKESPeriod gParams)) - cSlot
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
      let time = epochInfoSlotToUTCTime
                   (tentative eInfo)
                   (SystemStart $ protocolParamSystemStart gParams)
                   (fromIntegral $ oCertExpiryKesPeriod * fromIntegral (protocolParamSlotsPerKESPeriod gParams))
      in case time of
           Left _ -> Nothing
           Right t -> Just t

    renderOpCertNodeAndOnDiskCounterInformation :: FilePath -> OpCertNodeAndOnDiskCounterInformation -> Doc AnsiStyle
    renderOpCertNodeAndOnDiskCounterInformation opCertFile = \case
      OpCertOnDiskCounterEqualToNodeState _ _ ->
        green "✓" <+> hang 0
            ( vsep
              [ "The operational certificate counter agrees with the node protocol state counter"
              ]
            )
      OpCertOnDiskCounterAheadOfNodeState _ _ ->
        green "✓" <+> hang 0
            ( vsep
              [ "The operational certificate counter ahead of the node protocol state counter by 1"
              ]
            )
      OpCertOnDiskCounterTooFarAheadOfNodeState onDiskC nodeStateC ->
        red "✗" <+> hang 0
          ( vsep
            [ "The operational certificate counter too far ahead of the node protocol state counter in the operational certificate at: " <> pretty opCertFile
            , "On disk operational certificate counter: " <> pretty (unOpCertOnDiskCounter onDiskC)
            , "Protocol state counter: " <> pretty (unOpCertNodeStateCounter nodeStateC)
            ]
          )
      OpCertOnDiskCounterBehindNodeState onDiskC nodeStateC ->
        red "✗" <+> hang 0
          ( vsep
            [ "The protocol state counter is greater than the counter in the operational certificate at: " <> pretty opCertFile
            , "On disk operational certificate counter: " <> pretty (unOpCertOnDiskCounter onDiskC)
            , "Protocol state counter: " <> pretty (unOpCertNodeStateCounter nodeStateC)
            ]
          )
      OpCertNoBlocksMintedYet (OpCertOnDiskCounter onDiskC) ->
        red "✗" <+> hang 0
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
    createQueryKesPeriodInfoOutput oCertIntervalInfo oCertCounterInfo eInfo gParams  =
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
    opCertOnDiskAndStateCounters :: forall era . ()
       => Consensus.PraosProtocolSupportsNode (ConsensusProtocol era)
       => FromCBOR (Consensus.ChainDepState (ConsensusProtocol era))
       => Crypto.ADDRHASH (Consensus.PraosProtocolSupportsNodeCrypto (ConsensusProtocol era)) ~ Blake2b.Blake2b_224
       => ProtocolState era
       -> OperationalCertificate
       -> ExceptT QueryCmdError IO (OpCertOnDiskCounter, Maybe OpCertNodeStateCounter)
    opCertOnDiskAndStateCounters ptclState opCert@(OperationalCertificate _ stakePoolVKey) = do
      let onDiskOpCertCount = fromIntegral $ getOpCertCount opCert

      chainDepState <- pure (decodeProtocolState ptclState)
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
    green "✓" <+> hang 0
      ( vsep
        [ "Operational certificate's KES period is within the correct KES period interval"
        ]
      )
  OpCertStartingKesPeriodIsInTheFuture (OpCertStartingKesPeriod start) (OpCertEndingKesPeriod end) (CurrentKesPeriod current) ->
    red "✗" <+> hang 0
      ( vsep
        [ "Node operational certificate at: " <> pretty opCertFile <> " has an incorrectly specified starting KES period. "
        , "Current KES period: " <> pretty current
        , "Operational certificate's starting KES period: " <> pretty start
        , "Operational certificate's expiry KES period: " <> pretty end
        ]
      )
  OpCertExpired _ (OpCertEndingKesPeriod end) (CurrentKesPeriod current) ->
    red "✗" <+> hang 0
      ( vsep
        [ "Node operational certificate at: " <> pretty opCertFile <> " has expired. "
        , "Current KES period: " <> pretty current
        , "Operational certificate's expiry KES period: " <> pretty end
        ]
      )

  OpCertSomeOtherError (OpCertStartingKesPeriod start) (OpCertEndingKesPeriod end) (CurrentKesPeriod current) ->
    red "✗" <+> hang 0
      ( vsep
        [ "An unknown error occurred with operational certificate at: " <> pretty opCertFile
        , "Current KES period: " <> pretty current
        , "Operational certificate's starting KES period: " <> pretty start
        , "Operational certificate's expiry KES period: " <> pretty end
        ]
      )

-- | Query the current and future parameters for a stake pool, including the retirement date.
-- Any of these may be empty (in which case a null will be displayed).
--
runQueryPoolStateCmd :: ()
  => Cmd.QueryPoolStateCmdArgs
  -> ExceptT QueryCmdError IO ()
runQueryPoolStateCmd
    Cmd.QueryPoolStateCmdArgs
    { Cmd.nodeSocketPath
    , Cmd.consensusModeParams
    , Cmd.networkId
    , Cmd.poolIds
    } = do
  let localNodeConnInfo = LocalNodeConnectInfo consensusModeParams networkId nodeSocketPath

  join $ lift
    ( executeLocalStateQueryExpr localNodeConnInfo Nothing $ runExceptT $ do
        AnyCardanoEra era <- lift queryCurrentEra
          & onLeft (left . QueryCmdUnsupportedNtcVersion)

        sbe <- requireShelleyBasedEra era
          & onNothing (left QueryCmdByronEra)

        beo <- requireEon BabbageEra era

        result <- lift (queryPoolState beo $ Just $ Set.fromList poolIds)
          & onLeft (left . QueryCmdUnsupportedNtcVersion)
          & onLeft (left . QueryCmdLocalStateQueryError . EraMismatchError)

        pure $ do
          shelleyBasedEraConstraints sbe writePoolState result
    )
    & onLeft (left . QueryCmdAcquireFailure)
    & onLeft left

-- | Query the local mempool state
runQueryTxMempoolCmd :: ()
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
        AnyCardanoEra era <- lift (determineEra localNodeConnInfo)
          & onLeft (left . QueryCmdAcquireFailure)
        pure $ LocalTxMonitoringQueryTx $ TxIdInMode era tx
      TxMempoolQueryNextTx -> pure LocalTxMonitoringSendNextTx
      TxMempoolQueryInfo -> pure LocalTxMonitoringMempoolInformation

  result <- liftIO $ queryTxMonitoringLocal localNodeConnInfo localQuery
  let renderedResult = encodePretty result
  case mOutFile of
    Nothing -> liftIO $ LBS.putStrLn renderedResult
    Just (File oFp) -> handleIOExceptT (QueryCmdWriteFileError . FileIOError oFp)
        $ LBS.writeFile oFp renderedResult

runQuerySlotNumberCmd :: ()
  => Cmd.QuerySlotNumberCmdArgs
  -> ExceptT QueryCmdError IO ()
runQuerySlotNumberCmd
    Cmd.QuerySlotNumberCmdArgs
    { Cmd.nodeSocketPath
    , Cmd.consensusModeParams
    , Cmd.networkId
    , Cmd.utcTime
    } = do
  SlotNo slotNo <- utcTimeToSlotNo nodeSocketPath consensusModeParams networkId utcTime
  liftIO . putStr $ show slotNo

-- | Obtain stake snapshot information for a pool, plus information about the total active stake.
-- This information can be used for leader slot calculation, for example, and has been requested by SPOs.
-- Obtaining the information directly is significantly more time and memory efficient than using a full ledger state dump.
runQueryStakeSnapshotCmd :: ()
  => Cmd.QueryStakeSnapshotCmdArgs
  -> ExceptT QueryCmdError IO ()
runQueryStakeSnapshotCmd
    Cmd.QueryStakeSnapshotCmdArgs
    { Cmd.nodeSocketPath
    , Cmd.consensusModeParams
    , Cmd.networkId
    , Cmd.allOrOnlyPoolIds
    , Cmd.mOutFile
    } = do
  let localNodeConnInfo = LocalNodeConnectInfo consensusModeParams networkId nodeSocketPath

  join $ lift
    ( executeLocalStateQueryExpr localNodeConnInfo Nothing $ runExceptT $ do
        AnyCardanoEra era <- lift queryCurrentEra
          & onLeft (left . QueryCmdUnsupportedNtcVersion)

        sbe <- requireShelleyBasedEra era
          & onNothing (left QueryCmdByronEra)

        let poolFilter = case allOrOnlyPoolIds of
              All -> Nothing
              Only poolIds -> Just $ Set.fromList poolIds

        beo <- requireEon BabbageEra era

        result <- lift (queryStakeSnapshot beo poolFilter)
          & onLeft (left . QueryCmdUnsupportedNtcVersion)
          & onLeft (left . QueryCmdLocalStateQueryError . EraMismatchError)

        pure $ do
          shelleyBasedEraConstraints sbe (writeStakeSnapshots mOutFile) result
    )
    & onLeft (left . QueryCmdAcquireFailure)
    & onLeft left

runQueryLedgerStateCmd :: ()
  => Cmd.QueryLedgerStateCmdArgs
  -> ExceptT QueryCmdError IO ()
runQueryLedgerStateCmd
    Cmd.QueryLedgerStateCmdArgs
    { Cmd.nodeSocketPath
    , Cmd.consensusModeParams
    , Cmd.networkId
    , Cmd.mOutFile
     } = do
  let localNodeConnInfo = LocalNodeConnectInfo consensusModeParams networkId nodeSocketPath

  join $ lift
    ( executeLocalStateQueryExpr localNodeConnInfo Nothing $ runExceptT $ do
        AnyCardanoEra era <- lift queryCurrentEra
          & onLeft (left . QueryCmdUnsupportedNtcVersion)

        sbe <- requireShelleyBasedEra era
          & onNothing (left QueryCmdByronEra)

        result <- lift (queryDebugLedgerState sbe)
          & onLeft (left . QueryCmdUnsupportedNtcVersion)
          & onLeft (left . QueryCmdLocalStateQueryError . EraMismatchError)

        pure $ do
          shelleyBasedEraConstraints sbe (writeLedgerState mOutFile) result
    )
    & onLeft (left . QueryCmdAcquireFailure)
    & onLeft left

runQueryProtocolStateCmd :: ()
  => Cmd.QueryProtocolStateCmdArgs
  -> ExceptT QueryCmdError IO ()
runQueryProtocolStateCmd
    Cmd.QueryProtocolStateCmdArgs
    { Cmd.nodeSocketPath
    , Cmd.consensusModeParams
    , Cmd.networkId
    , Cmd.mOutFile
     } = do
  let localNodeConnInfo = LocalNodeConnectInfo consensusModeParams networkId nodeSocketPath

  join $ lift
    ( executeLocalStateQueryExpr localNodeConnInfo Nothing $ runExceptT $ do
        AnyCardanoEra era <- lift queryCurrentEra
          & onLeft (left . QueryCmdUnsupportedNtcVersion)

        sbe <- requireShelleyBasedEra era
          & onNothing (left QueryCmdByronEra)

        result <- lift (queryProtocolState sbe)
          & onLeft (left . QueryCmdUnsupportedNtcVersion)
          & onLeft (left . QueryCmdLocalStateQueryError . EraMismatchError)

        pure $ shelleyBasedEraConstraints sbe $ writeProtocolState mOutFile result
    )
    & onLeft (left . QueryCmdAcquireFailure)
    & onLeft left

-- | Query the current delegations and reward accounts, filtered by a given
-- set of addresses, from a Shelley node via the local state query protocol.

runQueryStakeAddressInfoCmd :: ()
  => Cmd.QueryStakeAddressInfoCmdArgs
  -> ExceptT QueryCmdError IO ()
runQueryStakeAddressInfoCmd
    Cmd.QueryStakeAddressInfoCmdArgs
    { Cmd.nodeSocketPath
    , Cmd.consensusModeParams
    , Cmd.addr = StakeAddress _ addr
    , Cmd.networkId
    , Cmd.mOutFile
    } = do
  let localNodeConnInfo = LocalNodeConnectInfo consensusModeParams networkId nodeSocketPath

  join $ lift
    ( executeLocalStateQueryExpr localNodeConnInfo Nothing $ runExceptT $ do
        AnyCardanoEra era <- lift queryCurrentEra
          & onLeft (left . QueryCmdUnsupportedNtcVersion)

        sbe <- requireShelleyBasedEra era
          & onNothing (left QueryCmdByronEra)

        let stakeAddr = Set.singleton $ fromShelleyStakeCredential addr

        (stakeRewardAccountBalances, stakePools) <- lift (queryStakeAddresses sbe stakeAddr networkId)
          & onLeft (left . QueryCmdUnsupportedNtcVersion)
          & onLeft (left . QueryCmdLocalStateQueryError . EraMismatchError)

        beo <- requireEon BabbageEra era

        stakeDelegDeposits <- lift (queryStakeDelegDeposits beo stakeAddr)
          & onLeft (left . QueryCmdUnsupportedNtcVersion)
          & onLeft (left . QueryCmdLocalStateQueryError . EraMismatchError)

        stakeVoteDelegatees <- monoidForEraInEonA era $ \ceo ->
          lift (queryStakeVoteDelegatees ceo stakeAddr)
            & onLeft (left . QueryCmdUnsupportedNtcVersion)
            & onLeft (left . QueryCmdLocalStateQueryError . EraMismatchError)

        return $ do
          writeStakeAddressInfo
            era
            mOutFile
            (DelegationsAndRewards (stakeRewardAccountBalances, stakePools))
            (Map.mapKeys (makeStakeAddress networkId) stakeDelegDeposits)
            (Map.mapKeys (makeStakeAddress networkId) stakeVoteDelegatees)
    )
    & onLeft (left . QueryCmdAcquireFailure)
    & onLeft left

-- -------------------------------------------------------------------------------------------------

writeStakeAddressInfo
  :: CardanoEra era
  -> Maybe (File () Out)
  -> DelegationsAndRewards
  -> Map StakeAddress Lovelace -- ^ deposits
  -> Map StakeAddress (L.DRep L.StandardCrypto) -- ^ vote delegatees
  -> ExceptT QueryCmdError IO ()
writeStakeAddressInfo
  era
  mOutFile
  (DelegationsAndRewards (stakeAccountBalances, stakePools))
  stakeDelegDeposits
  voteDelegatees =
    firstExceptT QueryCmdWriteFileError . newExceptT
      $ writeLazyByteStringOutput mOutFile (encodePretty jsonInfo)
 where
  jsonInfo :: [Aeson.Value]
  jsonInfo =
    map
      (\(addr, mBalance, mPoolId, mDRep, mDeposit) ->
        Aeson.object
        [ "address" .= addr
        , forEraInEon @ConwayEraOnwards era  "delegation" (const "stakeDelegation") .= mPoolId
        , "voteDelegation" .= fmap friendlyDRep mDRep
        , "rewardAccountBalance" .= mBalance
        , "delegationDeposit" .= mDeposit
        ]
      )
      merged

  friendlyDRep :: L.DRep L.StandardCrypto -> Text
  friendlyDRep L.DRepAlwaysAbstain = "alwaysAbstain"
  friendlyDRep L.DRepAlwaysNoConfidence = "alwaysNoConfidence"
  friendlyDRep (L.DRepCredential cred) =
    L.credToText cred -- this will pring "keyHash-..." or "scriptHash-...", depending on the type of credential

  merged :: [(StakeAddress, Maybe Lovelace, Maybe PoolId, Maybe (L.DRep L.StandardCrypto), Maybe Lovelace)]
  merged =
    [ (addr, mBalance, mPoolId, mDRep, mDeposit)
    | addr <- Set.toList (Set.unions [ Map.keysSet stakeAccountBalances
                                     , Map.keysSet stakePools
                                     , Map.keysSet stakeDelegDeposits
                                     , Map.keysSet voteDelegatees
                                     ])
    , let mBalance = Map.lookup addr stakeAccountBalances
          mPoolId  = Map.lookup addr stakePools
          mDeposit = Map.lookup addr stakeDelegDeposits
          mDRep = Map.lookup addr voteDelegatees
    ]

writeLedgerState :: forall era ledgerera.
                    ShelleyLedgerEra era ~ ledgerera
                 => ToJSON (DebugLedgerState era)
                 => FromCBOR (DebugLedgerState era)
                 => Maybe (File () Out)
                 -> SerialisedDebugLedgerState era
                 -> ExceptT QueryCmdError IO ()
writeLedgerState mOutFile qState@(SerialisedDebugLedgerState serLedgerState) =
  case mOutFile of
    Nothing ->
      case decodeDebugLedgerState qState of
        Left bs -> firstExceptT QueryCmdHelpersError $ pPrintCBOR bs
        Right ledgerState -> liftIO . LBS.putStrLn $ Aeson.encode ledgerState
    Just (File fpath) ->
      handleIOExceptT (QueryCmdWriteFileError . FileIOError fpath)
        $ LBS.writeFile fpath $ unSerialised serLedgerState

writeStakeSnapshots :: forall era ledgerera. ()
  => ShelleyLedgerEra era ~ ledgerera
  => Core.EraCrypto ledgerera ~ StandardCrypto
  => Maybe (File () Out)
  -> SerialisedStakeSnapshots era
  -> ExceptT QueryCmdError IO ()
writeStakeSnapshots mOutFile qState = do
  StakeSnapshot snapshot <- pure (decodeStakeSnapshot qState)
    & onLeft (left . QueryCmdStakeSnapshotDecodeError)

  -- Calculate the three pool and active stake values for the given pool
  liftIO . maybe LBS.putStrLn (LBS.writeFile . unFile) mOutFile $ encodePretty snapshot

-- | This function obtains the pool parameters, equivalent to the following jq query on the output of query ledger-state
--   .nesEs.esLState.lsDPState.dpsPState.psStakePoolParams.<pool_id>
writePoolState :: forall era ledgerera. ()
  => ShelleyLedgerEra era ~ ledgerera
  => Core.EraCrypto ledgerera ~ StandardCrypto
  => Core.Era ledgerera
  => SerialisedPoolState era
  -> ExceptT QueryCmdError IO ()
writePoolState serialisedCurrentEpochState = do
  PoolState poolState <- pure (decodePoolState serialisedCurrentEpochState)
    & onLeft (left . QueryCmdPoolStateDecodeError)

  let hks = Set.toList $ Set.fromList $ Map.keys (psStakePoolParams poolState)
            <> Map.keys (psFutureStakePoolParams poolState) <> Map.keys (psRetiring poolState)

  let poolStates :: Map (KeyHash 'StakePool StandardCrypto) (Params StandardCrypto)
      poolStates = Map.fromList $ hks <&>
        ( \hk ->
          ( hk
          , Params
            { poolParameters        = Map.lookup hk (SL.psStakePoolParams  poolState)
            , futurePoolParameters  = Map.lookup hk (SL.psFutureStakePoolParams poolState)
            , retiringEpoch         = Map.lookup hk (SL.psRetiring poolState)
            }
          )
        )

  liftIO . LBS.putStrLn $ encodePretty poolStates

writeProtocolState ::
  ( FromCBOR (Consensus.ChainDepState (ConsensusProtocol era))
  , ToJSON (Consensus.ChainDepState (ConsensusProtocol era))
  )
  => Maybe (File () Out)
  -> ProtocolState era
  -> ExceptT QueryCmdError IO ()
writeProtocolState mOutFile ps@(ProtocolState pstate) =
  case mOutFile of
    Nothing -> case decodeProtocolState ps of
      Left (bs, _) -> firstExceptT QueryCmdHelpersError $ pPrintCBOR bs
      Right chainDepstate -> liftIO . LBS.putStrLn $ encodePretty chainDepstate
    Just (File fpath) ->
      handleIOExceptT (QueryCmdWriteFileError . FileIOError fpath)
        . LBS.writeFile fpath $ unSerialised pstate

writeFilteredUTxOs :: Api.ShelleyBasedEra era
                   -> Maybe (File () Out)
                   -> UTxO era
                   -> ExceptT QueryCmdError IO ()
writeFilteredUTxOs sbe mOutFile utxo =
  case mOutFile of
    Nothing -> liftIO $ printFilteredUTxOs sbe utxo
    Just (File fpath) ->
      case sbe of
        ShelleyBasedEraShelley -> writeUTxo fpath utxo
        ShelleyBasedEraAllegra -> writeUTxo fpath utxo
        ShelleyBasedEraMary -> writeUTxo fpath utxo
        ShelleyBasedEraAlonzo -> writeUTxo fpath utxo
        ShelleyBasedEraBabbage -> writeUTxo fpath utxo
        ShelleyBasedEraConway -> writeUTxo fpath utxo
 where
   writeUTxo fpath utxo' =
     handleIOExceptT (QueryCmdWriteFileError . FileIOError fpath)
       $ LBS.writeFile fpath (encodePretty utxo')

printFilteredUTxOs :: Api.ShelleyBasedEra era -> UTxO era -> IO ()
printFilteredUTxOs sbe (UTxO utxo) = do
  Text.putStrLn title
  putStrLn $ replicate (Text.length title + 2) '-'
  case sbe of
    ShelleyBasedEraShelley ->
      mapM_ (printUtxo sbe) $ Map.toList utxo
    ShelleyBasedEraAllegra ->
      mapM_ (printUtxo sbe) $ Map.toList utxo
    ShelleyBasedEraMary    ->
      mapM_ (printUtxo sbe) $ Map.toList utxo
    ShelleyBasedEraAlonzo ->
      mapM_ (printUtxo sbe) $ Map.toList utxo
    ShelleyBasedEraBabbage ->
      mapM_ (printUtxo sbe) $ Map.toList utxo
    ShelleyBasedEraConway ->
      mapM_ (printUtxo sbe) $ Map.toList utxo

 where
   title :: Text
   title =
     "                           TxHash                                 TxIx        Amount"

printUtxo
  :: Api.ShelleyBasedEra era
  -> (TxIn, TxOut CtxUTxO era)
  -> IO ()
printUtxo sbe txInOutTuple =
  case sbe of
    ShelleyBasedEraShelley ->
      let (TxIn (TxId txhash) (TxIx index), TxOut _ value _ _) = txInOutTuple
      in Text.putStrLn $
           mconcat
             [ Text.decodeLatin1 (hashToBytesAsHex txhash)
             , textShowN 6 index
             , "        " <> printableValue value
             ]

    ShelleyBasedEraAllegra ->
      let (TxIn (TxId txhash) (TxIx index), TxOut _ value _ _) = txInOutTuple
      in Text.putStrLn $
           mconcat
             [ Text.decodeLatin1 (hashToBytesAsHex txhash)
             , textShowN 6 index
             , "        " <> printableValue value
             ]
    ShelleyBasedEraMary ->
      let (TxIn (TxId txhash) (TxIx index), TxOut _ value _ _) = txInOutTuple
      in Text.putStrLn $
           mconcat
             [ Text.decodeLatin1 (hashToBytesAsHex txhash)
             , textShowN 6 index
             , "        " <> printableValue value
             ]
    ShelleyBasedEraAlonzo ->
      let (TxIn (TxId txhash) (TxIx index), TxOut _ value mDatum _) = txInOutTuple
      in Text.putStrLn $
           mconcat
             [ Text.decodeLatin1 (hashToBytesAsHex txhash)
             , textShowN 6 index
             , "        " <> printableValue value <> " + " <> Text.pack (show mDatum)
             ]
    ShelleyBasedEraBabbage ->
      let (TxIn (TxId txhash) (TxIx index), TxOut _ value mDatum _) = txInOutTuple
      in Text.putStrLn $
           mconcat
             [ Text.decodeLatin1 (hashToBytesAsHex txhash)
             , textShowN 6 index
             , "        " <> printableValue value <> " + " <> Text.pack (show mDatum)
             ]
    ShelleyBasedEraConway ->
      let (TxIn (TxId txhash) (TxIx index), TxOut _ value mDatum _) = txInOutTuple
      in Text.putStrLn $
           mconcat
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
    TxOutValueByron (Lovelace i) -> Text.pack $ show i
    TxOutValueShelleyBased sbe2 val -> renderValue $ Api.fromLedgerValue sbe2 val

runQueryStakePoolsCmd :: ()
  => Cmd.QueryStakePoolsCmdArgs
  -> ExceptT QueryCmdError IO ()
runQueryStakePoolsCmd
    Cmd.QueryStakePoolsCmdArgs
    { Cmd.nodeSocketPath
    , Cmd.consensusModeParams
    , Cmd.networkId
    , Cmd.mOutFile
    } = do
  let localNodeConnInfo = LocalNodeConnectInfo consensusModeParams networkId nodeSocketPath

  join $ lift
    ( executeLocalStateQueryExpr localNodeConnInfo Nothing $ runExceptT @QueryCmdError $ do
        AnyCardanoEra era <- lift queryCurrentEra & onLeft (left . QueryCmdUnsupportedNtcVersion)

        sbe <- requireShelleyBasedEra era
          & onNothing (left QueryCmdByronEra)

        poolIds <- lift (queryStakePools sbe)
          & onLeft (left . QueryCmdUnsupportedNtcVersion)
          & onLeft (left . QueryCmdEraMismatch)

        pure $ do
          writeStakePools mOutFile poolIds
    ) & onLeft (left . QueryCmdAcquireFailure)
      & onLeft left

writeStakePools
  :: Maybe (File () Out)
  -> Set PoolId
  -> ExceptT QueryCmdError IO ()
writeStakePools (Just (File outFile)) stakePools =
  handleIOExceptT (QueryCmdWriteFileError . FileIOError outFile) $
    LBS.writeFile outFile (encodePretty stakePools)

writeStakePools Nothing stakePools =
  forM_ (Set.toList stakePools) $ \poolId ->
    liftIO . putStrLn $ Text.unpack (serialiseToBech32 poolId)

runQueryStakeDistributionCmd :: ()
  => Cmd.QueryStakeDistributionCmdArgs
  -> ExceptT QueryCmdError IO ()
runQueryStakeDistributionCmd
    Cmd.QueryStakeDistributionCmdArgs
    { Cmd.nodeSocketPath
    , Cmd.consensusModeParams
    , Cmd.networkId
    , Cmd.mOutFile
    } = do
  let localNodeConnInfo = LocalNodeConnectInfo consensusModeParams networkId nodeSocketPath

  join $ lift
    ( executeLocalStateQueryExpr localNodeConnInfo Nothing $ runExceptT $ do
        AnyCardanoEra era <- lift queryCurrentEra
          & onLeft (left . QueryCmdUnsupportedNtcVersion)

        sbe <- requireShelleyBasedEra era
          & onNothing (left QueryCmdByronEra)

        result <- lift (queryStakeDistribution sbe)
          & onLeft (left . QueryCmdUnsupportedNtcVersion)
          & onLeft (left . QueryCmdLocalStateQueryError . EraMismatchError)

        pure $ do
          writeStakeDistribution mOutFile result
    )
    & onLeft (left . QueryCmdAcquireFailure)
    & onLeft left

writeStakeDistribution
  :: Maybe (File () Out)
  -> Map PoolId Rational
  -> ExceptT QueryCmdError IO ()
writeStakeDistribution (Just (File outFile)) stakeDistrib =
  handleIOExceptT (QueryCmdWriteFileError . FileIOError outFile) $
    LBS.writeFile outFile (encodePretty stakeDistrib)

writeStakeDistribution Nothing stakeDistrib =
  liftIO $ printStakeDistribution stakeDistrib


printStakeDistribution :: Map PoolId Rational -> IO ()
printStakeDistribution stakeDistrib = do
  Text.putStrLn title
  putStrLn $ replicate (Text.length title + 2) '-'
  sequence_
    [ putStrLn $ showStakeDistr poolId stakeFraction
    | (poolId, stakeFraction) <- Map.toList stakeDistrib ]
 where
   title :: Text
   title =
     "                           PoolId                                 Stake frac"

   showStakeDistr :: PoolId
                  -> Rational
                  -- ^ Stake fraction
                  -> String
   showStakeDistr poolId stakeFraction =
     concat
       [ Text.unpack (serialiseToBech32 poolId)
       , "   "
       , showEFloat (Just 3) (fromRational stakeFraction :: Double) ""
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
    , Cmd.mOutFile
    } = do
  let localNodeConnInfo = LocalNodeConnectInfo consensusModeParams networkId nodeSocketPath

  poolid <- lift (readVerificationKeyOrHashOrFile AsStakePoolKey poolColdVerKeyFile)
    & onLeft (left . QueryCmdTextReadError)

  vrkSkey <- lift (readFileTextEnvelope (AsSigningKey AsVrfKey) vrkSkeyFp)
    & onLeft (left . QueryCmdTextEnvelopeReadError)

  shelleyGenesis <- lift (readAndDecodeShelleyGenesis genFile)
    & onLeft (left . QueryCmdGenesisReadError)

  join $ lift
    ( executeLocalStateQueryExpr localNodeConnInfo Nothing $ runExceptT $ do
        AnyCardanoEra era <- lift queryCurrentEra
          & onLeft (left . QueryCmdUnsupportedNtcVersion)

        sbe <- requireShelleyBasedEra era
          & onNothing (left QueryCmdByronEra)

        pparams <- lift (queryProtocolParameters sbe)
          & onLeft (left . QueryCmdUnsupportedNtcVersion)
          & onLeft (left . QueryCmdLocalStateQueryError . EraMismatchError)

        ptclState <- lift (queryProtocolState sbe)
          & onLeft (left . QueryCmdUnsupportedNtcVersion)
          & onLeft (left . QueryCmdLocalStateQueryError . EraMismatchError)

        eraHistory <- lift queryEraHistory
          & onLeft (left . QueryCmdUnsupportedNtcVersion)

        let eInfo = toEpochInfo eraHistory

        curentEpoch <- lift (queryEpoch sbe)
          & onLeft (left . QueryCmdUnsupportedNtcVersion)
          & onLeft (left . QueryCmdLocalStateQueryError . EraMismatchError)

        case whichSchedule of
          CurrentEpoch -> do
            beo <- requireEon BabbageEra era

            serCurrentEpochState <- lift (queryPoolDistribution beo (Just (Set.singleton poolid)))
              & onLeft (left . QueryCmdUnsupportedNtcVersion)
              & onLeft (left . QueryCmdLocalStateQueryError . EraMismatchError)

            pure $ do
              schedule <- firstExceptT QueryCmdLeaderShipError $ hoistEither
                $ shelleyBasedEraConstraints sbe
                $ currentEpochEligibleLeadershipSlots
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
            serCurrentEpochState <- lift (queryCurrentEpochState sbe)
              & onLeft (left . QueryCmdUnsupportedNtcVersion)
              & onLeft (left . QueryCmdLocalStateQueryError . EraMismatchError)

            pure $ do
              tip <- liftIO $ getLocalChainTip localNodeConnInfo

              schedule <- firstExceptT QueryCmdLeaderShipError $ hoistEither
                $ shelleyBasedEraConstraints sbe
                $ nextEpochEligibleLeadershipSlots sbe shelleyGenesis
                  serCurrentEpochState ptclState poolid vrkSkey pparams
                  eInfo (tip, curentEpoch)

              writeSchedule mOutFile eInfo shelleyGenesis schedule
    )
    & onLeft (left . QueryCmdAcquireFailure)
    & onLeft left
  where
    writeSchedule mOutFile' eInfo shelleyGenesis schedule =
      case mOutFile' of
        Nothing -> liftIO $ printLeadershipScheduleAsText schedule eInfo (SystemStart $ sgSystemStart shelleyGenesis)
        Just (File jsonOutputFile) ->
          liftIO $ LBS.writeFile jsonOutputFile $
            printLeadershipScheduleAsJson schedule eInfo (SystemStart $ sgSystemStart shelleyGenesis)

    printLeadershipScheduleAsText
      :: Set SlotNo
      -> EpochInfo (Either Text)
      -> SystemStart
      -> IO ()
    printLeadershipScheduleAsText leadershipSlots eInfo sStart = do
      Text.putStrLn title
      putStrLn $ replicate (Text.length title + 2) '-'
      sequence_
        [ putStrLn $ showLeadershipSlot slot eInfo sStart
        | slot <- Set.toList leadershipSlots ]
      where
        title :: Text
        title =
          "     SlotNo                          UTC Time              "

        showLeadershipSlot
          :: SlotNo
          -> EpochInfo (Either Text)
          -> SystemStart
          -> String
        showLeadershipSlot lSlot@(SlotNo sn) eInfo' sStart' =
          case epochInfoSlotToUTCTime eInfo' sStart' lSlot of
            Right slotTime ->
              concat
              [ "     "
              , show sn
              , "                   "
              , show slotTime
              ]
            Left err ->
              concat
              [ "     "
              , show sn
              , "                   "
              , Text.unpack err
              ]
    printLeadershipScheduleAsJson
      :: Set SlotNo
      -> EpochInfo (Either Text)
      -> SystemStart
      -> LBS.ByteString
    printLeadershipScheduleAsJson leadershipSlots eInfo sStart =
      encodePretty $ showLeadershipSlot <$> List.sort (Set.toList leadershipSlots)
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
      , Cmd.mOutFile
      } = conwayEraOnwardsConstraints eon $ do
  let localNodeConnInfo = LocalNodeConnectInfo consensusModeParams networkId nodeSocketPath
  constitution <- runQuery localNodeConnInfo $ queryConstitution eon
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
      , Cmd.mOutFile
      } = conwayEraOnwardsConstraints eon $ do
  let localNodeConnInfo = LocalNodeConnectInfo consensusModeParams networkId nodeSocketPath
  govState <- runQuery localNodeConnInfo $ queryGovState eon
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
      , Cmd.drepKeys = drepKeys
      , Cmd.mOutFile
      } = conwayEraOnwardsConstraints eon $ do
  let localNodeConnInfo = LocalNodeConnectInfo consensusModeParams networkId nodeSocketPath

  drepCreds <- Set.fromList <$> mapM (firstExceptT QueryCmdDRepKeyError . getDRepCredentialFromVerKeyHashOrFile) drepKeys

  drepState <- runQuery localNodeConnInfo $ queryDRepState eon drepCreds
  writeOutput mOutFile $
    second drepStateToJson <$> Map.assocs drepState
  where
    drepStateToJson ds = A.object
      [ "expiry" .= (ds ^. Ledger.drepExpiryL)
      , "anchor" .= (ds ^. Ledger.drepAnchorL)
      , "deposit" .= (ds ^. Ledger.drepDepositL)
      ]

runQueryDRepStakeDistribution
  :: Cmd.QueryDRepStakeDistributionCmdArgs era
  -> ExceptT QueryCmdError IO ()
runQueryDRepStakeDistribution
    Cmd.QueryDRepStakeDistributionCmdArgs
      { Cmd.eon
      , Cmd.nodeSocketPath
      , Cmd.consensusModeParams
      , Cmd.networkId
      , Cmd.drepKeys = drepKeys
      , Cmd.mOutFile
      } = conwayEraOnwardsConstraints eon $ do
  let localNodeConnInfo = LocalNodeConnectInfo consensusModeParams networkId nodeSocketPath

  let drepFromVrfKey = fmap Ledger.DRepCredential
                     . firstExceptT QueryCmdDRepKeyError
                     . getDRepCredentialFromVerKeyHashOrFile
  dreps <- Set.fromList <$> mapM drepFromVrfKey drepKeys

  drepStakeDistribution <- runQuery localNodeConnInfo $ queryDRepStakeDistribution eon dreps
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
      , Cmd.mOutFile
      , Cmd.committeeColdKeys  = coldCredKeys
      , Cmd.committeeHotKeys = hotCredKeys
      , Cmd.memberStatuses = memberStatuses
      } = conwayEraOnwardsConstraints eon $ do
  let localNodeConnInfo = LocalNodeConnectInfo consensusModeParams networkId nodeSocketPath

  let coldKeysFromVerKeyHashOrFile =
        firstExceptT QueryCmdCommitteeColdKeyError . getCommitteeColdCredentialFromVerKeyHashOrFile
  coldKeys <- Set.fromList <$> mapM coldKeysFromVerKeyHashOrFile coldCredKeys

  let hotKeysFromVerKeyHashOrFile =
        firstExceptT QueryCmdCommitteeHotKeyError . getCommitteeHotCredentialFromVerKeyHashOrFile
  hotKeys <- Set.fromList <$> mapM hotKeysFromVerKeyHashOrFile hotCredKeys

  committeeState <- runQuery localNodeConnInfo $
    queryCommitteeMembersState eon coldKeys hotKeys (Set.fromList memberStatuses)
  writeOutput mOutFile $ A.toJSON committeeState

runQuery :: LocalNodeConnectInfo
         -> LocalStateQueryExpr
             BlockInMode
             ChainPoint
             QueryInMode
             ()
             IO
             (Either
                UnsupportedNtcVersionError
                (Either Consensus.EraMismatch a))
         -> ExceptT QueryCmdError IO a
runQuery localNodeConnInfo query =
  firstExceptT QueryCmdAcquireFailure
    ( newExceptT $ executeLocalStateQueryExpr localNodeConnInfo Nothing query)
      & onLeft (left . QueryCmdUnsupportedNtcVersion)
      & onLeft (left . QueryCmdEraMismatch)

writeOutput :: ToJSON b
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
  hoistEpochInfo (first (Text.pack . show) . runExcept)
    $ Consensus.interpreterToEpochInfo interpreter

-- | A value that is tentative or produces a tentative value if used.  These values
-- are considered accurate only if some future event such as a hard fork does not
-- render them invalid.
newtype Tentative a = Tentative { tentative :: a } deriving (Eq, Show)

-- | Get an Epoch Info that computes tentative values.  The values computed are
-- tentative because it uses an interpreter that is extended past the horizon.
-- This interpreter will compute accurate values into the future as long as a
-- a hard fork does not happen in the intervening time.  Those values are thus
-- "tentative" because they can change in the event of a hard fork.
toTentativeEpochInfo :: EraHistory -> Tentative (EpochInfo (Either Text))
toTentativeEpochInfo (EraHistory interpreter) =
  Tentative
    $ hoistEpochInfo (first (Text.pack . show) . runExcept)
    $ Consensus.interpreterToEpochInfo (Consensus.unsafeExtendSafeZone interpreter)


-- | Get slot number for timestamp, or an error if the UTC timestamp is before 'SystemStart' or after N+1 era
utcTimeToSlotNo
  :: SocketPath
  -> ConsensusModeParams
  -> NetworkId
  -> UTCTime
  -> ExceptT QueryCmdError IO SlotNo
utcTimeToSlotNo nodeSocketPath consensusModeParams networkId utcTime = do
  let localNodeConnInfo = LocalNodeConnectInfo consensusModeParams networkId nodeSocketPath

  lift
    ( executeLocalStateQueryExpr localNodeConnInfo Nothing $ runExceptT $ do
        systemStart <- lift querySystemStart
          & onLeft (left . QueryCmdUnsupportedNtcVersion)

        eraHistory <- lift queryEraHistory
          & onLeft (left . QueryCmdUnsupportedNtcVersion)

        let relTime = toRelativeTime systemStart utcTime

        pure (Api.getSlotForRelativeTime relTime eraHistory)
          & onLeft (left . QueryCmdPastHorizon)
    )
    & onLeft (left . QueryCmdAcquireFailure)
    & onLeft left


requireEon :: forall eon era minEra m. (Eon eon, Monad m)
           => CardanoEra minEra -- ^ minimal required era i.e. for 'ConwayEraOnwards' eon it's 'Conway'
           -> CardanoEra era -- ^ node era
           -> ExceptT QueryCmdError m (eon era)
-- TODO: implement 'Bounded' for `Some eon` and remove 'minEra'
requireEon minEra era =
  hoistMaybe
    (QueryCmdLocalStateQueryError $ mkEraMismatchError NodeEraMismatchError { nodeEra = era, era = minEra })
    (forEraMaybeEon era)
