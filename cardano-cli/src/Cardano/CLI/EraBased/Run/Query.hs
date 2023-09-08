{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.CLI.EraBased.Run.Query
  ( runQueryConstitutionHashCmd
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
  , renderShelleyQueryCmdError
  , renderLocalStateQueryError
  , percentage
  ) where

import           Cardano.Api hiding (QueryInShelleyBasedEra (..))
import qualified Cardano.Api as Api
import           Cardano.Api.Byron hiding (QueryInShelleyBasedEra (..))
import qualified Cardano.Api.Ledger as Ledger
import           Cardano.Api.Shelley hiding (QueryInShelleyBasedEra (..))

import           Cardano.CLI.EraBased.Run.Genesis (readAndDecodeShelleyGenesis)
import           Cardano.CLI.Helpers (pPrintCBOR)
import           Cardano.CLI.Pretty
import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Errors.ShelleyQueryCmdError
import           Cardano.CLI.Types.Errors.ShelleyQueryCmdLocalStateQueryError
import           Cardano.CLI.Types.Key (VerificationKeyOrHashOrFile,
                   readVerificationKeyOrHashOrFile)
import qualified Cardano.CLI.Types.Output as O
import           Cardano.Crypto.Hash (hashToBytesAsHex)
import qualified Cardano.Crypto.Hash.Blake2b as Blake2b
import qualified Cardano.Ledger.BaseTypes as L
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Crypto as Crypto
import           Cardano.Ledger.Keys (KeyHash (..), KeyRole (..))
import           Cardano.Ledger.SafeHash (SafeHash)
import           Cardano.Ledger.Shelley.LedgerState
                   (PState (psFutureStakePoolParams, psRetiring, psStakePoolParams))
import qualified Cardano.Ledger.Shelley.LedgerState as SL
import           Cardano.Slotting.EpochInfo (EpochInfo (..), epochInfoSlotToUTCTime, hoistEpochInfo)
import           Ouroboros.Consensus.BlockchainTime.WallClock.Types (RelativeTime (..),
                   toRelativeTime)
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
import           Data.Aeson.Encode.Pretty (encodePretty)
import           Data.Aeson.Types as Aeson
import           Data.Bifunctor (Bifunctor (..))
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Coerce (coerce)
import           Data.Function ((&))
import           Data.Functor ((<&>))
import           Data.List (nub)
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
import           Data.Time.Clock
import qualified Data.Vector as Vector
import           Numeric (showEFloat)
import           Prettyprinter
import qualified System.IO as IO
import           Text.Printf (printf)

{- HLINT ignore "Move brackets to avoid $" -}
{- HLINT ignore "Redundant flip" -}

runQueryConstitutionHashCmd
  :: SocketPath
  -> AnyConsensusModeParams
  -> NetworkId
  -> Maybe (File () Out)
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryConstitutionHashCmd socketPath (AnyConsensusModeParams cModeParams) network mOutFile = do
  let localNodeConnInfo = LocalNodeConnectInfo cModeParams network socketPath

  result <- liftIO $ executeLocalStateQueryExpr localNodeConnInfo Nothing $ runExceptT $ do
    anyE@(AnyCardanoEra era) <- lift (determineEraExpr cModeParams)
      & onLeft (left . ShelleyQueryCmdUnsupportedNtcVersion)

    sbe <- requireShelleyBasedEra era
      & onNothing (left ShelleyQueryCmdByronEra)

    let cMode = consensusModeOnly cModeParams

    eInMode <- toEraInMode era cMode
      & hoistMaybe (ShelleyQueryCmdEraConsensusModeMismatch (AnyConsensusMode cMode) anyE)

    lift (shelleyBasedEraConstraints sbe (queryConstitutionHash eInMode sbe))
      & onLeft (left . ShelleyQueryCmdUnsupportedNtcVersion)
      & onLeft (left . ShelleyQueryCmdEraMismatch)

  writeConstitutionHash mOutFile =<< except (join (first ShelleyQueryCmdAcquireFailure result))
  where
    writeConstitutionHash
      :: Maybe (File () Out)
      -> Maybe (SafeHash StandardCrypto L.AnchorData)
      -> ExceptT ShelleyQueryCmdError IO ()
    writeConstitutionHash mOutFile' cHash =
      case mOutFile' of
        Nothing -> liftIO $ LBS.putStrLn (encodePretty cHash)
        Just (File fpath) ->
          handleIOExceptT (ShelleyQueryCmdWriteFileError . FileIOError fpath) $
            LBS.writeFile fpath (encodePretty cHash)

runQueryProtocolParametersCmd
  :: SocketPath
  -> AnyConsensusModeParams
  -> NetworkId
  -> Maybe (File () Out)
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryProtocolParametersCmd socketPath (AnyConsensusModeParams cModeParams) network mOutFile = do
  let localNodeConnInfo = LocalNodeConnectInfo cModeParams network socketPath
  anyE@(AnyCardanoEra era) <- firstExceptT ShelleyQueryCmdAcquireFailure $ newExceptT $ determineEra cModeParams localNodeConnInfo
  sbe <- case cardanoEraStyle era of
            LegacyByronEra -> left ShelleyQueryCmdByronEra
            ShelleyBasedEra sbe -> return sbe
  let cMode = consensusModeOnly cModeParams
  eInMode <- toEraInMode era cMode
                 & hoistMaybe (ShelleyQueryCmdEraConsensusModeMismatch (AnyConsensusMode cMode) anyE)

  let qInMode = QueryInEra eInMode $ QueryInShelleyBasedEra sbe Api.QueryProtocolParameters
  pp <- firstExceptT ShelleyQueryCmdConvenienceError
          . newExceptT $ executeQueryAnyMode era localNodeConnInfo qInMode
  writeProtocolParameters sbe mOutFile pp
  where
    -- TODO: Conway era - use ledger PParams JSON
    writeProtocolParameters
      :: ShelleyBasedEra era
      -> Maybe (File () Out)
      -> Ledger.PParams (ShelleyLedgerEra era)
      -> ExceptT ShelleyQueryCmdError IO ()
    writeProtocolParameters sbe mOutFile' pparams =
      let apiPParamsJSON = (encodePretty $ fromLedgerPParams sbe pparams)
      in case mOutFile' of
        Nothing -> liftIO $ LBS.putStrLn apiPParamsJSON
        Just (File fpath) ->
          handleIOExceptT (ShelleyQueryCmdWriteFileError . FileIOError fpath) $
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
queryChainTipViaChainSync :: MonadIO m => LocalNodeConnectInfo mode -> m ChainTip
queryChainTipViaChainSync localNodeConnInfo = do
  liftIO . T.hPutStrLn IO.stderr $
    "Warning: Local header state query unavailable. Falling back to chain sync query"
  liftIO $ getLocalChainTip localNodeConnInfo

runQueryTipCmd
  :: SocketPath
  -> AnyConsensusModeParams
  -> NetworkId
  -> Maybe (File () Out)
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryTipCmd socketPath (AnyConsensusModeParams cModeParams) network mOutFile = do
  case consensusModeOnly cModeParams of
    CardanoMode -> do
      let localNodeConnInfo = LocalNodeConnectInfo cModeParams network socketPath

      eLocalState <- ExceptT $ fmap sequence $
        executeLocalStateQueryExpr localNodeConnInfo Nothing $ runExceptT $ do
          era <- lift queryCurrentEra & onLeft (left . ShelleyQueryCmdUnsupportedNtcVersion)
          eraHistory <- lift queryEraHistory & onLeft (left . ShelleyQueryCmdUnsupportedNtcVersion)
          mChainBlockNo <- lift queryChainBlockNo & onLeft (left . ShelleyQueryCmdUnsupportedNtcVersion) & fmap Just
          mChainPoint <- lift queryChainPoint & onLeft (left . ShelleyQueryCmdUnsupportedNtcVersion) & fmap Just
          mSystemStart <- lift querySystemStart & onLeft (left . ShelleyQueryCmdUnsupportedNtcVersion) & fmap Just

          return O.QueryTipLocalState
            { O.era = era
            , O.eraHistory = eraHistory
            , O.mSystemStart = mSystemStart
            , O.mChainTip = makeChainTip <$> mChainBlockNo <*> mChainPoint
            }

      mLocalState <- hushM (first ShelleyQueryCmdAcquireFailure eLocalState) $ \e ->
        liftIO . T.hPutStrLn IO.stderr $ "Warning: Local state unavailable: " <> renderShelleyQueryCmdError e

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
            liftIO . T.hPutStrLn IO.stderr $
              "Warning: Epoch unavailable: " <> renderShelleyQueryCmdError (ShelleyQueryCmdPastHorizon e)
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
              systemStart <- fmap getSystemStart (O.mSystemStart localState) & hoistMaybe ShelleyQueryCmdSystemStartUnavailable
              nowSeconds <- toRelativeTime (SystemStart systemStart) <$> liftIO getCurrentTime
              tipTimeResult <- getProgress tipSlotNo (O.eraHistory localState) & bimap ShelleyQueryCmdPastHorizon fst & except

              let tolerance = RelativeTime (secondsToNominalDiffTime 600)

              return $ flip (percentage tolerance) nowSeconds tipTimeResult

            mSyncProgress <- hushM syncProgressResult $ \e -> do
              liftIO . T.hPutStrLn IO.stderr $ "Warning: Sync progress unavailable: " <> renderShelleyQueryCmdError e

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

    mode -> left (ShelleyQueryCmdUnsupportedMode (AnyConsensusMode mode))

-- | Query the UTxO, filtered by a given set of addresses, from a Shelley node
-- via the local state query protocol.
runQueryUTxOCmd
  :: SocketPath
  -> AnyConsensusModeParams
  -> QueryUTxOFilter
  -> NetworkId
  -> Maybe (File () Out)
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryUTxOCmd socketPath (AnyConsensusModeParams cModeParams)
             qfilter network mOutFile = do
  let localNodeConnInfo = LocalNodeConnectInfo cModeParams network socketPath

  join $ lift
    ( executeLocalStateQueryExpr localNodeConnInfo Nothing $ runExceptT $ do
        anyE@(AnyCardanoEra era) <- lift (determineEraExpr cModeParams)
          & onLeft (left . ShelleyQueryCmdUnsupportedNtcVersion)

        let cMode = consensusModeOnly cModeParams

        sbe <- requireShelleyBasedEra era
          & onNothing (left ShelleyQueryCmdByronEra)

        eInMode <- pure (toEraInMode era cMode)
          & onNothing (left (ShelleyQueryCmdEraConsensusModeMismatch (AnyConsensusMode cMode) anyE))

        eraInMode <- calcEraInMode era $ consensusModeOnly cModeParams

        requireNotByronEraInByronMode eraInMode

        utxo <- lift (queryUtxo eInMode sbe qfilter)
          & onLeft (left . ShelleyQueryCmdUnsupportedNtcVersion)
          & onLeft (left . ShelleyQueryCmdLocalStateQueryError . EraMismatchError)

        pure $ do
          writeFilteredUTxOs sbe mOutFile utxo
    )
    & onLeft (left . ShelleyQueryCmdAcquireFailure)
    & onLeft left

runQueryKesPeriodInfoCmd
  :: SocketPath
  -> AnyConsensusModeParams
  -> NetworkId
  -> File () In
  -> Maybe (File () Out)
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryKesPeriodInfoCmd socketPath (AnyConsensusModeParams cModeParams) network nodeOpCertFile mOutFile = do
  opCert <- lift (readFileTextEnvelope AsOperationalCertificate nodeOpCertFile)
    & onLeft (left . ShelleyQueryCmdOpCertCounterReadError)

  let localNodeConnInfo = LocalNodeConnectInfo cModeParams network socketPath

  let cMode = consensusModeOnly cModeParams

  case cMode of
    CardanoMode -> do
      join $ lift
        ( executeLocalStateQueryExpr localNodeConnInfo Nothing $ runExceptT $ do
            anyE@(AnyCardanoEra era) <- lift (determineEraExpr cModeParams)
              & onLeft (left . ShelleyQueryCmdUnsupportedNtcVersion)

            sbe <- requireShelleyBasedEra era
              & onNothing (left ShelleyQueryCmdByronEra)

            eInMode <- toEraInMode era cMode
              & hoistMaybe (ShelleyQueryCmdEraConsensusModeMismatch (AnyConsensusMode cMode) anyE)

            -- We check that the KES period specified in the operational certificate is correct
            -- based on the KES period defined in the genesis parameters and the current slot number
            eraInMode <- calcEraInMode era $ consensusModeOnly cModeParams

            requireNotByronEraInByronMode eraInMode

            gParams <- lift (queryGenesisParameters eInMode sbe)
              & onLeft (left . ShelleyQueryCmdUnsupportedNtcVersion)
              & onLeft (left . ShelleyQueryCmdLocalStateQueryError . EraMismatchError)

            eraHistory <- lift queryEraHistory
              & onLeft (left . ShelleyQueryCmdUnsupportedNtcVersion)

            let eInfo = toTentativeEpochInfo eraHistory

            -- We get the operational certificate counter from the protocol state and check that
            -- it is equivalent to what we have on disk.
            ptclState <- lift (queryProtocolState eInMode sbe)
              & onLeft (left . ShelleyQueryCmdUnsupportedNtcVersion)
              & onLeft (left . ShelleyQueryCmdLocalStateQueryError . EraMismatchError)

            pure $ do
              chainTip <- liftIO $ getLocalChainTip localNodeConnInfo

              let curKesPeriod = currentKesPeriod chainTip gParams
                  oCertStartKesPeriod = opCertStartingKesPeriod opCert
                  oCertEndKesPeriod = opCertEndKesPeriod gParams opCert
                  opCertIntervalInformation = opCertIntervalInfo gParams chainTip curKesPeriod oCertStartKesPeriod oCertEndKesPeriod

              (onDiskC, stateC) <- shelleyBasedEraConstraints sbe $ opCertOnDiskAndStateCounters ptclState opCert

              let counterInformation = opCertNodeAndOnDiskCounters onDiskC stateC

              -- Always render diagnostic information
              liftIO . putStrLn $ renderOpCertIntervalInformation (unFile nodeOpCertFile) opCertIntervalInformation
              liftIO . putStrLn $ renderOpCertNodeAndOnDiskCounterInformation (unFile nodeOpCertFile) counterInformation

              let qKesInfoOutput = createQueryKesPeriodInfoOutput opCertIntervalInformation counterInformation eInfo gParams
                  kesPeriodInfoJSON = encodePretty qKesInfoOutput

              liftIO $ LBS.putStrLn kesPeriodInfoJSON
              forM_ mOutFile (\(File oFp) ->
                handleIOExceptT (ShelleyQueryCmdWriteFileError . FileIOError oFp)
                  $ LBS.writeFile oFp kesPeriodInfoJSON)
        )
        & onLeft (left . ShelleyQueryCmdAcquireFailure)
        & onLeft left

    mode -> left . ShelleyQueryCmdUnsupportedMode $ AnyConsensusMode mode

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

   renderOpCertNodeAndOnDiskCounterInformation :: FilePath -> OpCertNodeAndOnDiskCounterInformation -> String
   renderOpCertNodeAndOnDiskCounterInformation opCertFile opCertCounterInfo =
     case opCertCounterInfo of
      OpCertOnDiskCounterEqualToNodeState _ _ ->
        renderStringDefault $
          green "✓" <+> hang 0
              ( vsep
                [ "The operational certificate counter agrees with the node protocol state counter"
                ]
              )
      OpCertOnDiskCounterAheadOfNodeState _ _ ->
        renderStringDefault $
          green "✓" <+> hang 0
              ( vsep
                [ "The operational certificate counter ahead of the node protocol state counter by 1"
                ]
              )
      OpCertOnDiskCounterTooFarAheadOfNodeState onDiskC nodeStateC ->
        renderStringDefault $
          red "✗" <+> hang 0
            ( vsep
              [ "The operational certificate counter too far ahead of the node protocol state counter in the operational certificate at: " <> pretty opCertFile
              , "On disk operational certificate counter: " <> pretty (unOpCertOnDiskCounter onDiskC)
              , "Protocol state counter: " <> pretty (unOpCertNodeStateCounter nodeStateC)
              ]
            )
      OpCertOnDiskCounterBehindNodeState onDiskC nodeStateC ->
        renderStringDefault $
          red "✗" <+> hang 0
            ( vsep
              [ "The protocol state counter is greater than the counter in the operational certificate at: " <> pretty opCertFile
              , "On disk operational certificate counter: " <> pretty (unOpCertOnDiskCounter onDiskC)
              , "Protocol state counter: " <> pretty (unOpCertNodeStateCounter nodeStateC)
              ]
            )
      OpCertNoBlocksMintedYet (OpCertOnDiskCounter onDiskC) ->
        renderStringDefault $
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
      -> ExceptT ShelleyQueryCmdError IO (OpCertOnDiskCounter, Maybe OpCertNodeStateCounter)
   opCertOnDiskAndStateCounters ptclState opCert@(OperationalCertificate _ stakePoolVKey) = do
    let onDiskOpCertCount = fromIntegral $ getOpCertCount opCert

    chainDepState <- pure (decodeProtocolState ptclState)
      & onLeft (left . ShelleyQueryCmdProtocolStateDecodeFailure)

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


renderOpCertIntervalInformation :: FilePath -> OpCertIntervalInformation -> String
renderOpCertIntervalInformation opCertFile opCertInfo = case opCertInfo of
  OpCertWithinInterval _start _end _current _stillExp ->
    renderStringDefault $
      green "✓" <+> hang 0
        ( vsep
          [ "Operational certificate's KES period is within the correct KES period interval"
          ]
        )
  OpCertStartingKesPeriodIsInTheFuture (OpCertStartingKesPeriod start) (OpCertEndingKesPeriod end) (CurrentKesPeriod current) ->
    renderStringDefault $
      red "✗" <+> hang 0
        ( vsep
          [ "Node operational certificate at: " <> pretty opCertFile <> " has an incorrectly specified starting KES period. "
          , "Current KES period: " <> pretty current
          , "Operational certificate's starting KES period: " <> pretty start
          , "Operational certificate's expiry KES period: " <> pretty end
          ]
        )
  OpCertExpired _ (OpCertEndingKesPeriod end) (CurrentKesPeriod current) ->
    renderStringDefault $
      red "✗" <+> hang 0
        ( vsep
          [ "Node operational certificate at: " <> pretty opCertFile <> " has expired. "
          , "Current KES period: " <> pretty current
          , "Operational certificate's expiry KES period: " <> pretty end
          ]
        )

  OpCertSomeOtherError (OpCertStartingKesPeriod start) (OpCertEndingKesPeriod end) (CurrentKesPeriod current) ->
    renderStringDefault $
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
runQueryPoolStateCmd
  :: SocketPath
  -> AnyConsensusModeParams
  -> NetworkId
  -> [Hash StakePoolKey]
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryPoolStateCmd socketPath (AnyConsensusModeParams cModeParams) network poolIds = do
  let localNodeConnInfo = LocalNodeConnectInfo cModeParams network socketPath

  join $ lift
    ( executeLocalStateQueryExpr localNodeConnInfo Nothing $ runExceptT $ do
        anyE@(AnyCardanoEra era) <- lift (determineEraExpr cModeParams)
          & onLeft (left . ShelleyQueryCmdUnsupportedNtcVersion)

        let cMode = consensusModeOnly cModeParams

        sbe <- requireShelleyBasedEra era
          & onNothing (left ShelleyQueryCmdByronEra)

        eInMode <- toEraInMode era cMode
          & hoistMaybe (ShelleyQueryCmdEraConsensusModeMismatch (AnyConsensusMode cMode) anyE)

        eraInMode <- calcEraInMode era $ consensusModeOnly cModeParams

        requireNotByronEraInByronMode eraInMode

        result <- lift (queryPoolState eInMode sbe $ Just $ Set.fromList poolIds)
          & onLeft (left . ShelleyQueryCmdUnsupportedNtcVersion)
          & onLeft (left . ShelleyQueryCmdLocalStateQueryError . EraMismatchError)

        pure $ do
          shelleyBasedEraConstraints sbe writePoolState result
    )
    & onLeft (left . ShelleyQueryCmdAcquireFailure)
    & onLeft left

-- | Query the local mempool state
runQueryTxMempoolCmd
  :: SocketPath
  -> AnyConsensusModeParams
  -> NetworkId
  -> TxMempoolQuery
  -> Maybe (File () Out)
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryTxMempoolCmd socketPath (AnyConsensusModeParams cModeParams) network query mOutFile = do
  let localNodeConnInfo = LocalNodeConnectInfo cModeParams network socketPath

  localQuery <- case query of
      TxMempoolQueryTxExists tx -> do
        anyE@(AnyCardanoEra era) <- lift (executeLocalStateQueryExpr localNodeConnInfo Nothing (determineEraExpr cModeParams))
          & onLeft (left . ShelleyQueryCmdAcquireFailure)
          & onLeft (left . ShelleyQueryCmdUnsupportedNtcVersion)
        let cMode = consensusModeOnly cModeParams
        eInMode <- toEraInMode era cMode
          & hoistMaybe (ShelleyQueryCmdEraConsensusModeMismatch (AnyConsensusMode cMode) anyE)
        pure $ LocalTxMonitoringQueryTx $ TxIdInMode tx eInMode
      TxMempoolQueryNextTx -> pure LocalTxMonitoringSendNextTx
      TxMempoolQueryInfo -> pure LocalTxMonitoringMempoolInformation

  result <- liftIO $ queryTxMonitoringLocal localNodeConnInfo localQuery
  let renderedResult = encodePretty result
  case mOutFile of
    Nothing -> liftIO $ LBS.putStrLn renderedResult
    Just (File oFp) -> handleIOExceptT (ShelleyQueryCmdWriteFileError . FileIOError oFp)
        $ LBS.writeFile oFp renderedResult

runQuerySlotNumberCmd
  :: SocketPath
  -> AnyConsensusModeParams
  -> NetworkId
  -> UTCTime
  -> ExceptT ShelleyQueryCmdError IO ()
runQuerySlotNumberCmd sockPath aCmp network utcTime = do
  SlotNo slotNo <- utcTimeToSlotNo sockPath aCmp network utcTime
  liftIO . putStr $ show slotNo

-- | Obtain stake snapshot information for a pool, plus information about the total active stake.
-- This information can be used for leader slot calculation, for example, and has been requested by SPOs.
-- Obtaining the information directly is significantly more time and memory efficient than using a full ledger state dump.
runQueryStakeSnapshotCmd
  :: SocketPath
  -> AnyConsensusModeParams
  -> NetworkId
  -> AllOrOnly [Hash StakePoolKey]
  -> Maybe (File () Out)
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryStakeSnapshotCmd socketPath (AnyConsensusModeParams cModeParams) network allOrOnlyPoolIds mOutFile = do
  let localNodeConnInfo = LocalNodeConnectInfo cModeParams network socketPath

  join $ lift
    ( executeLocalStateQueryExpr localNodeConnInfo Nothing $ runExceptT $ do
        anyE@(AnyCardanoEra era) <- lift (determineEraExpr cModeParams)
          & onLeft (left . ShelleyQueryCmdUnsupportedNtcVersion)

        let cMode = consensusModeOnly cModeParams

        sbe <- requireShelleyBasedEra era
          & onNothing (left ShelleyQueryCmdByronEra)

        eInMode <- toEraInMode era cMode
          & hoistMaybe (ShelleyQueryCmdEraConsensusModeMismatch (AnyConsensusMode cMode) anyE)

        let poolFilter = case allOrOnlyPoolIds of
              All -> Nothing
              Only poolIds -> Just $ Set.fromList poolIds

        eraInMode2 <- calcEraInMode era $ consensusModeOnly cModeParams

        requireNotByronEraInByronMode eraInMode2

        result <- lift (queryStakeSnapshot eInMode sbe poolFilter)
          & onLeft (left . ShelleyQueryCmdUnsupportedNtcVersion)
          & onLeft (left . ShelleyQueryCmdLocalStateQueryError . EraMismatchError)

        pure $ do
          shelleyBasedEraConstraints sbe (writeStakeSnapshots mOutFile) result
    )
    & onLeft (left . ShelleyQueryCmdAcquireFailure)
    & onLeft left

runQueryLedgerStateCmd
  :: SocketPath
  -> AnyConsensusModeParams
  -> NetworkId
  -> Maybe (File () Out)
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryLedgerStateCmd socketPath (AnyConsensusModeParams cModeParams) network mOutFile = do
  let localNodeConnInfo = LocalNodeConnectInfo cModeParams network socketPath

  join $ lift
    ( executeLocalStateQueryExpr localNodeConnInfo Nothing $ runExceptT $ do
        anyE@(AnyCardanoEra era) <- lift (determineEraExpr cModeParams)
          & onLeft (left . ShelleyQueryCmdUnsupportedNtcVersion)

        let cMode = consensusModeOnly cModeParams

        sbe <- requireShelleyBasedEra era
          & onNothing (left ShelleyQueryCmdByronEra)

        eInMode <- pure (toEraInMode era cMode)
          & onNothing (left (ShelleyQueryCmdEraConsensusModeMismatch (AnyConsensusMode cMode) anyE))

        eraInMode <- calcEraInMode era $ consensusModeOnly cModeParams

        requireNotByronEraInByronMode eraInMode

        result <- lift (queryDebugLedgerState eInMode sbe)
          & onLeft (left . ShelleyQueryCmdUnsupportedNtcVersion)
          & onLeft (left . ShelleyQueryCmdLocalStateQueryError . EraMismatchError)

        pure $ do
          shelleyBasedEraConstraints sbe (writeLedgerState mOutFile) result
    )
    & onLeft (left . ShelleyQueryCmdAcquireFailure)
    & onLeft left

runQueryProtocolStateCmd
  :: SocketPath
  -> AnyConsensusModeParams
  -> NetworkId
  -> Maybe (File () Out)
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryProtocolStateCmd socketPath (AnyConsensusModeParams cModeParams) network mOutFile = do
  let localNodeConnInfo = LocalNodeConnectInfo cModeParams network socketPath

  join $ lift
    ( executeLocalStateQueryExpr localNodeConnInfo Nothing $ runExceptT $ do
        anyE@(AnyCardanoEra era) <- lift (determineEraExpr cModeParams)
          & onLeft (left . ShelleyQueryCmdUnsupportedNtcVersion)

        let cMode = consensusModeOnly cModeParams

        sbe <- requireShelleyBasedEra era
          & onNothing (left ShelleyQueryCmdByronEra)

        eInMode <- pure (toEraInMode era cMode)
          & onNothing (left (ShelleyQueryCmdEraConsensusModeMismatch (AnyConsensusMode cMode) anyE))

        eraInMode <- calcEraInMode era $ consensusModeOnly cModeParams

        requireNotByronEraInByronMode eraInMode

        result <- lift (queryProtocolState eInMode sbe)
          & onLeft (left . ShelleyQueryCmdUnsupportedNtcVersion)
          & onLeft (left . ShelleyQueryCmdLocalStateQueryError . EraMismatchError)

        pure $ do
          case cMode of
            CardanoMode -> shelleyBasedEraConstraints sbe $ writeProtocolState mOutFile result
            mode -> left . ShelleyQueryCmdUnsupportedMode $ AnyConsensusMode mode
    )
    & onLeft (left . ShelleyQueryCmdAcquireFailure)
    & onLeft left

-- | Query the current delegations and reward accounts, filtered by a given
-- set of addresses, from a Shelley node via the local state query protocol.

runQueryStakeAddressInfoCmd
  :: SocketPath
  -> AnyConsensusModeParams
  -> StakeAddress
  -> NetworkId
  -> Maybe (File () Out)
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryStakeAddressInfoCmd socketPath (AnyConsensusModeParams cModeParams) (StakeAddress _ addr) network mOutFile = do
  let localNodeConnInfo = LocalNodeConnectInfo cModeParams network socketPath

  join $ lift
    ( executeLocalStateQueryExpr localNodeConnInfo Nothing $ runExceptT $ do
        anyE@(AnyCardanoEra era) <- lift (determineEraExpr cModeParams)
          & onLeft (left . ShelleyQueryCmdUnsupportedNtcVersion)

        let cMode = consensusModeOnly cModeParams

        sbe <- requireShelleyBasedEra era
          & onNothing (left ShelleyQueryCmdByronEra)

        eInMode <- pure (toEraInMode era cMode)
          & onNothing (left (ShelleyQueryCmdEraConsensusModeMismatch (AnyConsensusMode cMode) anyE))

        let stakeAddr = Set.singleton $ fromShelleyStakeCredential addr

        eraInMode <- calcEraInMode era $ consensusModeOnly cModeParams

        requireNotByronEraInByronMode eraInMode

        result <- lift (queryStakeAddresses eInMode sbe stakeAddr network)
          & onLeft (left . ShelleyQueryCmdUnsupportedNtcVersion)
          & onLeft (left . ShelleyQueryCmdLocalStateQueryError . EraMismatchError)

        pure $ do
          writeStakeAddressInfo mOutFile $ DelegationsAndRewards result
    )
    & onLeft (left . ShelleyQueryCmdAcquireFailure)
    & onLeft left

-- -------------------------------------------------------------------------------------------------

writeStakeAddressInfo
  :: Maybe (File () Out)
  -> DelegationsAndRewards
  -> ExceptT ShelleyQueryCmdError IO ()
writeStakeAddressInfo mOutFile delegsAndRewards =
  case mOutFile of
    Nothing -> liftIO $ LBS.putStrLn (encodePretty delegsAndRewards)
    Just (File fpath) ->
      handleIOExceptT (ShelleyQueryCmdWriteFileError . FileIOError fpath)
        $ LBS.writeFile fpath (encodePretty delegsAndRewards)

writeLedgerState :: forall era ledgerera.
                    ShelleyLedgerEra era ~ ledgerera
                 => ToJSON (DebugLedgerState era)
                 => FromCBOR (DebugLedgerState era)
                 => Maybe (File () Out)
                 -> SerialisedDebugLedgerState era
                 -> ExceptT ShelleyQueryCmdError IO ()
writeLedgerState mOutFile qState@(SerialisedDebugLedgerState serLedgerState) =
  case mOutFile of
    Nothing ->
      case decodeDebugLedgerState qState of
        Left bs -> firstExceptT ShelleyQueryCmdHelpersError $ pPrintCBOR bs
        Right ledgerState -> liftIO . LBS.putStrLn $ Aeson.encode ledgerState
    Just (File fpath) ->
      handleIOExceptT (ShelleyQueryCmdWriteFileError . FileIOError fpath)
        $ LBS.writeFile fpath $ unSerialised serLedgerState

writeStakeSnapshots :: forall era ledgerera. ()
  => ShelleyLedgerEra era ~ ledgerera
  => Core.EraCrypto ledgerera ~ StandardCrypto
  => Maybe (File () Out)
  -> SerialisedStakeSnapshots era
  -> ExceptT ShelleyQueryCmdError IO ()
writeStakeSnapshots mOutFile qState = do
  StakeSnapshot snapshot <- pure (decodeStakeSnapshot qState)
    & onLeft (left . ShelleyQueryCmdStakeSnapshotDecodeError)

  -- Calculate the three pool and active stake values for the given pool
  liftIO . maybe LBS.putStrLn (LBS.writeFile . unFile) mOutFile $ encodePretty snapshot

-- | This function obtains the pool parameters, equivalent to the following jq query on the output of query ledger-state
--   .nesEs.esLState.lsDPState.dpsPState.psStakePoolParams.<pool_id>
writePoolState :: forall era ledgerera. ()
  => ShelleyLedgerEra era ~ ledgerera
  => Core.EraCrypto ledgerera ~ StandardCrypto
  => Core.Era ledgerera
  => SerialisedPoolState era
  -> ExceptT ShelleyQueryCmdError IO ()
writePoolState serialisedCurrentEpochState = do
  PoolState poolState <- pure (decodePoolState serialisedCurrentEpochState)
    & onLeft (left . ShelleyQueryCmdPoolStateDecodeError)

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
  -> ExceptT ShelleyQueryCmdError IO ()
writeProtocolState mOutFile ps@(ProtocolState pstate) =
  case mOutFile of
    Nothing -> case decodeProtocolState ps of
      Left (bs, _) -> firstExceptT ShelleyQueryCmdHelpersError $ pPrintCBOR bs
      Right chainDepstate -> liftIO . LBS.putStrLn $ encodePretty chainDepstate
    Just (File fpath) ->
      handleIOExceptT (ShelleyQueryCmdWriteFileError . FileIOError fpath)
        . LBS.writeFile fpath $ unSerialised pstate

writeFilteredUTxOs :: Api.ShelleyBasedEra era
                   -> Maybe (File () Out)
                   -> UTxO era
                   -> ExceptT ShelleyQueryCmdError IO ()
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
     handleIOExceptT (ShelleyQueryCmdWriteFileError . FileIOError fpath)
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
  printableValue (TxOutValue _ val) = renderValue val
  printableValue (TxOutAdaOnly _ (Lovelace i)) = Text.pack $ show i

runQueryStakePoolsCmd
  :: SocketPath
  -> AnyConsensusModeParams
  -> NetworkId
  -> Maybe (File () Out)
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryStakePoolsCmd socketPath (AnyConsensusModeParams cModeParams) network mOutFile = do
  let localNodeConnInfo = LocalNodeConnectInfo cModeParams network socketPath

  join $ lift
    ( executeLocalStateQueryExpr localNodeConnInfo Nothing $ runExceptT @ShelleyQueryCmdError $ do
        anyE@(AnyCardanoEra era) <- case consensusModeOnly cModeParams of
          ByronMode -> return $ AnyCardanoEra ByronEra
          ShelleyMode -> return $ AnyCardanoEra ShelleyEra
          CardanoMode -> lift queryCurrentEra & onLeft (left . ShelleyQueryCmdUnsupportedNtcVersion)

        let cMode = consensusModeOnly cModeParams

        eInMode <- toEraInMode era cMode
          & hoistMaybe (ShelleyQueryCmdEraConsensusModeMismatch (AnyConsensusMode cMode) anyE)

        sbe <- requireShelleyBasedEra era
          & onNothing (left ShelleyQueryCmdByronEra)

        poolIds <- lift (queryStakePools eInMode sbe)
          & onLeft (left . ShelleyQueryCmdUnsupportedNtcVersion)
          & onLeft (left . ShelleyQueryCmdEraMismatch)

        pure $ do
          writeStakePools mOutFile poolIds
    ) & onLeft (left . ShelleyQueryCmdAcquireFailure)
      & onLeft left

writeStakePools
  :: Maybe (File () Out)
  -> Set PoolId
  -> ExceptT ShelleyQueryCmdError IO ()
writeStakePools (Just (File outFile)) stakePools =
  handleIOExceptT (ShelleyQueryCmdWriteFileError . FileIOError outFile) $
    LBS.writeFile outFile (encodePretty stakePools)

writeStakePools Nothing stakePools =
  forM_ (Set.toList stakePools) $ \poolId ->
    liftIO . putStrLn $ Text.unpack (serialiseToBech32 poolId)

runQueryStakeDistributionCmd
  :: SocketPath
  -> AnyConsensusModeParams
  -> NetworkId
  -> Maybe (File () Out)
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryStakeDistributionCmd socketPath (AnyConsensusModeParams cModeParams) network mOutFile = do
  let localNodeConnInfo = LocalNodeConnectInfo cModeParams network socketPath

  join $ lift
    ( executeLocalStateQueryExpr localNodeConnInfo Nothing $ runExceptT $ do
        anyE@(AnyCardanoEra era) <- lift (determineEraExpr cModeParams)
          & onLeft (left . ShelleyQueryCmdUnsupportedNtcVersion)

        let cMode = consensusModeOnly cModeParams

        sbe <- requireShelleyBasedEra era
          & onNothing (left ShelleyQueryCmdByronEra)

        eInMode <- pure (toEraInMode era cMode)
          & onNothing (left (ShelleyQueryCmdEraConsensusModeMismatch (AnyConsensusMode cMode) anyE))

        eraInMode <- calcEraInMode era $ consensusModeOnly cModeParams

        requireNotByronEraInByronMode eraInMode

        result <- lift (queryStakeDistribution eInMode sbe)
          & onLeft (left . ShelleyQueryCmdUnsupportedNtcVersion)
          & onLeft (left . ShelleyQueryCmdLocalStateQueryError . EraMismatchError)

        pure $ do
          writeStakeDistribution mOutFile result
    )
    & onLeft (left . ShelleyQueryCmdAcquireFailure)
    & onLeft left

writeStakeDistribution
  :: Maybe (File () Out)
  -> Map PoolId Rational
  -> ExceptT ShelleyQueryCmdError IO ()
writeStakeDistribution (Just (File outFile)) stakeDistrib =
  handleIOExceptT (ShelleyQueryCmdWriteFileError . FileIOError outFile) $
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

-- | A mapping of Shelley reward accounts to both the stake pool that they
-- delegate to and their reward account balance.
-- TODO: Move to cardano-api
newtype DelegationsAndRewards
  = DelegationsAndRewards (Map StakeAddress Lovelace, Map StakeAddress PoolId)
    deriving (Eq, Show)


mergeDelegsAndRewards :: DelegationsAndRewards -> [(StakeAddress, Maybe Lovelace, Maybe PoolId)]
mergeDelegsAndRewards (DelegationsAndRewards (rewardsMap, delegMap)) =
 [ (stakeAddr, Map.lookup stakeAddr rewardsMap, Map.lookup stakeAddr delegMap)
 | stakeAddr <- nub $ Map.keys rewardsMap ++ Map.keys delegMap
 ]


instance ToJSON DelegationsAndRewards where
  toJSON delegsAndRwds =
      Aeson.Array . Vector.fromList
        . map delegAndRwdToJson $ mergeDelegsAndRewards delegsAndRwds
    where
      delegAndRwdToJson :: (StakeAddress, Maybe Lovelace, Maybe PoolId) -> Aeson.Value
      delegAndRwdToJson (addr, mRewards, mPoolId) =
        Aeson.object
          [ "address" .= addr
          , "delegation" .= mPoolId
          , "rewardAccountBalance" .= mRewards
          ]

instance FromJSON DelegationsAndRewards where
  parseJSON = withArray "DelegationsAndRewards" $ \arr -> do
    let vals = Vector.toList arr
    decoded <- mapM decodeObject vals
    pure $ zipper decoded
    where
      zipper :: [(StakeAddress, Maybe Lovelace, Maybe PoolId)]
              -> DelegationsAndRewards
      zipper l = do
        let maps = [ ( maybe mempty (Map.singleton sa) delegAmt
                     , maybe mempty (Map.singleton sa) mPool
                     )
                   | (sa, delegAmt, mPool) <- l
                   ]
        DelegationsAndRewards
          $ foldl
              (\(amtA, delegA) (amtB, delegB) -> (amtA <> amtB, delegA <> delegB))
              (mempty, mempty)
              maps

      decodeObject :: Aeson.Value
                   -> Aeson.Parser (StakeAddress, Maybe Lovelace, Maybe PoolId)
      decodeObject  = withObject "DelegationsAndRewards" $ \o -> do
        address <- o .: "address"
        delegation <- o .:? "delegation"
        rewardAccountBalance <- o .:? "rewardAccountBalance"
        pure (address, rewardAccountBalance, delegation)

runQueryLeadershipScheduleCmd
  :: SocketPath
  -> AnyConsensusModeParams
  -> NetworkId
  -> GenesisFile -- ^ Shelley genesis
  -> VerificationKeyOrHashOrFile StakePoolKey
  -> SigningKeyFile In -- ^ VRF signing key
  -> EpochLeadershipSchedule
  -> Maybe (File () Out)
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryLeadershipScheduleCmd
    socketPath (AnyConsensusModeParams cModeParams) network
    (GenesisFile genFile) coldVerKeyFile vrfSkeyFp
    whichSchedule mJsonOutputFile = do
  let localNodeConnInfo = LocalNodeConnectInfo cModeParams network socketPath

  poolid <- lift (readVerificationKeyOrHashOrFile AsStakePoolKey coldVerKeyFile)
    & onLeft (left . ShelleyQueryCmdTextReadError)

  vrkSkey <- lift (readFileTextEnvelope (AsSigningKey AsVrfKey) vrfSkeyFp)
    & onLeft (left . ShelleyQueryCmdTextEnvelopeReadError)

  shelleyGenesis <- lift (readAndDecodeShelleyGenesis genFile)
    & onLeft (left . ShelleyQueryCmdGenesisReadError)

  join $ lift
    ( executeLocalStateQueryExpr localNodeConnInfo Nothing $ runExceptT $ do
        anyE@(AnyCardanoEra era) <- lift (determineEraExpr cModeParams)
          & onLeft (left . ShelleyQueryCmdUnsupportedNtcVersion)

        sbe <- requireShelleyBasedEra era
          & onNothing (left ShelleyQueryCmdByronEra)

        let cMode = consensusModeOnly cModeParams

        case cMode of
          CardanoMode -> do
            eInMode <- toEraInMode era cMode
                & hoistMaybe (ShelleyQueryCmdEraConsensusModeMismatch (AnyConsensusMode cMode) anyE)

            eraInMode <- calcEraInMode era $ consensusModeOnly cModeParams

            requireNotByronEraInByronMode eraInMode

            pparams <- lift (queryProtocolParameters eInMode sbe)
              & onLeft (left . ShelleyQueryCmdUnsupportedNtcVersion)
              & onLeft (left . ShelleyQueryCmdLocalStateQueryError . EraMismatchError)

            ptclState <- lift (queryProtocolState eInMode sbe)
              & onLeft (left . ShelleyQueryCmdUnsupportedNtcVersion)
              & onLeft (left . ShelleyQueryCmdLocalStateQueryError . EraMismatchError)

            eraHistory <- lift queryEraHistory
              & onLeft (left . ShelleyQueryCmdUnsupportedNtcVersion)

            let eInfo = toEpochInfo eraHistory

            curentEpoch <- lift (queryEpoch eInMode sbe)
              & onLeft (left . ShelleyQueryCmdUnsupportedNtcVersion)
              & onLeft (left . ShelleyQueryCmdLocalStateQueryError . EraMismatchError)

            case whichSchedule of
              CurrentEpoch -> do
                serCurrentEpochState <- lift (queryPoolDistribution eInMode sbe (Just (Set.singleton poolid)))
                  & onLeft (left . ShelleyQueryCmdUnsupportedNtcVersion)
                  & onLeft (left . ShelleyQueryCmdLocalStateQueryError . EraMismatchError)

                pure $ do
                  schedule <- firstExceptT ShelleyQueryCmdLeaderShipError $ hoistEither
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

                  writeSchedule mJsonOutputFile eInfo shelleyGenesis schedule

              NextEpoch -> do
                serCurrentEpochState <- lift (queryCurrentEpochState eInMode sbe)
                  & onLeft (left . ShelleyQueryCmdUnsupportedNtcVersion)
                  & onLeft (left . ShelleyQueryCmdLocalStateQueryError . EraMismatchError)

                pure $ do
                  tip <- liftIO $ getLocalChainTip localNodeConnInfo

                  schedule <- firstExceptT ShelleyQueryCmdLeaderShipError $ hoistEither
                    $ shelleyBasedEraConstraints sbe
                    $ nextEpochEligibleLeadershipSlots sbe shelleyGenesis
                      serCurrentEpochState ptclState poolid vrkSkey pparams
                      eInfo (tip, curentEpoch)

                  writeSchedule mJsonOutputFile eInfo shelleyGenesis schedule
          mode ->
            pure $ do
              left . ShelleyQueryCmdUnsupportedMode $ AnyConsensusMode mode
    )
    & onLeft (left . ShelleyQueryCmdAcquireFailure)
    & onLeft left
  where
    writeSchedule mOutFile eInfo shelleyGenesis schedule =
      case mOutFile of
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


-- Helpers

calcEraInMode :: ()
  => Monad m
  => CardanoEra era
  -> ConsensusMode mode
  -> ExceptT ShelleyQueryCmdError m (EraInMode era mode)
calcEraInMode era mode =
  pure (toEraInMode era mode)
    & onNothing (left (ShelleyQueryCmdEraConsensusModeMismatch (AnyConsensusMode mode) (anyCardanoEra era)))

requireNotByronEraInByronMode :: ()
  => Monad m
  => EraInMode era mode
  -> ExceptT ShelleyQueryCmdError m ()
requireNotByronEraInByronMode = \case
  ByronEraInByronMode -> left ShelleyQueryCmdByronEra
  _ -> pure ()

toEpochInfo :: EraHistory CardanoMode -> EpochInfo (Either Text)
toEpochInfo (EraHistory _ interpreter) =
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
toTentativeEpochInfo :: EraHistory CardanoMode -> Tentative (EpochInfo (Either Text))
toTentativeEpochInfo (EraHistory _ interpreter) =
  Tentative
    $ hoistEpochInfo (first (Text.pack . show) . runExcept)
    $ Consensus.interpreterToEpochInfo (Consensus.unsafeExtendSafeZone interpreter)


-- | Get slot number for timestamp, or an error if the UTC timestamp is before 'SystemStart' or after N+1 era
utcTimeToSlotNo
  :: SocketPath
  -> AnyConsensusModeParams
  -> NetworkId
  -> UTCTime
  -> ExceptT ShelleyQueryCmdError IO SlotNo
utcTimeToSlotNo socketPath (AnyConsensusModeParams cModeParams) network utcTime = do
  let localNodeConnInfo = LocalNodeConnectInfo cModeParams network socketPath
  case consensusModeOnly cModeParams of
    CardanoMode -> do
      lift
        ( executeLocalStateQueryExpr localNodeConnInfo Nothing $ runExceptT $ do
            systemStart <- lift querySystemStart
              & onLeft (left . ShelleyQueryCmdUnsupportedNtcVersion)

            eraHistory <- lift queryEraHistory
              & onLeft (left . ShelleyQueryCmdUnsupportedNtcVersion)

            let relTime = toRelativeTime systemStart utcTime

            pure (Api.getSlotForRelativeTime relTime eraHistory)
              & onLeft (left . ShelleyQueryCmdPastHorizon)
        )
        & onLeft (left . ShelleyQueryCmdAcquireFailure)
        & onLeft left

    mode -> left . ShelleyQueryCmdUnsupportedMode $ AnyConsensusMode mode
