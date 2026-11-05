{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.CLI.EraBased.StakePool.Run
  ( runStakePoolCmds
  , runStakePoolIdCmd
  , runStakePoolMetadataHashCmd
  , runStakePoolRegistrationCertificateCmd
  , runStakePoolBlsRegistrationCertificateCmd
  , runStakePoolDeregistrationCertificateCmd
  )
where

import Cardano.Api
import Cardano.Api.Experimental
import Cardano.Api.Experimental.Certificate
  ( StakePoolMetadata
  , StakePoolParameters (..)
  , toShelleyPoolParams
  , validateAndHashStakePoolMetadata
  )
import Cardano.Api.Ledger qualified as L

import Cardano.CLI.Compatible.Exception
import Cardano.CLI.EraBased.Query.Command (QueryCommons (..))
import Cardano.CLI.EraBased.Query.Run (easyRunQuery)
import Cardano.CLI.EraBased.StakePool.Command
import Cardano.CLI.EraBased.StakePool.Command qualified as Cmd
import Cardano.CLI.EraBased.StakePool.Internal.Metadata (carryHashChecks)
import Cardano.CLI.EraBased.StakePool.Internal.Relay (validateStakePoolRelays)
import Cardano.CLI.EraIndependent.Hash.Command qualified as Cmd
import Cardano.CLI.EraIndependent.Hash.Internal.Common
  ( allSchemes
  , getByteStringFromURL
  )
import Cardano.CLI.Helper (printWarning)
import Cardano.CLI.Orphan ()
import Cardano.CLI.Read (getVerificationKeyFromStakePoolVerificationKeySource)
import Cardano.CLI.Type.Common
import Cardano.CLI.Type.Error.HashCmdError (FetchURLError (..))
import Cardano.CLI.Type.Error.StakePoolCmdError
import Cardano.CLI.Type.Key (readVerificationKeyOrFile)
import Cardano.Ledger.Api.State.Query qualified as L
import Cardano.Ledger.State qualified as L

import Control.Monad (forM, forM_, when)
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Function ((&))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe.Strict (isSJust)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Encoding qualified as Text
import Data.Text.IO qualified as T
import System.IO (stderr)
import Vary qualified

registrationCertDesc :: TextEnvelopeDescr
registrationCertDesc = "Stake Pool Registration Certificate"

runStakePoolCmds
  :: IsEra era
  => StakePoolCmds era
  -> CIO e ()
runStakePoolCmds = \case
  StakePoolBlsRegistrationCertificateCmd args -> runStakePoolBlsRegistrationCertificateCmd args
  StakePoolDeregistrationCertificateCmd args -> runStakePoolDeregistrationCertificateCmd args
  StakePoolIdCmd args -> runStakePoolIdCmd args
  StakePoolMetadataHashCmd args -> runStakePoolMetadataHashCmd args
  StakePoolRegistrationCertificateCmd args -> runStakePoolRegistrationCertificateCmd args

--
-- Stake pool command implementations
--

-- | Create a stake pool registration cert.
-- TODO: Metadata and more stake pool relay support to be
-- added in the future.
runStakePoolRegistrationCertificateCmd
  :: forall era e
   . IsEra era
  => StakePoolRegistrationCertificateCmdArgs era
  -> CIO e ()
runStakePoolRegistrationCertificateCmd
  Cmd.StakePoolRegistrationCertificateCmdArgs
    { era
    , poolVerificationKeyOrFile
    , vrfVerificationKeyOrFile
    , blsSkeyFile
    , poolPledge
    , poolCost
    , poolMargin
    , rewardStakeVerificationKeyOrFile
    , ownerStakeVerificationKeyOrFiles
    , relays
    , validateRelays
    , mMetadata
    , network
    , outFile
    } =
    obtainCommonConstraints era $ do
      when (validateRelays == ValidateRelays) $
        validateStakePoolRelays network relays

      -- Pool verification key
      stakePoolVerKey <- getVerificationKeyFromStakePoolVerificationKeySource poolVerificationKeyOrFile
      let stakePoolId' = anyStakePoolVerificationKeyHash stakePoolVerKey

      -- VRF verification key
      vrfVerKey <-
        readVerificationKeyOrFile vrfVerificationKeyOrFile
      let vrfKeyHash' = verificationKeyHash vrfVerKey

      -- Pool reward account
      rwdStakeVerKey <-
        readVerificationKeyOrFile rewardStakeVerificationKeyOrFile
      let stakeCred = StakeCredentialByKey (verificationKeyHash rwdStakeVerKey)
          rewardAccountAddr = makeStakeAddress network stakeCred

      -- Pool owner(s)
      sPoolOwnerVkeys <-
        mapM
          readVerificationKeyOrFile
          ownerStakeVerificationKeyOrFiles
      let stakePoolOwners' = map verificationKeyHash sPoolOwnerVkeys

      -- BLS voting key, registered from Dijkstra onwards
      mBlsKey <-
        forM blsSkeyFile $ \skeyFile -> do
          blsSkey <-
            fromEitherIOCli @(FileError TextEnvelopeError) $
              readFileTextEnvelope @(SigningKey BlsKey) skeyFile
          pure $ toLedgerBlsKey blsSkey

      let stakePoolParams =
            StakePoolParameters
              { stakePoolId = stakePoolId'
              , stakePoolVRF = vrfKeyHash'
              , stakePoolBlsKey = mBlsKey
              , stakePoolCost = poolCost
              , stakePoolMargin = poolMargin
              , stakePoolRewardAccount = rewardAccountAddr
              , stakePoolPledge = poolPledge
              , stakePoolOwners = stakePoolOwners'
              , stakePoolRelays = relays
              , stakePoolMetadata = pcaAnchor <$> mMetadata
              }

      let ledgerStakePoolParams = toShelleyPoolParams stakePoolParams
          registrationCert = makeStakePoolRegistrationCertificate ledgerStakePoolParams :: Certificate (LedgerEra era)

      mapM_ (fromExceptTCli . carryHashChecks) mMetadata
      fromEitherIOCli @(FileError ()) $
        writeLazyByteStringFile outFile $
          textEnvelopeToJSON (Just registrationCertDesc) registrationCert

-- | Create a stake pool registration certificate that keeps every parameter of
-- an existing one and replaces only its BLS key.
runStakePoolBlsRegistrationCertificateCmd
  :: forall era e
   . IsEra era
  => StakePoolBlsRegistrationCertificateCmdArgs era
  -> CIO e ()
runStakePoolBlsRegistrationCertificateCmd
  Cmd.StakePoolBlsRegistrationCertificateCmdArgs
    { era
    , poolParametersSource
    , blsSkeyFile
    , outFile
    } =
    obtainCommonConstraints era $ do
      oldParams <- readStakePoolParameters @era poolParametersSource

      blsSkey <-
        fromEitherIOCli @(FileError TextEnvelopeError) $
          readFileTextEnvelope @(SigningKey BlsKey) blsSkeyFile
      let newBlsKey = toLedgerBlsKey blsSkey
          mOldBlsKey = L.strictMaybeToMaybe $ L.sppBlsKey oldParams
          newParams = oldParams{L.sppBlsKey = L.SJust newBlsKey}
          registrationCert = makeStakePoolRegistrationCertificate newParams :: Certificate (LedgerEra era)

      liftIO $ do
        T.hPutStrLn stderr $ "Pool ID: " <> serialiseToBech32 (StakePoolKeyHash $ L.sppId oldParams)
        T.hPutStrLn stderr $ "Old BLS key: " <> maybe "none (first registration)" renderBlsKey mOldBlsKey
        T.hPutStrLn stderr $ "New BLS key: " <> renderBlsKey newBlsKey
      when (mOldBlsKey == Just newBlsKey) $
        printWarning "The new BLS key is the same as the registered one."

      fromEitherIOCli @(FileError ()) $
        writeLazyByteStringFile outFile $
          textEnvelopeToJSON (Just registrationCertDesc) registrationCert
   where
    renderBlsKey :: L.BlsKey -> T.Text
    renderBlsKey = serialiseToBech32 . BlsVerificationKey . L.blsPubKey

-- | The pool parameters to keep, with warnings for what the new certificate overrides.
readStakePoolParameters
  :: forall era e
   . IsEra era
  => StakePoolParametersSource
  -> CIO e (L.StakePoolParams (LedgerEra era))
readStakePoolParameters (StakePoolParametersFromCertificate (CertificateFile certFile)) =
  obtainCommonConstraints (useEra @era) $ do
    Certificate cert <-
      fromEitherIOCli @(FileError TextEnvelopeError) $
        readFileTextEnvelope @(Certificate (LedgerEra era)) (File certFile)
    fromMaybeCli (StakePoolCmdNotRegistrationCertificateError certFile) $
      L.getRegPoolTxCert cert
readStakePoolParameters (StakePoolParametersFromPoolState (File poolStateFilePath) poolId choice) =
  obtainCommonConstraints (useEra @era) $ do
    poolStates <-
      fromEitherIOCli @(FileError JsonDecodeError) $
        readFileJSON @(Map (L.KeyHash L.StakePool) (L.QueryResultStakePoolState (LedgerEra era)))
          poolStateFilePath
    selectPoolParameters @era (PoolStateFromFile poolStateFilePath) poolId choice poolStates
readStakePoolParameters (StakePoolParametersFromNode QueryCommons{nodeConnInfo, target} poolId choice) =
  obtainCommonConstraints (useEra @era) $ do
    poolStates <-
      fromEitherIOCli
        ( executeLocalStateQueryExpr nodeConnInfo target $
            runExceptT $
              easyRunQuery $
                queryStakePoolState
                  (convert $ useEra @era)
                  (Just . Set.singleton $ unStakePoolKeyHash poolId)
                  NotIncludeDelegators
        )
        & fromEitherCIOCli
    selectPoolParameters @era PoolStateFromNode poolId choice poolStates

-- | Select the parameters of one pool from a pool state, which comes from
-- @query pool-state@ output or a node query ('PoolStateOrigin' only names the
-- source in error messages).
--
-- * 'PoolStateCurrentParameters': the pool's active parameters. The reward account
--   gets the given network, because the pool state has none.
-- * 'PoolStatePendingParameters': the parameters of the pool's pending update.
--   Fails if there is no pending update.
--
-- Fails if the pool is not in the pool state. Prints a warning for everything the
-- new registration certificate overrides: a pending update (current parameters
-- only) and a scheduled retirement, which the ledger cancels when a registered
-- pool registers again.
selectPoolParameters
  :: forall era e
   . IsEra era
  => PoolStateOrigin
  -> Hash StakePoolKey
  -> PoolStateParametersChoice
  -> Map (L.KeyHash L.StakePool) (L.QueryResultStakePoolState (LedgerEra era))
  -> CIO e (L.StakePoolParams (LedgerEra era))
selectPoolParameters origin poolId choice poolStates =
  obtainCommonConstraints (useEra @era) $ do
    let
      -- Re-registering a registered pool removes its scheduled retirement.
      warnAboutRetirement :: L.QueryResultStakePoolState (LedgerEra era) -> CIO e ()
      warnAboutRetirement poolState =
        forM_ (L.strictMaybeToMaybe $ L.qrspsRetiring poolState) $ \epoch ->
          printWarning $
            "The pool is scheduled to retire in epoch "
              <> show (unEpochNo epoch)
              <> ". The new certificate cancels the retirement."

      currentParameters
        :: NetworkId
        -> L.QueryResultStakePoolState (LedgerEra era)
        -> L.StakePoolParams (LedgerEra era)
      currentParameters network poolState =
        L.StakePoolParams
          { L.sppId = unStakePoolKeyHash poolId
          , L.sppVrf = L.qrspsVrf poolState
          , L.sppBlsKey = L.bksKey <$> L.qrspsBlsKey poolState
          , L.sppPledge = L.qrspsPledge poolState
          , L.sppCost = L.qrspsCost poolState
          , L.sppMargin = L.qrspsMargin poolState
          , L.sppAccountAddress = L.AccountAddress (toShelleyNetwork network) (L.qrspsAccountId poolState)
          , L.sppOwners = L.qrspsOwners poolState
          , L.sppRelays = L.qrspsRelays poolState
          , L.sppMetadata = L.qrspsMetadata poolState
          }

    poolState <-
      fromMaybeCli (StakePoolCmdPoolNotInPoolStateError origin poolId) $
        Map.lookup (unStakePoolKeyHash poolId) poolStates
    case choice of
      PoolStateCurrentParameters network -> do
        when (isSJust $ L.qrspsFutureStakePoolParams poolState) $
          printWarning "The pool has a pending parameter update. The new certificate replaces it."
        warnAboutRetirement poolState
        pure $ currentParameters network poolState
      PoolStatePendingParameters -> do
        pendingParams <-
          fromMaybeCli (StakePoolCmdNoPendingParametersError origin poolId) $
            L.strictMaybeToMaybe $
              L.qrspsFutureStakePoolParams poolState
        warnAboutRetirement poolState
        pure pendingParams

runStakePoolDeregistrationCertificateCmd
  :: forall era e
   . StakePoolDeregistrationCertificateCmdArgs era
  -> CIO e ()
runStakePoolDeregistrationCertificateCmd
  Cmd.StakePoolDeregistrationCertificateCmdArgs
    { era
    , poolVerificationKeyOrFile
    , retireEpoch
    , outFile
    } =
    obtainCommonConstraints era $ do
      -- Pool verification key
      stakePoolVerKey <- getVerificationKeyFromStakePoolVerificationKeySource poolVerificationKeyOrFile

      let stakePoolId' = anyStakePoolVerificationKeyHash stakePoolVerKey
          retireCert =
            makeStakePoolRetirementCertificate stakePoolId' retireEpoch :: Certificate (ShelleyLedgerEra era)

      fromEitherIOCli @(FileError ()) $
        writeLazyByteStringFile outFile $
          textEnvelopeToJSON (Just retireCertDesc) retireCert
   where
    retireCertDesc :: TextEnvelopeDescr
    retireCertDesc = "Stake Pool Retirement Certificate"

runStakePoolIdCmd
  :: ()
  => StakePoolIdCmdArgs era
  -> CIO e ()
runStakePoolIdCmd
  Cmd.StakePoolIdCmdArgs
    { poolVerificationKeyOrFile
    , outputFormat
    , mOutFile
    } = do
    stakePoolVerKey <- getVerificationKeyFromStakePoolVerificationKeySource poolVerificationKeyOrFile
    let stakePoolKeyHash = anyStakePoolVerificationKeyHash stakePoolVerKey

    let output =
          outputFormat
            & ( id
                  . Vary.on (\FormatBech32 -> Text.encodeUtf8 . serialiseToBech32)
                  . Vary.on (\FormatHex -> serialiseToRawBytesHex)
                  $ Vary.exhaustiveCase
              )
            $ stakePoolKeyHash

    fromEitherIOCli @(FileError ()) $
      writeByteStringOutput mOutFile output

runStakePoolMetadataHashCmd
  :: ()
  => StakePoolMetadataHashCmdArgs era
  -> CIO e ()
runStakePoolMetadataHashCmd
  Cmd.StakePoolMetadataHashCmdArgs
    { poolMetadataSource
    , hashGoal
    } = do
    metadataBytes <-
      case poolMetadataSource of
        StakePoolMetadataFileIn poolMetadataFile ->
          fromEitherIOCli @(FileError ()) $
            readByteStringFile poolMetadataFile
        StakePoolMetadataURL urlText ->
          fromExceptTCli . fetchURLToStakePoolCmdError $ getByteStringFromURL allSchemes $ L.urlToText urlText

    (_metadata, metadataHash) <-
      fromEitherCli $
        validateAndHashStakePoolMetadata metadataBytes

    case hashGoal of
      Cmd.CheckHash expectedHash
        | metadataHash /= expectedHash ->
            throwCliError $ StakePoolCmdHashMismatchError expectedHash metadataHash
        | otherwise -> liftIO $ putStrLn "Hashes match!"
      Cmd.HashToFile outFile -> writeOutput (Just outFile) metadataHash
      Cmd.HashToStdout -> writeOutput Nothing metadataHash
   where
    writeOutput :: Maybe (File () Out) -> Hash StakePoolMetadata -> CIO e ()
    writeOutput mOutFile metadataHash = do
      let output = LBS.fromStrict $ serialiseToRawBytesHex metadataHash

      fromEitherIOCli @(FileError ()) $
        writeLazyByteStringOutput mOutFile output

    fetchURLToStakePoolCmdError
      :: ExceptT FetchURLError IO BS.ByteString -> ExceptT StakePoolCmdError IO BS.ByteString
    fetchURLToStakePoolCmdError = withExceptT StakePoolCmdFetchURLError
