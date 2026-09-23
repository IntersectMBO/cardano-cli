{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.CLI.EraIndependent.Debug.CheckPoolRegistration.Run
  ( runCheckPoolRegistrationCmd
  )
where

import Cardano.Api
import Cardano.Api.Experimental qualified as Exp
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Network qualified as Consensus

import Cardano.CLI.Compatible.Exception
import Cardano.CLI.EraIndependent.Debug.CheckPoolRegistration.Command
import Cardano.CLI.EraIndependent.Debug.CheckPoolRegistration.Internal.Diff
import Cardano.CLI.Json.Encode qualified as Json
import Cardano.CLI.LocalStateQuery (checkNodeNetworkId)
import Cardano.CLI.Type.Common
import Cardano.CLI.Type.Error.DebugCmdError
import Cardano.CLI.Type.Error.QueryCmdError
import Cardano.Ledger.Api.State.Query qualified as L
import Cardano.Ledger.State qualified as L

import Data.Bifunctor (first)
import Data.ByteString.Lazy qualified as LBS
import Data.Function ((&))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import GHC.Stack (HasCallStack)
import Vary qualified

-- | Compare a stake pool registration certificate that has been built but not
-- yet submitted against the pool's live parameters.
--
-- This is diagnostics only: a parameter difference is something for the
-- operator to look at, not a failure, so the command exits 0 for every
-- comparison it can carry out. It fails only when it cannot do its job, that
-- is when the file cannot be read, is not a stake pool registration
-- certificate, or the node cannot be reached or queried.
runCheckPoolRegistrationCmd
  :: ()
  => CheckPoolRegistrationCmdArgs
  -> CIO e ()
runCheckPoolRegistrationCmd
  CheckPoolRegistrationCmdArgs
    { nodeConnInfo
    , poolRegistrationCertFile
    , outputFormat
    , mOutFile
    } = do
    let certFilePath = unFile poolRegistrationCertFile

    -- Read the envelope before touching the node, so that a file that is not a
    -- certificate at all is reported without needing one.
    envelope <-
      fromEitherIOCli @(FileError TextEnvelopeError) $
        readTextEnvelopeOfTypeFromFile certificateEnvelopeType certFilePath

    checkNodeNetworkId nodeConnInfo

    report <-
      fromEitherIOCli $
        executeLocalStateQueryExpr nodeConnInfo Consensus.VolatileTip $ do
          -- The era comes from the node rather than from a flag: the
          -- certificate's text envelope does not carry one, and decoding it
          -- with the wrong era would misread the parameters.
          AnyCardanoEra cEra <- runNodeQuery queryCurrentEra
          era <-
            maybe (throwCliError $ QueryCmdEraNotSupported (AnyCardanoEra cEra)) pure $
              forEraMaybeEon cEra
          buildReport (localNodeNetworkId nodeConnInfo) certFilePath envelope era

    let output =
          outputFormat
            & ( id
                  . Vary.on (\FormatJson -> Json.encodeJson report)
                  . Vary.on (\FormatText -> renderReportText report)
                  . Vary.on (\FormatYaml -> Json.encodeYaml report)
                  $ Vary.exhaustiveCase
              )

    fromEitherIOCli @(FileError ()) $
      writeLazyByteStringOutput mOutFile output

-- | The text envelope type every certificate is written with. It does not
-- distinguish the kinds of certificate, nor the era, so it is only the first
-- of the checks this command makes.
certificateEnvelopeType :: TextEnvelopeType
certificateEnvelopeType = "Certificate"

-- | Decode the certificate, ask the node for the pool's live parameters, and
-- compare the two.
buildReport
  :: forall era block point r
   . NetworkId
  -> FilePath
  -> TextEnvelope
  -> Exp.Era era
  -> LocalStateQueryExpr block point QueryInMode r IO PoolRegistrationReport
buildReport networkId certFilePath envelope era =
  Exp.obtainCommonConstraints era $ do
    certParams <- decodePoolRegistrationCertificate era certFilePath envelope

    let poolId = L.sppId certParams

    serialisedPoolState <-
      runEraQuery $
        queryPoolState (convert era) (Just (Set.singleton (StakePoolKeyHash poolId)))

    PoolState poolState <-
      fromEitherCli $ decodePoolState (convert era) serialisedPoolState

    -- The live parameters are held as a 'L.StakePoolState', which drops the
    -- pool id and stamps the voting key with the epoch it was registered in.
    -- Putting both sides into 'L.StakePoolParams' is what makes them
    -- comparable; the network tag comes from the node, which
    -- 'checkNodeNetworkId' has already agreed with.
    let mCurrent =
          L.stakePoolStateToStakePoolParams (toShelleyNetwork networkId) poolId
            <$> Map.lookup poolId (L.qpsrStakePools poolState)
        mStaged = Map.lookup poolId (L.qpsrFutureStakePoolParams poolState)
        mRetiring = Map.lookup poolId (L.qpsrRetiring poolState)

    pure $ mkPoolRegistrationReport certParams mCurrent mStaged mRetiring

-- | Decode a stake pool registration certificate, in the era the node is in.
--
-- A retirement or delegation certificate has the same text envelope type as a
-- registration one, so the envelope check is not enough on its own: only the
-- certificate's own contents say which kind it is.
decodePoolRegistrationCertificate
  :: forall era m
   . MonadIO m
  => Exp.Era era
  -> FilePath
  -> TextEnvelope
  -> m (L.StakePoolParams (Exp.LedgerEra era))
decodePoolRegistrationCertificate era certFilePath envelope =
  Exp.obtainCommonConstraints era $ do
    certificate :: Exp.Certificate (Exp.LedgerEra era) <-
      fromEitherCli @(FileError TextEnvelopeError) $
        first (FileError certFilePath) $
          deserialiseFromTextEnvelope envelope

    case certificate of
      Exp.Certificate txCert ->
        case L.getRegPoolTxCert txCert of
          Just poolParams -> pure poolParams
          Nothing ->
            throwCliError $
              DebugNotAPoolRegistrationCertificate certFilePath (describeEnvelope envelope)

-- | What the file turned out to hold, for an error message. The envelope's
-- description is what the tool that wrote it called the certificate, which is
-- more use to an operator than the decoded value.
describeEnvelope :: TextEnvelope -> Text
describeEnvelope TextEnvelope{teType = TextEnvelopeType envelopeType, teDescription = TextEnvelopeDescr descr}
  | null descr = Text.pack envelopeType
  | otherwise = Text.pack descr

-- | Run a node query, turning an unsupported node-to-client version into a CLI
-- error.
runNodeQuery
  :: HasCallStack
  => MonadIO m
  => m (Either UnsupportedNtcVersionError a)
  -> m a
runNodeQuery query =
  query >>= either (throwCliError . QueryCmdUnsupportedNtcVersion) pure

-- | Run an era-bound node query, turning an era mismatch into a CLI error too.
runEraQuery
  :: HasCallStack
  => MonadIO m
  => m (Either UnsupportedNtcVersionError (Either EraMismatch a))
  -> m a
runEraQuery query =
  runNodeQuery query >>= either (throwCliError . QueryCmdEraMismatch) pure

-- | The human readable diff, as UTF-8: the markers and the abbreviating
-- ellipsis are not ASCII, so this cannot go through 'fromString'.
renderReportText :: PoolRegistrationReport -> LBS.ByteString
renderReportText =
  LBS.fromStrict
    . Text.encodeUtf8
    . (<> "\n")
    . Text.pack
    . docToString
    . renderPoolRegistrationReport
