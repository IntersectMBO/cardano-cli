
module Cardano.CLI.Types.Errors.QueryCmdError
  ( QueryCmdError(..)
  , renderQueryCmdError
  ) where

import           Cardano.Api hiding (QueryInShelleyBasedEra (..))
import           Cardano.Api.Shelley hiding (QueryInShelleyBasedEra (..))

import           Cardano.Binary (DecoderError)
import           Cardano.CLI.Helpers (HelpersError (..), renderHelpersError)
import           Cardano.CLI.Types.Errors.GenesisCmdError
import           Cardano.CLI.Types.Errors.QueryCmdLocalStateQueryError
import           Ouroboros.Consensus.Cardano.Block as Consensus (EraMismatch (..))
import qualified Ouroboros.Consensus.HardFork.History.Qry as Qry

import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Text.Lazy (toStrict)
import           Data.Text.Lazy.Builder (toLazyText)
import           Formatting.Buildable (build)

data QueryCmdError
  = QueryCmdLocalStateQueryError !QueryCmdLocalStateQueryError
  | QueryCmdWriteFileError !(FileError ())
  | QueryCmdHelpersError !HelpersError
  | QueryCmdAcquireFailureError !AcquiringFailure
  | QueryCmdEraConsensusModeMismatchError !AnyConsensusMode !AnyCardanoEra
  | QueryCmdByronEraInvalidError
  | QueryCmdEraMismatchError !EraMismatch
  | QueryCmdUnsupportedModeError !AnyConsensusMode
  | QueryCmdPastHorizonError !Qry.PastHorizonException
  | QueryCmdSystemStartUnavailableError
  | QueryCmdGenesisReadError !GenesisCmdError
  | QueryCmdLeaderShipError !LeadershipError
  | QueryCmdTextEnvelopeReadError !(FileError TextEnvelopeError)
  | QueryCmdTextReadError !(FileError InputDecodeError)
  | QueryCmdOpCertCounterReadError !(FileError TextEnvelopeError)
  | QueryCmdProtocolStateDecodeFailureError !(LBS.ByteString, DecoderError)
  | QueryCmdPoolStateDecodeError DecoderError
  | QueryCmdStakeSnapshotDecodeError DecoderError
  | QueryCmdUnsupportedNtcVersionError !UnsupportedNtcVersionError
  | QueryCmdProtocolParameterConversionError !ProtocolParametersConversionError
  deriving Show

renderQueryCmdError :: QueryCmdError -> Text
renderQueryCmdError err =
  case err of
    QueryCmdLocalStateQueryError lsqErr -> renderLocalStateQueryError lsqErr
    QueryCmdWriteFileError fileErr -> Text.pack (displayError fileErr)
    QueryCmdHelpersError helpersErr -> renderHelpersError helpersErr
    QueryCmdAcquireFailureError acquireFail -> Text.pack $ show acquireFail
    QueryCmdByronEraInvalidError -> "This query cannot be used for the Byron era"
    QueryCmdEraConsensusModeMismatchError (AnyConsensusMode cMode) (AnyCardanoEra era) ->
      "Consensus mode and era mismatch. Consensus mode: " <> textShow cMode <>
      " Era: " <> textShow era
    QueryCmdEraMismatchError (EraMismatch ledgerEra queryEra) ->
      "\nAn error mismatch occurred." <> "\nSpecified query era: " <> queryEra <>
      "\nCurrent ledger era: " <> ledgerEra
    QueryCmdUnsupportedModeError mode -> "Unsupported mode: " <> renderMode mode
    QueryCmdPastHorizonError e -> "Past horizon: " <> textShow e
    QueryCmdSystemStartUnavailableError -> "System start unavailable"
    QueryCmdGenesisReadError err' -> Text.pack $ displayError err'
    QueryCmdLeaderShipError e -> Text.pack $ displayError e
    QueryCmdTextEnvelopeReadError e -> Text.pack $ displayError e
    QueryCmdTextReadError e -> Text.pack $ displayError e
    QueryCmdOpCertCounterReadError e -> Text.pack $ displayError e
    QueryCmdProtocolStateDecodeFailureError (_, decErr) ->
      "Failed to decode the protocol state: " <> toStrict (toLazyText $ build decErr)
    QueryCmdPoolStateDecodeError decoderError ->
      "Failed to decode PoolState.  Error: " <> Text.pack (show decoderError)
    QueryCmdStakeSnapshotDecodeError decoderError ->
      "Failed to decode StakeSnapshot.  Error: " <> Text.pack (show decoderError)
    QueryCmdUnsupportedNtcVersionError (UnsupportedNtcVersionError minNtcVersion ntcVersion) ->
      "Unsupported feature for the node-to-client protocol version.\n" <>
      "This query requires at least " <> textShow minNtcVersion <> " but the node negotiated " <> textShow ntcVersion <> ".\n" <>
      "Later node versions support later protocol versions (but development protocol versions are not enabled in the node by default)."
    QueryCmdProtocolParameterConversionError ppce ->
      Text.pack $ "Failed to convert protocol parameter: " <> displayError ppce
