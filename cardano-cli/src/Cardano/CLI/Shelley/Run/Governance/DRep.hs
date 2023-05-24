{-# LANGUAGE DataKinds #-}

module Cardano.CLI.Shelley.Run.Governance.DRep
  ( ShelleyGovernanceDRepCmdError(ShelleyDRepCmdReadFileError)
  , renderShelleyGovernanceDRepCmdError
  , runGovernanceDRepCmd
  ) where

import           Cardano.Api

import           Cardano.CLI.Shelley.Commands
import           Cardano.CLI.Shelley.Key (VerificationKeyOrFile, readVerificationKeyOrFile)
import           Cardano.CLI.Types (PoolIdOutputFormat (..))
import qualified Cardano.Ledger.Slot as Shelley

import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Trans (lift)
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT, hoistEither, left,
                   newExceptT, onLeft)
import qualified Data.ByteString.Char8 as BS
import           Data.Function ((&))
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

data ShelleyGovernanceDRepCmdError
  = ShelleyDRepCmdReadFileError !(FileError TextEnvelopeError)
  | ShelleyDRepCmdReadKeyFileError !(FileError InputDecodeError)
  | ShelleyDRepCmdWriteFileError !(FileError ())
  | ShelleyDRepCmdMetadataValidationError !DRepMetadataValidationError
  deriving Show

renderShelleyGovernanceDRepCmdError :: ShelleyGovernanceDRepCmdError -> Text
renderShelleyGovernanceDRepCmdError err =
  case err of
    ShelleyDRepCmdReadFileError fileErr -> Text.pack (displayError fileErr)
    ShelleyDRepCmdReadKeyFileError fileErr -> Text.pack (displayError fileErr)
    ShelleyDRepCmdWriteFileError fileErr -> Text.pack (displayError fileErr)
    ShelleyDRepCmdMetadataValidationError validationErr ->
      "Error validating drep metadata: " <> Text.pack (displayError validationErr)



runGovernanceDRepCmd :: DRepCmd -> ExceptT ShelleyGovernanceDRepCmdError IO ()
runGovernanceDRepCmd (DRepRegistrationCert network sPvkey mbMetadata outfp) =
  runDRepRegistrationCert network sPvkey mbMetadata outfp
runGovernanceDRepCmd (DRepRetirementCert sPvkeyFp retireEpoch outfp) =
  runDRepRetirementCert sPvkeyFp retireEpoch outfp
runGovernanceDRepCmd (DRepGetId sPvkey outputFormat) = runDRepId sPvkey outputFormat
runGovernanceDRepCmd (DRepMetadataHash drepMetadataFile mOutFile) = runDRepMetadataHash drepMetadataFile mOutFile

--
-- DRep command implementations
--

-- | Create a drep registration cert.
-- TODO: Metadata and more drep relay support to be
-- added in the future.
runDRepRegistrationCert
  :: NetworkId
  -- ^ Network ID.
  -> VerificationKeyOrFile DRepKey
  -- ^ DRep verification key.
  -> Maybe DRepMetadataReference
  -- ^ DRep metadata.
  -> File Certificate Out
  -> ExceptT ShelleyGovernanceDRepCmdError IO ()
runDRepRegistrationCert
  _network
  _drepVerKeyOrFile
  _mbMetadata
  _outfp = error "Not implemented"

runDRepRetirementCert
  :: VerificationKeyOrFile DRepKey
  -> Shelley.EpochNo
  -> File Certificate Out
  -> ExceptT ShelleyGovernanceDRepCmdError IO ()
runDRepRetirementCert _drepVerKeyOrFile _retireEpoch _outfp = error "Not implemented"

runDRepId
  :: VerificationKeyOrFile DRepKey
  -> PoolIdOutputFormat
  -> ExceptT ShelleyGovernanceDRepCmdError IO ()
runDRepId verKeyOrFile outputFormat = do
    drepVerKey <- firstExceptT ShelleyDRepCmdReadKeyFileError
      . newExceptT
      $ readVerificationKeyOrFile AsDRepKey verKeyOrFile
    liftIO $
      case outputFormat of
        PoolIdOutputFormatHex ->
          BS.putStrLn $ serialiseToRawBytesHex (verificationKeyHash drepVerKey)
        PoolIdOutputFormatBech32 ->
          Text.putStrLn $ serialiseToBech32 (verificationKeyHash drepVerKey)

runDRepMetadataHash :: File DRepMetadata In -> Maybe (File () Out) -> ExceptT ShelleyGovernanceDRepCmdError IO ()
runDRepMetadataHash drepMDPath mOutFile = do
  metadataBytes <- lift (readByteStringFile drepMDPath)
    & onLeft (left . ShelleyDRepCmdReadFileError)

  (_metadata, metadataHash) <-
      firstExceptT ShelleyDRepCmdMetadataValidationError
    . hoistEither
    $ validateAndHashDRepMetadata metadataBytes
  case mOutFile of
    Nothing -> liftIO $ BS.putStrLn (serialiseToRawBytesHex metadataHash)
    Just (File fpath) ->
      handleIOExceptT (ShelleyDRepCmdWriteFileError . FileIOError fpath)
        $ BS.writeFile fpath (serialiseToRawBytesHex metadataHash)
