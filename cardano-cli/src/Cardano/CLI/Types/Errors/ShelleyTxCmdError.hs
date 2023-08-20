{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.Types.Errors.ShelleyTxCmdError
  ( ShelleyTxCmdError(..)
  , renderShelleyTxCmdError
  ) where

import           Cardano.Api
import           Cardano.Api.Shelley

import           Cardano.CLI.Legacy.Run.Genesis
import           Cardano.CLI.Legacy.Run.Validate
import           Cardano.CLI.Read
import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Errors.ShelleyBootstrapWitnessError
import           Cardano.CLI.Types.Output
import           Cardano.CLI.Types.TxFeature
import           Ouroboros.Consensus.Cardano.Block (EraMismatch (..))

import           Data.Text (Text)
import qualified Data.Text as Text

{- HLINT ignore "Use let" -}

data ShelleyTxCmdError
  = ShelleyTxCmdMetadataError MetadataError
  | ShelleyTxCmdVoteError VoteError
  | ShelleyTxCmdConstitutionError ConstitutionError
  | ShelleyTxCmdScriptWitnessError ScriptWitnessError
  | ShelleyTxCmdProtocolParamsError ProtocolParamsError
  | ShelleyTxCmdScriptFileError (FileError ScriptDecodeError)
  | ShelleyTxCmdReadTextViewFileError !(FileError TextEnvelopeError)
  | ShelleyTxCmdReadWitnessSigningDataError !ReadWitnessSigningDataError
  | ShelleyTxCmdWriteFileError !(FileError ())
  | ShelleyTxCmdEraConsensusModeMismatch
      !(Maybe FilePath)
      !AnyConsensusMode
      !AnyCardanoEra
      -- ^ Era
  | ShelleyTxCmdBootstrapWitnessError !ShelleyBootstrapWitnessError
  | ShelleyTxCmdTxSubmitError !Text
  | ShelleyTxCmdTxSubmitErrorEraMismatch !EraMismatch
  | ShelleyTxCmdTxFeatureMismatch !AnyCardanoEra !TxFeature
  | ShelleyTxCmdTxBodyError !TxBodyError
  | ShelleyTxCmdNotImplemented !Text
  | ShelleyTxCmdWitnessEraMismatch !AnyCardanoEra !AnyCardanoEra !WitnessFile
  | ShelleyTxCmdPolicyIdsMissing ![PolicyId]
  | ShelleyTxCmdPolicyIdsExcess  ![PolicyId]
  | ShelleyTxCmdUnsupportedMode !AnyConsensusMode
  | ShelleyTxCmdByronEra
  | ShelleyTxCmdEraConsensusModeMismatchTxBalance
      !TxBuildOutputOptions
      !AnyConsensusMode
      !AnyCardanoEra
  | ShelleyTxCmdBalanceTxBody !TxBodyErrorAutoBalance
  | ShelleyTxCmdTxInsDoNotExist !TxInsExistError
  | ShelleyTxCmdPParamsErr !ProtocolParametersError
  | ShelleyTxCmdTextEnvCddlError
      !(FileError TextEnvelopeError)
      !(FileError TextEnvelopeCddlError)
  | ShelleyTxCmdTxExecUnitsErr !TransactionValidityError
  | ShelleyTxCmdPlutusScriptCostErr !PlutusScriptCostError
  | ShelleyTxCmdPParamExecutionUnitsNotAvailable
  | ShelleyTxCmdPlutusScriptsRequireCardanoMode
  | ShelleyTxCmdProtocolParametersNotPresentInTxBody
  | ShelleyTxCmdTxEraCastErr EraCastError
  | ShelleyTxCmdQueryConvenienceError !QueryConvenienceError
  | ShelleyTxCmdQueryNotScriptLocked !ScriptLockedTxInsError
  | ShelleyTxCmdScriptDataError !ScriptDataError
  | ShelleyTxCmdCddlError CddlError
  | ShelleyTxCmdCddlWitnessError CddlWitnessError
  | ShelleyTxCmdRequiredSignerError RequiredSignerError
  -- Validation errors
  | ShelleyTxCmdAuxScriptsValidationError TxAuxScriptsValidationError
  | ShelleyTxCmdTotalCollateralValidationError TxTotalCollateralValidationError
  | ShelleyTxCmdReturnCollateralValidationError TxReturnCollateralValidationError
  | ShelleyTxCmdTxFeeValidationError TxFeeValidationError
  | ShelleyTxCmdTxValidityLowerBoundValidationError TxValidityLowerBoundValidationError
  | ShelleyTxCmdTxValidityUpperBoundValidationError TxValidityUpperBoundValidationError
  | ShelleyTxCmdRequiredSignersValidationError TxRequiredSignersValidationError
  | ShelleyTxCmdProtocolParametersValidationError TxProtocolParametersValidationError
  | ShelleyTxCmdTxWithdrawalsValidationError TxWithdrawalsValidationError
  | ShelleyTxCmdTxCertificatesValidationError TxCertificatesValidationError
  | ShelleyTxCmdTxUpdateProposalValidationError TxUpdateProposalValidationError
  | ShelleyTxCmdScriptValidityValidationError TxScriptValidityValidationError
  | ShelleyTxCmdProtocolParamsConverstionError ProtocolParametersConversionError

renderShelleyTxCmdError :: ShelleyTxCmdError -> Text
renderShelleyTxCmdError err =
  case err of
    ShelleyTxCmdProtocolParamsConverstionError err' ->
      "Error while converting protocol parameters: " <> Text.pack (displayError err')
    ShelleyTxCmdVoteError voteErr -> Text.pack $ show voteErr
    ShelleyTxCmdConstitutionError constErr -> Text.pack $ show constErr
    ShelleyTxCmdReadTextViewFileError fileErr -> Text.pack (displayError fileErr)
    ShelleyTxCmdScriptFileError fileErr -> Text.pack (displayError fileErr)
    ShelleyTxCmdReadWitnessSigningDataError witSignDataErr ->
      renderReadWitnessSigningDataError witSignDataErr
    ShelleyTxCmdWriteFileError fileErr -> Text.pack (displayError fileErr)
    ShelleyTxCmdTxSubmitError res -> "Error while submitting tx: " <> res
    ShelleyTxCmdTxSubmitErrorEraMismatch EraMismatch{ledgerEraName, otherEraName} ->
      "The era of the node and the tx do not match. " <>
      "The node is running in the " <> ledgerEraName <>
      " era, but the transaction is for the " <> otherEraName <> " era."
    ShelleyTxCmdBootstrapWitnessError sbwErr ->
      renderShelleyBootstrapWitnessError sbwErr
    ShelleyTxCmdTxFeatureMismatch era TxFeatureImplicitFees ->
      "An explicit transaction fee must be specified for " <>
      renderEra era <> " era transactions."

    ShelleyTxCmdTxFeatureMismatch (AnyCardanoEra ShelleyEra)
                                  TxFeatureValidityNoUpperBound ->
      "A TTL must be specified for Shelley era transactions."

    ShelleyTxCmdTxFeatureMismatch era feature ->
      renderFeature feature <> " cannot be used for " <> renderEra era <>
      " era transactions."

    ShelleyTxCmdTxBodyError err' ->
      "Transaction validaton error: " <> Text.pack (displayError err')

    ShelleyTxCmdNotImplemented msg ->
      "Feature not yet implemented: " <> msg

    ShelleyTxCmdWitnessEraMismatch era era' (WitnessFile file) ->
      "The era of a witness does not match the era of the transaction. " <>
      "The transaction is for the " <> renderEra era <> " era, but the " <>
      "witness in " <> textShow file <> " is for the " <> renderEra era' <> " era."

    ShelleyTxCmdEraConsensusModeMismatch fp mode era ->
       "Submitting " <> renderEra era <> " era transaction (" <> textShow fp <>
       ") is not supported in the " <> renderMode mode <> " consensus mode."
    ShelleyTxCmdPolicyIdsMissing policyids -> mconcat
      [ "The \"--mint\" flag specifies an asset with a policy Id, but no "
      , "corresponding monetary policy script has been provided as a witness "
      , "(via the \"--mint-script-file\" flag). The policy Id in question is: "
      , Text.intercalate ", " (map serialiseToRawBytesHexText policyids)
      ]

    ShelleyTxCmdPolicyIdsExcess policyids -> mconcat
      [ "A script provided to witness minting does not correspond to the policy "
      , "id of any asset specified in the \"--mint\" field. The script hash is: "
      , Text.intercalate ", " (map serialiseToRawBytesHexText policyids)
      ]
    ShelleyTxCmdUnsupportedMode mode -> "Unsupported mode: " <> renderMode mode
    ShelleyTxCmdByronEra -> "This query cannot be used for the Byron era"
    ShelleyTxCmdEraConsensusModeMismatchTxBalance fp mode era ->
       "Cannot balance " <> renderEra era <> " era transaction body (" <> textShow fp <>
       ") because is not supported in the " <> renderMode mode <> " consensus mode."
    ShelleyTxCmdBalanceTxBody err' -> Text.pack $ displayError err'
    ShelleyTxCmdTxInsDoNotExist e ->
      renderTxInsExistError e
    ShelleyTxCmdPParamsErr err' -> Text.pack $ displayError err'
    ShelleyTxCmdTextEnvCddlError textEnvErr cddlErr -> mconcat
      [ "Failed to decode neither the cli's serialisation format nor the ledger's "
      , "CDDL serialisation format. TextEnvelope error: " <> Text.pack (displayError textEnvErr) <> "\n"
      , "TextEnvelopeCddl error: " <> Text.pack (displayError cddlErr)
      ]
    ShelleyTxCmdTxExecUnitsErr err' ->  Text.pack $ displayError err'
    ShelleyTxCmdPlutusScriptCostErr err'-> Text.pack $ displayError err'
    ShelleyTxCmdPParamExecutionUnitsNotAvailable -> mconcat
      [ "Execution units not available in the protocol parameters. This is "
      , "likely due to not being in the Alonzo era"
      ]
    ShelleyTxCmdTxEraCastErr (EraCastError value fromEra toEra) ->
      "Unable to cast era from " <> textShow fromEra <> " to " <> textShow toEra <> " the value " <> textShow value
    ShelleyTxCmdQueryConvenienceError e ->
      renderQueryConvenienceError e
    ShelleyTxCmdQueryNotScriptLocked e ->
      renderNotScriptLockedTxInsError e
    ShelleyTxCmdPlutusScriptsRequireCardanoMode ->
      "Plutus scripts are only available in CardanoMode"
    ShelleyTxCmdProtocolParametersNotPresentInTxBody ->
      "Protocol parameters were not found in transaction body"
    ShelleyTxCmdMetadataError e -> renderMetadataError e
    ShelleyTxCmdScriptWitnessError e -> renderScriptWitnessError e
    ShelleyTxCmdScriptDataError e -> renderScriptDataError e
    ShelleyTxCmdProtocolParamsError e -> renderProtocolParamsError e
    ShelleyTxCmdCddlError e -> Text.pack $ displayError e
    ShelleyTxCmdCddlWitnessError e -> Text.pack $ displayError e
    ShelleyTxCmdRequiredSignerError e -> Text.pack $ displayError e
    -- Validation errors
    ShelleyTxCmdAuxScriptsValidationError e ->
      Text.pack $ displayError e
    ShelleyTxCmdTotalCollateralValidationError e ->
      Text.pack $ displayError e
    ShelleyTxCmdReturnCollateralValidationError e ->
      Text.pack $ displayError e
    ShelleyTxCmdTxFeeValidationError e ->
      Text.pack $ displayError e
    ShelleyTxCmdTxValidityLowerBoundValidationError e ->
      Text.pack $ displayError e
    ShelleyTxCmdTxValidityUpperBoundValidationError e ->
      Text.pack $ displayError e
    ShelleyTxCmdRequiredSignersValidationError e ->
      Text.pack $ displayError e
    ShelleyTxCmdProtocolParametersValidationError e ->
      Text.pack $ displayError e
    ShelleyTxCmdTxWithdrawalsValidationError e ->
      Text.pack $ displayError e
    ShelleyTxCmdTxCertificatesValidationError e ->
      Text.pack $ displayError e
    ShelleyTxCmdTxUpdateProposalValidationError e ->
      Text.pack $ displayError e
    ShelleyTxCmdScriptValidityValidationError e ->
      Text.pack $ displayError e
