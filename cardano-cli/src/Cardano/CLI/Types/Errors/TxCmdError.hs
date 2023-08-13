{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Cardano.CLI.Types.Errors.TxCmdError
  ( TxCmdError(..)
  , renderTxCmdError
  ) where

import           Cardano.Api
import           Cardano.Api.Shelley

import           Cardano.CLI.Read
import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Errors.BootstrapWitnessError
import           Cardano.CLI.Types.Errors.ProtocolParamsError
import           Cardano.CLI.Types.Output
import           Cardano.CLI.Types.TxFeature.Core
import           Cardano.CLI.Validation
import           Ouroboros.Consensus.Cardano.Block (EraMismatch (..))

import           Data.Text (Text)
import qualified Data.Text as Text

{- HLINT ignore "Use let" -}

data TxCmdError
  = TxCmdMetadataError
      MetadataError
  | TxCmdVoteError
      VoteError
  | TxCmdConstitutionError
      ConstitutionError
  | TxCmdScriptWitnessError
      ScriptWitnessError
  | TxCmdProtocolParamsError
      ProtocolParamsError
  | TxCmdScriptFileError
      (FileError ScriptDecodeError)
  | TxCmdReadTextViewFileError
      !(FileError TextEnvelopeError)
  | TxCmdReadWitnessSigningDataError
      !ReadWitnessSigningDataError
  | TxCmdWriteFileError
      !(FileError ())
  | TxCmdEraConsensusModeMismatchError
      !(Maybe FilePath)
      !AnyConsensusMode
      !AnyCardanoEra
      -- ^ Era
  | TxCmdBootstrapWitnessError
      !BootstrapWitnessError
  | TxCmdTxSubmitError
      !Text
  | TxCmdTxSubmitErrorEraMismatchError
      !EraMismatch
  | TxCmdTxFeatureMismatchError
      !AnyCardanoEra !TxFeature
  | TxCmdTxBodyError
      !TxBodyError
  | TxCmdNotImplementedError
      !Text
  | TxCmdWitnessEraMismatchError
      !AnyCardanoEra !AnyCardanoEra !WitnessFile
  | TxCmdPolicyIdsMissingError
      ![PolicyId]
  | TxCmdPolicyIdsExcessError
       ![PolicyId]
  | TxCmdUnsupportedModeError
      !AnyConsensusMode
  | TxCmdByronEraInvalidError
  | TxCmdEraConsensusModeMismatchTxBalanceError
      !TxBuildOutputOptions
      !AnyConsensusMode
      !AnyCardanoEra
  | TxCmdBalanceTxBodyError
      !TxBodyErrorAutoBalance
  | TxCmdTxInsDoNotExistError
      !TxInsExistError
  | TxCmdPParamsError
      !ProtocolParametersError
  | TxCmdTextEnvCddlError
      !(FileError TextEnvelopeError)
      !(FileError TextEnvelopeCddlError)
  | TxCmdTxExecUnitsError
      !TransactionValidityError
  | TxCmdPlutusScriptCostError
      !PlutusScriptCostError
  | TxCmdPParamExecutionUnitsNotAvailableError
  | TxCmdPlutusScriptsRequireCardanoModeError
  | TxCmdProtocolParametersNotPresentInTxBodyError
  | TxCmdTxEraCastError
      EraCastError
  | TxCmdQueryConvenienceError
      !QueryConvenienceError
  | TxCmdQueryNotScriptLockedError
      !ScriptLockedTxInsError
  | TxCmdScriptDataError
      !ScriptDataError
  | TxCmdCddlError
      CddlError
  | TxCmdCddlWitnessError
      CddlWitnessError
  | TxCmdRequiredSignerError
      RequiredSignerError
  -- Validation errors
  | TxCmdAuxScriptsValidationError
      TxAuxScriptsValidationError
  | TxCmdTotalCollateralValidationError
      TxTotalCollateralValidationError
  | TxCmdReturnCollateralValidationError
      TxReturnCollateralValidationError
  | TxCmdTxFeeValidationError
      TxFeeValidationError
  | TxCmdTxValidityLowerBoundValidationError
      TxValidityLowerBoundValidationError
  | TxCmdTxValidityUpperBoundValidationError
      TxValidityUpperBoundValidationError
  | TxCmdRequiredSignersValidationError
      TxRequiredSignersValidationError
  | TxCmdProtocolParametersValidationError
      TxProtocolParametersValidationError
  | TxCmdTxWithdrawalsValidationError
      TxWithdrawalsValidationError
  | TxCmdTxCertificatesValidationError
      TxCertificatesValidationError
  | TxCmdTxUpdateProposalValidationError
      TxUpdateProposalValidationError
  | TxCmdScriptValidityValidationError
      TxScriptValidityValidationError

renderTxCmdError :: TxCmdError ->
  Text
renderTxCmdError = \case
  TxCmdVoteError voteErr ->
    Text.pack $ show voteErr
  TxCmdConstitutionError constErr ->
    Text.pack $ show constErr
  TxCmdReadTextViewFileError fileErr ->
    Text.pack (displayError fileErr)
  TxCmdScriptFileError fileErr ->
    Text.pack (displayError fileErr)
  TxCmdReadWitnessSigningDataError witSignDataErr ->
    renderReadWitnessSigningDataError witSignDataErr
  TxCmdWriteFileError fileErr ->
    Text.pack (displayError fileErr)
  TxCmdTxSubmitError res ->
    mconcat
      [ "Error while submitting tx: "
      , res
      ]
  TxCmdTxSubmitErrorEraMismatchError EraMismatch{ledgerEraName, otherEraName} ->
    mconcat
      [ "The era of the node and the tx do not match. "
      , "The node is running in the " <> ledgerEraName
      , " era, but the transaction is for the " <> otherEraName <> " era."
      ]
  TxCmdBootstrapWitnessError sbwErr ->
    renderBootstrapWitnessError sbwErr
  TxCmdTxFeatureMismatchError era TxFeatureImplicitFees ->
    mconcat
      [ "An explicit transaction fee must be specified for "
      , renderEra era <> " era transactions."
      ]

  TxCmdTxFeatureMismatchError (AnyCardanoEra ShelleyEra) TxFeatureValidityNoUpperBound ->
    "A TTL must be specified for Shelley era transactions."

  TxCmdTxFeatureMismatchError era feature ->
    mconcat
      [ renderFeature feature <> " cannot be used for " <> renderEra era
      , " era transactions."
      ]


  TxCmdTxBodyError err' ->
    mconcat
      [ "Transaction validaton error: " <> Text.pack (displayError err')
      ]

  TxCmdNotImplementedError msg ->
    "Feature not yet implemented: " <> msg

  TxCmdWitnessEraMismatchError era era' (WitnessFile file) ->
    mconcat
      [ "The era of a witness does not match the era of the transaction. "
      , "The transaction is for the " <> renderEra era <> " era, but the "
      , "witness in " <> textShow file <> " is for the " <> renderEra era' <> " era."
      ]

  TxCmdEraConsensusModeMismatchError fp mode era ->
    mconcat
      [ "Submitting " <> renderEra era <> " era transaction (" <> textShow fp
      , ") is not supported in the " <> renderMode mode <> " consensus mode."
      ]
  TxCmdPolicyIdsMissingError policyids ->
    mconcat
      [ "The \"--mint\" flag specifies an asset with a policy Id, but no "
      , "corresponding monetary policy script has been provided as a witness "
      , "(via the \"--mint-script-file\" flag). The policy Id in question is: "
      , Text.intercalate ", " (map serialiseToRawBytesHexText policyids)
      ]

  TxCmdPolicyIdsExcessError policyids ->
    mconcat
      [ "A script provided to witness minting does not correspond to the policy "
      , "id of any asset specified in the \"--mint\" field. The script hash is: "
      , Text.intercalate ", " (map serialiseToRawBytesHexText policyids)
      ]
  TxCmdUnsupportedModeError mode ->
    "Unsupported mode: " <> renderMode mode
  TxCmdByronEraInvalidError ->
    "This query cannot be used for the Byron era"
  TxCmdEraConsensusModeMismatchTxBalanceError fp mode era ->
    mconcat
      [ "Cannot balance " <> renderEra era <> " era transaction body (" <> textShow fp
      , ") because is not supported in the " <> renderMode mode <> " consensus mode."
      ]
  TxCmdBalanceTxBodyError err' ->
    Text.pack $ displayError err'
  TxCmdTxInsDoNotExistError e ->
    renderTxInsExistError e
  TxCmdPParamsError err' ->
    Text.pack $ displayError err'
  TxCmdTextEnvCddlError textEnvErr cddlErr ->
    mconcat
      [ "Failed to decode neither the cli's serialisation format nor the ledger's "
      , "CDDL serialisation format. TextEnvelope error: " <> Text.pack (displayError textEnvErr) <> "\n"
      , "TextEnvelopeCddl error: " <> Text.pack (displayError cddlErr)
      ]
  TxCmdTxExecUnitsError err' ->
     Text.pack $ displayError err'
  TxCmdPlutusScriptCostError err'-> Text.pack $ displayError err'
  TxCmdPParamExecutionUnitsNotAvailableError ->
    mconcat
      [ "Execution units not available in the protocol parameters. This is "
      , "likely due to not being in the Alonzo era"
      ]
  TxCmdTxEraCastError (EraCastError value fromEra toEra) ->
    mconcat
      [ "Unable to cast era from " <> textShow fromEra <> " to "
      , textShow toEra <> " the value " <> textShow value
      ]
  TxCmdQueryConvenienceError e ->
    renderQueryConvenienceError e
  TxCmdQueryNotScriptLockedError e ->
    renderNotScriptLockedTxInsError e
  TxCmdPlutusScriptsRequireCardanoModeError ->
    "Plutus scripts are only available in CardanoMode"
  TxCmdProtocolParametersNotPresentInTxBodyError ->
    "Protocol parameters were not found in transaction body"
  TxCmdMetadataError e ->
    renderMetadataError e
  TxCmdScriptWitnessError e ->
    renderScriptWitnessError e
  TxCmdScriptDataError e ->
    renderScriptDataError e
  TxCmdProtocolParamsError e ->
    renderProtocolParamsError e
  TxCmdCddlError e ->
    Text.pack $ displayError e
  TxCmdCddlWitnessError e ->
    Text.pack $ displayError e
  TxCmdRequiredSignerError e ->
    Text.pack $ displayError e
  -- Validation errors
  TxCmdAuxScriptsValidationError e ->
    Text.pack $ displayError e
  TxCmdTotalCollateralValidationError e ->
    Text.pack $ displayError e
  TxCmdReturnCollateralValidationError e ->
    Text.pack $ displayError e
  TxCmdTxFeeValidationError e ->
    Text.pack $ displayError e
  TxCmdTxValidityLowerBoundValidationError e ->
    Text.pack $ displayError e
  TxCmdTxValidityUpperBoundValidationError e ->
    Text.pack $ displayError e
  TxCmdRequiredSignersValidationError e ->
    Text.pack $ displayError e
  TxCmdProtocolParametersValidationError e ->
    Text.pack $ displayError e
  TxCmdTxWithdrawalsValidationError e ->
    Text.pack $ displayError e
  TxCmdTxCertificatesValidationError e ->
    Text.pack $ displayError e
  TxCmdTxUpdateProposalValidationError e ->
    Text.pack $ displayError e
  TxCmdScriptValidityValidationError e ->
    Text.pack $ displayError e
