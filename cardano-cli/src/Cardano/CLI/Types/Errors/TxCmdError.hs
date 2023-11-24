{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.Types.Errors.TxCmdError
  ( TxCmdError(..)
  , renderTxCmdError
  ) where

import           Cardano.Api
import           Cardano.Api.Pretty
import           Cardano.Api.Shelley

import           Cardano.CLI.Read
import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Errors.BootstrapWitnessError
import           Cardano.CLI.Types.Errors.NodeEraMismatchError
import qualified Cardano.CLI.Types.Errors.NodeEraMismatchError as NEM
import           Cardano.CLI.Types.Errors.ProtocolParamsError
import           Cardano.CLI.Types.Errors.TxValidationError
import           Cardano.CLI.Types.Output
import           Cardano.CLI.Types.TxFeature
import qualified Cardano.Prelude as List
import           Ouroboros.Consensus.Cardano.Block (EraMismatch (..))

import           Data.Text (Text)

{- HLINT ignore "Use let" -}

data TxCmdError
  = TxCmdMetadataError MetadataError
  | TxCmdVoteError VoteError
  | TxCmdConstitutionError ConstitutionError
  | TxCmdScriptWitnessError ScriptWitnessError
  | TxCmdProtocolParamsError ProtocolParamsError
  | TxCmdScriptFileError (FileError ScriptDecodeError)
  | TxCmdReadTextViewFileError !(FileError TextEnvelopeError)
  | TxCmdReadWitnessSigningDataError !ReadWitnessSigningDataError
  | TxCmdWriteFileError !(FileError ())
  | TxCmdBootstrapWitnessError !BootstrapWitnessError
  | TxCmdTxSubmitError !Text
  | TxCmdTxSubmitErrorEraMismatch !EraMismatch
  | TxCmdTxFeatureMismatch !AnyCardanoEra !TxFeature
  | TxCmdTxBodyError !TxBodyError
  | TxCmdNotImplemented !Text
  | TxCmdWitnessEraMismatch !AnyCardanoEra !AnyCardanoEra !WitnessFile
  | TxCmdPolicyIdsMissing ![PolicyId]
  | TxCmdPolicyIdsExcess  ![PolicyId]
  | TxCmdByronEra
  | TxCmdBalanceTxBody !TxBodyErrorAutoBalance
  | TxCmdTxInsDoNotExist !TxInsExistError
  | TxCmdPParamsErr !ProtocolParametersError
  | TxCmdTextEnvCddlError
      !(FileError TextEnvelopeError)
      !(FileError TextEnvelopeCddlError)
  | TxCmdTxExecUnitsErr !TransactionValidityError
  | TxCmdPlutusScriptCostErr !PlutusScriptCostError
  | TxCmdPParamExecutionUnitsNotAvailable
  | TxCmdPlutusScriptsRequireCardanoMode
  | TxCmdProtocolParametersNotPresentInTxBody
  | TxCmdTxNodeEraMismatchError !NodeEraMismatchError
  | TxCmdQueryConvenienceError !QueryConvenienceError
  | TxCmdQueryNotScriptLocked !ScriptLockedTxInsError
  | TxCmdScriptDataError !ScriptDataError
  | TxCmdCddlError CddlError
  | TxCmdCddlWitnessError CddlWitnessError
  | TxCmdRequiredSignerError RequiredSignerError
  -- Validation errors
  | TxCmdAuxScriptsValidationError TxAuxScriptsValidationError
  | TxCmdTotalCollateralValidationError TxTotalCollateralValidationError
  | TxCmdReturnCollateralValidationError TxReturnCollateralValidationError
  | TxCmdTxFeeValidationError TxFeeValidationError
  | TxCmdTxValidityLowerBoundValidationError TxValidityLowerBoundValidationError
  | TxCmdTxValidityUpperBoundValidationError TxValidityUpperBoundValidationError
  | TxCmdRequiredSignersValidationError TxRequiredSignersValidationError
  | TxCmdProtocolParametersValidationError TxProtocolParametersValidationError
  | TxCmdTxWithdrawalsValidationError TxWithdrawalsValidationError
  | TxCmdTxCertificatesValidationError TxCertificatesValidationError
  | TxCmdTxUpdateProposalValidationError TxUpdateProposalValidationError
  | TxCmdScriptValidityValidationError TxScriptValidityValidationError
  | TxCmdProtocolParamsConverstionError ProtocolParametersConversionError

renderTxCmdError :: TxCmdError -> Doc ann
renderTxCmdError = \case
  TxCmdProtocolParamsConverstionError err' ->
    "Error while converting protocol parameters: " <> prettyError err'
  TxCmdVoteError voteErr ->
    pshow voteErr
  TxCmdConstitutionError constErr ->
    pshow constErr
  TxCmdReadTextViewFileError fileErr ->
    prettyError fileErr
  TxCmdScriptFileError fileErr ->
    prettyError fileErr
  TxCmdReadWitnessSigningDataError witSignDataErr ->
    renderReadWitnessSigningDataError witSignDataErr
  TxCmdWriteFileError fileErr ->
    prettyError fileErr
  TxCmdTxSubmitError res ->
    "Error while submitting tx: " <> pretty res
  TxCmdTxSubmitErrorEraMismatch EraMismatch{ledgerEraName, otherEraName} ->
    "The era of the node and the tx do not match. " <>
    "The node is running in the " <> pretty ledgerEraName <>
    " era, but the transaction is for the " <> pretty otherEraName <> " era."
  TxCmdBootstrapWitnessError sbwErr ->
    renderBootstrapWitnessError sbwErr
  TxCmdTxFeatureMismatch era TxFeatureImplicitFees ->
    "An explicit transaction fee must be specified for " <>
    pretty era <> " era transactions."

  TxCmdTxFeatureMismatch (AnyCardanoEra ShelleyEra) TxFeatureValidityNoUpperBound ->
    "A TTL must be specified for Shelley era transactions."

  TxCmdTxFeatureMismatch era feature ->
    pretty (renderFeature feature) <> " cannot be used for " <> pretty era <>
    " era transactions."

  TxCmdTxBodyError err' ->
    "Transaction validaton error: " <> prettyError err'

  TxCmdNotImplemented msg ->
    "Feature not yet implemented: " <> pretty msg

  TxCmdWitnessEraMismatch era era' (WitnessFile file) ->
    "The era of a witness does not match the era of the transaction. " <>
    "The transaction is for the " <> pretty era <> " era, but the " <>
    "witness in " <> pshow file <> " is for the " <> pretty era' <> " era."

  TxCmdPolicyIdsMissing policyids ->
    mconcat
    [ "The \"--mint\" flag specifies an asset with a policy Id, but no "
    , "corresponding monetary policy script has been provided as a witness "
    , "(via the \"--mint-script-file\" flag). The policy Id in question is: "
    , mconcat $ List.intersperse ", " (map (pretty . serialiseToRawBytesHexText) policyids)
    ]

  TxCmdPolicyIdsExcess policyids ->
    mconcat
    [ "A script provided to witness minting does not correspond to the policy "
    , "id of any asset specified in the \"--mint\" field. The script hash is: "
    , mconcat $ List.intersperse ", " (map (pretty . serialiseToRawBytesHexText) policyids)
    ]
  TxCmdByronEra ->
    "This query cannot be used for the Byron era"
  TxCmdBalanceTxBody err' ->
    prettyError err'
  TxCmdTxInsDoNotExist e ->
    pretty $ renderTxInsExistError e
  TxCmdPParamsErr err' ->
    prettyError err'
  TxCmdTextEnvCddlError textEnvErr cddlErr ->
    mconcat
    [ "Failed to decode neither the cli's serialisation format nor the ledger's "
    , "CDDL serialisation format. TextEnvelope error: " <> prettyError textEnvErr <> "\n"
    , "TextEnvelopeCddl error: " <> prettyError cddlErr
    ]
  TxCmdTxExecUnitsErr err' ->
    prettyError err'
  TxCmdPlutusScriptCostErr err'->
    prettyError err'
  TxCmdPParamExecutionUnitsNotAvailable ->
    mconcat
    [ "Execution units not available in the protocol parameters. This is "
    , "likely due to not being in the Alonzo era"
    ]
  TxCmdTxNodeEraMismatchError (NodeEraMismatchError { NEM.era = valueEra, nodeEra = nodeEra }) ->
    cardanoEraConstraints nodeEra $ cardanoEraConstraints valueEra $ mconcat
      [ "Transactions can only be produced in the same era as the node. Requested era: "
      , pretty valueEra <> ", node era: "
      , pretty nodeEra <> "."
      ]
  TxCmdQueryConvenienceError e ->
    pretty $ renderQueryConvenienceError e
  TxCmdQueryNotScriptLocked e ->
    pretty $ renderNotScriptLockedTxInsError e
  TxCmdPlutusScriptsRequireCardanoMode ->
    "Plutus scripts are only available in CardanoMode"
  TxCmdProtocolParametersNotPresentInTxBody ->
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
    prettyError e
  TxCmdCddlWitnessError e ->
    prettyError e
  TxCmdRequiredSignerError e ->
    prettyError e
  -- Validation errors
  TxCmdAuxScriptsValidationError e ->
    prettyError e
  TxCmdTotalCollateralValidationError e ->
    prettyError e
  TxCmdReturnCollateralValidationError e ->
    prettyError e
  TxCmdTxFeeValidationError e ->
    prettyError e
  TxCmdTxValidityLowerBoundValidationError e ->
    prettyError e
  TxCmdTxValidityUpperBoundValidationError e ->
    prettyError e
  TxCmdRequiredSignersValidationError e ->
    prettyError e
  TxCmdProtocolParametersValidationError e ->
    prettyError e
  TxCmdTxWithdrawalsValidationError e ->
    prettyError e
  TxCmdTxCertificatesValidationError e ->
    prettyError e
  TxCmdTxUpdateProposalValidationError e ->
    prettyError e
  TxCmdScriptValidityValidationError e ->
    prettyError e
