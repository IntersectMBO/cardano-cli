{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.Types.Errors.TxCmdError
  ( TxCmdError (..)
  , AnyTxBodyErrorAutoBalance (..)
  , AnyTxCmdTxExecUnitsErr (..)
  , renderTxCmdError
  )
where

import           Cardano.Api
import qualified Cardano.Api.Ledger as L
import           Cardano.Api.Shelley

import           Cardano.CLI.Read
import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Errors.BootstrapWitnessError
import           Cardano.CLI.Types.Errors.HashCmdError (HashCheckError)
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

data AnyTxCmdTxExecUnitsErr where
  AnyTxCmdTxExecUnitsErr :: TransactionValidityError era -> AnyTxCmdTxExecUnitsErr

data AnyTxBodyErrorAutoBalance where
  AnyTxBodyErrorAutoBalance :: TxBodyErrorAutoBalance era -> AnyTxBodyErrorAutoBalance

data TxCmdError
  = TxCmdMetadataError MetadataError
  | TxCmdVoteError VoteError
  | TxCmdConstitutionError ConstitutionError
  | TxCmdProposalError ProposalError
  | TxCmdScriptWitnessError ScriptWitnessError
  | TxCmdProtocolParamsError ProtocolParamsError
  | TxCmdScriptFileError (FileError ScriptDecodeError)
  | TxCmdKeyFileError (FileError InputDecodeError)
  | TxCmdReadTextViewFileError !(FileError TextEnvelopeError)
  | TxCmdReadWitnessSigningDataError !ReadWitnessSigningDataError
  | TxCmdWriteFileError !(FileError ())
  | TxCmdBootstrapWitnessError !BootstrapWitnessError
  | TxCmdTxSubmitError !Text
  | TxCmdTxSubmitErrorEraMismatch !EraMismatch
  | TxCmdTxFeatureMismatch !AnyCardanoEra !TxFeature
  | TxCmdTxBodyError !TxBodyError
  | TxCmdWitnessEraMismatch !AnyCardanoEra !AnyCardanoEra !WitnessFile
  | TxCmdPolicyIdsMissing ![PolicyId] ![PolicyId]
  | -- The first list is the missing policy Ids, the second list is the
    -- policy Ids that were provided in the transaction.
    TxCmdPolicyIdsExcess ![PolicyId]
  | TxCmdByronEra
  | TxCmdBalanceTxBody !AnyTxBodyErrorAutoBalance
  | TxCmdTxInsDoNotExist !TxInsExistError
  | TxCmdPParamsErr !ProtocolParametersError
  | TxCmdTextEnvError !(FileError TextEnvelopeError)
  | TxCmdTextEnvCddlError !(FileError TextEnvelopeCddlError)
  | TxCmdTxExecUnitsErr !AnyTxCmdTxExecUnitsErr
  | TxCmdPlutusScriptCostErr !PlutusScriptCostError
  | TxCmdPParamExecutionUnitsNotAvailable
  | TxCmdPlutusScriptsRequireCardanoMode
  | TxCmdProtocolParametersNotPresentInTxBody
  | TxCmdTxNodeEraMismatchError !NodeEraMismatchError
  | TxCmdQueryConvenienceError !QueryConvenienceError
  | TxCmdQueryNotScriptLocked !ScriptLockedTxInsError
  | TxCmdScriptDataError !ScriptDataError
  | TxCmdCddlWitnessError CddlWitnessError
  | TxCmdRequiredSignerError RequiredSignerError
  | -- Validation errors
    forall era. TxCmdNotSupportedInEraValidationError (TxNotSupportedInEraValidationError era)
  | TxCmdAuxScriptsValidationError TxAuxScriptsValidationError
  | TxCmdProtocolParamsConverstionError ProtocolParametersConversionError
  | forall era. TxCmdTxGovDuplicateVotes (TxGovDuplicateVotes era)
  | forall era. TxCmdFeeEstimationError (TxFeeEstimationError era)
  | TxCmdPoolMetadataHashError AnchorDataFromCertificateError
  | TxCmdHashCheckError L.Url HashCheckError

renderTxCmdError :: TxCmdError -> Doc ann
renderTxCmdError = \case
  TxCmdProtocolParamsConverstionError err' ->
    "Error while converting protocol parameters: " <> prettyError err'
  TxCmdVoteError voteErr ->
    prettyError voteErr
  TxCmdConstitutionError constErr ->
    pshow constErr
  TxCmdProposalError propErr ->
    pshow propErr
  TxCmdReadTextViewFileError fileErr ->
    prettyError fileErr
  TxCmdScriptFileError fileErr ->
    prettyError fileErr
  TxCmdKeyFileError fileErr ->
    prettyError fileErr
  TxCmdReadWitnessSigningDataError witSignDataErr ->
    renderReadWitnessSigningDataError witSignDataErr
  TxCmdWriteFileError fileErr ->
    prettyError fileErr
  TxCmdTxSubmitError res ->
    "Error while submitting tx: " <> pretty res
  TxCmdTxSubmitErrorEraMismatch EraMismatch{ledgerEraName, otherEraName} ->
    "The era of the node and the tx do not match. "
      <> "The node is running in the "
      <> pretty ledgerEraName
      <> " era, but the transaction is for the "
      <> pretty otherEraName
      <> " era."
  TxCmdBootstrapWitnessError sbwErr ->
    renderBootstrapWitnessError sbwErr
  TxCmdTxFeatureMismatch era TxFeatureImplicitFees ->
    "An explicit transaction fee must be specified for "
      <> pretty era
      <> " era transactions."
  TxCmdTxFeatureMismatch (AnyCardanoEra ShelleyEra) TxFeatureValidityNoUpperBound ->
    "A TTL must be specified for Shelley era transactions."
  TxCmdTxFeatureMismatch era feature ->
    pretty (renderFeature feature)
      <> " cannot be used for "
      <> pretty era
      <> " era transactions."
  TxCmdTxBodyError err' ->
    "Transaction validaton error: " <> prettyError err'
  TxCmdWitnessEraMismatch era era' (WitnessFile file) ->
    "The era of a witness does not match the era of the transaction. "
      <> "The transaction is for the "
      <> pretty era
      <> " era, but the "
      <> "witness in "
      <> pshow file
      <> " is for the "
      <> pretty era'
      <> " era."
  TxCmdPolicyIdsMissing missingPolicyIds knownPolicyIds ->
    mconcat $
      [ "The \"--mint\" flag specifies an asset with a policy Id, but no "
      , "corresponding monetary policy script has been provided as a witness "
      , "(via the \"--mint-script-file\" flag). The policy Id in question is: "
      , prettyPolicyIdList missingPolicyIds
      ]
        <> [". Known policy Ids are: " <> prettyPolicyIdList knownPolicyIds | not (null knownPolicyIds)]
  TxCmdPolicyIdsExcess policyids ->
    mconcat
      [ "A script provided to witness minting does not correspond to the policy "
      , "id of any asset specified in the \"--mint\" field. The script hash is: "
      , prettyPolicyIdList policyids
      ]
  TxCmdByronEra ->
    "This query cannot be used for the Byron era"
  TxCmdBalanceTxBody (AnyTxBodyErrorAutoBalance err') ->
    prettyError err'
  TxCmdTxInsDoNotExist e ->
    pretty $ renderTxInsExistError e
  TxCmdPParamsErr err' ->
    prettyError err'
  TxCmdTextEnvError err' ->
    mconcat
      [ "Failed to decode the ledger's CDDL serialisation format. "
      , "File error: " <> prettyError err'
      ]
  TxCmdTextEnvCddlError cddlErr ->
    mconcat
      [ "Failed to decode the ledger's CDDL serialisation format. "
      , "TextEnvelopeCddl error: " <> prettyError cddlErr
      ]
  TxCmdTxExecUnitsErr (AnyTxCmdTxExecUnitsErr err') ->
    prettyError err'
  TxCmdPlutusScriptCostErr err' ->
    prettyError err'
  TxCmdPParamExecutionUnitsNotAvailable ->
    mconcat
      [ "Execution units not available in the protocol parameters. This is "
      , "likely due to not being in the Alonzo era"
      ]
  TxCmdTxNodeEraMismatchError (NodeEraMismatchError{NEM.era = valueEra, nodeEra = nodeEra}) ->
    cardanoEraConstraints nodeEra $
      cardanoEraConstraints valueEra $
        mconcat
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
  TxCmdCddlWitnessError e ->
    prettyError e
  TxCmdRequiredSignerError e ->
    prettyError e
  -- Validation errors
  TxCmdNotSupportedInEraValidationError e ->
    prettyError e
  TxCmdAuxScriptsValidationError e ->
    prettyError e
  TxCmdTxGovDuplicateVotes e ->
    prettyError e
  TxCmdFeeEstimationError e ->
    prettyError e
  TxCmdPoolMetadataHashError e ->
    "Hash of the pool metadata hash is not valid:" <+> prettyError e
  TxCmdHashCheckError url e ->
    "Hash of the file is not valid. Url:" <+> pretty (L.urlToText url) <+> prettyException e

prettyPolicyIdList :: [PolicyId] -> Doc ann
prettyPolicyIdList =
  mconcat . List.intersperse ", " . fmap (pretty . serialiseToRawBytesHexText)
