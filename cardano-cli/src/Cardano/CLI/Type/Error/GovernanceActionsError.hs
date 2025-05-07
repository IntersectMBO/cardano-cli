{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Type.Error.GovernanceActionsError
  ( GovernanceActionsError (..)
  , AnchorDataTypeCheck (..)
  )
where

import Cardano.Api
import Cardano.Api.Ledger qualified as L

import Cardano.CLI.Read
import Cardano.CLI.Type.Error.HashCmdError (FetchURLError)

import Control.Exception (displayException)

data GovernanceActionsError
  = GovernanceActionsCmdCostModelsError CostModelsError
  | GovernanceActionsCmdWriteFileError (FileError ())
  | GovernanceActionsValueUpdateProtocolParametersNotFound AnyShelleyBasedEra
  | GovernanceActionsMismatchedHashError
      AnchorDataTypeCheck
      -- ^ Type of anchor data that we were checking
      !(L.SafeHash L.AnchorData)
      -- ^ Expected hash
      !(L.SafeHash L.AnchorData)
      -- ^ Actual hash
  | GovernanceActionsProposalFetchURLError
      AnchorDataTypeCheck
      -- ^ Type of anchor data that we were checking
      FetchURLError
      -- ^ Error that occurred while fetching the anchor data
  deriving Show

instance Error GovernanceActionsError where
  prettyError = \case
    GovernanceActionsCmdCostModelsError e ->
      prettyError e
    GovernanceActionsCmdWriteFileError e ->
      "Cannot write file: " <> prettyError e
    GovernanceActionsValueUpdateProtocolParametersNotFound (AnyShelleyBasedEra expectedShelleyEra) ->
      "Protocol parameters update value for" <+> pretty expectedShelleyEra <+> "was not found."
    GovernanceActionsMismatchedHashError adt expectedHash actualHash ->
      "Hashes do not match while checking"
        <+> pretty (anchorDataTypeCheckName adt)
        <+> "hashes!"
        <> "\nExpected:"
          <+> pretty (show (L.extractHash expectedHash))
        <> "\n  Actual:"
          <+> pretty (show (L.extractHash actualHash))
    GovernanceActionsProposalFetchURLError adt fetchErr ->
      "Error while checking"
        <+> pretty (anchorDataTypeCheckName adt)
        <+> "hash:"
        <+> pretty (displayException fetchErr)

data AnchorDataTypeCheck
  = ProposalCheck
  | ConstitutionCheck
  deriving Show

anchorDataTypeCheckName :: AnchorDataTypeCheck -> String
anchorDataTypeCheckName ProposalCheck = "proposal"
anchorDataTypeCheckName ConstitutionCheck = "constitution"
