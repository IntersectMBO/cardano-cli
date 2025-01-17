{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Types.Errors.GovernanceActionsError
  ( GovernanceActionsError (..)
  , AnchorDataTypeCheck (..)
  )
where

import           Cardano.Api

import           Cardano.CLI.Read
import           Cardano.CLI.Types.Errors.HashCmdError (HashCheckError)
import           Cardano.CLI.Types.Errors.StakeCredentialError

data GovernanceActionsError
  = GovernanceActionsCmdConstitutionError ConstitutionError
  | GovernanceActionsCmdProposalError ProposalError
  | GovernanceActionsCmdCostModelsError CostModelsError
  | GovernanceActionsCmdReadFileError (FileError InputDecodeError)
  | GovernanceActionsReadStakeCredErrror StakeCredentialError
  | GovernanceActionsCmdReadTextEnvelopeFileError (FileError TextEnvelopeError)
  | GovernanceActionsCmdWriteFileError (FileError ())
  | GovernanceActionsValueUpdateProtocolParametersNotFound AnyShelleyBasedEra
  | GovernanceActionsHashCheckError
      AnchorDataTypeCheck
      -- ^ Type of anchor data that we were checking
      HashCheckError
      -- ^ The error that occurred while checking the hash
  deriving Show

instance Error GovernanceActionsError where
  prettyError = \case
    GovernanceActionsCmdCostModelsError e ->
      prettyError e
    GovernanceActionsCmdProposalError e ->
      "Cannot read proposal: " <> pshow e -- TODO Conway render this properly
    GovernanceActionsCmdConstitutionError e ->
      "Cannot read constitution: " <> pshow e -- TODO Conway render this properly
    GovernanceActionsCmdReadFileError e ->
      "Cannot read file: " <> prettyError e
    GovernanceActionsCmdReadTextEnvelopeFileError e ->
      "Cannot read text envelope file: " <> prettyError e
    GovernanceActionsCmdWriteFileError e ->
      "Cannot write file: " <> prettyError e
    GovernanceActionsValueUpdateProtocolParametersNotFound (AnyShelleyBasedEra expectedShelleyEra) ->
      "Protocol parameters update value for" <+> pretty expectedShelleyEra <+> "was not found."
    GovernanceActionsReadStakeCredErrror e ->
      prettyError e
    GovernanceActionsHashCheckError adt hashCheckErr ->
      "Error while checking hash for"
        <+> pretty (anchorDataTypeCheckName adt)
        <> ":"
          <+> prettyException hashCheckErr

data AnchorDataTypeCheck
  = ProposalCheck
  | ConstitutionCheck
  deriving Show

anchorDataTypeCheckName :: AnchorDataTypeCheck -> String
anchorDataTypeCheckName ProposalCheck = "proposal"
anchorDataTypeCheckName ConstitutionCheck = "constitution"
