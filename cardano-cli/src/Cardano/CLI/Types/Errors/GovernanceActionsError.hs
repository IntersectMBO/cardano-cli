{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Types.Errors.GovernanceActionsError
  ( GovernanceActionsError(..)
  ) where

import           Cardano.Api

import           Cardano.CLI.Read

data GovernanceActionsError
  = GovernanceActionsCmdConstitutionError ConstitutionError
  | GovernanceActionsCmdProposalError ProposalError
  | GovernanceActionsCmdCostModelsError CostModelsError
  | GovernanceActionsCmdReadFileError (FileError InputDecodeError)
  | GovernanceActionsCmdReadTextEnvelopeFileError (FileError TextEnvelopeError)
  | GovernanceActionsCmdWriteFileError (FileError ())
  | GovernanceActionsValueUpdateProtocolParametersNotFound AnyShelleyBasedEra
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
      mconcat
        [ "Protocol parameters update value for " <> pshow (toCardanoEra expectedShelleyEra)
        , " was not found."
        ]
