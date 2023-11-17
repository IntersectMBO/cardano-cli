{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Types.Errors.GovernanceActionsError
  ( GovernanceActionsError(..)
  ) where

import           Cardano.Api
import           Cardano.Api.Pretty

import           Cardano.CLI.Read

data GovernanceActionsError
  = GovernanceActionsCmdConstitutionError ConstitutionError
  | GovernanceActionsCmdProposalError ProposalError
  | GovernanceActionsCmdReadFileError (FileError InputDecodeError)
  | GovernanceActionsCmdReadTextEnvelopeFileError (FileError TextEnvelopeError)
  | GovernanceActionsCmdWriteFileError (FileError ())
  | GovernanceActionsValueUpdateProtocolParametersNotFound AnyShelleyBasedEra
  deriving Show

instance Error GovernanceActionsError where
  prettyError = \case
    GovernanceActionsCmdProposalError e ->
      "Cannot read proposal: " <> pshow e -- TODO Conway render this properly
    GovernanceActionsCmdConstitutionError e ->
      "Cannot read constitution: " <> pshow e -- TODO Conway render this properly
    GovernanceActionsCmdReadFileError e ->
      "Cannot read file: " <> pretty e
    GovernanceActionsCmdReadTextEnvelopeFileError e ->
      "Cannot read text envelope file: " <> pretty e
    GovernanceActionsCmdWriteFileError e ->
      "Cannot write file: " <> pretty e
    GovernanceActionsValueUpdateProtocolParametersNotFound (AnyShelleyBasedEra expectedShelleyEra) ->
      mconcat
        [ "Protocol parameters update value for " <> pshow (toCardanoEra expectedShelleyEra)
        , " was not found."
        ]
