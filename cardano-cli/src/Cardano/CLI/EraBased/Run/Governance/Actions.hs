{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.Run.Governance.Actions
  ( runGovernanceActionCmds
  ) where

import           Cardano.Api
import           Cardano.Api.Ledger (HasKeyRole (coerceKeyRole))
import           Cardano.Api.Shelley

import           Cardano.CLI.EraBased.Commands.Governance.Actions
import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Errors.GovernanceActionsError
import           Cardano.CLI.Types.Key

import           Control.Monad.Except (ExceptT)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Except.Extra
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as Text

runGovernanceActionCmds :: ()
  => GovernanceActionCmds era
  -> ExceptT GovernanceActionsError IO ()
runGovernanceActionCmds = \case
  GovernanceActionCreateConstitution cOn newConstitution ->
    runGovernanceActionCreateConstitution cOn newConstitution

runGovernanceActionCreateConstitution :: ()
  => ConwayEraOnwards era
  -> EraBasedNewConstitution
  -> ExceptT GovernanceActionsError IO ()
runGovernanceActionCreateConstitution cOn (EraBasedNewConstitution deposit anyStake constit outFp) = do

  stakeKeyHash
    <- case anyStake of
         AnyStakeKey stake ->
           firstExceptT GovernanceActionsCmdReadFileError
             . newExceptT $ readVerificationKeyOrHashOrFile AsStakeKey stake

         AnyStakePoolKey stake -> do
           StakePoolKeyHash t <- firstExceptT GovernanceActionsCmdReadFileError
                                   . newExceptT $ readVerificationKeyOrHashOrFile AsStakePoolKey stake
           return $ StakeKeyHash $ coerceKeyRole t

  case constit of
    ConstitutionFromFile fp  -> do
      cBs <- liftIO $ BS.readFile $ unFile fp
      _utf8EncodedText <- firstExceptT GovernanceActionsCmdNonUtf8EncodedConstitution . hoistEither $ Text.decodeUtf8' cBs
      let govAct = ProposeNewConstitution cBs
          sbe = conwayEraOnwardsToShelleyBasedEra cOn
          proposal = createProposalProcedure sbe deposit stakeKeyHash govAct

      firstExceptT GovernanceActionsCmdWriteFileError . newExceptT
        $ conwayEraOnwardsConstraints cOn
        $ writeFileTextEnvelope outFp Nothing proposal

    ConstitutionFromText c -> do
      let constitBs = Text.encodeUtf8 c
          sbe = conwayEraOnwardsToShelleyBasedEra cOn
          govAct = ProposeNewConstitution constitBs
          proposal = createProposalProcedure sbe deposit stakeKeyHash govAct

      firstExceptT GovernanceActionsCmdWriteFileError . newExceptT
        $ conwayEraOnwardsConstraints cOn
        $ writeFileTextEnvelope outFp Nothing proposal


