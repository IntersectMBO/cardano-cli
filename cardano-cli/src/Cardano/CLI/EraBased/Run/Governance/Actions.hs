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
import           Cardano.CLI.Types.Key

import           Control.Monad.Except (ExceptT)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Except.Extra
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as Text
import           Data.Text.Encoding.Error


data GovernanceActionsError
  = GovernanceActionsCmdWriteFileError (FileError ())
  | GovernanceActionsCmdReadFileError (FileError InputDecodeError)
  | GovernanceActionsCmdNonUtf8EncodedConstitution UnicodeException


runGovernanceActionCmds :: ()
  => GovernanceActionCmds era
  -> ExceptT GovernanceActionsError IO ()
runGovernanceActionCmds = \case
  GovernanceActionCreateConstitution cOn newConstitution ->
    runGovernanceActionCreateConstitution cOn newConstitution

  GovernanceActionProtocolParametersUpdate sbe eraBasedProtocolParametersUpdate ofp ->
    runGovernanceActionCreateProtocolParametersUpdate sbe eraBasedProtocolParametersUpdate ofp

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


runGovernanceActionCreateProtocolParametersUpdate :: ()
  => ShelleyBasedEra era
  -> EraBasedProtocolParametersUpdate era
  -> File () Out
  -> ExceptT GovernanceActionsError IO ()
runGovernanceActionCreateProtocolParametersUpdate sbe eraBasedPParams oFp = do
  let updateProtocolParams = createEraBasedProtocolParamUpdate sbe eraBasedPParams
      apiUpdateProtocolParamsType = fromLedgerPParamsUpdate sbe updateProtocolParams
      -- TODO: Update EraBasedProtocolParametersUpdate to require genesis delegate keys
      -- depending on the era
      -- TODO: Require expiration epoch no
      upProp = makeShelleyUpdateProposal apiUpdateProtocolParamsType [] (error "runGovernanceActionCreateProtocolParametersUpdate")

  firstExceptT GovernanceActionsCmdWriteFileError . newExceptT
    $ writeLazyByteStringFile oFp $ textEnvelopeToJSON Nothing upProp
