{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Cardano.CLI.Commands.Governance where

import           Cardano.Api
import qualified Cardano.Api.Ledger as Ledger
import           Cardano.Api.Shelley

import           Cardano.CLI.Read
import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Errors.GovernanceCmdError
import           Cardano.CLI.Types.Governance
import           Cardano.CLI.Types.Key
import qualified Cardano.Ledger.Conway.Governance as Ledger

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, hoistEither, newExceptT)
import           Data.Bifunctor
import           Data.Function
import           Data.Text (Text)
import qualified Data.Text.Encoding as Text
import           Data.Word

runGovernanceCreateVoteCmd :: ()
  => AnyShelleyBasedEra
  -> Vote
  -> VType
  -> (TxId, Word32)
  -> VerificationKeyOrFile StakePoolKey
  -> VoteFile Out
  -> ExceptT GovernanceCmdError IO ()
runGovernanceCreateVoteCmd anyEra vChoice vType (govActionTxId, govActionIndex) votingStakeCred oFp = do
  AnyShelleyBasedEra sbe <- pure anyEra

  shelleyBasedEraConstraints sbe $ do
    vStakePoolKey <- firstExceptT ReadFileError . newExceptT $ readVerificationKeyOrFile AsStakePoolKey votingStakeCred
    let stakePoolKeyHash = verificationKeyHash vStakePoolKey
        vStakeCred = StakeCredentialByKey . (verificationKeyHash . castVerificationKey) $ vStakePoolKey
    case vType of
      VCC -> do
        votingCred <- hoistEither $ first VotingCredentialDecodeGovCmdEror $ toVotingCredential sbe vStakeCred
        let voter = Ledger.CommitteeVoter (Ledger.coerceKeyRole (unVotingCredential votingCred)) -- TODO Conway - remove coerceKeyRole
            govActIdentifier = createGovernanceActionId govActionTxId govActionIndex
            voteProcedure = createVotingProcedure sbe vChoice Nothing
            votingProcedures = singletonVotingProcedures sbe voter govActIdentifier (unVotingProcedure voteProcedure)
        firstExceptT WriteFileError . newExceptT $ writeFileTextEnvelope oFp Nothing votingProcedures

      VDR -> do
        votingCred <- hoistEither $ first VotingCredentialDecodeGovCmdEror $ toVotingCredential sbe vStakeCred
        let voter = Ledger.DRepVoter (unVotingCredential votingCred)
            govActIdentifier = createGovernanceActionId govActionTxId govActionIndex
            voteProcedure = createVotingProcedure sbe vChoice Nothing
            votingProcedures = singletonVotingProcedures sbe voter govActIdentifier (unVotingProcedure voteProcedure)
        firstExceptT WriteFileError . newExceptT $ writeFileTextEnvelope oFp Nothing votingProcedures

      VSP -> do
        let voter = Ledger.StakePoolVoter (unStakePoolKeyHash stakePoolKeyHash)
            govActIdentifier = createGovernanceActionId govActionTxId govActionIndex
            voteProcedure = createVotingProcedure sbe vChoice Nothing
            votingProcedures = singletonVotingProcedures sbe voter govActIdentifier (unVotingProcedure voteProcedure)
        firstExceptT WriteFileError . newExceptT $ writeFileTextEnvelope oFp Nothing votingProcedures


runGovernanceNewConstitutionCmd
  :: Ledger.Network
  -> AnyShelleyBasedEra
  -> Lovelace
  -> VerificationKeyOrFile StakePoolKey
  -> Maybe (TxId, Word32)
  -> PropposalUrl
  -> ProposalHashSource
  -> ConstitutionUrl
  -> ConstitutionHashSource
  -> File ConstitutionText Out
  -> ExceptT GovernanceCmdError IO ()
runGovernanceNewConstitutionCmd network sbe deposit stakeVoteCred mPrevGovAct proposalUrl proposalHashSource constitutionUrl constitutionHashSource oFp = do
  vStakePoolKeyHash
    <- fmap (verificationKeyHash . castVerificationKey)
        <$> firstExceptT ReadFileError . newExceptT
              $ readVerificationKeyOrFile AsStakePoolKey stakeVoteCred

  constitutionHash <-
    constitutionHashSourceToHash constitutionHashSource
      & firstExceptT GovernanceCmdConstitutionError

  let constitutionAnchor = Ledger.Anchor
        { Ledger.anchorUrl = unConstitutionUrl constitutionUrl
        , Ledger.anchorDataHash = constitutionHash
        }
      prevGovActId = Ledger.maybeToStrictMaybe $ uncurry createPreviousGovernanceActionId <$> mPrevGovAct
      govAct = ProposeNewConstitution prevGovActId constitutionAnchor

  runGovernanceCreateActionCmd network sbe deposit vStakePoolKeyHash propAnchor govAct oFp

runGovernanceCreateActionCmd
  :: Ledger.Network
  -> AnyShelleyBasedEra
  -> Lovelace
  -> Hash StakeKey
  -> ProposalUrl
  -> ProposalHashSource
  -> GovernanceAction
  -> File a Out
  -> ExceptT GovernanceCmdError IO ()
runGovernanceCreateActionCmd network anyEra deposit depositReturnAddr proposalUrl proposalHashSource govAction oFp = do
  AnyShelleyBasedEra sbe <- pure anyEra
  let proposal = createProposalProcedure
                   sbe
                   network
                   deposit
                   depositReturnAddr
                   govAction
                   (uncurry createAnchor (fmap Text.encodeUtf8 propAnchor))

  firstExceptT WriteFileError . newExceptT
    $ shelleyBasedEraConstraints sbe
    $ writeFileTextEnvelope oFp Nothing proposal

