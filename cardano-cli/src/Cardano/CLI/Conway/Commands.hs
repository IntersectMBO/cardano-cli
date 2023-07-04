{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Cardano.CLI.Conway.Commands where

import           Cardano.Api
import           Cardano.Api.Shelley

import           Cardano.Binary (DecoderError)
import           Cardano.CLI.Conway.Types
import           Cardano.CLI.Shelley.Key
import           Cardano.CLI.Shelley.Run.StakeAddress

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, hoistEither, newExceptT)
import           Data.Bifunctor
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as Text
import           Data.Text.Encoding.Error


data GovernanceCmdError
  = -- Voting related
    StakeCredGovCmdError ShelleyStakeAddressCmdError
  | VotingCredentialDecodeGovCmdEror DecoderError
  | WriteFileError (FileError ())
    -- Governance action related
  | ExpectedStakeKeyCredentialGovCmdError
  | NonUtf8EncodedConstitution UnicodeException
  deriving Show

runGovernanceCreateVoteCmd
  :: AnyShelleyBasedEra
  -> VoteChoice
  -> VoterType
  -> TxIn
  -> StakeIdentifier
  -> ConwayVoteFile Out
  -> ExceptT GovernanceCmdError IO ()
runGovernanceCreateVoteCmd (AnyShelleyBasedEra sbe) vChoice cType govActionTxIn votingStakeCred oFp = do
  stakeCred <- firstExceptT StakeCredGovCmdError $ getStakeCredentialFromIdentifier votingStakeCred
  votingCred <- hoistEither $ first VotingCredentialDecodeGovCmdEror $ toVotingCredential sbe stakeCred
  let govActIdentifier = makeGoveranceActionIdentifier sbe govActionTxIn
      voteProcedure = createVotingProcedure sbe vChoice cType govActIdentifier votingCred
  firstExceptT WriteFileError . newExceptT $ obtainEraPParamsConstraint sbe $ writeFileTextEnvelope oFp Nothing voteProcedure



runGovernanceNewConstitutionCmd
  :: AnyShelleyBasedEra
  -> Lovelace
  -> StakeIdentifier
  -> Constitution
  -> NewConstitutionFile Out
  -> ExceptT GovernanceCmdError IO ()
runGovernanceNewConstitutionCmd sbe deposit stakeVoteCred constitution oFp = do
  stakeCred <- firstExceptT StakeCredGovCmdError $ getStakeCredentialFromIdentifier stakeVoteCred
  stakeKey <- hoistEither $ stakeKeyHashOnly stakeCred
  case constitution of
    ConstitutionFromFile fp  -> do
      cBs <- liftIO $ BS.readFile $ unFile fp
      _utf8EncodedText <- firstExceptT NonUtf8EncodedConstitution . hoistEither $ Text.decodeUtf8' cBs
      let govAct = ProposeNewConstitution cBs
      runGovernanceCreateActionCmd sbe deposit stakeKey govAct oFp

    ConstitutionFromText c -> do
      let constitBs = Text.encodeUtf8 c
          govAct = ProposeNewConstitution constitBs
      runGovernanceCreateActionCmd sbe deposit stakeKey govAct oFp

runGovernanceCreateActionCmd
  :: AnyShelleyBasedEra
  -> Lovelace
  -> Hash StakeKey
  -> GovernanceAction
  -> File a Out
  -> ExceptT GovernanceCmdError IO ()
runGovernanceCreateActionCmd (AnyShelleyBasedEra sbe) deposit depositReturnAddr govAction oFp =
  let proposal = createProposalProcedure sbe deposit depositReturnAddr govAction
  in firstExceptT WriteFileError . newExceptT $ obtainEraPParamsConstraint sbe $ writeFileTextEnvelope oFp Nothing proposal

stakeKeyHashOnly :: StakeCredential -> Either GovernanceCmdError (Hash StakeKey)
stakeKeyHashOnly (StakeCredentialByKey k) = Right k
stakeKeyHashOnly StakeCredentialByScript{} = Left ExpectedStakeKeyCredentialGovCmdError
