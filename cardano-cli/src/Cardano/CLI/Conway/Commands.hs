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
  | ReadFileError (FileError InputDecodeError)
    -- Governance action related
  | ExpectedStakeKeyCredentialGovCmdError
  | NonUtf8EncodedConstitution UnicodeException
  deriving Show

runGovernanceCreateVoteCmd
  :: AnyShelleyBasedEra
  -> VoteChoice
  -> VType
  -> TxIn
  -> VerificationKeyOrFile StakePoolKey
  -> ConwayVoteFile Out
  -> ExceptT GovernanceCmdError IO ()
runGovernanceCreateVoteCmd (AnyShelleyBasedEra sbe) vChoice vType govActionTxIn votingStakeCred oFp = do
  vStakePoolKey <- firstExceptT ReadFileError . newExceptT $ readVerificationKeyOrFile AsStakePoolKey votingStakeCred
  let stakePoolKeyHash = verificationKeyHash vStakePoolKey
      vStakeCred = StakeCredentialByKey . (verificationKeyHash . castVerificationKey) $ vStakePoolKey
  case vType of
    VCC -> do
      votingCred <- hoistEither $ first VotingCredentialDecodeGovCmdEror $ toVotingCredential sbe vStakeCred
      let govActIdentifier = makeGoveranceActionIdentifier sbe govActionTxIn
          voteProcedure = createVotingProcedure sbe vChoice (CC votingCred) govActIdentifier
      firstExceptT WriteFileError . newExceptT $ obtainEraPParamsConstraint sbe $ writeFileTextEnvelope oFp Nothing voteProcedure

    VDR -> do
      votingCred <- hoistEither $ first VotingCredentialDecodeGovCmdEror $ toVotingCredential sbe vStakeCred
      let govActIdentifier = makeGoveranceActionIdentifier sbe govActionTxIn
          voteProcedure = createVotingProcedure sbe vChoice (DR votingCred) govActIdentifier
      firstExceptT WriteFileError . newExceptT $ obtainEraPParamsConstraint sbe $ writeFileTextEnvelope oFp Nothing voteProcedure

    VSP -> do
      let govActIdentifier = makeGoveranceActionIdentifier sbe govActionTxIn
          voteProcedure = createVotingProcedure sbe vChoice (SP stakePoolKeyHash) govActIdentifier
      firstExceptT WriteFileError . newExceptT $ obtainEraPParamsConstraint sbe $ writeFileTextEnvelope oFp Nothing voteProcedure


runGovernanceNewConstitutionCmd
  :: AnyShelleyBasedEra
  -> Lovelace
  -> VerificationKeyOrFile StakePoolKey
  -> Constitution
  -> NewConstitutionFile Out
  -> ExceptT GovernanceCmdError IO ()
runGovernanceNewConstitutionCmd sbe deposit stakeVoteCred constitution oFp = do
  vStakePoolKeyHash
    <- fmap (verificationKeyHash . castVerificationKey)
        <$> firstExceptT ReadFileError . newExceptT
              $ readVerificationKeyOrFile AsStakePoolKey stakeVoteCred
  case constitution of
    ConstitutionFromFile fp  -> do
      cBs <- liftIO $ BS.readFile $ unFile fp
      _utf8EncodedText <- firstExceptT NonUtf8EncodedConstitution . hoistEither $ Text.decodeUtf8' cBs
      let govAct = ProposeNewConstitution cBs
      runGovernanceCreateActionCmd sbe deposit vStakePoolKeyHash govAct oFp

    ConstitutionFromText c -> do
      let constitBs = Text.encodeUtf8 c
          govAct = ProposeNewConstitution constitBs
      runGovernanceCreateActionCmd sbe deposit vStakePoolKeyHash govAct oFp

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
