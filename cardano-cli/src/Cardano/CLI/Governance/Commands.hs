{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Governance.Commands where

import           Cardano.Api
import           Cardano.Api.Shelley

import           Cardano.Binary (DecoderError)
import           Cardano.CLI.Shelley.Key
import           Cardano.CLI.Shelley.Run.Read (CddlError)
import           Cardano.CLI.Shelley.Run.StakeAddress
import           Cardano.CLI.Types.Governance

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, hoistEither, newExceptT)
import           Data.Bifunctor
import qualified Data.ByteString as BS
import           Data.Text (Text)
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

  | GovernanceCmdTextEnvReadError !(FileError TextEnvelopeError)
  | GovernanceCmdCddlError !CddlError
  | GovernanceCmdKeyReadError !(FileError InputDecodeError)
  | GovernanceCmdCostModelReadError !(FileError ())
  | GovernanceCmdTextEnvWriteError !(FileError ())
  | GovernanceCmdEmptyUpdateProposalError
  | GovernanceCmdMIRCertificateKeyRewardMistmach
      !FilePath
      !Int
      -- ^ Number of stake verification keys
      !Int
      -- ^ Number of reward amounts
  | GovernanceCmdCostModelsJsonDecodeErr !FilePath !Text
  | GovernanceCmdEmptyCostModel !FilePath
  | GovernanceCmdUnexpectedKeyType
      ![TextEnvelopeType]
      -- ^ Expected key types
  | GovernanceCmdPollOutOfBoundAnswer
      !Int
      -- ^ Maximum answer index
  | GovernanceCmdPollInvalidChoice
  | GovernanceCmdDecoderError !DecoderError
  | GovernanceCmdVerifyPollError !GovernancePollError
  | GovernanceCmdWriteFileError !(FileError ())
  deriving Show

runGovernanceCreateVoteCmd
  :: AnyShelleyBasedEra
  -> VoteChoice
  -> GovVoterType
  -> TxIn
  -> VerificationKeyOrFile StakePoolKey
  -> VoteFile Out
  -> ExceptT GovernanceCmdError IO ()
runGovernanceCreateVoteCmd anyEra vChoice vType govActionTxIn votingStakeCred oFp = do
  AnyShelleyBasedEra sbe <- pure anyEra
  vStakePoolKey <- firstExceptT ReadFileError . newExceptT $ readVerificationKeyOrFile AsStakePoolKey votingStakeCred
  let stakePoolKeyHash = verificationKeyHash vStakePoolKey
      vStakeCred = StakeCredentialByKey . (verificationKeyHash . castVerificationKey) $ vStakePoolKey
  case vType of
    GovVoterTypeCommittee -> do
      votingCred <- hoistEither $ first VotingCredentialDecodeGovCmdEror $ toVotingCredential sbe vStakeCred
      let govActIdentifier = makeGoveranceActionIdentifier sbe govActionTxIn
          voteProcedure = createVotingProcedure sbe vChoice (CC votingCred) govActIdentifier
      firstExceptT WriteFileError . newExceptT $ obtainEraPParamsConstraint sbe $ writeFileTextEnvelope oFp Nothing voteProcedure

    GovVoterTypeDRep -> do
      votingCred <- hoistEither $ first VotingCredentialDecodeGovCmdEror $ toVotingCredential sbe vStakeCred
      let govActIdentifier = makeGoveranceActionIdentifier sbe govActionTxIn
          voteProcedure = createVotingProcedure sbe vChoice (DR votingCred) govActIdentifier
      firstExceptT WriteFileError . newExceptT $ obtainEraPParamsConstraint sbe $ writeFileTextEnvelope oFp Nothing voteProcedure

    GovVoterTypeSpo -> do
      let govActIdentifier = makeGoveranceActionIdentifier sbe govActionTxIn
          voteProcedure = createVotingProcedure sbe vChoice (SP stakePoolKeyHash) govActIdentifier
      firstExceptT WriteFileError . newExceptT $ obtainEraPParamsConstraint sbe $ writeFileTextEnvelope oFp Nothing voteProcedure


runGovernanceNewConstitutionCmd
  :: AnyShelleyBasedEra
  -> Lovelace
  -> VerificationKeyOrFile StakePoolKey
  -> ConstitutionSource
  -> NewConstitutionFile Out
  -> ExceptT GovernanceCmdError IO ()
runGovernanceNewConstitutionCmd sbe deposit stakeVoteCred constitution oFp = do
  vStakePoolKeyHash
    <- fmap (verificationKeyHash . castVerificationKey)
        <$> firstExceptT ReadFileError . newExceptT
              $ readVerificationKeyOrFile AsStakePoolKey stakeVoteCred
  case constitution of
    ConstitutionSourceFile fp  -> do
      cBs <- liftIO $ BS.readFile $ unFile fp
      _utf8EncodedText <- firstExceptT NonUtf8EncodedConstitution . hoistEither $ Text.decodeUtf8' cBs
      let govAct = ProposeNewConstitution cBs
      runGovernanceCreateActionCmd sbe deposit vStakePoolKeyHash govAct oFp

    ConstitutionSourceText c -> do
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
runGovernanceCreateActionCmd anyEra deposit depositReturnAddr govAction oFp = do
  AnyShelleyBasedEra sbe <- pure anyEra
  let proposal = createProposalProcedure sbe deposit depositReturnAddr govAction

  firstExceptT WriteFileError . newExceptT
    $ obtainEraPParamsConstraint sbe
    $ writeFileTextEnvelope oFp Nothing proposal

stakeKeyHashOnly :: StakeCredential -> Either GovernanceCmdError (Hash StakeKey)
stakeKeyHashOnly = \case
  StakeCredentialByKey k -> Right k
  StakeCredentialByScript{} -> Left ExpectedStakeKeyCredentialGovCmdError
