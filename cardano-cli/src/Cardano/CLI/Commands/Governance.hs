{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Commands.Governance where

import           Cardano.Api
import           Cardano.Api.Shelley

import           Cardano.Binary (DecoderError)
import           Cardano.CLI.Legacy.Run.StakeAddress
import           Cardano.CLI.Read (CddlError)
import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Governance
import           Cardano.CLI.Types.Key
import           Cardano.Prelude (intercalate, toS)

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, hoistEither, newExceptT)
import           Data.Bifunctor
import qualified Data.ByteString as BS
import           Data.Text (Text)
import qualified Data.Text.Encoding as Text
import           Data.Text.Encoding.Error
import qualified Data.Text.Lazy.Builder as TL
import qualified Formatting.Buildable as B

data GovernanceCmdError
  = -- Voting related
    StakeCredGovCmdError ShelleyStakeAddressCmdError
  | VotingCredentialDecodeGovCmdEror DecoderError
  | WriteFileError (FileError ())
  | ReadFileError (FileError InputDecodeError)
    -- Governance action related
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
  -- Legacy - remove me after cardano-cli transitions to new era based structure
  | GovernanceCmdMIRCertNotSupportedInConway
  | GovernanceCmdGenesisDelegationNotSupportedInConway
  deriving Show

instance Error GovernanceCmdError where
  displayError = \case
    StakeCredGovCmdError stakeAddressCmdError ->
      "Stake credential error: " <> displayError stakeAddressCmdError
    VotingCredentialDecodeGovCmdEror decoderError ->
      "Could not decode voting credential: " <> renderDecoderError decoderError
    WriteFileError fileError ->
      displayError fileError
    ReadFileError fileError ->
      displayError fileError
    NonUtf8EncodedConstitution unicodeException ->
      "Constitution encoded in a format different than UTF-8: " <> show unicodeException
    GovernanceCmdTextEnvReadError fileError ->
      "Cannot read text envelope: " <> displayError fileError
    GovernanceCmdCddlError cddlError ->
      "Reading transaction CDDL file error: " <> displayError cddlError
    GovernanceCmdKeyReadError fileError ->
      "Cannot read key: " <> displayError fileError
    GovernanceCmdCostModelReadError fileError ->
      "Cannot read cost model: " <> displayError fileError
    GovernanceCmdTextEnvWriteError fileError ->
      displayError fileError
    GovernanceCmdEmptyUpdateProposalError ->
      "Empty update proposals are not allowed."
    GovernanceCmdMIRCertificateKeyRewardMistmach fp nStakeVerKeys nRewards ->
      "Error creating the MIR certificate at: " <> fp
      <> " The number of staking keys: " <> show nStakeVerKeys
      <> " and the number of reward amounts: " <> show nRewards
      <> " are not equivalent."
    GovernanceCmdCostModelsJsonDecodeErr fp msg ->
      "Error decoding cost model: " <> toS msg <> " at: " <> fp
    GovernanceCmdEmptyCostModel fp ->
      "The decoded cost model was empty at: " <> fp
    GovernanceCmdUnexpectedKeyType expectedTypes ->
      "Unexpected poll key type; expected one of: "
      <> intercalate ", " (show <$> expectedTypes)
    GovernanceCmdPollOutOfBoundAnswer maxIdx ->
      "Poll answer out of bounds. Choices are between 0 and " <> show maxIdx
    GovernanceCmdPollInvalidChoice ->
      "Invalid choice. Please choose from the available answers."
    GovernanceCmdDecoderError decoderError ->
      "Unable to decode metadata: " <> renderDecoderError decoderError
    GovernanceCmdVerifyPollError pollError ->
      toS (renderGovernancePollError pollError)
    GovernanceCmdWriteFileError fileError ->
      "Cannot write file: " <> displayError fileError
    GovernanceCmdMIRCertNotSupportedInConway ->
      "MIR certificates are not supported in Conway era onwards."
    GovernanceCmdGenesisDelegationNotSupportedInConway ->
      "Genesis delegation is not supported in Conway era onwards."
    where
      renderDecoderError = toS . TL.toLazyText . B.build

runGovernanceCreateVoteCmd
  :: AnyShelleyBasedEra
  -> Vote
  -> VType
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
    VCC -> do
      votingCred <- hoistEither $ first VotingCredentialDecodeGovCmdEror $ toVotingCredential sbe vStakeCred
      let govActIdentifier = makeGoveranceActionId sbe govActionTxIn
          voteProcedure = createVotingProcedure sbe vChoice (VoterCommittee votingCred) govActIdentifier
      firstExceptT WriteFileError . newExceptT $ shelleyBasedEraConstraints sbe $ writeFileTextEnvelope oFp Nothing voteProcedure

    VDR -> do
      votingCred <- hoistEither $ first VotingCredentialDecodeGovCmdEror $ toVotingCredential sbe vStakeCred
      let govActIdentifier = makeGoveranceActionId sbe govActionTxIn
          voteProcedure = createVotingProcedure sbe vChoice (VoterDRep votingCred) govActIdentifier
      firstExceptT WriteFileError . newExceptT $ shelleyBasedEraConstraints sbe $ writeFileTextEnvelope oFp Nothing voteProcedure

    VSP -> do
      let govActIdentifier = makeGoveranceActionId sbe govActionTxIn
          voteProcedure = createVotingProcedure sbe vChoice (VoterSpo stakePoolKeyHash) govActIdentifier
      firstExceptT WriteFileError . newExceptT $ shelleyBasedEraConstraints sbe $ writeFileTextEnvelope oFp Nothing voteProcedure


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
runGovernanceCreateActionCmd anyEra deposit depositReturnAddr govAction oFp = do
  AnyShelleyBasedEra sbe <- pure anyEra
  let proposal = createProposalProcedure sbe deposit depositReturnAddr govAction

  firstExceptT WriteFileError . newExceptT
    $ shelleyBasedEraConstraints sbe
    $ writeFileTextEnvelope oFp Nothing proposal

