{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

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
import qualified Cardano.Binary as Plain
import qualified Cardano.Ledger.Credential as Ledger
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Alonzo.Core
import qualified Data.ByteString.Lazy as BL


data GovernanceCmdError
  = -- Voting related
    VotingCredentialDecodeError (FileError InputDecodeError)
  | StakeCredGovCmdError ShelleyStakeAddressCmdError
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
  -> VerificationKeyOrFile StakePoolKey
  -> ConwayVoteFile Out
  -> ExceptT GovernanceCmdError IO ()
runGovernanceCreateVoteCmd (AnyShelleyBasedEra sbe) vChoice cType govActionTxIn stakePoolVerKeyOrFile oFp = do
  stakePoolVerKey <- firstExceptT VotingCredentialDecodeError
    . newExceptT
    $ readVerificationKeyOrFile AsStakePoolKey stakePoolVerKeyOrFile

  let stakePoolVerificationKeyHash = unStakePoolKeyHash $ verificationKeyHash stakePoolVerKey
  votingCred <- hoistEither $ first VotingCredentialDecodeGovCmdEror $ eraDecodeVotingCredential sbe $ Plain.serialize $ Ledger.KeyHashObj stakePoolVerificationKeyHash

  let govActIdentifier = makeGoveranceActionIdentifier sbe govActionTxIn
      voteProcedure = createVotingProcedure sbe vChoice cType govActIdentifier votingCred
  firstExceptT WriteFileError . newExceptT $ obtainEraPParamsConstraint sbe $ writeFileTextEnvelope oFp Nothing voteProcedure
  where
    obtainCryptoConstraints
      :: ShelleyBasedEra era
      -> ((Crypto (EraCrypto (ShelleyLedgerEra era))) => a)
      -> a
    obtainCryptoConstraints ShelleyBasedEraShelley f = f
    obtainCryptoConstraints ShelleyBasedEraAllegra f = f
    obtainCryptoConstraints ShelleyBasedEraMary    f = f
    obtainCryptoConstraints ShelleyBasedEraAlonzo  f = f
    obtainCryptoConstraints ShelleyBasedEraBabbage f = f
    obtainCryptoConstraints ShelleyBasedEraConway  f = f

    eraDecodeVotingCredential
      :: ShelleyBasedEra era
      -> BL.ByteString
      -> Either Plain.DecoderError (VotingCredential era)
    eraDecodeVotingCredential sbe' bs = obtainCryptoConstraints sbe' $
      case Plain.decodeFull bs of
        Left e -> Left e
        Right x -> Right $ VotingCredential x


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
