{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Cardano.CLI.Commands.Governance where

import           Cardano.Api
import qualified Cardano.Api.Ledger as Ledger
import           Cardano.Api.Shelley

import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Errors.GovernanceCmdError
import           Cardano.CLI.Types.Governance
import           Cardano.CLI.Types.Key

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, hoistEither, newExceptT)
import qualified Data.ByteString as BS
import           Data.Text (Text)
import qualified Data.Text.Encoding as Text
import           Data.Word

runGovernanceCreateVoteCmd
  :: AnyShelleyBasedEra
  -> Vote
  -> VoteFile Out
  -> ExceptT GovernanceCmdError IO ()
runGovernanceCreateVoteCmd anyEra vChoice   oFp = do
  AnyShelleyBasedEra sbe <- pure anyEra
  let voteProcedure = createVotingProcedure sbe vChoice Nothing
  firstExceptT WriteFileError . newExceptT $ shelleyBasedEraConstraints sbe $ writeFileTextEnvelope oFp Nothing voteProcedure


runGovernanceNewConstitutionCmd
  :: Ledger.Network
  -> AnyShelleyBasedEra
  -> Lovelace
  -> VerificationKeyOrFile StakePoolKey
  -> Maybe (TxId, Word32)
  -> (Ledger.Url, Text)
  -> Constitution
  -> NewConstitutionFile Out
  -> ExceptT GovernanceCmdError IO ()
runGovernanceNewConstitutionCmd network sbe deposit stakeVoteCred mPrevGovAct propAnchor constitution oFp = do
  vStakePoolKeyHash
    <- fmap (verificationKeyHash . castVerificationKey)
        <$> firstExceptT ReadFileError . newExceptT
              $ readVerificationKeyOrFile AsStakePoolKey stakeVoteCred
  case constitution of
    ConstitutionFromFile url fp  -> do
      cBs <- liftIO $ BS.readFile $ unFile fp
      _utf8EncodedText <- firstExceptT NonUtf8EncodedConstitution . hoistEither $ Text.decodeUtf8' cBs
      let prevGovActId = Ledger.maybeToStrictMaybe $ uncurry createPreviousGovernanceActionId <$> mPrevGovAct
          govAct = ProposeNewConstitution
                     prevGovActId
                     (createAnchor url cBs) -- TODO: Conway era - this is wrong, create `AnchorData` then hash that with hashAnchorData
      runGovernanceCreateActionCmd network sbe deposit vStakePoolKeyHash propAnchor govAct oFp

    ConstitutionFromText url c -> do
      let constitBs = Text.encodeUtf8 c
          prevGovActId = Ledger.maybeToStrictMaybe $ uncurry createPreviousGovernanceActionId <$> mPrevGovAct
          govAct = ProposeNewConstitution
                     prevGovActId
                     (createAnchor url constitBs)
      runGovernanceCreateActionCmd network sbe deposit vStakePoolKeyHash propAnchor govAct oFp

runGovernanceCreateActionCmd
  :: Ledger.Network
  -> AnyShelleyBasedEra
  -> Lovelace
  -> Hash StakeKey
  -> (Ledger.Url, Text)
  -> GovernanceAction
  -> File a Out
  -> ExceptT GovernanceCmdError IO ()
runGovernanceCreateActionCmd network anyEra deposit depositReturnAddr propAnchor govAction oFp = do
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

