{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- HLINT ignore "Use let" -}

module Cardano.CLI.EraBased.Run.Governance.DRep
  ( runGovernanceDRepCmds
  ) where

import           Cardano.Api
import qualified Cardano.Api.Ledger as Ledger
import           Cardano.Api.Shelley

import           Cardano.CLI.EraBased.Commands.Governance.DRep
import           Cardano.CLI.EraBased.Run.Governance
import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Errors.CmdError
import           Cardano.CLI.Types.Errors.GovernanceCmdError
import           Cardano.CLI.Types.Errors.RegistrationError
import           Cardano.CLI.Types.Key

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Except.Extra
import           Data.Function
import qualified Data.Text.Encoding as Text

runGovernanceDRepCmds :: ()
  => GovernanceDRepCmds era
  -> ExceptT CmdError IO ()
runGovernanceDRepCmds = \case
  GovernanceDRepGenerateKeyCmd w vrf sgn ->
    runGovernanceDRepKeyGen w vrf sgn
      & firstExceptT CmdGovernanceCmdError

  GovernanceDRepIdCmd w vkey idOutputFormat mOutFp ->
    runGovernanceDRepIdCmd w vkey idOutputFormat mOutFp
      & firstExceptT CmdGovernanceCmdError

  GovernanceDRepRegistrationCertificateCmd w vkey lovelace anchor outFp ->
    conwayEraOnwardsConstraints w $ do
      runGovernanceRegistrationCertificateCmd w vkey lovelace anchor outFp
        & firstExceptT CmdRegistrationError

  GovernanceDRepRetirementCertificateCmd w vkeyOrHashOrFile epoch outFp ->
    runGovernanceDrepRetirementCertificateCmd w vkeyOrHashOrFile epoch outFp
      & firstExceptT CmdGovernanceCmdError

runGovernanceDRepIdCmd :: ()
  => ConwayEraOnwards era
  -> VerificationKeyOrFile DRepKey
  -> IdOutputFormat
  -> Maybe (File () Out)
  -> ExceptT GovernanceCmdError IO ()
runGovernanceDRepIdCmd _ vkOrFp idOutputFormat mOutFile = do
  drepVerKey <-
    lift (readVerificationKeyOrTextEnvFile AsDRepKey vkOrFp)
      & onLeft (left . ReadFileError)

  content <-
    pure $ case idOutputFormat of
      IdOutputFormatHex -> serialiseToRawBytesHex $ verificationKeyHash drepVerKey
      IdOutputFormatBech32 -> Text.encodeUtf8 $ serialiseToBech32 $ verificationKeyHash drepVerKey

  lift (writeByteStringOutput mOutFile content)
    & onLeft (left . WriteFileError)

--------------------------------------------------------------------------------

-- Registration Certificate related

runGovernanceRegistrationCertificateCmd
  :: ConwayEraOnwards era
  -> VerificationKeyOrHashOrFile DRepKey
  -> Lovelace
  -> Maybe (Ledger.Anchor (Ledger.EraCrypto (ShelleyLedgerEra era)))
  -> File () Out
  -> ExceptT RegistrationError IO ()
runGovernanceRegistrationCertificateCmd cOnwards drepKOrHOrF deposit anchor outfp = do
    DRepKeyHash drepKeyHash <- firstExceptT RegistrationReadError
      . newExceptT
      $ readVerificationKeyOrHashOrFile AsDRepKey drepKOrHOrF
    let drepCred = Ledger.KeyHashObj $ conwayEraOnwardsConstraints cOnwards drepKeyHash
        votingCredential = VotingCredential drepCred
        req = DRepRegistrationRequirements cOnwards votingCredential deposit
        registrationCert = makeDrepRegistrationCertificate req anchor
        description = Just @TextEnvelopeDescr "DRep Key Registration Certificate"

    firstExceptT RegistrationWriteFileError
      . newExceptT
      . writeLazyByteStringFile outfp
      $ conwayEraOnwardsConstraints cOnwards
      $ textEnvelopeToJSON description registrationCert

runGovernanceDrepRetirementCertificateCmd
  :: ConwayEraOnwards era
  -> VerificationKeyOrHashOrFile DRepKey
  -> EpochNo
  -> File () 'Out
  -> ExceptT GovernanceCmdError IO ()
runGovernanceDrepRetirementCertificateCmd = undefined
-- w vKeyOrHashOrFile outFile =
--   conwayEraOnwardsConstraints w $ do
--     DRepKeyHash drepKeyHash <- firstExceptT GovernanceCmdKeyReadError
--       . newExceptT
--       $ readVerificationKeyOrHashOrFile AsDRepKey vKeyOrHashOrFile
--     undefined
--
