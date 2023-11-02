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
import           Cardano.Api.Ledger (Credential (KeyHashObj))
import qualified Cardano.Api.Ledger as Ledger
import           Cardano.Api.Shelley

import           Cardano.CLI.EraBased.Commands.Governance.DRep
import qualified Cardano.CLI.EraBased.Run.Key as Key
import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Errors.CmdError
import           Cardano.CLI.Types.Errors.GovernanceCmdError
import           Cardano.CLI.Types.Errors.RegistrationError
import           Cardano.CLI.Types.Key

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Except.Extra
import           Data.Function
import qualified Data.Text.Encoding as Text

runGovernanceDRepCmds :: ()
  => GovernanceDRepCmds era
  -> ExceptT CmdError IO ()
runGovernanceDRepCmds = \case
  GovernanceDRepKeyGenCmd w vrf sgn ->
    runGovernanceDRepKeyGenCmd w vrf sgn
      & firstExceptT CmdGovernanceCmdError

  GovernanceDRepIdCmd w vkey idOutputFormat mOutFp ->
    runGovernanceDRepIdCmd w vkey idOutputFormat mOutFp
      & firstExceptT CmdGovernanceCmdError

  GovernanceDRepRegistrationCertificateCmd w vkey lovelace anchor outFp ->
    conwayEraOnwardsConstraints w $ do
      runGovernanceDRepRegistrationCertificateCmd w vkey lovelace anchor outFp
        & firstExceptT CmdRegistrationError

  GovernanceDRepRetirementCertificateCmd w vkeyOrHashOrFile deposit outFp ->
    runGovernanceDRepRetirementCertificateCmd w vkeyOrHashOrFile deposit outFp
      & firstExceptT CmdGovernanceCmdError

  GovernanceDRepMetadataHashCmd _ inFp mOutFp ->
    runGovernanceDRepMetadataHashCmd inFp mOutFp
      & firstExceptT CmdGovernanceCmdError

runGovernanceDRepKeyGenCmd :: ()
  => ConwayEraOnwards era
  -> VerificationKeyFile Out
  -> SigningKeyFile Out
  -> ExceptT GovernanceCmdError IO ()
runGovernanceDRepKeyGenCmd _w vkeyPath skeyPath = firstExceptT GovernanceCmdWriteFileError $ do
  skey <- liftIO $ generateSigningKey AsDRepKey
  let vkey = getVerificationKey skey
  newExceptT $ writeLazyByteStringFile skeyPath (textEnvelopeToJSON (Just skeyDesc) skey)
  newExceptT $ writeLazyByteStringFile vkeyPath (textEnvelopeToJSON (Just Key.drepKeyEnvelopeDescr) vkey)
  where
    skeyDesc :: TextEnvelopeDescr
    skeyDesc = "Delegate Representative Signing Key"

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

runGovernanceDRepRegistrationCertificateCmd :: ()
  => ConwayEraOnwards era
  -> VerificationKeyOrHashOrFile DRepKey
  -> Lovelace
  -> Maybe (Ledger.Anchor (Ledger.EraCrypto (ShelleyLedgerEra era)))
  -> File () Out
  -> ExceptT RegistrationError IO ()
runGovernanceDRepRegistrationCertificateCmd cOnwards drepKOrHOrF deposit anchor outfp = do
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

runGovernanceDRepRetirementCertificateCmd :: ()
  => ConwayEraOnwards era
  -> VerificationKeyOrHashOrFile DRepKey
  -> Lovelace
  -> File () 'Out
  -> ExceptT GovernanceCmdError IO ()
runGovernanceDRepRetirementCertificateCmd w vKeyOrHashOrFile deposit outFile =
   conwayEraOnwardsConstraints w $ do
     DRepKeyHash drepKeyHash <- firstExceptT GovernanceCmdKeyReadError
       . newExceptT
       $ readVerificationKeyOrHashOrFile AsDRepKey vKeyOrHashOrFile
     makeDrepUnregistrationCertificate (DRepUnregistrationRequirements w (VotingCredential $ KeyHashObj drepKeyHash) deposit)
      & writeFileTextEnvelope outFile (Just genKeyDelegCertDesc)
      & firstExceptT GovernanceCmdTextEnvWriteError . newExceptT

  where
    genKeyDelegCertDesc :: TextEnvelopeDescr
    genKeyDelegCertDesc = "DRep Retirement Certificate"

runGovernanceDRepMetadataHashCmd :: ()
  => DRepMetadataFile In
  -> Maybe (File () Out)
  -> ExceptT GovernanceCmdError IO ()
runGovernanceDRepMetadataHashCmd drepMDPath mOutFile = do
  metadataBytes <- firstExceptT ReadFileError $ newExceptT (readByteStringFile drepMDPath)
  (_metadata, metadataHash) <-
    firstExceptT GovernanceCmdDRepMetadataValidationError
     . hoistEither
     $ validateAndHashDRepMetadata metadataBytes
  firstExceptT WriteFileError
    . newExceptT
    . writeByteStringOutput mOutFile
    . serialiseToRawBytesHex
    $ metadataHash
