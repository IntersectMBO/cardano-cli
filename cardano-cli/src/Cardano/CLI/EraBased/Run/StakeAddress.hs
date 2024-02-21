{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- HLINT ignore "Monad law, left identity" -}

module Cardano.CLI.EraBased.Run.StakeAddress
  ( runStakeAddressCmds

  , runStakeAddressBuildCmd
  , runStakeAddressKeyGenCmd
  , runStakeAddressKeyHashCmd
  , runStakeAddressStakeDelegationCertificateCmd
  , runStakeAddressDeregistrationCertificateCmd
  , runStakeAddressRegistrationCertificateCmd
  ) where

import           Cardano.Api
import qualified Cardano.Api.Ledger as Ledger
import           Cardano.Api.Shelley

import           Cardano.CLI.EraBased.Commands.StakeAddress
import qualified Cardano.CLI.EraBased.Run.Key as Key
import           Cardano.CLI.Read
import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Errors.StakeAddressCmdError
import           Cardano.CLI.Types.Errors.StakeAddressRegistrationError
import           Cardano.CLI.Types.Governance
import           Cardano.CLI.Types.Key

import qualified Data.ByteString.Char8 as BS
import           Data.Function ((&))
import qualified Data.Text.IO as Text

runStakeAddressCmds :: ()
  => StakeAddressCmds era
  -> ExceptT StakeAddressCmdError IO ()
runStakeAddressCmds = \case
  StakeAddressKeyGenCmd _ fmt vk sk ->
    runStakeAddressKeyGenCmd fmt vk sk
  StakeAddressKeyHashCmd _ vk mOutputFp ->
    runStakeAddressKeyHashCmd vk mOutputFp
  StakeAddressBuildCmd _ stakeVerifier nw mOutputFp ->
    runStakeAddressBuildCmd stakeVerifier nw mOutputFp
  StakeAddressRegistrationCertificateCmd sbe stakeIdentifier mDeposit outputFp ->
    runStakeAddressRegistrationCertificateCmd sbe stakeIdentifier mDeposit outputFp
  StakeAddressStakeDelegationCertificateCmd sbe stakeIdentifier stkPoolVerKeyHashOrFp outputFp ->
    runStakeAddressStakeDelegationCertificateCmd sbe stakeIdentifier stkPoolVerKeyHashOrFp outputFp
  StakeAddressStakeAndVoteDelegationCertificateCmd w stakeIdentifier stakePoolVerificationKeyHashSource voteDelegationTarget outputFp ->
    runStakeAddressStakeAndVoteDelegationCertificateCmd w stakeIdentifier stakePoolVerificationKeyHashSource voteDelegationTarget outputFp
  StakeAddressVoteDelegationCertificateCmd w stakeIdentifier voteDelegationTarget outputFp ->
    runStakeAddressVoteDelegationCertificateCmd w stakeIdentifier voteDelegationTarget outputFp
  StakeAddressDeregistrationCertificateCmd sbe stakeIdentifier mDeposit outputFp ->
    runStakeAddressDeregistrationCertificateCmd sbe stakeIdentifier mDeposit outputFp

runStakeAddressKeyGenCmd :: ()
  => KeyOutputFormat
  -> VerificationKeyFile Out
  -> SigningKeyFile Out
  -> ExceptT StakeAddressCmdError IO ()
runStakeAddressKeyGenCmd fmt vkFp skFp = do
  let skeyDesc = "Stake Signing Key"

  skey <- liftIO $ generateSigningKey AsStakeKey

  let vkey = getVerificationKey skey

  firstExceptT StakeAddressCmdWriteFileError $ do
    case fmt of
      KeyOutputFormatTextEnvelope ->
        newExceptT $ writeLazyByteStringFile skFp $ textEnvelopeToJSON (Just skeyDesc) skey
      KeyOutputFormatBech32 ->
        newExceptT $ writeTextFile skFp $ serialiseToBech32 skey

    case fmt of
      KeyOutputFormatTextEnvelope ->
        newExceptT $ writeLazyByteStringFile vkFp $ textEnvelopeToJSON (Just Key.stakeVkeyDesc) vkey
      KeyOutputFormatBech32 ->
        newExceptT $ writeTextFile vkFp $ serialiseToBech32 vkey

runStakeAddressKeyHashCmd :: ()
  => VerificationKeyOrFile StakeKey
  -> Maybe (File () Out)
  -> ExceptT StakeAddressCmdError IO ()
runStakeAddressKeyHashCmd stakeVerKeyOrFile mOutputFp = do
  vkey <- firstExceptT StakeAddressCmdReadKeyFileError
    . newExceptT
    $ readVerificationKeyOrFile AsStakeKey stakeVerKeyOrFile

  let hexKeyHash = serialiseToRawBytesHex (verificationKeyHash vkey)

  case mOutputFp of
    Just (File fpath) -> liftIO $ BS.writeFile fpath hexKeyHash
    Nothing -> liftIO $ BS.putStrLn hexKeyHash

runStakeAddressBuildCmd :: ()
  => StakeVerifier
  -> NetworkId
  -> Maybe (File () Out)
  -> ExceptT StakeAddressCmdError IO ()
runStakeAddressBuildCmd stakeVerifier network mOutputFp = do
  stakeAddr <-
    getStakeAddressFromVerifier network stakeVerifier
      & firstExceptT StakeAddressCmdStakeCredentialError
  let stakeAddrText = serialiseAddress stakeAddr
  liftIO $
    case mOutputFp of
      Just (File fpath) -> Text.writeFile fpath stakeAddrText
      Nothing -> Text.putStrLn stakeAddrText


runStakeAddressRegistrationCertificateCmd :: ()
  => ShelleyBasedEra era
  -> StakeIdentifier
  -> Maybe Lovelace -- ^ Deposit required in conway era
  -> File () Out
  -> ExceptT StakeAddressCmdError IO ()
runStakeAddressRegistrationCertificateCmd sbe stakeIdentifier mDeposit oFp = do
  stakeCred <-
    getStakeCredentialFromIdentifier stakeIdentifier
      & firstExceptT StakeAddressCmdStakeCredentialError

  req <- firstExceptT StakeAddressCmdRegistrationError
           . hoistEither $ createRegistrationCertRequirements sbe stakeCred mDeposit

  let regCert = makeStakeAddressRegistrationCertificate req

  firstExceptT StakeAddressCmdWriteFileError
    . newExceptT
    $ writeLazyByteStringFile oFp
    $ shelleyBasedEraConstraints sbe
    $ textEnvelopeToJSON (Just regCertDesc) regCert

  where
    regCertDesc :: TextEnvelopeDescr
    regCertDesc = "Stake Address Registration Certificate"

createRegistrationCertRequirements :: ()
  => ShelleyBasedEra era
  -> StakeCredential
  -> Maybe Lovelace -- ^ Deposit required in conway era
  -> Either StakeAddressRegistrationError (StakeAddressRequirements era)
createRegistrationCertRequirements sbe stakeCred mdeposit =
  case sbe of
    ShelleyBasedEraShelley ->
      return $ StakeAddrRegistrationPreConway ShelleyToBabbageEraShelley stakeCred
    ShelleyBasedEraAllegra ->
      return $ StakeAddrRegistrationPreConway ShelleyToBabbageEraAllegra stakeCred
    ShelleyBasedEraMary ->
      return $ StakeAddrRegistrationPreConway ShelleyToBabbageEraMary stakeCred
    ShelleyBasedEraAlonzo ->
      return $ StakeAddrRegistrationPreConway ShelleyToBabbageEraAlonzo stakeCred
    ShelleyBasedEraBabbage ->
      return $ StakeAddrRegistrationPreConway ShelleyToBabbageEraBabbage stakeCred
    ShelleyBasedEraConway ->
      case mdeposit of
        Nothing ->
          -- This case is made impossible by the parser, that distinguishes between Conway
          -- and pre-Conway.
          Left StakeAddressRegistrationDepositRequired
        Just dep ->
          return $ StakeAddrRegistrationConway ConwayEraOnwardsConway dep stakeCred

runStakeAddressStakeDelegationCertificateCmd :: ()
  => ShelleyBasedEra era
  -> StakeIdentifier
  -- ^ Delegator stake verification key, verification key file or script file.
  -> VerificationKeyOrHashOrFile StakePoolKey
  -- ^ Delegatee stake pool verification key or verification key file or
  -- verification key hash.
  -> File () Out
  -> ExceptT StakeAddressCmdError IO ()
runStakeAddressStakeDelegationCertificateCmd sbe stakeVerifier poolVKeyOrHashOrFile outFp =
  shelleyBasedEraConstraints sbe $ do
    poolStakeVKeyHash <-
      lift (readVerificationKeyOrHashOrFile AsStakePoolKey poolVKeyOrHashOrFile)
        & onLeft (left . StakeAddressCmdReadKeyFileError)

    stakeCred <-
      getStakeCredentialFromIdentifier stakeVerifier
        & firstExceptT StakeAddressCmdStakeCredentialError

    let certificate = createStakeDelegationCertificate stakeCred poolStakeVKeyHash sbe

    firstExceptT StakeAddressCmdWriteFileError
      . newExceptT
      $ writeLazyByteStringFile outFp
      $ textEnvelopeToJSON (Just @TextEnvelopeDescr "Stake Delegation Certificate") certificate

runStakeAddressStakeAndVoteDelegationCertificateCmd :: ()
  => ConwayEraOnwards era
  -> StakeIdentifier
  -- ^ Delegator stake verification key, verification key file or script file.
  -> VerificationKeyOrHashOrFile StakePoolKey
  -- ^ Delegatee stake pool verification key or verification key file or
  -> VoteDelegationTarget
  -- verification key hash.
  -> File () Out
  -> ExceptT StakeAddressCmdError IO ()
runStakeAddressStakeAndVoteDelegationCertificateCmd w stakeVerifier poolVKeyOrHashOrFile voteDelegationTarget outFp =
  conwayEraOnwardsConstraints w $ do
    StakePoolKeyHash poolStakeVKeyHash <-
      lift (readVerificationKeyOrHashOrFile AsStakePoolKey poolVKeyOrHashOrFile)
        & onLeft (left . StakeAddressCmdReadKeyFileError)

    stakeCredential <-
      getStakeCredentialFromIdentifier stakeVerifier
        & firstExceptT StakeAddressCmdStakeCredentialError

    drep <-
      readVoteDelegationTarget voteDelegationTarget
        & firstExceptT StakeAddressCmdDelegationError

    let delegatee = Ledger.DelegStakeVote poolStakeVKeyHash drep

    let certificate =
          ConwayCertificate w
            $ Ledger.mkDelegTxCert (toShelleyStakeCredential stakeCredential) delegatee

    firstExceptT StakeAddressCmdWriteFileError
      . newExceptT
      $ writeLazyByteStringFile outFp
      $ textEnvelopeToJSON (Just @TextEnvelopeDescr "Stake and Vote Delegation Certificate") certificate

runStakeAddressVoteDelegationCertificateCmd :: ()
  => ConwayEraOnwards era
  -> StakeIdentifier
  -- ^ Delegatee stake pool verification key or verification key file or
  -> VoteDelegationTarget
  -- ^ Delegatee stake pool verification key or verification key file or verification key hash.
  -> File () Out
  -> ExceptT StakeAddressCmdError IO ()
runStakeAddressVoteDelegationCertificateCmd w stakeVerifier voteDelegationTarget outFp =
  conwayEraOnwardsConstraints w $ do
    stakeCredential <-
      getStakeCredentialFromIdentifier stakeVerifier
        & firstExceptT StakeAddressCmdStakeCredentialError

    drep <-
      readVoteDelegationTarget voteDelegationTarget
        & firstExceptT StakeAddressCmdDelegationError

    let delegatee = Ledger.DelegVote drep

    let certificate =
          ConwayCertificate w
            $ Ledger.mkDelegTxCert (toShelleyStakeCredential stakeCredential) delegatee

    firstExceptT StakeAddressCmdWriteFileError
      . newExceptT
      $ writeLazyByteStringFile outFp
      $ textEnvelopeToJSON (Just @TextEnvelopeDescr "Vote Delegation Certificate") certificate

createStakeDelegationCertificate :: forall era. ()
  => StakeCredential
  -> Hash StakePoolKey
  -> ShelleyBasedEra era
  -> Certificate era
createStakeDelegationCertificate stakeCredential (StakePoolKeyHash poolStakeVKeyHash) = do
  caseShelleyToBabbageOrConwayEraOnwards
    (\w ->
      shelleyToBabbageEraConstraints w
        $ ShelleyRelatedCertificate w
        $ Ledger.mkDelegStakeTxCert (toShelleyStakeCredential stakeCredential) poolStakeVKeyHash)
    (\w ->
      conwayEraOnwardsConstraints w
        $ ConwayCertificate w
        $ Ledger.mkDelegTxCert (toShelleyStakeCredential stakeCredential) (Ledger.DelegStake poolStakeVKeyHash)
    )

runStakeAddressDeregistrationCertificateCmd :: ()
  => ShelleyBasedEra era
  -> StakeIdentifier
  -> Maybe Lovelace -- ^ Deposit required in conway era
  -> File () Out
  -> ExceptT StakeAddressCmdError IO ()
runStakeAddressDeregistrationCertificateCmd sbe stakeVerifier mDeposit oFp = do
  stakeCred <-
    getStakeCredentialFromIdentifier stakeVerifier
      & firstExceptT StakeAddressCmdStakeCredentialError

  req <- firstExceptT StakeAddressCmdRegistrationError
           . hoistEither $ createRegistrationCertRequirements sbe stakeCred mDeposit

  let deRegCert = makeStakeAddressUnregistrationCertificate req

  firstExceptT StakeAddressCmdWriteFileError
    . newExceptT
    $ writeLazyByteStringFile oFp
    $ shelleyBasedEraConstraints sbe
    $ textEnvelopeToJSON (Just deregCertDesc) deRegCert

  where
    deregCertDesc :: TextEnvelopeDescr
    deregCertDesc = "Stake Address Deregistration Certificate"
