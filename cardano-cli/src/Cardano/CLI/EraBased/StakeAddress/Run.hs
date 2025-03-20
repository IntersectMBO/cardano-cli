{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- HLINT ignore "Monad law, left identity" -}

module Cardano.CLI.EraBased.StakeAddress.Run
  ( runStakeAddressCmds
  , runStakeAddressBuildCmd
  , runStakeAddressKeyGenCmd
  , runStakeAddressKeyHashCmd
  , runStakeAddressStakeDelegationCertificateCmd
  , runStakeAddressDeregistrationCertificateCmd
  , runStakeAddressRegistrationCertificateCmd
  , runStakeAddressRegistrationAndDelegationCertificateCmd
  , runStakeAddressRegistrationStakeAndVoteDelegationCertificateCmd
  )
where

import Cardano.Api
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Shelley

import Cardano.CLI.EraBased.StakeAddress.Command
import Cardano.CLI.EraIndependent.Key.Run qualified as Key
import Cardano.CLI.Read
import Cardano.CLI.Type.Common
import Cardano.CLI.Type.Error.StakeAddressCmdError
import Cardano.CLI.Type.Error.StakeAddressRegistrationError
import Cardano.CLI.Type.Governance
import Cardano.CLI.Type.Key

import Control.Monad (void)
import Data.ByteString.Char8 qualified as BS
import Data.Function ((&))
import Data.Text.IO qualified as Text

runStakeAddressCmds
  :: ()
  => StakeAddressCmds era
  -> ExceptT StakeAddressCmdError IO ()
runStakeAddressCmds = \case
  StakeAddressKeyGenCmd fmt vk sk ->
    void $ runStakeAddressKeyGenCmd fmt vk sk
  StakeAddressKeyHashCmd vk mOutputFp ->
    runStakeAddressKeyHashCmd vk mOutputFp
  StakeAddressBuildCmd stakeVerifier nw mOutputFp ->
    runStakeAddressBuildCmd stakeVerifier nw mOutputFp
  StakeAddressRegistrationCertificateCmd sbe stakeIdentifier mDeposit outputFp ->
    runStakeAddressRegistrationCertificateCmd sbe stakeIdentifier mDeposit outputFp
  StakeAddressStakeDelegationCertificateCmd sbe stakeIdentifier stkPoolVerKeyHashOrFp outputFp ->
    runStakeAddressStakeDelegationCertificateCmd sbe stakeIdentifier stkPoolVerKeyHashOrFp outputFp
  StakeAddressStakeAndVoteDelegationCertificateCmd
    w
    stakeIdentifier
    stakePoolVerificationKeyHashSource
    voteDelegationTarget
    outputFp ->
      runStakeAddressStakeAndVoteDelegationCertificateCmd
        w
        stakeIdentifier
        stakePoolVerificationKeyHashSource
        voteDelegationTarget
        outputFp
  StakeAddressVoteDelegationCertificateCmd w stakeIdentifier voteDelegationTarget outputFp ->
    runStakeAddressVoteDelegationCertificateCmd w stakeIdentifier voteDelegationTarget outputFp
  StakeAddressDeregistrationCertificateCmd sbe stakeIdentifier mDeposit outputFp ->
    runStakeAddressDeregistrationCertificateCmd sbe stakeIdentifier mDeposit outputFp
  StakeAddressRegistrationAndDelegationCertificateCmd
    w
    stakeIdentifier
    poolVKeyOrHashOrFile
    deposit
    outFp ->
      runStakeAddressRegistrationAndDelegationCertificateCmd
        w
        stakeIdentifier
        poolVKeyOrHashOrFile
        deposit
        outFp
  StakeAddressRegistrationAndVoteDelegationCertificateCmd
    w
    stakeIdentifier
    voteDelegationTarget
    keydeposit
    outFp ->
      runStakeAddressRegistrationAndVoteDelegationCertificateCmd
        w
        stakeIdentifier
        voteDelegationTarget
        keydeposit
        outFp
  StakeAddressRegistrationStakeAndVoteDelegationCertificateCmd
    w
    stakeIdentifier
    poolVKeyOrHashOrFile
    voteDelegationTarget
    keydeposit
    outFp ->
      runStakeAddressRegistrationStakeAndVoteDelegationCertificateCmd
        w
        stakeIdentifier
        poolVKeyOrHashOrFile
        voteDelegationTarget
        keydeposit
        outFp

runStakeAddressKeyGenCmd
  :: ()
  => KeyOutputFormat
  -> VerificationKeyFile Out
  -> SigningKeyFile Out
  -> ExceptT StakeAddressCmdError IO (VerificationKey StakeKey, SigningKey StakeKey)
runStakeAddressKeyGenCmd fmt vkFp skFp = do
  let skeyDesc = "Stake Signing Key"

  skey <- generateSigningKey AsStakeKey

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
  return (vkey, skey)

runStakeAddressKeyHashCmd
  :: ()
  => VerificationKeyOrFile StakeKey
  -> Maybe (File () Out)
  -> ExceptT StakeAddressCmdError IO ()
runStakeAddressKeyHashCmd stakeVerKeyOrFile mOutputFp = do
  vkey <-
    modifyError StakeAddressCmdReadKeyFileError $
      readVerificationKeyOrFile AsStakeKey stakeVerKeyOrFile

  let hexKeyHash = serialiseToRawBytesHex (verificationKeyHash vkey)

  case mOutputFp of
    Just (File fpath) -> liftIO $ BS.writeFile fpath hexKeyHash
    Nothing -> liftIO $ BS.putStrLn hexKeyHash

runStakeAddressBuildCmd
  :: ()
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

runStakeAddressRegistrationCertificateCmd
  :: ()
  => ShelleyBasedEra era
  -> StakeIdentifier
  -> Maybe (Featured ConwayEraOnwards era Lovelace)
  -- ^ Deposit required in conway era
  -> File () Out
  -> ExceptT StakeAddressCmdError IO ()
runStakeAddressRegistrationCertificateCmd sbe stakeIdentifier mDeposit oFp = do
  stakeCred <-
    getStakeCredentialFromIdentifier stakeIdentifier
      & firstExceptT StakeAddressCmdStakeCredentialError

  req <-
    firstExceptT StakeAddressCmdRegistrationError
      . hoistEither
      $ createRegistrationCertRequirements sbe stakeCred mDeposit

  let regCert = makeStakeAddressRegistrationCertificate req

  firstExceptT StakeAddressCmdWriteFileError
    . newExceptT
    $ writeLazyByteStringFile oFp
    $ shelleyBasedEraConstraints sbe
    $ textEnvelopeToJSON (Just regCertDesc) regCert
 where
  regCertDesc :: TextEnvelopeDescr
  regCertDesc = "Stake Address Registration Certificate"

createRegistrationCertRequirements
  :: ()
  => ShelleyBasedEra era
  -> StakeCredential
  -> Maybe (Featured ConwayEraOnwards era Lovelace)
  -- ^ Deposit required in conway era
  -> Either StakeAddressRegistrationError (StakeAddressRequirements era)
createRegistrationCertRequirements sbe stakeCred mDeposit =
  caseShelleyToBabbageOrConwayEraOnwards
    (\stb -> pure $ StakeAddrRegistrationPreConway stb stakeCred)
    ( \ceo -> do
        case mDeposit of
          Nothing ->
            -- This case is made impossible by the parser, that distinguishes between Conway
            -- and pre-Conway.
            throwError StakeAddressRegistrationDepositRequired
          Just (Featured _ dep) ->
            pure $ StakeAddrRegistrationConway ceo dep stakeCred
    )
    sbe

runStakeAddressStakeDelegationCertificateCmd
  :: ()
  => ShelleyBasedEra era
  -> StakeIdentifier
  -- ^ Delegator stake verification key, verification key file or script file.
  -> StakePoolKeyHashSource
  -- ^ Delegatee stake pool verification key or verification key file or
  -- verification key hash.
  -> File () Out
  -> ExceptT StakeAddressCmdError IO ()
runStakeAddressStakeDelegationCertificateCmd sbe stakeVerifier poolVKeyOrHashOrFile outFp =
  shelleyBasedEraConstraints sbe $ do
    poolStakeVKeyHash <- getHashFromStakePoolKeyHashSource poolVKeyOrHashOrFile

    stakeCred <-
      getStakeCredentialFromIdentifier stakeVerifier
        & firstExceptT StakeAddressCmdStakeCredentialError

    let certificate = createStakeDelegationCertificate stakeCred poolStakeVKeyHash sbe

    firstExceptT StakeAddressCmdWriteFileError
      . newExceptT
      $ writeLazyByteStringFile outFp
      $ textEnvelopeToJSON (Just @TextEnvelopeDescr "Stake Delegation Certificate") certificate

runStakeAddressStakeAndVoteDelegationCertificateCmd
  :: ()
  => ConwayEraOnwards era
  -> StakeIdentifier
  -- ^ Delegator stake verification key, verification key file or script file.
  -> StakePoolKeyHashSource
  -- ^ Delegatee stake pool verification key or verification key file or
  -> VoteDelegationTarget
  -- verification key hash.
  -> File () Out
  -> ExceptT StakeAddressCmdError IO ()
runStakeAddressStakeAndVoteDelegationCertificateCmd w stakeVerifier poolVKeyOrHashOrFile voteDelegationTarget outFp =
  conwayEraOnwardsConstraints w $ do
    StakePoolKeyHash poolStakeVKeyHash <- getHashFromStakePoolKeyHashSource poolVKeyOrHashOrFile

    stakeCredential <-
      modifyError StakeAddressCmdStakeCredentialError $
        getStakeCredentialFromIdentifier stakeVerifier

    drep <-
      readVoteDelegationTarget voteDelegationTarget
        & firstExceptT StakeAddressCmdDelegationError

    let delegatee = L.DelegStakeVote poolStakeVKeyHash drep

    let certificate =
          ConwayCertificate w $
            L.mkDelegTxCert (toShelleyStakeCredential stakeCredential) delegatee

    firstExceptT StakeAddressCmdWriteFileError
      . newExceptT
      $ writeLazyByteStringFile outFp
      $ textEnvelopeToJSON (Just @TextEnvelopeDescr "Stake and Vote Delegation Certificate") certificate

runStakeAddressVoteDelegationCertificateCmd
  :: ()
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

    let delegatee = L.DelegVote drep

    let certificate =
          ConwayCertificate w $
            L.mkDelegTxCert (toShelleyStakeCredential stakeCredential) delegatee

    firstExceptT StakeAddressCmdWriteFileError
      . newExceptT
      $ writeLazyByteStringFile outFp
      $ textEnvelopeToJSON (Just @TextEnvelopeDescr "Vote Delegation Certificate") certificate

createStakeDelegationCertificate
  :: forall era
   . ()
  => StakeCredential
  -> Hash StakePoolKey
  -> ShelleyBasedEra era
  -> Certificate era
createStakeDelegationCertificate stakeCredential (StakePoolKeyHash poolStakeVKeyHash) = do
  caseShelleyToBabbageOrConwayEraOnwards
    ( \w ->
        shelleyToBabbageEraConstraints w $
          ShelleyRelatedCertificate w $
            L.mkDelegStakeTxCert (toShelleyStakeCredential stakeCredential) poolStakeVKeyHash
    )
    ( \w ->
        conwayEraOnwardsConstraints w $
          ConwayCertificate w $
            L.mkDelegTxCert (toShelleyStakeCredential stakeCredential) (L.DelegStake poolStakeVKeyHash)
    )

runStakeAddressDeregistrationCertificateCmd
  :: ()
  => ShelleyBasedEra era
  -> StakeIdentifier
  -> Maybe (Featured ConwayEraOnwards era Lovelace)
  -- ^ Deposit required in conway era
  -> File () Out
  -> ExceptT StakeAddressCmdError IO ()
runStakeAddressDeregistrationCertificateCmd sbe stakeVerifier mDeposit oFp = do
  stakeCred <-
    getStakeCredentialFromIdentifier stakeVerifier
      & firstExceptT StakeAddressCmdStakeCredentialError

  req <-
    firstExceptT StakeAddressCmdRegistrationError
      . hoistEither
      $ createRegistrationCertRequirements sbe stakeCred mDeposit

  let deRegCert = makeStakeAddressUnregistrationCertificate req

  firstExceptT StakeAddressCmdWriteFileError
    . newExceptT
    $ writeLazyByteStringFile oFp
    $ shelleyBasedEraConstraints sbe
    $ textEnvelopeToJSON (Just deregCertDesc) deRegCert
 where
  deregCertDesc :: TextEnvelopeDescr
  deregCertDesc = "Stake Address Deregistration Certificate"

runStakeAddressRegistrationAndDelegationCertificateCmd
  :: ()
  => ConwayEraOnwards era
  -> StakeIdentifier
  -> StakePoolKeyHashSource
  -- ^ Delegatee stake pool verification key or verification key file or id
  -> Lovelace
  -> File () Out
  -> ExceptT StakeAddressCmdError IO ()
runStakeAddressRegistrationAndDelegationCertificateCmd w stakeVerifier poolVKeyOrHashOrFile deposit outFp =
  conwayEraOnwardsConstraints w $ do
    StakePoolKeyHash poolStakeVKeyHash <- getHashFromStakePoolKeyHashSource poolVKeyOrHashOrFile

    stakeCred <-
      getStakeCredentialFromIdentifier stakeVerifier
        & firstExceptT StakeAddressCmdStakeCredentialError

    let delegatee = L.DelegStake poolStakeVKeyHash

    let certificate = makeStakeAddressAndDRepDelegationCertificate w stakeCred delegatee deposit

    firstExceptT StakeAddressCmdWriteFileError
      . newExceptT
      $ writeLazyByteStringFile outFp
      $ textEnvelopeToJSON
        (Just @TextEnvelopeDescr "Stake address registration and stake delegation certificate")
        certificate

runStakeAddressRegistrationAndVoteDelegationCertificateCmd
  :: ()
  => ConwayEraOnwards era
  -> StakeIdentifier
  -> VoteDelegationTarget
  -> Lovelace
  -> File () Out
  -> ExceptT StakeAddressCmdError IO ()
runStakeAddressRegistrationAndVoteDelegationCertificateCmd w stakeVerifier voteDelegationTarget keydeposit outFp =
  conwayEraOnwardsConstraints w $ do
    stakeCred <-
      getStakeCredentialFromIdentifier stakeVerifier
        & firstExceptT StakeAddressCmdStakeCredentialError

    drep <-
      readVoteDelegationTarget voteDelegationTarget
        & firstExceptT StakeAddressCmdDelegationError

    let delegatee = L.DelegVote drep

    let certificate = makeStakeAddressAndDRepDelegationCertificate w stakeCred delegatee keydeposit

    firstExceptT StakeAddressCmdWriteFileError
      . newExceptT
      $ writeLazyByteStringFile outFp
      $ textEnvelopeToJSON
        (Just @TextEnvelopeDescr "Stake address registration and vote delegation certificate")
        certificate

runStakeAddressRegistrationStakeAndVoteDelegationCertificateCmd
  :: ()
  => ConwayEraOnwards era
  -> StakeIdentifier
  -> StakePoolKeyHashSource
  -> VoteDelegationTarget
  -> Lovelace
  -> File () Out
  -> ExceptT StakeAddressCmdError IO ()
runStakeAddressRegistrationStakeAndVoteDelegationCertificateCmd w stakeVerifier poolVKeyOrHashOrFile voteDelegationTarget keydeposit outFp =
  conwayEraOnwardsConstraints w $ do
    StakePoolKeyHash poolStakeVKeyHash <- getHashFromStakePoolKeyHashSource poolVKeyOrHashOrFile

    stakeCred <-
      getStakeCredentialFromIdentifier stakeVerifier
        & firstExceptT StakeAddressCmdStakeCredentialError

    drep <-
      readVoteDelegationTarget voteDelegationTarget
        & firstExceptT StakeAddressCmdDelegationError

    let delegatee = L.DelegStakeVote poolStakeVKeyHash drep

    let certificate = makeStakeAddressAndDRepDelegationCertificate w stakeCred delegatee keydeposit

    firstExceptT StakeAddressCmdWriteFileError
      . newExceptT
      $ writeLazyByteStringFile outFp
      $ textEnvelopeToJSON
        (Just @TextEnvelopeDescr "Stake address registration and vote delegation certificate")
        certificate
