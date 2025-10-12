{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

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
import Cardano.Api.Experimental (obtainCommonConstraints)
import Cardano.Api.Experimental qualified as Exp
import Cardano.Api.Ledger qualified as L

import Cardano.CLI.Compatible.Exception
import Cardano.CLI.EraBased.StakeAddress.Command
import Cardano.CLI.EraIndependent.Key.Run qualified as Key
import Cardano.CLI.Orphan ()
import Cardano.CLI.Read
import Cardano.CLI.Type.Common
import Cardano.CLI.Type.Error.StakeAddressRegistrationError
import Cardano.CLI.Type.Governance
import Cardano.CLI.Type.Key

import RIO

import Data.ByteString.Char8 qualified as BS
import Data.Text.IO qualified as Text
import Vary (Vary)
import Vary qualified

runStakeAddressCmds
  :: ()
  => forall era e
   . StakeAddressCmds era
  -> CIO e ()
runStakeAddressCmds = \case
  StakeAddressKeyGenCmd fmt vk sk ->
    void $ runStakeAddressKeyGenCmd fmt vk sk
  StakeAddressKeyHashCmd vk mOutputFp ->
    runStakeAddressKeyHashCmd vk mOutputFp
  StakeAddressBuildCmd stakeVerifier nw mOutputFp ->
    runStakeAddressBuildCmd stakeVerifier nw mOutputFp
  StakeAddressRegistrationCertificateCmd era stakeIdentifier mDeposit outputFp ->
    Exp.obtainCommonConstraints era $
      runStakeAddressRegistrationCertificateCmd stakeIdentifier mDeposit outputFp
  StakeAddressStakeDelegationCertificateCmd
    era
    stakeIdentifier
    stkPoolVerKeyHashOrFp
    outputFp ->
      Exp.obtainCommonConstraints era $
        runStakeAddressStakeDelegationCertificateCmd
          era
          stakeIdentifier
          stkPoolVerKeyHashOrFp
          outputFp
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
  StakeAddressDeregistrationCertificateCmd era stakeIdentifier mDeposit outputFp ->
    obtainCommonConstraints era $
      runStakeAddressDeregistrationCertificateCmd stakeIdentifier mDeposit outputFp
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
  :: Vary [FormatBech32, FormatTextEnvelope]
  -> VerificationKeyFile Out
  -> SigningKeyFile Out
  -> CIO e (VerificationKey StakeKey, SigningKey StakeKey)
runStakeAddressKeyGenCmd fmt vkFp skFp = do
  let skeyDesc = "Stake Signing Key"

  skey <- generateSigningKey AsStakeKey

  let vkey = getVerificationKey skey

  void $
    fmt
      & ( id
            . Vary.on
              ( \FormatBech32 ->
                  writeTextFile skFp $ serialiseToBech32 skey
              )
            . Vary.on
              ( \FormatTextEnvelope ->
                  writeLazyByteStringFile skFp $ textEnvelopeToJSON (Just skeyDesc) skey
              )
            $ Vary.exhaustiveCase
        )

  void $
    fmt
      & ( id
            . Vary.on
              ( \FormatBech32 ->
                  writeTextFile vkFp $ serialiseToBech32 vkey
              )
            . Vary.on
              ( \FormatTextEnvelope ->
                  writeLazyByteStringFile vkFp $ textEnvelopeToJSON (Just Key.stakeVkeyDesc) vkey
              )
            $ Vary.exhaustiveCase
        )

  return (vkey, skey)

runStakeAddressKeyHashCmd
  :: ()
  => VerificationKeyOrFile StakeKey
  -> Maybe (File () Out)
  -> CIO e ()
runStakeAddressKeyHashCmd stakeVerKeyOrFile mOutputFp = do
  vkey <-
    readVerificationKeyOrFile stakeVerKeyOrFile

  let hexKeyHash = serialiseToRawBytesHex (verificationKeyHash vkey)

  case mOutputFp of
    Just (File fpath) -> liftIO $ BS.writeFile fpath hexKeyHash
    Nothing -> liftIO $ BS.putStrLn hexKeyHash

runStakeAddressBuildCmd
  :: ()
  => StakeVerifier
  -> NetworkId
  -> Maybe (File () Out)
  -> CIO e ()
runStakeAddressBuildCmd stakeVerifier network mOutputFp = do
  stakeAddr <-
    getStakeAddressFromVerifier network stakeVerifier
  let stakeAddrText = serialiseAddress stakeAddr
  liftIO $
    case mOutputFp of
      Just (File fpath) -> Text.writeFile fpath stakeAddrText
      Nothing -> Text.putStrLn stakeAddrText

runStakeAddressRegistrationCertificateCmd
  :: forall era e
   . Exp.IsEra era
  => StakeIdentifier
  -> Maybe (Featured ConwayEraOnwards era Lovelace)
  -- ^ Deposit required in conway era
  -> File () Out
  -> CIO e ()
runStakeAddressRegistrationCertificateCmd stakeIdentifier mDeposit oFp = do
  let sbe = convert $ Exp.useEra @era
  stakeCred <-
    getStakeCredentialFromIdentifier stakeIdentifier

  req <-
    fromEitherCli $
      createRegistrationCertRequirements stakeCred mDeposit

  let regCert = makeStakeAddressRegistrationCertificate req

  fromEitherIOCli @(FileError ()) $
    writeLazyByteStringFile oFp $
      shelleyBasedEraConstraints sbe $
        textEnvelopeToJSON (Just regCertDesc) regCert
 where
  regCertDesc :: TextEnvelopeDescr
  regCertDesc = "Stake Address Registration Certificate"

createRegistrationCertRequirements
  :: Exp.IsEra era
  => StakeCredential
  -> Maybe (Featured ConwayEraOnwards era Lovelace)
  -- ^ Deposit required in conway era
  -> Either StakeAddressRegistrationError (StakeAddressRequirements era)
createRegistrationCertRequirements stakeCred mDeposit =
  do
    case mDeposit of
      Nothing ->
        -- This case is made impossible by the parser, that distinguishes between Conway
        -- and pre-Conway.
        throwError StakeAddressRegistrationDepositRequired
      Just (Featured _ dep) ->
        pure $ StakeAddrRegistrationConway (convert Exp.useEra) dep stakeCred

runStakeAddressStakeDelegationCertificateCmd
  :: forall era e
   . Exp.IsEra era
  => Exp.Era era
  -> StakeIdentifier
  -- ^ Delegator stake verification key, verification key file or script file.
  -> StakePoolKeyHashSource
  -- ^ Delegatee stake pool verification key or verification key file or
  -- verification key hash.
  -> File () Out
  -> CIO e ()
runStakeAddressStakeDelegationCertificateCmd era stakeVerifier poolVKeyOrHashOrFile outFp =
  obtainCommonConstraints era $ do
    poolStakeVKeyHash <- getHashFromStakePoolKeyHashSource poolVKeyOrHashOrFile

    stakeCred <-
      getStakeCredentialFromIdentifier stakeVerifier

    let certificate :: Certificate era = createStakeDelegationCertificate stakeCred poolStakeVKeyHash

    fromEitherIOCli @(FileError ()) $
      writeLazyByteStringFile outFp $
        textEnvelopeToJSON (Just @TextEnvelopeDescr "Stake Delegation Certificate") certificate

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
  -> CIO e ()
runStakeAddressStakeAndVoteDelegationCertificateCmd w stakeVerifier poolVKeyOrHashOrFile voteDelegationTarget outFp =
  conwayEraOnwardsConstraints w $ do
    StakePoolKeyHash poolStakeVKeyHash <- getHashFromStakePoolKeyHashSource poolVKeyOrHashOrFile

    stakeCredential <-
      getStakeCredentialFromIdentifier stakeVerifier

    drep <-
      readVoteDelegationTarget voteDelegationTarget

    let delegatee = L.DelegStakeVote poolStakeVKeyHash drep

    let certificate =
          ConwayCertificate w $
            L.mkDelegTxCert (toShelleyStakeCredential stakeCredential) delegatee

    fromEitherIOCli @(FileError ()) $
      writeLazyByteStringFile outFp $
        textEnvelopeToJSON (Just @TextEnvelopeDescr "Stake and Vote Delegation Certificate") certificate

runStakeAddressVoteDelegationCertificateCmd
  :: ()
  => ConwayEraOnwards era
  -> StakeIdentifier
  -- ^ Delegatee stake pool verification key or verification key file or
  -> VoteDelegationTarget
  -- ^ Delegatee stake pool verification key or verification key file or verification key hash.
  -> File () Out
  -> CIO e ()
runStakeAddressVoteDelegationCertificateCmd w stakeVerifier voteDelegationTarget outFp =
  conwayEraOnwardsConstraints w $ do
    stakeCredential <-
      getStakeCredentialFromIdentifier stakeVerifier

    drep <-
      readVoteDelegationTarget voteDelegationTarget

    let delegatee = L.DelegVote drep

    let certificate =
          ConwayCertificate w $
            L.mkDelegTxCert (toShelleyStakeCredential stakeCredential) delegatee

    fromEitherIOCli @(FileError ())
      $ writeLazyByteStringFile
        outFp
      $ textEnvelopeToJSON (Just @TextEnvelopeDescr "Vote Delegation Certificate") certificate

createStakeDelegationCertificate
  :: forall era
   . Exp.IsEra era
  => StakeCredential
  -> Hash StakePoolKey
  -> Certificate era
createStakeDelegationCertificate stakeCredential (StakePoolKeyHash poolStakeVKeyHash) = do
  let w = convert $ Exp.useEra @era
  conwayEraOnwardsConstraints w $
    ConwayCertificate (convert Exp.useEra) $
      L.mkDelegTxCert (toShelleyStakeCredential stakeCredential) (L.DelegStake poolStakeVKeyHash)

runStakeAddressDeregistrationCertificateCmd
  :: forall era e
   . Exp.IsEra era
  => StakeIdentifier
  -> Maybe (Featured ConwayEraOnwards era Lovelace)
  -- ^ Deposit required in conway era
  -> File () Out
  -> CIO e ()
runStakeAddressDeregistrationCertificateCmd stakeVerifier mDeposit oFp = do
  let sbe = convert $ Exp.useEra @era
  stakeCred <-
    getStakeCredentialFromIdentifier stakeVerifier

  req <-
    fromEitherCli $
      createRegistrationCertRequirements stakeCred mDeposit

  let deRegCert = makeStakeAddressUnregistrationCertificate req

  fromEitherIOCli @(FileError ()) $
    writeLazyByteStringFile oFp $
      shelleyBasedEraConstraints sbe $
        textEnvelopeToJSON (Just deregCertDesc) deRegCert
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
  -> CIO e ()
runStakeAddressRegistrationAndDelegationCertificateCmd w stakeVerifier poolVKeyOrHashOrFile deposit outFp =
  conwayEraOnwardsConstraints w $ do
    StakePoolKeyHash poolStakeVKeyHash <- getHashFromStakePoolKeyHashSource poolVKeyOrHashOrFile

    stakeCred <-
      getStakeCredentialFromIdentifier stakeVerifier

    let delegatee = L.DelegStake poolStakeVKeyHash

    let certificate = makeStakeAddressAndDRepDelegationCertificate w stakeCred delegatee deposit

    fromEitherIOCli @(FileError ()) $
      writeLazyByteStringFile outFp $
        textEnvelopeToJSON
          (Just @TextEnvelopeDescr "Stake address registration and stake delegation certificate")
          certificate

runStakeAddressRegistrationAndVoteDelegationCertificateCmd
  :: ()
  => ConwayEraOnwards era
  -> StakeIdentifier
  -> VoteDelegationTarget
  -> Lovelace
  -> File () Out
  -> CIO e ()
runStakeAddressRegistrationAndVoteDelegationCertificateCmd w stakeVerifier voteDelegationTarget keydeposit outFp =
  conwayEraOnwardsConstraints w $ do
    stakeCred <-
      getStakeCredentialFromIdentifier stakeVerifier

    drep <-
      readVoteDelegationTarget voteDelegationTarget

    let delegatee = L.DelegVote drep

    let certificate = makeStakeAddressAndDRepDelegationCertificate w stakeCred delegatee keydeposit

    fromEitherIOCli @(FileError ()) $
      writeLazyByteStringFile outFp $
        textEnvelopeToJSON
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
  -> CIO e ()
runStakeAddressRegistrationStakeAndVoteDelegationCertificateCmd w stakeVerifier poolVKeyOrHashOrFile voteDelegationTarget keydeposit outFp =
  conwayEraOnwardsConstraints w $ do
    StakePoolKeyHash poolStakeVKeyHash <- getHashFromStakePoolKeyHashSource poolVKeyOrHashOrFile

    stakeCred <-
      getStakeCredentialFromIdentifier stakeVerifier

    drep <-
      readVoteDelegationTarget voteDelegationTarget

    let delegatee = L.DelegStakeVote poolStakeVKeyHash drep

    let certificate = makeStakeAddressAndDRepDelegationCertificate w stakeCred delegatee keydeposit

    fromEitherIOCli @(FileError ()) $
      writeLazyByteStringFile outFp $
        textEnvelopeToJSON
          (Just @TextEnvelopeDescr "Stake address registration and vote delegation certificate")
          certificate
