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
import           Cardano.CLI.Read
import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Errors.ShelleyStakeAddressCmdError
import           Cardano.CLI.Types.Errors.DelegationError
import           Cardano.CLI.Types.Errors.StakeAddressRegistrationError
import           Cardano.CLI.Types.Key

import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Trans (lift)
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, hoistEither, left, newExceptT,
                   onLeft)
import qualified Data.ByteString.Char8 as BS
import           Data.Function ((&))
import qualified Data.Text.IO as Text

runStakeAddressCmds :: ()
  => StakeAddressCmds era
  -> ExceptT ShelleyStakeAddressCmdError IO ()
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
  StakeAddressStakeAndVoteDelegationCertificateCmd w stakeIdentifier stakePoolVerificationKeyHashSource drepVerificationKeyHashSource outputFp ->
    runStakeAddressStakeAndVoteDelegationCertificateCmd w stakeIdentifier stakePoolVerificationKeyHashSource drepVerificationKeyHashSource outputFp
  StakeAddressDeregistrationCertificateCmd sbe stakeIdentifier mDeposit outputFp ->
    runStakeAddressDeregistrationCertificateCmd sbe stakeIdentifier mDeposit outputFp

runStakeAddressKeyGenCmd :: ()
  => KeyOutputFormat
  -> VerificationKeyFile Out
  -> SigningKeyFile Out
  -> ExceptT ShelleyStakeAddressCmdError IO ()
runStakeAddressKeyGenCmd fmt vkFp skFp = do
  let skeyDesc = "Stake Signing Key"
  let vkeyDesc = "Stake Verification Key"

  skey <- liftIO $ generateSigningKey AsStakeKey

  let vkey = getVerificationKey skey

  firstExceptT ShelleyStakeAddressCmdWriteFileError $ do
    case fmt of
      KeyOutputFormatTextEnvelope ->
        newExceptT $ writeLazyByteStringFile skFp $ textEnvelopeToJSON (Just skeyDesc) skey
      KeyOutputFormatBech32 ->
        newExceptT $ writeTextFile skFp $ serialiseToBech32 skey

    case fmt of
      KeyOutputFormatTextEnvelope ->
        newExceptT $ writeLazyByteStringFile vkFp $ textEnvelopeToJSON (Just vkeyDesc) vkey
      KeyOutputFormatBech32 ->
        newExceptT $ writeTextFile vkFp $ serialiseToBech32 vkey

runStakeAddressKeyHashCmd :: ()
  => VerificationKeyOrFile StakeKey
  -> Maybe (File () Out)
  -> ExceptT ShelleyStakeAddressCmdError IO ()
runStakeAddressKeyHashCmd stakeVerKeyOrFile mOutputFp = do
  vkey <- firstExceptT ShelleyStakeAddressCmdReadKeyFileError
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
  -> ExceptT ShelleyStakeAddressCmdError IO ()
runStakeAddressBuildCmd stakeVerifier network mOutputFp = do
  stakeAddr <-
    getStakeAddressFromVerifier network stakeVerifier
      & firstExceptT ShelleyStakeAddressCmdStakeCredentialError
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
  -> ExceptT ShelleyStakeAddressCmdError IO ()
runStakeAddressRegistrationCertificateCmd sbe stakeIdentifier mDeposit oFp = do
  stakeCred <-
    getStakeCredentialFromIdentifier stakeIdentifier
      & firstExceptT ShelleyStakeAddressCmdStakeCredentialError

  req <- firstExceptT StakeRegistrationError
           . hoistEither $ createRegistrationCertRequirements sbe stakeCred mDeposit

  let regCert = makeStakeAddressRegistrationCertificate req

  firstExceptT ShelleyStakeAddressCmdWriteFileError
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
  -> Maybe Lovelace
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
        Nothing -> Left StakeAddressRegistrationDepositRequired
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
  -> ExceptT ShelleyStakeAddressCmdError IO ()
runStakeAddressStakeDelegationCertificateCmd sbe stakeVerifier poolVKeyOrHashOrFile outFp =
  shelleyBasedEraConstraints sbe $ do
    poolStakeVKeyHash <-
      lift (readVerificationKeyOrHashOrFile AsStakePoolKey poolVKeyOrHashOrFile)
        & onLeft (left . ShelleyStakeAddressCmdReadKeyFileError)

    stakeCred <-
      getStakeCredentialFromIdentifier stakeVerifier
        & firstExceptT ShelleyStakeAddressCmdStakeCredentialError

    let certificate = createStakeDelegationCertificate stakeCred poolStakeVKeyHash sbe

    firstExceptT ShelleyStakeAddressCmdWriteFileError
      . newExceptT
      $ writeLazyByteStringFile outFp
      $ textEnvelopeToJSON (Just @TextEnvelopeDescr "Stake Address Delegation Certificate") certificate

-- TODO use the version in cardano-api
caseShelleyToBabbageAndConwayEraOnwards :: forall a era. ()
  => (ShelleyToBabbageEra era -> a)
  -> (ConwayEraOnwards era -> a)
  -> ShelleyBasedEra era
  -> a
caseShelleyToBabbageAndConwayEraOnwards l r = \case
  ShelleyBasedEraShelley -> l ShelleyToBabbageEraShelley
  ShelleyBasedEraAllegra -> l ShelleyToBabbageEraAllegra
  ShelleyBasedEraMary    -> l ShelleyToBabbageEraMary
  ShelleyBasedEraAlonzo  -> l ShelleyToBabbageEraAlonzo
  ShelleyBasedEraBabbage -> l ShelleyToBabbageEraBabbage
  ShelleyBasedEraConway  -> r ConwayEraOnwardsConway

runStakeAddressStakeAndVoteDelegationCertificateCmd :: ()
  => ConwayEraOnwards era
  -> StakeIdentifier
  -- ^ Delegator stake verification key, verification key file or script file.
  -> VerificationKeyOrHashOrFile StakePoolKey
  -- ^ Delegatee stake pool verification key or verification key file or
  -> VerificationKeyOrHashOrFile DRepKey
  -- verification key hash.
  -> File () Out
  -> ExceptT ShelleyStakeAddressCmdError IO ()
runStakeAddressStakeAndVoteDelegationCertificateCmd w stakeVerifier poolVKeyOrHashOrFile drepVKeyOrHashOrFile outFp =
  conwayEraOnwardsConstraints w $ do
    StakePoolKeyHash poolStakeVKeyHash <-
      lift (readVerificationKeyOrHashOrFile AsStakePoolKey poolVKeyOrHashOrFile)
        & onLeft (left . ShelleyStakeAddressCmdReadKeyFileError)

    stakeCredential <-
      getStakeCredentialFromIdentifier stakeVerifier
        & firstExceptT ShelleyStakeAddressCmdStakeCredentialError

    DRepKeyHash drepKeyHash <-
      lift (readVerificationKeyOrHashOrTextEnvFile AsDRepKey drepVKeyOrHashOrFile)
        & onLeft (left . StakeAddressDelegationError . DelegationDRepReadError)

    let drepCred = Ledger.DRepCredential $ Ledger.KeyHashObj drepKeyHash

    let delegatee =
          Ledger.DelegStakeVote
                (conwayEraOnwardsConstraints w poolStakeVKeyHash)
                (conwayEraOnwardsConstraints w drepCred)

    let certificate =
          ConwayCertificate w
            $ Ledger.mkDelegTxCert (toShelleyStakeCredential stakeCredential) delegatee

    firstExceptT ShelleyStakeAddressCmdWriteFileError
      . newExceptT
      $ writeLazyByteStringFile outFp
      $ textEnvelopeToJSON (Just @TextEnvelopeDescr "Stake Address Delegation Certificate") certificate

createStakeDelegationCertificate :: forall era. ()
  => StakeCredential
  -> Hash StakePoolKey
  -> ShelleyBasedEra era
  -> Certificate era
createStakeDelegationCertificate stakeCredential (StakePoolKeyHash poolStakeVKeyHash) = do
  caseShelleyToBabbageAndConwayEraOnwards
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
  -> ExceptT ShelleyStakeAddressCmdError IO ()
runStakeAddressDeregistrationCertificateCmd sbe stakeVerifier mDeposit oFp = do
  stakeCred <-
    getStakeCredentialFromIdentifier stakeVerifier
      & firstExceptT ShelleyStakeAddressCmdStakeCredentialError

  req <- firstExceptT StakeRegistrationError
           . hoistEither $ createRegistrationCertRequirements sbe stakeCred mDeposit

  let deRegCert = makeStakeAddressUnregistrationCertificate req

  firstExceptT ShelleyStakeAddressCmdWriteFileError
    . newExceptT
    $ writeLazyByteStringFile oFp
    $ shelleyBasedEraConstraints sbe
    $ textEnvelopeToJSON (Just deregCertDesc) deRegCert

  where
    deregCertDesc :: TextEnvelopeDescr
    deregCertDesc = "Stake Address Deregistration Certificate"
