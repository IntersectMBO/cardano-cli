{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}


{- HLINT ignore "Monad law, left identity" -}

module Cardano.CLI.EraBased.Run.StakeAddress
  ( StakeAddressCmdError(StakeAddressCmdReadKeyFileError)
  , getStakeCredentialFromIdentifier
  , renderStakeAddressCmdError
  , runStakeAddressCmd
  , runStakeAddressKeyGenToFile

  , StakeAddressDelegationError(..)
  , createDelegationCertRequirements
  ) where

import           Cardano.Api
import qualified Cardano.Api.Ledger as Ledger
import           Cardano.Api.Shelley

import           Cardano.CLI.EraBased.Options.StakeAddress
import           Cardano.CLI.Run.Legacy.Read
import           Cardano.CLI.Types.Governance
import           Cardano.CLI.Types.Key (DelegationTarget (..), StakeIdentifier (..),
                   StakeVerifier (..), VerificationKeyOrFile, readVerificationKeyOrFile,
                   readVerificationKeyOrHashOrFile)
import           Cardano.CLI.Types.Legacy

import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Trans (lift)
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, hoistEither, left, newExceptT,
                   onLeft)
import qualified Data.ByteString.Char8 as BS
import           Data.Function ((&))
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

data StakeAddressCmdError
  = StakeAddressCmdReadKeyFileError !(FileError InputDecodeError)
  | StakeAddressCmdReadScriptFileError !(FileError ScriptDecodeError)
  | StakeAddressCmdWriteFileError !(FileError ())
  | StakeRegistrationError !StakeAddressRegistrationError
  | StakeDelegationError !StakeAddressDelegationError
  deriving Show

renderStakeAddressCmdError :: StakeAddressCmdError -> Text
renderStakeAddressCmdError err =
  case err of
    StakeAddressCmdReadKeyFileError fileErr -> Text.pack (displayError fileErr)
    StakeAddressCmdWriteFileError fileErr -> Text.pack (displayError fileErr)
    StakeAddressCmdReadScriptFileError fileErr -> Text.pack (displayError fileErr)
    StakeRegistrationError regErr -> Text.pack $ show regErr
    StakeDelegationError delegErr -> Text.pack $ show delegErr

runStakeAddressCmd :: StakeAddressCmd era-> ExceptT StakeAddressCmdError IO ()
runStakeAddressCmd (StakeAddressKeyGen fmt vk sk) = runStakeAddressKeyGenToFile fmt vk sk
runStakeAddressCmd (StakeAddressKeyHash vk mOutputFp) = runStakeAddressKeyHash vk mOutputFp
runStakeAddressCmd (StakeAddressBuild stakeVerifier nw mOutputFp) =
  runStakeAddressBuild stakeVerifier nw mOutputFp
runStakeAddressCmd (StakeRegistrationCert anyEra stakeIdentifier mDeposit outputFp) =
  runStakeCredentialRegistrationCert anyEra stakeIdentifier mDeposit outputFp
runStakeAddressCmd (StakeCredentialDelegationCert anyEra stakeIdentifier stkPoolVerKeyHashOrFp outputFp) =
  runStakeCredentialDelegationCert anyEra stakeIdentifier stkPoolVerKeyHashOrFp outputFp
runStakeAddressCmd (StakeCredentialDeRegistrationCert anyEra stakeIdentifier mDeposit outputFp) =
  runStakeCredentialDeRegistrationCert anyEra stakeIdentifier mDeposit outputFp


--
-- Stake address command implementations
--

runStakeAddressKeyGenToFile
  :: KeyOutputFormat
  -> VerificationKeyFile Out
  -> SigningKeyFile Out
  -> ExceptT StakeAddressCmdError IO ()
runStakeAddressKeyGenToFile fmt vkFp skFp = do
  let skeyDesc = "Stake Signing Key"
  let vkeyDesc = "Stake Verification Key"

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
        newExceptT $ writeLazyByteStringFile vkFp $ textEnvelopeToJSON (Just vkeyDesc) vkey
      KeyOutputFormatBech32 ->
        newExceptT $ writeTextFile vkFp $ serialiseToBech32 vkey

runStakeAddressKeyHash
  :: VerificationKeyOrFile StakeKey
  -> Maybe (File () Out)
  -> ExceptT StakeAddressCmdError IO ()
runStakeAddressKeyHash stakeVerKeyOrFile mOutputFp = do
  vkey <- firstExceptT StakeAddressCmdReadKeyFileError
    . newExceptT
    $ readVerificationKeyOrFile AsStakeKey stakeVerKeyOrFile

  let hexKeyHash = serialiseToRawBytesHex (verificationKeyHash vkey)

  case mOutputFp of
    Just (File fpath) -> liftIO $ BS.writeFile fpath hexKeyHash
    Nothing -> liftIO $ BS.putStrLn hexKeyHash

runStakeAddressBuild
  :: StakeVerifier
  -> NetworkId
  -> Maybe (File () Out)
  -> ExceptT StakeAddressCmdError IO ()
runStakeAddressBuild stakeVerifier network mOutputFp = do
  stakeAddr <- getStakeAddressFromVerifier network stakeVerifier
  let stakeAddrText = serialiseAddress stakeAddr
  liftIO $
    case mOutputFp of
      Just (File fpath) -> Text.writeFile fpath stakeAddrText
      Nothing -> Text.putStrLn stakeAddrText


runStakeCredentialRegistrationCert
  :: AnyShelleyBasedEra
  -> StakeIdentifier
  -> Maybe Lovelace -- ^ Deposit required in conway era
  -> File () Out
  -> ExceptT StakeAddressCmdError IO ()
runStakeCredentialRegistrationCert anyEra stakeIdentifier mDeposit oFp = do
  AnyShelleyBasedEra sbe <- pure anyEra
  stakeCred <- getStakeCredentialFromIdentifier stakeIdentifier
  req <- firstExceptT StakeRegistrationError
           . hoistEither $ createRegistrationCertRequirements sbe stakeCred mDeposit
  writeRegistrationCert sbe req

 where
  writeRegistrationCert
    :: ShelleyBasedEra era
    -> StakeAddressRequirements era
    -> ExceptT StakeAddressCmdError IO ()
  writeRegistrationCert sbe req = do
    let regCert = makeStakeAddressRegistrationCertificate req
    firstExceptT StakeAddressCmdWriteFileError
      . newExceptT
      $ writeLazyByteStringFile oFp
      $ shelleyBasedEraConstraints sbe
      $ textEnvelopeToJSON (Just regCertDesc) regCert

  regCertDesc :: TextEnvelopeDescr
  regCertDesc = "Stake Address Registration Certificate"


data StakeAddressRegistrationError = StakeAddressRegistrationDepositRequired deriving Show

createRegistrationCertRequirements
  :: ShelleyBasedEra era
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


runStakeCredentialDelegationCert
  :: AnyShelleyBasedEra
  -> StakeIdentifier
  -- ^ Delegator stake verification key, verification key file or script file.
  -> DelegationTarget
  -- ^ Delegatee stake pool verification key or verification key file or
  -- verification key hash.
  -> File () Out
  -> ExceptT StakeAddressCmdError IO ()
runStakeCredentialDelegationCert anyEra stakeVerifier delegationTarget outFp = do
  AnyShelleyBasedEra sbe <- pure anyEra
  case delegationTarget of
    StakePoolDelegationTarget poolVKeyOrHashOrFile -> do
      StakePoolKeyHash poolStakeVKeyHash <- lift (readVerificationKeyOrHashOrFile AsStakePoolKey poolVKeyOrHashOrFile)
        & onLeft (left . StakeAddressCmdReadKeyFileError)
      let delegatee = Ledger.DelegStake poolStakeVKeyHash
      stakeCred <- getStakeCredentialFromIdentifier stakeVerifier
      req <- firstExceptT StakeDelegationError . hoistEither
               $ createDelegationCertRequirements sbe stakeCred delegatee
      let delegCert = makeStakeAddressDelegationCertificate req
      firstExceptT StakeAddressCmdWriteFileError
        . newExceptT
        $ writeLazyByteStringFile outFp
        $ textEnvelopeToJSON (Just @TextEnvelopeDescr "Stake Address Delegation Certificate") delegCert

createDelegationCertRequirements
  :: ShelleyBasedEra era
  -> StakeCredential
  -> Ledger.Delegatee Ledger.StandardCrypto
  -> Either StakeAddressDelegationError (StakeDelegationRequirements era)
createDelegationCertRequirements sbe stakeCred delegatee =
  case sbe of
    ShelleyBasedEraShelley -> do
      pId <- onlySpoDelegatee ShelleyToBabbageEraShelley delegatee
      return $ StakeDelegationRequirementsPreConway ShelleyToBabbageEraShelley stakeCred pId
    ShelleyBasedEraAllegra -> do
      pId <- onlySpoDelegatee ShelleyToBabbageEraAllegra delegatee
      return $ StakeDelegationRequirementsPreConway ShelleyToBabbageEraAllegra stakeCred pId
    ShelleyBasedEraMary -> do
      pId <- onlySpoDelegatee ShelleyToBabbageEraMary delegatee
      return $ StakeDelegationRequirementsPreConway ShelleyToBabbageEraMary stakeCred pId
    ShelleyBasedEraAlonzo -> do
      pId <- onlySpoDelegatee ShelleyToBabbageEraAlonzo delegatee
      return $ StakeDelegationRequirementsPreConway ShelleyToBabbageEraAlonzo stakeCred pId
    ShelleyBasedEraBabbage -> do
      pId <- onlySpoDelegatee ShelleyToBabbageEraBabbage delegatee
      return $ StakeDelegationRequirementsPreConway ShelleyToBabbageEraBabbage stakeCred pId
    ShelleyBasedEraConway ->
      return $ StakeDelegationRequirementsConwayOnwards ConwayEraOnwardsConway stakeCred delegatee

onlySpoDelegatee
  :: IsShelleyBasedEra era
  => ShelleyToBabbageEra era
  -> Ledger.Delegatee (Ledger.EraCrypto (ShelleyLedgerEra era))
  -> Either StakeAddressDelegationError PoolId
onlySpoDelegatee aMost ledgerDelegatee =
  case ledgerDelegatee of
    Ledger.DelegStake stakePoolKeyHash ->
      Right $ StakePoolKeyHash $ obtainEraCryptoAtMostBabbageConstraints aMost stakePoolKeyHash
    Ledger.DelegVote{} ->
      Left . VoteDelegationNotSupported $ AnyAtMostBabbageEra aMost
    Ledger.DelegStakeVote{} ->
      Left . VoteDelegationNotSupported $ AnyAtMostBabbageEra aMost

newtype StakeAddressDelegationError = VoteDelegationNotSupported AnyAtMostBabbageEra deriving Show


obtainEraCryptoAtMostBabbageConstraints
  :: ShelleyToBabbageEra era
  -> ((Ledger.EraCrypto (ShelleyLedgerEra era) ~ Ledger.StandardCrypto) => a)
  -> a
obtainEraCryptoAtMostBabbageConstraints ShelleyToBabbageEraShelley f = f
obtainEraCryptoAtMostBabbageConstraints ShelleyToBabbageEraAllegra f = f
obtainEraCryptoAtMostBabbageConstraints ShelleyToBabbageEraMary    f = f
obtainEraCryptoAtMostBabbageConstraints ShelleyToBabbageEraAlonzo  f = f
obtainEraCryptoAtMostBabbageConstraints ShelleyToBabbageEraBabbage f = f


runStakeCredentialDeRegistrationCert
  :: AnyShelleyBasedEra
  -> StakeIdentifier
  -> Maybe Lovelace -- ^ Deposit required in conway era
  -> File () Out
  -> ExceptT StakeAddressCmdError IO ()
runStakeCredentialDeRegistrationCert anyEra stakeVerifier mDeposit oFp = do
  AnyShelleyBasedEra sbe <- pure anyEra
  stakeCred <- getStakeCredentialFromIdentifier stakeVerifier
  req <- firstExceptT StakeRegistrationError
           . hoistEither $ createRegistrationCertRequirements sbe stakeCred mDeposit

  writeDeregistrationCert sbe req

  where
    writeDeregistrationCert
      :: ShelleyBasedEra era
      -> StakeAddressRequirements era
      -> ExceptT StakeAddressCmdError IO ()
    writeDeregistrationCert sbe req = do
      let deRegCert = makeStakeAddressRegistrationCertificate req
      firstExceptT StakeAddressCmdWriteFileError
        . newExceptT
        $ writeLazyByteStringFile oFp
        $ shelleyBasedEraConstraints sbe
        $ textEnvelopeToJSON (Just deregCertDesc) deRegCert

    deregCertDesc :: TextEnvelopeDescr
    deregCertDesc = "Stake Address Deregistration Certificate"




getStakeCredentialFromVerifier
  :: StakeVerifier
  -> ExceptT StakeAddressCmdError IO StakeCredential
getStakeCredentialFromVerifier = \case
  StakeVerifierScriptFile (ScriptFile sFile) -> do
    ScriptInAnyLang _ script <-
      firstExceptT StakeAddressCmdReadScriptFileError $
        readFileScriptInAnyLang sFile
    pure $ StakeCredentialByScript $ hashScript script

  StakeVerifierKey stakeVerKeyOrFile -> do
    stakeVerKey <-
      firstExceptT StakeAddressCmdReadKeyFileError
        . newExceptT
        $ readVerificationKeyOrFile AsStakeKey stakeVerKeyOrFile
    pure $ StakeCredentialByKey $ verificationKeyHash stakeVerKey

getStakeCredentialFromIdentifier
  :: StakeIdentifier
  -> ExceptT StakeAddressCmdError IO StakeCredential
getStakeCredentialFromIdentifier = \case
  StakeIdentifierAddress stakeAddr -> pure $ stakeAddressCredential stakeAddr
  StakeIdentifierVerifier stakeVerifier -> getStakeCredentialFromVerifier stakeVerifier

getStakeAddressFromVerifier
  :: NetworkId
  -> StakeVerifier
  -> ExceptT StakeAddressCmdError IO StakeAddress
getStakeAddressFromVerifier networkId stakeVerifier =
  makeStakeAddress networkId <$> getStakeCredentialFromVerifier stakeVerifier
