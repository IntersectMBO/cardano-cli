{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Shelley.Run.Governance.Committee
  ( ShelleyGovernanceCommitteeCmdError(..)
  , SomeCommitteeKey(..)
  , renderShelleyGovernanceCommitteeCmdError
  , runGovernanceCommitteeCmd
  , runCommitteeKeyGenCold
  , runCommitteeKeyHash
  , runCommitteeKeyGenHot
  , runCommitteeRegisterHotKey
  , runCommitteeUnregisterHotKey
  ) where

import           Cardano.Api
import           Cardano.Api.Shelley

import           Cardano.CLI.Shelley.Commands
import           Cardano.CLI.Shelley.Key (VerificationKeyOrFile, VerificationKeyOrHashOrFile,
                   readVerificationKeyOrFile, readVerificationKeyOrHashOrTextEnvFile)
import           Cardano.CLI.Types (SigningKeyFile, VerificationKeyFile)

import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, hoistEither, newExceptT)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.Function ((&))
import           Data.String (fromString)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Word (Word64)

{- HLINT ignore "Reduce duplication" -}

data ShelleyGovernanceCommitteeCmdError
  = ShelleyGovernanceCommitteeCmdReadFileError !(FileError TextEnvelopeError)
  | ShelleyGovernanceCommitteeCmdReadKeyFileError !(FileError InputDecodeError)
  | ShelleyGovernanceCommitteeCmdWriteFileError !(FileError ())
  | ShelleyGovernanceCommitteeCmdOperationalCertificateIssueError !OperationalCertIssueError
  | ShelleyGovernanceCommitteeCmdVrfSigningKeyCreationError
      FilePath
      -- ^ Target path
      FilePath
      -- ^ Temp path
  | ShelleyGovernanceCommitteeCmdTextEnvReadFileError !(FileError TextEnvelopeError)
  | ShelleyGovernanceCommitteeCmdKeyReadError !(FileError InputDecodeError)
  | ShelleyGovernanceCommitteeCmdTextEnvWriteError !(FileError ())
  deriving Show

renderShelleyGovernanceCommitteeCmdError :: ShelleyGovernanceCommitteeCmdError -> Text
renderShelleyGovernanceCommitteeCmdError err =
  case err of
    ShelleyGovernanceCommitteeCmdVrfSigningKeyCreationError targetPath tempPath ->
      Text.pack $ "Error creating VRF signing key file. Target path: " <> targetPath
      <> " Temporary path: " <> tempPath

    ShelleyGovernanceCommitteeCmdReadFileError fileErr -> Text.pack (displayError fileErr)

    ShelleyGovernanceCommitteeCmdReadKeyFileError fileErr -> Text.pack (displayError fileErr)

    ShelleyGovernanceCommitteeCmdWriteFileError fileErr -> Text.pack (displayError fileErr)

    ShelleyGovernanceCommitteeCmdOperationalCertificateIssueError issueErr ->
      Text.pack (displayError issueErr)

    ShelleyGovernanceCommitteeCmdTextEnvReadFileError fileErr -> Text.pack (displayError fileErr)

    ShelleyGovernanceCommitteeCmdKeyReadError fileErr -> Text.pack (displayError fileErr)

    ShelleyGovernanceCommitteeCmdTextEnvWriteError fileErr -> Text.pack (displayError fileErr)

runGovernanceCommitteeCmd :: CommitteeCmd -> ExceptT ShelleyGovernanceCommitteeCmdError IO ()
runGovernanceCommitteeCmd = \case
  CommitteeKeyGenCold vk sk -> runCommitteeKeyGenCold vk sk
  CommitteeKeyGenHot vk sk ctr -> runCommitteeKeyGenHot vk sk ctr
  CommitteeNewCounter vk ctr out -> runCommitteeNewCounter vk ctr out
  CommitteeIssueOpCert vk sk ctr p out -> runCommitteeIssueOpCert vk sk ctr p out
  CommitteeKeyHash vk -> runCommitteeKeyHash vk
  CommitteeRegisterHotKey ck hk outCertFile -> runCommitteeRegisterHotKey ck hk outCertFile
  CommitteeUnregisterHotKey ck outCertFile -> runCommitteeUnregisterHotKey ck outCertFile


--
-- Constitutional Committee command implementations
--

runCommitteeKeyGenCold ::
     VerificationKeyFile Out
  -> SigningKeyFile Out
  -> ExceptT ShelleyGovernanceCommitteeCmdError IO ()
runCommitteeKeyGenCold vkeyPath skeyPath = do
  skey <- liftIO $ generateSigningKey AsGenesisKey
  let vkey = getVerificationKey skey
  writeLazyByteStringFile skeyPath (textEnvelopeToJSON (Just skeyDesc) skey)
    & firstExceptT ShelleyGovernanceCommitteeCmdWriteFileError . newExceptT
  firstExceptT ShelleyGovernanceCommitteeCmdWriteFileError
    . newExceptT
    $ writeLazyByteStringFile vkeyPath
    $ textEnvelopeToJSON (Just vkeyDesc) vkey
  where
    skeyDesc, vkeyDesc :: TextEnvelopeDescr
    skeyDesc = "Constitutional Committee Cold Signing Key"
    vkeyDesc = "Constitutional Committee Cold Verification Key"

runCommitteeKeyGenHot ::
     VerificationKeyFile Out
  -> SigningKeyFile Out
  -> OpCertCounterFile Out
  -> ExceptT ShelleyGovernanceCommitteeCmdError IO ()
runCommitteeKeyGenHot vkeyPath skeyPath ocertCtrPath = do
  skey <- liftIO $ generateSigningKey AsGenesisDelegateKey
  let vkey = getVerificationKey skey
  firstExceptT ShelleyGovernanceCommitteeCmdWriteFileError
    . newExceptT
    $ writeLazyByteStringFile skeyPath
    $ textEnvelopeToJSON (Just skeyDesc) skey
  firstExceptT ShelleyGovernanceCommitteeCmdWriteFileError
    . newExceptT
    $ writeLazyByteStringFile vkeyPath
    $ textEnvelopeToJSON (Just vkeyDesc) vkey
  firstExceptT ShelleyGovernanceCommitteeCmdWriteFileError
    . newExceptT
    $ writeLazyByteStringFile ocertCtrPath
    $ textEnvelopeToJSON (Just certCtrDesc)
    $ OperationalCertificateIssueCounter
        initialCounter
        (castVerificationKey vkey)
  where
    skeyDesc, vkeyDesc, certCtrDesc :: TextEnvelopeDescr
    skeyDesc = "Constitutional Committee Hot Signing Key"
    vkeyDesc = "Constitutional Committee Hot Verification Key"
    certCtrDesc = "Next certificate issue number: "
               <> fromString (show initialCounter)

    initialCounter :: Word64
    initialCounter = 0

runCommitteeNewCounter :: ColdVerificationKeyOrFile
                  -> Word
                  -> OpCertCounterFile InOut
                  -> ExceptT ShelleyGovernanceCommitteeCmdError IO ()
runCommitteeNewCounter coldVerKeyOrFile counter ocertCtrPath = do

    vkey <- firstExceptT ShelleyGovernanceCommitteeCmdReadFileError . newExceptT $
      readColdVerificationKeyOrFile coldVerKeyOrFile

    let ocertIssueCounter =
          OperationalCertificateIssueCounter (fromIntegral counter) vkey

    firstExceptT ShelleyGovernanceCommitteeCmdWriteFileError . newExceptT
      $ writeLazyByteStringFile (onlyOut ocertCtrPath)
      $ textEnvelopeToJSON Nothing ocertIssueCounter


runCommitteeIssueOpCert :: VerificationKeyOrFile KesKey
                   -- ^ This is the hot KES verification key.
                   -> SigningKeyFile In
                   -- ^ This is the cold signing key.
                   -> OpCertCounterFile InOut
                   -- ^ Counter that establishes the precedence
                   -- of the operational certificate.
                   -> KESPeriod
                   -- ^ Start of the validity period for this certificate.
                   -> File () Out
                   -> ExceptT ShelleyGovernanceCommitteeCmdError IO ()
runCommitteeIssueOpCert kesVerKeyOrFile stakePoolSKeyFile ocertCtrPath kesPeriod certFile = do

    ocertIssueCounter <- firstExceptT ShelleyGovernanceCommitteeCmdReadFileError
      . newExceptT
      $ readFileTextEnvelope AsOperationalCertificateIssueCounter (onlyIn ocertCtrPath)

    verKeyKes <- firstExceptT ShelleyGovernanceCommitteeCmdReadKeyFileError
      . newExceptT
      $ readVerificationKeyOrFile AsKesKey kesVerKeyOrFile

    signKey <- firstExceptT ShelleyGovernanceCommitteeCmdReadKeyFileError
      . newExceptT
      $ readKeyFileAnyOf
          bech32PossibleBlockIssuers
          textEnvPossibleBlockIssuers
          stakePoolSKeyFile

    (ocert, nextOcertCtr) <-
      firstExceptT ShelleyGovernanceCommitteeCmdOperationalCertificateIssueError
        . hoistEither
        $ issueOperationalCertificate
            verKeyKes
            signKey
            kesPeriod
            ocertIssueCounter

    -- Write the counter first, to reduce the chance of ending up with
    -- a new cert but without updating the counter.
    firstExceptT ShelleyGovernanceCommitteeCmdWriteFileError
      . newExceptT
      $ writeLazyByteStringFile (onlyOut ocertCtrPath)
      $ textEnvelopeToJSON (Just $ ocertCtrDesc $ getCounter nextOcertCtr) nextOcertCtr

    firstExceptT ShelleyGovernanceCommitteeCmdWriteFileError
      . newExceptT
      $ writeLazyByteStringFile certFile
      $ textEnvelopeToJSON Nothing ocert
  where
    getCounter :: OperationalCertificateIssueCounter -> Word64
    getCounter (OperationalCertificateIssueCounter n _) = n

    ocertCtrDesc :: Word64 -> TextEnvelopeDescr
    ocertCtrDesc n = "Next certificate issue number: " <> fromString (show n)

    textEnvPossibleBlockIssuers
      :: [FromSomeType HasTextEnvelope
                       (Either (SigningKey StakePoolKey)
                               (SigningKey GenesisDelegateExtendedKey))]
    textEnvPossibleBlockIssuers =
      [ FromSomeType (AsSigningKey AsStakePoolKey)        Left
      , FromSomeType (AsSigningKey AsGenesisDelegateKey) (Left . castSigningKey)
      , FromSomeType (AsSigningKey AsGenesisDelegateExtendedKey) Right
      ]

    bech32PossibleBlockIssuers
      :: [FromSomeType SerialiseAsBech32
                       (Either (SigningKey StakePoolKey)
                               (SigningKey GenesisDelegateExtendedKey))]
    bech32PossibleBlockIssuers =
      [FromSomeType (AsSigningKey AsStakePoolKey) Left]

-- | Read a cold verification key or file.
--
-- If a filepath is provided, it will be interpreted as a text envelope
-- formatted file.
readColdVerificationKeyOrFile
  :: ColdVerificationKeyOrFile
  -> IO (Either (FileError TextEnvelopeError) (VerificationKey StakePoolKey))
readColdVerificationKeyOrFile coldVerKeyOrFile =
  case coldVerKeyOrFile of
    ColdStakePoolVerificationKey vk -> pure (Right vk)
    ColdGenesisDelegateVerificationKey vk ->
      pure $ Right (castVerificationKey vk)
    ColdVerificationKeyFile fp ->
      readFileTextEnvelopeAnyOf
        [ FromSomeType (AsVerificationKey AsStakePoolKey) id
        , FromSomeType (AsVerificationKey AsGenesisDelegateKey) castVerificationKey
        ]
        fp

data SomeCommitteeKey f
  = ACommitteeHotKey  (f CommitteeHotKey)
  | ACommitteeColdKey (f CommitteeColdKey)

runCommitteeKeyHash :: VerificationKeyFile In -> ExceptT ShelleyGovernanceCommitteeCmdError IO ()
runCommitteeKeyHash vkeyPath = do
    vkey <-
      readFileTextEnvelopeAnyOf
        [ FromSomeType (AsVerificationKey AsCommitteeHotKey ) ACommitteeHotKey
        , FromSomeType (AsVerificationKey AsCommitteeColdKey) ACommitteeColdKey
        ]
        vkeyPath
      & firstExceptT ShelleyGovernanceCommitteeCmdTextEnvReadFileError . newExceptT

    liftIO $ BS.putStrLn (renderKeyHash vkey)

  where
    renderKeyHash :: SomeCommitteeKey VerificationKey -> ByteString
    renderKeyHash = \case
      ACommitteeHotKey  vk -> renderVerificationKeyHash vk
      ACommitteeColdKey vk -> renderVerificationKeyHash vk

    renderVerificationKeyHash :: Key keyrole => VerificationKey keyrole -> ByteString
    renderVerificationKeyHash = serialiseToRawBytesHex
                              . verificationKeyHash

runCommitteeRegisterHotKey
  :: VerificationKeyOrHashOrFile CommitteeColdKey
  -> VerificationKeyOrHashOrFile CommitteeHotKey
  -> File () Out
  -> ExceptT ShelleyGovernanceCommitteeCmdError IO ()
runCommitteeRegisterHotKey genVkOrHashOrFp genDelVkOrHashOrFp oFp = do
  genesisVkHash <-
    readVerificationKeyOrHashOrTextEnvFile AsCommitteeColdKey genVkOrHashOrFp
      & firstExceptT ShelleyGovernanceCommitteeCmdKeyReadError . newExceptT

  genesisDelVkHash <-
    readVerificationKeyOrHashOrTextEnvFile AsCommitteeHotKey genDelVkOrHashOrFp
      & firstExceptT ShelleyGovernanceCommitteeCmdKeyReadError . newExceptT

  makeCommitteeDelegationCertificate genesisVkHash genesisDelVkHash
    & textEnvelopeToJSON (Just genKeyDelegCertDesc)
    & writeLazyByteStringFile oFp
    & firstExceptT ShelleyGovernanceCommitteeCmdTextEnvWriteError . newExceptT

  where
    genKeyDelegCertDesc :: TextEnvelopeDescr
    genKeyDelegCertDesc = "Constitutional Committee Hot Key Registration Certificate"

runCommitteeUnregisterHotKey
  :: VerificationKeyOrHashOrFile CommitteeColdKey
  -> File () Out
  -> ExceptT ShelleyGovernanceCommitteeCmdError IO ()
runCommitteeUnregisterHotKey genVkOrHashOrFp oFp = do
  genesisVkHash <-
    readVerificationKeyOrHashOrTextEnvFile AsCommitteeColdKey genVkOrHashOrFp
      & firstExceptT ShelleyGovernanceCommitteeCmdKeyReadError . newExceptT

  makeCommitteeHotKeyUnregistrationCertificate genesisVkHash
    & textEnvelopeToJSON (Just hotKeyUnregistrationCertDesc)
    & writeLazyByteStringFile oFp
    & firstExceptT ShelleyGovernanceCommitteeCmdTextEnvWriteError . newExceptT

  where
    hotKeyUnregistrationCertDesc :: TextEnvelopeDescr
    hotKeyUnregistrationCertDesc = "Constitutional Committee Hot Key Unregistration Certificate"
