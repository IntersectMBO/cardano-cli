{-# LANGUAGE DataKinds #-}


{- HLINT ignore "Reduce duplication" -}

module Cardano.CLI.Legacy.Run.Node
  ( runLegacyNodeCmds
  , runNodeIssueOpCert
  , runNodeKeyGenCold
  , runNodeKeyGenKES
  , runNodeKeyGenVRF
  , readColdVerificationKeyOrFile
  ) where

import           Cardano.Api
import           Cardano.Api.Shelley

import           Cardano.CLI.Legacy.Commands.Node
import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Errors.NodeCmdError
import           Cardano.CLI.Types.Key

import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, hoistEither, newExceptT)
import qualified Data.ByteString.Char8 as BS
import           Data.String (fromString)
import           Data.Word (Word64)

runLegacyNodeCmds :: LegacyNodeCmds -> ExceptT NodeCmdError IO ()
runLegacyNodeCmds (NodeKeyGenCold fmt vk sk ctr) = runNodeKeyGenCold fmt vk sk ctr
runLegacyNodeCmds (NodeKeyGenKES  fmt vk sk)     = runNodeKeyGenKES fmt vk sk
runLegacyNodeCmds (NodeKeyGenVRF  fmt vk sk)     = runNodeKeyGenVRF fmt vk sk
runLegacyNodeCmds (NodeKeyHashVRF vk mOutFp) = runNodeKeyHashVRF vk mOutFp
runLegacyNodeCmds (NodeNewCounter vk ctr out) = runNodeNewCounter vk ctr out
runLegacyNodeCmds (NodeIssueOpCert vk sk ctr p out) =
  runNodeIssueOpCert vk sk ctr p out

--
-- Node command implementations
--

runNodeKeyGenCold
  :: KeyOutputFormat
  -> VerificationKeyFile Out
  -> SigningKeyFile Out
  -> OpCertCounterFile Out
  -> ExceptT NodeCmdError IO ()
runNodeKeyGenCold fmt vkeyPath skeyPath ocertCtrPath = do
    skey <- liftIO $ generateSigningKey AsStakePoolKey
    let vkey = getVerificationKey skey

    case fmt of
      KeyOutputFormatTextEnvelope ->
        firstExceptT NodeCmdWriteFileError
          . newExceptT
          $ writeLazyByteStringFile skeyPath
          $ textEnvelopeToJSON (Just skeyDesc) skey
      KeyOutputFormatBech32 ->
        firstExceptT NodeCmdWriteFileError
          . newExceptT
          $ writeTextFile skeyPath
          $ serialiseToBech32 skey

    case fmt of
      KeyOutputFormatTextEnvelope ->
        firstExceptT NodeCmdWriteFileError
          . newExceptT
          $ writeLazyByteStringFile vkeyPath
          $ textEnvelopeToJSON (Just vkeyDesc) vkey
      KeyOutputFormatBech32 ->
        firstExceptT NodeCmdWriteFileError
          . newExceptT
          $ writeTextFile vkeyPath
          $ serialiseToBech32 vkey

    firstExceptT NodeCmdWriteFileError
      . newExceptT
      $ writeLazyByteStringFile ocertCtrPath
      $ textEnvelopeToJSON (Just ocertCtrDesc)
      $ OperationalCertificateIssueCounter initialCounter vkey
  where
    skeyDesc :: TextEnvelopeDescr
    skeyDesc = "Stake Pool Operator Signing Key"

    vkeyDesc :: TextEnvelopeDescr
    vkeyDesc = "Stake Pool Operator Verification Key"

    ocertCtrDesc :: TextEnvelopeDescr
    ocertCtrDesc = "Next certificate issue number: "
                <> fromString (show initialCounter)

    initialCounter :: Word64
    initialCounter = 0


runNodeKeyGenKES
  :: KeyOutputFormat
  -> VerificationKeyFile Out
  -> SigningKeyFile Out
  -> ExceptT NodeCmdError IO ()
runNodeKeyGenKES fmt vkeyPath skeyPath = do
  skey <- liftIO $ generateSigningKey AsKesKey

  let vkey = getVerificationKey skey

  case fmt of
    KeyOutputFormatTextEnvelope ->
      firstExceptT NodeCmdWriteFileError
        . newExceptT
        $ writeLazyByteStringFileWithOwnerPermissions skeyPath
        $ textEnvelopeToJSON (Just skeyDesc) skey
    KeyOutputFormatBech32 ->
      firstExceptT NodeCmdWriteFileError
        . newExceptT
        $ writeTextFile skeyPath
        $ serialiseToBech32 skey

  case fmt of
    KeyOutputFormatTextEnvelope ->
      firstExceptT NodeCmdWriteFileError
        . newExceptT
        $ writeLazyByteStringFile vkeyPath
        $ textEnvelopeToJSON (Just vkeyDesc) vkey
    KeyOutputFormatBech32 ->
      firstExceptT NodeCmdWriteFileError
        . newExceptT
        $ writeTextFile vkeyPath
        $ serialiseToBech32 vkey

  where
    skeyDesc :: TextEnvelopeDescr
    skeyDesc = "KES Signing Key"

    vkeyDesc :: TextEnvelopeDescr
    vkeyDesc = "KES Verification Key"

runNodeKeyGenVRF
  :: KeyOutputFormat
  -> VerificationKeyFile Out
  -> SigningKeyFile Out
  -> ExceptT NodeCmdError IO ()
runNodeKeyGenVRF fmt vkeyPath skeyPath = do
  skey <- liftIO $ generateSigningKey AsVrfKey

  let vkey = getVerificationKey skey

  case fmt of
    KeyOutputFormatTextEnvelope ->
      firstExceptT NodeCmdWriteFileError
        . newExceptT
        $ writeLazyByteStringFileWithOwnerPermissions skeyPath
        $ textEnvelopeToJSON (Just skeyDesc) skey
    KeyOutputFormatBech32 ->
      firstExceptT NodeCmdWriteFileError
        . newExceptT
        $ writeTextFile skeyPath
        $ serialiseToBech32 skey

  case fmt of
    KeyOutputFormatTextEnvelope ->
      firstExceptT NodeCmdWriteFileError
        . newExceptT
        $ writeLazyByteStringFile vkeyPath
        $ textEnvelopeToJSON (Just vkeyDesc) vkey
    KeyOutputFormatBech32 ->
      firstExceptT NodeCmdWriteFileError
        . newExceptT
        $ writeTextFile vkeyPath
        $ serialiseToBech32 vkey
  where
    skeyDesc, vkeyDesc :: TextEnvelopeDescr
    skeyDesc = "VRF Signing Key"
    vkeyDesc = "VRF Verification Key"

runNodeKeyHashVRF :: VerificationKeyOrFile VrfKey
                  -> Maybe (File () Out)
                  -> ExceptT NodeCmdError IO ()
runNodeKeyHashVRF verKeyOrFile mOutputFp = do
  vkey <- firstExceptT NodeCmdReadKeyFileError
    . newExceptT
    $ readVerificationKeyOrFile AsVrfKey verKeyOrFile

  let hexKeyHash = serialiseToRawBytesHex (verificationKeyHash vkey)

  case mOutputFp of
    Just fpath -> liftIO $ BS.writeFile (unFile fpath) hexKeyHash
    Nothing -> liftIO $ BS.putStrLn hexKeyHash


runNodeNewCounter :: ColdVerificationKeyOrFile
                  -> Word
                  -> OpCertCounterFile InOut
                  -> ExceptT NodeCmdError IO ()
runNodeNewCounter coldVerKeyOrFile counter ocertCtrPath = do

    vkey <- firstExceptT NodeCmdReadFileError . newExceptT $
      readColdVerificationKeyOrFile coldVerKeyOrFile

    let ocertIssueCounter =
          OperationalCertificateIssueCounter (fromIntegral counter) vkey

    firstExceptT NodeCmdWriteFileError . newExceptT
      $ writeLazyByteStringFile (onlyOut ocertCtrPath)
      $ textEnvelopeToJSON Nothing ocertIssueCounter


runNodeIssueOpCert :: VerificationKeyOrFile KesKey
                   -- ^ This is the hot KES verification key.
                   -> SigningKeyFile In
                   -- ^ This is the cold signing key.
                   -> OpCertCounterFile InOut
                   -- ^ Counter that establishes the precedence
                   -- of the operational certificate.
                   -> KESPeriod
                   -- ^ Start of the validity period for this certificate.
                   -> File () Out
                   -> ExceptT NodeCmdError IO ()
runNodeIssueOpCert kesVerKeyOrFile stakePoolSKeyFile ocertCtrPath kesPeriod certFile = do

    ocertIssueCounter <- firstExceptT NodeCmdReadFileError
      . newExceptT
      $ readFileTextEnvelope AsOperationalCertificateIssueCounter (onlyIn ocertCtrPath)

    verKeyKes <- firstExceptT NodeCmdReadKeyFileError
      . newExceptT
      $ readVerificationKeyOrFile AsKesKey kesVerKeyOrFile

    signKey <- firstExceptT NodeCmdReadKeyFileError
      . newExceptT
      $ readKeyFileAnyOf
          bech32PossibleBlockIssuers
          textEnvPossibleBlockIssuers
          stakePoolSKeyFile

    (ocert, nextOcertCtr) <-
      firstExceptT NodeCmdOperationalCertificateIssueError
        . hoistEither
        $ issueOperationalCertificate
            verKeyKes
            signKey
            kesPeriod
            ocertIssueCounter

    -- Write the counter first, to reduce the chance of ending up with
    -- a new cert but without updating the counter.
    firstExceptT NodeCmdWriteFileError
      . newExceptT
      $ writeLazyByteStringFile (onlyOut ocertCtrPath)
      $ textEnvelopeToJSON (Just $ ocertCtrDesc $ getCounter nextOcertCtr) nextOcertCtr

    firstExceptT NodeCmdWriteFileError
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
