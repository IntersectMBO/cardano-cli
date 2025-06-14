{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{- HLINT ignore "Redundant id" -}

module Cardano.CLI.EraIndependent.Node.Run
  ( runNodeCmds
  , runNodeIssueOpCertCmd
  , runNodeKeyGenColdCmd
  , runNodeKeyGenKesCmd
  , runNodeKeyGenVrfCmd
  , runNodeKeyHashVrfCmd
  , runNodeNewCounterCmd
  )
where

import Cardano.Api

import Cardano.CLI.EraIndependent.Node.Command qualified as Cmd
import Cardano.CLI.Type.Common
import Cardano.CLI.Type.Error.NodeCmdError
import Cardano.CLI.Type.Key

import Data.Function ((&))
import Data.String (fromString)
import Data.Word (Word64)
import Vary qualified

{- HLINT ignore "Reduce duplication" -}

runNodeCmds
  :: ()
  => Cmd.NodeCmds
  -> ExceptT NodeCmdError IO ()
runNodeCmds = \case
  Cmd.NodeKeyGenColdCmd args -> runNodeKeyGenColdCmd args
  Cmd.NodeKeyGenKESCmd args -> runNodeKeyGenKesCmd args
  Cmd.NodeKeyGenVRFCmd args -> runNodeKeyGenVrfCmd args
  Cmd.NodeKeyHashVRFCmd args -> runNodeKeyHashVrfCmd args
  Cmd.NodeNewCounterCmd args -> runNodeNewCounterCmd args
  Cmd.NodeIssueOpCertCmd args -> runNodeIssueOpCertCmd args

runNodeKeyGenColdCmd
  :: ()
  => Cmd.NodeKeyGenColdCmdArgs
  -> ExceptT NodeCmdError IO ()
runNodeKeyGenColdCmd
  Cmd.NodeKeyGenColdCmdArgs
    { keyOutputFormat
    , vkeyFile
    , skeyFile
    , operationalCertificateIssueCounter
    } = do
    skey <- generateSigningKey AsStakePoolKey
    let vkey = getVerificationKey skey

    keyOutputFormat
      & ( id
            . Vary.on
              ( \FormatBech32 ->
                  firstExceptT NodeCmdWriteFileError
                    . newExceptT
                    . writeTextFile skeyFile
                    $ serialiseToBech32 skey
              )
            . Vary.on
              ( \FormatTextEnvelope ->
                  firstExceptT NodeCmdWriteFileError
                    . newExceptT
                    $ writeLazyByteStringFile skeyFile
                    $ textEnvelopeToJSON (Just skeyDesc) skey
              )
            $ Vary.exhaustiveCase
        )

    keyOutputFormat
      & ( id
            . Vary.on
              ( \FormatBech32 ->
                  firstExceptT NodeCmdWriteFileError
                    . newExceptT
                    . writeTextFile vkeyFile
                    $ serialiseToBech32 vkey
              )
            . Vary.on
              ( \FormatTextEnvelope ->
                  firstExceptT NodeCmdWriteFileError
                    . newExceptT
                    . writeLazyByteStringFile vkeyFile
                    $ textEnvelopeToJSON (Just vkeyDesc) vkey
              )
            $ Vary.exhaustiveCase
        )

    firstExceptT NodeCmdWriteFileError
      . newExceptT
      $ writeLazyByteStringFile operationalCertificateIssueCounter
      $ textEnvelopeToJSON (Just ocertCtrDesc)
      $ OperationalCertificateIssueCounter
        initialCounter
        vkey
   where
    skeyDesc :: TextEnvelopeDescr
    skeyDesc = "Stake Pool Operator Signing Key"

    vkeyDesc :: TextEnvelopeDescr
    vkeyDesc = "Stake Pool Operator Verification Key"

    ocertCtrDesc :: TextEnvelopeDescr
    ocertCtrDesc =
      "Next certificate issue number: "
        <> fromString (show initialCounter)

    initialCounter :: Word64
    initialCounter = 0

runNodeKeyGenKesCmd
  :: ()
  => Cmd.NodeKeyGenKESCmdArgs
  -> ExceptT NodeCmdError IO ()
runNodeKeyGenKesCmd
  Cmd.NodeKeyGenKESCmdArgs
    { keyOutputFormat
    , vkeyFile
    , skeyFile
    } = do
    skey <- generateSigningKey AsKesKey

    let vkey = getVerificationKey skey

    keyOutputFormat
      & ( id
            . Vary.on
              ( \FormatBech32 ->
                  firstExceptT NodeCmdWriteFileError
                    . newExceptT
                    . writeTextFile skeyFile
                    $ serialiseToBech32 skey
              )
            . Vary.on
              ( \FormatTextEnvelope ->
                  firstExceptT NodeCmdWriteFileError
                    . newExceptT
                    . writeLazyByteStringFileWithOwnerPermissions skeyFile
                    $ textEnvelopeToJSON (Just skeyDesc) skey
              )
            $ Vary.exhaustiveCase
        )

    keyOutputFormat
      & ( id
            . Vary.on
              ( \FormatBech32 ->
                  firstExceptT NodeCmdWriteFileError
                    . newExceptT
                    . writeTextFile vkeyFile
                    $ serialiseToBech32 vkey
              )
            . Vary.on
              ( \FormatTextEnvelope ->
                  firstExceptT NodeCmdWriteFileError
                    . newExceptT
                    . writeLazyByteStringFile vkeyFile
                    $ textEnvelopeToJSON (Just vkeyDesc) vkey
              )
            $ Vary.exhaustiveCase
        )
   where
    skeyDesc :: TextEnvelopeDescr
    skeyDesc = "KES Signing Key"

    vkeyDesc :: TextEnvelopeDescr
    vkeyDesc = "KES Verification Key"

runNodeKeyGenVrfCmd
  :: ()
  => Cmd.NodeKeyGenVRFCmdArgs
  -> ExceptT NodeCmdError IO ()
runNodeKeyGenVrfCmd
  Cmd.NodeKeyGenVRFCmdArgs
    { keyOutputFormat
    , vkeyFile
    , skeyFile
    } = do
    skey <- generateSigningKey AsVrfKey

    let vkey = getVerificationKey skey

    keyOutputFormat
      & ( id
            . Vary.on
              ( \FormatBech32 ->
                  firstExceptT NodeCmdWriteFileError
                    . newExceptT
                    . writeTextFile skeyFile
                    $ serialiseToBech32 skey
              )
            . Vary.on
              ( \FormatTextEnvelope ->
                  firstExceptT NodeCmdWriteFileError
                    . newExceptT
                    . writeLazyByteStringFileWithOwnerPermissions skeyFile
                    $ textEnvelopeToJSON (Just skeyDesc) skey
              )
            $ Vary.exhaustiveCase
        )

    keyOutputFormat
      & ( id
            . Vary.on
              ( \FormatBech32 ->
                  firstExceptT NodeCmdWriteFileError
                    . newExceptT
                    $ writeTextFile vkeyFile
                    $ serialiseToBech32 vkey
              )
            . Vary.on
              ( \FormatTextEnvelope ->
                  firstExceptT NodeCmdWriteFileError
                    . newExceptT
                    $ writeLazyByteStringFile vkeyFile
                    $ textEnvelopeToJSON (Just vkeyDesc) vkey
              )
            $ Vary.exhaustiveCase
        )
   where
    skeyDesc, vkeyDesc :: TextEnvelopeDescr
    skeyDesc = "VRF Signing Key"
    vkeyDesc = "VRF Verification Key"

runNodeKeyHashVrfCmd
  :: ()
  => Cmd.NodeKeyHashVRFCmdArgs
  -> ExceptT NodeCmdError IO ()
runNodeKeyHashVrfCmd
  Cmd.NodeKeyHashVRFCmdArgs
    { vkeySource
    , mOutFile
    } = do
    vkey <-
      firstExceptT NodeCmdReadKeyFileError $
        readVerificationKeyOrFile vkeySource

    let hexKeyHash = serialiseToRawBytesHex (verificationKeyHash vkey)

    firstExceptT NodeCmdWriteFileError
      . newExceptT
      $ writeByteStringOutput mOutFile hexKeyHash

runNodeNewCounterCmd
  :: ()
  => Cmd.NodeNewCounterCmdArgs
  -> ExceptT NodeCmdError IO ()
runNodeNewCounterCmd
  Cmd.NodeNewCounterCmdArgs
    { coldVkeyFile
    , counter
    , mOutFile
    } = do
    vkey <-
      firstExceptT NodeCmdReadFileError . newExceptT $
        readColdVerificationKeyOrFile coldVkeyFile

    let ocertIssueCounter =
          OperationalCertificateIssueCounter
            (fromIntegral counter)
            ( case vkey of
                AnyStakePoolNormalVerificationKey normalStakePoolVKey -> normalStakePoolVKey
                AnyStakePoolExtendedVerificationKey extendedStakePoolVKey ->
                  castVerificationKey extendedStakePoolVKey
            )

    firstExceptT NodeCmdWriteFileError . newExceptT $
      writeLazyByteStringFile (onlyOut mOutFile) $
        textEnvelopeToJSON Nothing ocertIssueCounter

runNodeIssueOpCertCmd
  :: ()
  => Cmd.NodeIssueOpCertCmdArgs
  -> ExceptT NodeCmdError IO ()
runNodeIssueOpCertCmd
  Cmd.NodeIssueOpCertCmdArgs
    { kesVkeySource
    , poolSkeyFile
    , operationalCertificateCounterFile
    , kesPeriod
    , outFile
    } = do
    ocertIssueCounter <-
      firstExceptT NodeCmdReadFileError
        . newExceptT
        $ readFileTextEnvelope (onlyIn operationalCertificateCounterFile)

    verKeyKes <-
      firstExceptT NodeCmdReadKeyFileError $
        readVerificationKeyOrFile kesVkeySource

    signKey <-
      firstExceptT NodeCmdReadKeyFileError
        . newExceptT
        $ readFormattedFileAnyOf
          bech32PossibleBlockIssuers
          textEnvPossibleBlockIssuers
          poolSkeyFile

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
      $ writeLazyByteStringFile (onlyOut operationalCertificateCounterFile)
      $ textEnvelopeToJSON (Just $ ocertCtrDesc $ getCounter nextOcertCtr) nextOcertCtr

    firstExceptT NodeCmdWriteFileError
      . newExceptT
      $ writeLazyByteStringFile outFile
      $ textEnvelopeToJSON Nothing ocert
   where
    getCounter :: OperationalCertificateIssueCounter -> Word64
    getCounter (OperationalCertificateIssueCounter n _) = n

    ocertCtrDesc :: Word64 -> TextEnvelopeDescr
    ocertCtrDesc n = "Next certificate issue number: " <> fromString (show n)

    textEnvPossibleBlockIssuers
      :: [ FromSomeType
             HasTextEnvelope
             ( Either
                 AnyStakePoolSigningKey
                 (SigningKey GenesisDelegateExtendedKey)
             )
         ]
    textEnvPossibleBlockIssuers =
      [ FromSomeType (AsSigningKey AsStakePoolKey) (Left . AnyStakePoolNormalSigningKey)
      , FromSomeType (AsSigningKey AsStakePoolExtendedKey) (Left . AnyStakePoolExtendedSigningKey)
      , FromSomeType
          (AsSigningKey AsGenesisDelegateKey)
          (Left . AnyStakePoolNormalSigningKey . castSigningKey)
      , FromSomeType (AsSigningKey AsGenesisDelegateExtendedKey) Right
      ]

    bech32PossibleBlockIssuers
      :: [ FromSomeType
             SerialiseAsBech32
             ( Either
                 AnyStakePoolSigningKey
                 (SigningKey GenesisDelegateExtendedKey)
             )
         ]
    bech32PossibleBlockIssuers =
      [ FromSomeType (AsSigningKey AsStakePoolKey) (Left . AnyStakePoolNormalSigningKey)
      , FromSomeType (AsSigningKey AsStakePoolExtendedKey) (Left . AnyStakePoolExtendedSigningKey)
      ]

-- | Read a cold verification key or file.
--
-- If a filepath is provided, it will be interpreted as a text envelope
-- formatted file.
readColdVerificationKeyOrFile
  :: ColdVerificationKeyOrFile
  -> IO (Either (FileError TextEnvelopeError) AnyStakePoolVerificationKey)
readColdVerificationKeyOrFile coldVerKeyOrFile =
  case coldVerKeyOrFile of
    ColdStakePoolVerificationKey vk -> pure (Right vk)
    ColdGenesisDelegateVerificationKey vk ->
      pure $ Right (AnyStakePoolNormalVerificationKey $ castVerificationKey vk)
    ColdVerificationKeyFile fp ->
      readFileTextEnvelopeAnyOf
        [ FromSomeType (AsVerificationKey AsStakePoolKey) AnyStakePoolNormalVerificationKey
        , FromSomeType (AsVerificationKey AsStakePoolExtendedKey) AnyStakePoolExtendedVerificationKey
        , FromSomeType
            (AsVerificationKey AsGenesisDelegateKey)
            (AnyStakePoolNormalVerificationKey . castVerificationKey)
        ]
        fp
