{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
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

import Cardano.CLI.Compatible.Exception
import Cardano.CLI.EraIndependent.Node.Command qualified as Cmd
import Cardano.CLI.Type.Common
import Cardano.CLI.Type.Key

import Data.Function ((&))
import Data.String (fromString)
import Data.Word (Word64)
import Vary qualified

{- HLINT ignore "Reduce duplication" -}

runNodeCmds
  :: ()
  => Cmd.NodeCmds
  -> CIO e ()
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
  -> CIO e ()
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
                  fromEitherIOCli @(FileError ())
                    . writeTextFile skeyFile
                    $ serialiseToBech32 skey
              )
            . Vary.on
              ( \FormatTextEnvelope ->
                  fromEitherIOCli @(FileError ()) . writeLazyByteStringFile skeyFile $
                    textEnvelopeToJSON (Just skeyDesc) skey
              )
            $ Vary.exhaustiveCase
        )

    keyOutputFormat
      & ( id
            . Vary.on
              ( \FormatBech32 ->
                  fromEitherIOCli @(FileError ())
                    . writeTextFile vkeyFile
                    $ serialiseToBech32 vkey
              )
            . Vary.on
              ( \FormatTextEnvelope ->
                  fromEitherIOCli @(FileError ())
                    . writeLazyByteStringFile vkeyFile
                    $ textEnvelopeToJSON (Just vkeyDesc) vkey
              )
            $ Vary.exhaustiveCase
        )

    fromEitherIOCli @(FileError ()) $
      writeLazyByteStringFile operationalCertificateIssueCounter $
        textEnvelopeToJSON (Just ocertCtrDesc) $
          OperationalCertificateIssueCounter
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
  -> CIO e ()
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
                  fromEitherIOCli @(FileError ())
                    . writeTextFile skeyFile
                    $ serialiseToBech32 skey
              )
            . Vary.on
              ( \FormatTextEnvelope ->
                  fromEitherIOCli @(FileError ())
                    . writeLazyByteStringFileWithOwnerPermissions skeyFile
                    $ textEnvelopeToJSON (Just skeyDesc) skey
              )
            $ Vary.exhaustiveCase
        )

    keyOutputFormat
      & ( id
            . Vary.on
              ( \FormatBech32 ->
                  fromEitherIOCli @(FileError ())
                    . writeTextFile vkeyFile
                    $ serialiseToBech32 vkey
              )
            . Vary.on
              ( \FormatTextEnvelope ->
                  fromEitherIOCli @(FileError ())
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
  -> CIO e ()
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
                  fromEitherIOCli @(FileError ())
                    . writeTextFile skeyFile
                    $ serialiseToBech32 skey
              )
            . Vary.on
              ( \FormatTextEnvelope ->
                  fromEitherIOCli @(FileError ())
                    . writeLazyByteStringFileWithOwnerPermissions skeyFile
                    $ textEnvelopeToJSON (Just skeyDesc) skey
              )
            $ Vary.exhaustiveCase
        )

    keyOutputFormat
      & ( id
            . Vary.on
              ( \FormatBech32 ->
                  fromEitherIOCli @(FileError ()) $
                    writeTextFile vkeyFile $
                      serialiseToBech32 vkey
              )
            . Vary.on
              ( \FormatTextEnvelope ->
                  fromEitherIOCli @(FileError ()) $
                    writeLazyByteStringFile vkeyFile $
                      textEnvelopeToJSON (Just vkeyDesc) vkey
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
  -> CIO e ()
runNodeKeyHashVrfCmd
  Cmd.NodeKeyHashVRFCmdArgs
    { vkeySource
    , mOutFile
    } = do
    vkey <-
      readVerificationKeyOrFile vkeySource

    let hexKeyHash = serialiseToRawBytesHex (verificationKeyHash vkey)

    fromEitherIOCli @(FileError ()) $
      writeByteStringOutput mOutFile hexKeyHash

runNodeNewCounterCmd
  :: ()
  => Cmd.NodeNewCounterCmdArgs
  -> CIO e ()
runNodeNewCounterCmd
  Cmd.NodeNewCounterCmdArgs
    { coldVkeyFile
    , counter
    , mOutFile
    } = do
    vkey <-
      fromEitherIOCli $
        readColdVerificationKeyOrFile coldVkeyFile

    let ocertIssueCounter =
          OperationalCertificateIssueCounter
            (fromIntegral counter)
            ( case vkey of
                AnyStakePoolNormalVerificationKey normalStakePoolVKey -> normalStakePoolVKey
                AnyStakePoolExtendedVerificationKey extendedStakePoolVKey ->
                  castVerificationKey extendedStakePoolVKey
            )

    fromEitherIOCli @(FileError ()) $
      writeLazyByteStringFile (onlyOut mOutFile) $
        textEnvelopeToJSON Nothing ocertIssueCounter

runNodeIssueOpCertCmd
  :: ()
  => Cmd.NodeIssueOpCertCmdArgs
  -> CIO e ()
runNodeIssueOpCertCmd
  Cmd.NodeIssueOpCertCmdArgs
    { kesVkeySource
    , poolSkeyFile
    , operationalCertificateCounterFile
    , kesPeriod
    , outFile
    } = do
    ocertIssueCounter <-
      fromEitherIOCli $
        readFileTextEnvelope (onlyIn operationalCertificateCounterFile)

    verKeyKes <-
      readVerificationKeyOrFile kesVkeySource

    signKey <-
      fromEitherIOCli $
        readFormattedFileAnyOf
          bech32PossibleBlockIssuers
          textEnvPossibleBlockIssuers
          poolSkeyFile

    (ocert, nextOcertCtr) <-
      fromEitherCli $
        issueOperationalCertificate
          verKeyKes
          signKey
          kesPeriod
          ocertIssueCounter

    -- Write the counter first, to reduce the chance of ending up with
    -- a new cert but without updating the counter.
    fromEitherIOCli @(FileError ()) $
      writeLazyByteStringFile (onlyOut operationalCertificateCounterFile) $
        textEnvelopeToJSON (Just $ ocertCtrDesc $ getCounter nextOcertCtr) nextOcertCtr

    fromEitherIOCli @(FileError ()) $
      writeLazyByteStringFile outFile $
        textEnvelopeToJSON Nothing ocert
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
