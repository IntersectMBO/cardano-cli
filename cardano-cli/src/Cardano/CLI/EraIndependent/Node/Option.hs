{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.EraIndependent.Node.Option
  ( pNodeCmds
  )
where

import Cardano.Api
  ( Bech32DecodeError (..)
  , BlsKey
  , BlsPossessionProof
  , File (..)
  , FileDirection (In)
  , VerificationKey
  , deserialiseFromBech32
  , deserialiseFromRawBytesHex
  , displayError
  , docToString
  , prettyError
  )

import Cardano.CLI.EraBased.Common.Option
import Cardano.CLI.EraIndependent.Node.Command
import Cardano.CLI.EraIndependent.Node.Command qualified as Cmd
import Cardano.CLI.Parser
import Cardano.CLI.Type.Common (SigningKeyFile, VerificationKeyFile)
import Cardano.CLI.Type.Key (VerificationKeyOrFile (..))

import Data.Bifunctor (first)
import Data.ByteString.Char8 qualified as BSC
import Data.Foldable
import Data.Text qualified as Text
import Options.Applicative hiding (help, str)
import Options.Applicative qualified as Opt

pNodeCmds :: Parser NodeCmds
pNodeCmds =
  let nodeCmdParsers =
        asum
          [ Opt.hsubparser $
              commandWithMetavar "key-gen" $
                Opt.info pKeyGenOperator $
                  Opt.progDesc $
                    mconcat
                      [ "Create a key pair for a node operator's offline "
                      , "key and a new certificate issue counter"
                      ]
          , Opt.hsubparser $
              commandWithMetavar "key-gen-KES" $
                Opt.info pKeyGenKES $
                  Opt.progDesc $
                    mconcat
                      [ "Create a key pair for a node KES operational key"
                      ]
          , Opt.hsubparser $
              commandWithMetavar "key-gen-VRF" $
                Opt.info pKeyGenVRF $
                  Opt.progDesc $
                    mconcat
                      [ "Create a key pair for a node VRF operational key"
                      ]
          , Opt.hsubparser $
              commandWithMetavar "key-gen-BLS" $
                Opt.info pKeyGenBLS $
                  Opt.progDesc $
                    mconcat
                      [ "Create a key pair for a node BLS operational key"
                      ]
          , Opt.hsubparser $
              commandWithMetavar "key-hash-VRF" . Opt.info pKeyHashVRF $
                Opt.progDesc $
                  mconcat
                    [ "Print hash of a node's operational VRF key."
                    ]
          , Opt.hsubparser $
              commandWithMetavar "key-hash-BLS" . Opt.info pKeyHashBLS $
                Opt.progDesc $
                  mconcat
                    [ "Print hash of a node's operational BLS key."
                    ]
          , Opt.hsubparser $
              commandWithMetavar "issue-pop-BLS" $
                Opt.info pIssuePopBLS $
                  Opt.progDesc $
                    mconcat
                      [ "Issue a BLS proof of possession"
                      ]
          , Opt.hsubparser $
              commandWithMetavar "new-counter" $
                Opt.info pNewCounter $
                  Opt.progDesc $
                    mconcat
                      [ "Create a new certificate issue counter"
                      ]
          , Opt.hsubparser $
              commandWithMetavar "issue-op-cert" $
                Opt.info pIssueOpCert $
                  Opt.progDesc $
                    mconcat
                      [ "Issue a node operational certificate"
                      ]
          , Opt.hsubparser $
              commandWithMetavar "issue-leios-op-cert" $
                Opt.info pIssueLeiosOpCert $
                  Opt.progDesc $
                    mconcat
                      [ "Issue a node operational certificate for Leios"
                      ]
          ]
   in Opt.hsubparser $
        commandWithMetavar "node" $
          Opt.info nodeCmdParsers $
            Opt.progDesc $
              mconcat
                [ "Node operation commands."
                ]

pKeyGenOperator :: Parser NodeCmds
pKeyGenOperator =
  fmap Cmd.NodeKeyGenColdCmd $
    Cmd.NodeKeyGenColdCmdArgs
      <$> pKeyOutputFormat
      <*> pColdVerificationKeyFile
      <*> pColdSigningKeyFile
      <*> pOperatorCertIssueCounterFile

pKeyGenKES :: Parser NodeCmds
pKeyGenKES =
  fmap Cmd.NodeKeyGenKESCmd $
    Cmd.NodeKeyGenKESCmdArgs
      <$> pKeyOutputFormat
      <*> pVerificationKeyFileOut
      <*> pSigningKeyFileOut

pKeyGenVRF :: Parser NodeCmds
pKeyGenVRF =
  fmap Cmd.NodeKeyGenVRFCmd $
    Cmd.NodeKeyGenVRFCmdArgs
      <$> pKeyOutputFormat
      <*> pVerificationKeyFileOut
      <*> pSigningKeyFileOut

pKeyGenBLS :: Parser NodeCmds
pKeyGenBLS =
  fmap Cmd.NodeKeyGenBLSCmd $
    Cmd.NodeKeyGenBLSCmdArgs
      <$> pKeyOutputFormat
      <*> pVerificationKeyFileOut
      <*> pSigningKeyFileOut

pKeyHashVRF :: Parser NodeCmds
pKeyHashVRF =
  fmap Cmd.NodeKeyHashVRFCmd $
    Cmd.NodeKeyHashVRFCmdArgs
      <$> pVerificationKeyOrFileIn
      <*> pMaybeOutputFile

pKeyHashBLS :: Parser NodeCmds
pKeyHashBLS =
  fmap Cmd.NodeKeyHashBLSCmd $
    Cmd.NodeKeyHashBLSCmdArgs
      <$> pVerificationKeyOrFileIn
      <*> pMaybeOutputFile

pIssuePopBLS :: Parser NodeCmds
pIssuePopBLS =
  fmap Cmd.NodeIssuePopBLSCmd $
    Cmd.NodeIssuePopBLSCmdArgs
      <$> pBlsSigningKeyFile
      <*> pOutputFile

pBlsSigningKeyFile :: Parser (SigningKeyFile In)
pBlsSigningKeyFile =
  File
    <$> parseFilePath
      "bls-signing-key-file"
      "Input filepath of the BLS signing key."

pNewCounter :: Parser NodeCmds
pNewCounter =
  fmap Cmd.NodeNewCounterCmd $
    Cmd.NodeNewCounterCmdArgs
      <$> pColdVerificationKeyOrFile Nothing
      <*> pCounterValue
      <*> pOperatorCertIssueCounterFile

pCounterValue :: Parser Word
pCounterValue =
  Opt.option integralReader $
    mconcat
      [ Opt.long "counter-value"
      , Opt.metavar "INT"
      , Opt.help "The next certificate issue counter value to use."
      ]

pIssueOpCert :: Parser NodeCmds
pIssueOpCert =
  fmap Cmd.NodeIssueOpCertCmd $
    Cmd.NodeIssueOpCertCmdArgs
      <$> pKesVerificationKeyOrFile
      <*> pColdSigningKeyFile
      <*> pOperatorCertIssueCounterFile
      <*> pKesPeriod
      <*> pOutputFile

pIssueLeiosOpCert :: Parser NodeCmds
pIssueLeiosOpCert =
  fmap Cmd.NodeIssueLeiosOpCertCmd $
    Cmd.NodeIssueLeiosOpCertCmdArgs
      <$> pKesVerificationKeyOrFile
      <*> pColdSigningKeyFile
      <*> pOperatorCertIssueCounterFile
      <*> pKesPeriod
      <*> pBlsVerificationKeyOrFile
      <*> pBlsPossessionProofFile
      <*> pOutputFile

pBlsVerificationKeyOrFile :: Parser (VerificationKeyOrFile BlsKey)
pBlsVerificationKeyOrFile =
  asum
    [ VerificationKeyValue <$> pBlsVerificationKey
    , VerificationKeyFilePath <$> pBlsVerificationKeyFile
    ]

pBlsVerificationKey :: Parser (VerificationKey BlsKey)
pBlsVerificationKey =
  Opt.option (Opt.eitherReader deserialiseVerKey) $
    mconcat
      [ Opt.long "bls-verification-key"
      , Opt.metavar "STRING"
      , Opt.help "A Bech32 or hex-encoded BLS verification key."
      ]
 where
  deserialiseVerKey :: String -> Either String (VerificationKey BlsKey)
  deserialiseVerKey str =
    case deserialiseFromBech32 (Text.pack str) of
      Right res -> Right res
      -- The input was valid Bech32, but some other error occurred.
      Left err@(Bech32UnexpectedPrefix _ _) -> Left $ displayError err
      Left err@(Bech32UnexpectedHeader _ _) -> Left $ displayError err
      Left err@(Bech32DataPartToBytesError _) -> Left $ displayError err
      Left err@(Bech32DeserialiseFromBytesError _) -> Left $ displayError err
      Left err@(Bech32WrongPrefix _ _) -> Left $ displayError err
      -- The input was not valid Bech32. Attempt to deserialise it as hex.
      Left (Bech32DecodingError _) ->
        first
          (\e -> docToString $ "Invalid BLS verification key: " <> prettyError e)
          $ deserialiseFromRawBytesHex (BSC.pack str)

pBlsVerificationKeyFile :: Parser (VerificationKeyFile In)
pBlsVerificationKeyFile =
  File
    <$> parseFilePath
      "bls-verification-key-file"
      "Filepath of the BLS verification key."

pBlsPossessionProofFile :: Parser (File BlsPossessionProof In)
pBlsPossessionProofFile =
  File
    <$> parseFilePath
      "bls-possession-proof-file"
      "Input filepath of the BLS possession proof."
