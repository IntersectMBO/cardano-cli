{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.CLI.EraIndependent.Node.Option
  ( pNodeCmds
  , pBlsSigningKeyFile
  , pBlsVerificationKeyFile
  , pBlsPossessionProofFile
  )
where

import Cardano.Api (BlsPossessionProof, File (..), FileDirection (In))
import Cardano.Api.Experimental qualified as Exp

import Cardano.CLI.EraBased.Common.Option
import Cardano.CLI.EraIndependent.Node.Command
import Cardano.CLI.EraIndependent.Node.Command qualified as Cmd
import Cardano.CLI.Parser
import Cardano.CLI.Type.Common (SigningKeyFile, VerificationKeyFile)

import Data.Foldable
import Data.Maybe (catMaybes)
import Options.Applicative hiding (help, str)
import Options.Applicative qualified as Opt

pNodeCmds :: forall era. Exp.IsEra era => Parser NodeCmds
pNodeCmds =
  let nodeCmdParsers =
        asum $
          catMaybes
            [ Just $
                Opt.hsubparser $
                  commandWithMetavar "key-gen" $
                    Opt.info pKeyGenOperator $
                      Opt.progDesc $
                        mconcat
                          [ "Create a key pair for a node operator's offline "
                          , "key and a new certificate issue counter"
                          ]
            , Just $
                Opt.hsubparser $
                  commandWithMetavar "key-gen-KES" $
                    Opt.info pKeyGenKES $
                      Opt.progDesc $
                        mconcat
                          [ "Create a key pair for a node KES operational key"
                          ]
            , Just $
                Opt.hsubparser $
                  commandWithMetavar "key-gen-VRF" $
                    Opt.info pKeyGenVRF $
                      Opt.progDesc $
                        mconcat
                          [ "Create a key pair for a node VRF operational key"
                          ]
            , pKeyGenBLS @era
            , Just $
                Opt.hsubparser $
                  commandWithMetavar "key-hash-VRF" . Opt.info pKeyHashVRF $
                    Opt.progDesc $
                      mconcat
                        [ "Print hash of a node's operational VRF key."
                        ]
            , pKeyHashBLS @era
            , pIssuePopBLS @era
            , Just $
                Opt.hsubparser $
                  commandWithMetavar "new-counter" $
                    Opt.info pNewCounter $
                      Opt.progDesc $
                        mconcat
                          [ "Create a new certificate issue counter"
                          ]
            , Just
                $ Opt.hsubparser
                $ commandWithMetavar "issue-op-cert"
                $ Opt.info
                  ( fmap Cmd.NodeIssueOpCertCmd $
                      Cmd.NodeIssueOpCertCmdArgs
                        <$> pKesVerificationKeyOrFile
                        <*> pColdSigningKeyFile
                        <*> pOperatorCertIssueCounterFile
                        <*> pKesPeriod
                        <*> pOutputFile
                  )
                $ Opt.progDesc
                $ mconcat
                  [ "Issue a node operational certificate"
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

pKeyGenBLS :: forall era. Exp.IsEra era => Maybe (Parser NodeCmds)
pKeyGenBLS = case Exp.useEra @era of
  Exp.ConwayEra -> Nothing
  Exp.DijkstraEra ->
    Just
      $ Opt.hsubparser
      $ commandWithMetavar "key-gen-BLS"
      $ Opt.info
        ( fmap Cmd.NodeKeyGenBLSCmd $
            Cmd.NodeKeyGenBLSCmdArgs
              <$> pKeyOutputFormat
              <*> pVerificationKeyFileOut
              <*> pSigningKeyFileOut
        )
      $ Opt.progDesc
      $ mconcat
        [ "Create a key pair for a node BLS operational key"
        ]

pKeyHashVRF :: Parser NodeCmds
pKeyHashVRF =
  fmap Cmd.NodeKeyHashVRFCmd $
    Cmd.NodeKeyHashVRFCmdArgs
      <$> pVerificationKeyOrFileIn
      <*> pMaybeOutputFile

pKeyHashBLS :: forall era. Exp.IsEra era => Maybe (Parser NodeCmds)
pKeyHashBLS = case Exp.useEra @era of
  Exp.ConwayEra -> Nothing
  Exp.DijkstraEra ->
    Just
      $ Opt.hsubparser
      $ commandWithMetavar "key-hash-BLS"
        . Opt.info
          ( fmap Cmd.NodeKeyHashBLSCmd $
              Cmd.NodeKeyHashBLSCmdArgs
                <$> pVerificationKeyOrFileIn
                <*> pMaybeOutputFile
          )
      $ Opt.progDesc
      $ mconcat
        [ "Print hash of a node's operational BLS key."
        ]

pIssuePopBLS :: forall era. Exp.IsEra era => Maybe (Parser NodeCmds)
pIssuePopBLS = case Exp.useEra @era of
  Exp.ConwayEra -> Nothing
  Exp.DijkstraEra ->
    Just
      $ Opt.hsubparser
      $ commandWithMetavar "issue-pop-BLS"
      $ Opt.info
        ( fmap Cmd.NodeIssuePopBLSCmd $
            Cmd.NodeIssuePopBLSCmdArgs
              <$> pBlsSigningKeyFile
              <*> pOutputFile
        )
      $ Opt.progDesc
      $ mconcat
        [ "Issue a BLS proof of possession for a node's operational BLS key. "
        , "Both a BLS key and its proof of possession are required by stake pool "
        , "operators to participate as voting member/block producing node in Leios."
        ]

pBlsSigningKeyFile :: Parser (SigningKeyFile In)
pBlsSigningKeyFile =
  File
    <$> parseFilePath
      "bls-signing-key-file"
      "Input filepath of the BLS signing key."

pBlsVerificationKeyFile :: Parser (VerificationKeyFile In)
pBlsVerificationKeyFile =
  File
    <$> parseFilePath
      "bls-verification-key-file"
      "Input filepath of the BLS verification key."

pBlsPossessionProofFile :: Parser (File BlsPossessionProof In)
pBlsPossessionProofFile =
  File
    <$> parseFilePath
      "bls-possession-proof-file"
      "Input filepath of the BLS proof of possession, as produced by issue-pop-BLS."

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
