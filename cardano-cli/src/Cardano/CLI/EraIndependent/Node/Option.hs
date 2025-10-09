{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.EraIndependent.Node.Option
  ( pNodeCmds
  )
where

import Cardano.CLI.EraBased.Common.Option
import Cardano.CLI.EraIndependent.Node.Command
import Cardano.CLI.EraIndependent.Node.Command qualified as Cmd
import Cardano.CLI.Parser

import Data.Foldable
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
              commandWithMetavar "key-hash-VRF" . Opt.info pKeyHashVRF $
                Opt.progDesc $
                  mconcat
                    [ "Print hash of a node's operational VRF key."
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

pKeyHashVRF :: Parser NodeCmds
pKeyHashVRF =
  fmap Cmd.NodeKeyHashVRFCmd $
    Cmd.NodeKeyHashVRFCmdArgs
      <$> pVerificationKeyOrFileIn
      <*> pMaybeOutputFile

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
