{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.EraBased.Options.Node
  ( pNodeCmds
  )
where

import           Cardano.Api hiding (QueryInShelleyBasedEra (..))

import           Cardano.CLI.EraBased.Commands.Node
import qualified Cardano.CLI.EraBased.Commands.Node as Cmd
import           Cardano.CLI.EraBased.Options.Common

import           Options.Applicative hiding (help, str)
import qualified Options.Applicative as Opt

{- HLINT ignore "Use <$>" -}
{- HLINT ignore "Move brackets to avoid $" -}

pNodeCmds :: Maybe (Parser (NodeCmds era))
pNodeCmds =
  subInfoParser
    "node"
    ( Opt.progDesc $
        mconcat
          [ "Node operation commands."
          ]
    )
    [ Just $
        subParser "key-gen" $
          Opt.info pKeyGenOperator $
            Opt.progDesc $
              mconcat
                [ "Create a key pair for a node operator's offline "
                , "key and a new certificate issue counter"
                ]
    , Just $
        subParser "key-gen-KES" $
          Opt.info pKeyGenKES $
            Opt.progDesc $
              mconcat
                [ "Create a key pair for a node KES operational key"
                ]
    , Just $
        subParser "key-gen-VRF" $
          Opt.info pKeyGenVRF $
            Opt.progDesc $
              mconcat
                [ "Create a key pair for a node VRF operational key"
                ]
    , Just $
        subParser "key-hash-VRF" . Opt.info pKeyHashVRF $
          Opt.progDesc $
            mconcat
              [ "Print hash of a node's operational VRF key."
              ]
    , Just $
        subParser "new-counter" $
          Opt.info pNewCounter $
            Opt.progDesc $
              mconcat
                [ "Create a new certificate issue counter"
                ]
    , Just $
        subParser "issue-op-cert" $
          Opt.info pIssueOpCert $
            Opt.progDesc $
              mconcat
                [ "Issue a node operational certificate"
                ]
    ]

pKeyGenOperator :: Parser (NodeCmds era)
pKeyGenOperator =
  fmap Cmd.NodeKeyGenColdCmd $
    Cmd.NodeKeyGenColdCmdArgs
      <$> pKeyOutputFormat
      <*> pColdVerificationKeyFile
      <*> pColdSigningKeyFile
      <*> pOperatorCertIssueCounterFile

pKeyGenKES :: Parser (NodeCmds era)
pKeyGenKES =
  fmap Cmd.NodeKeyGenKESCmd $
    Cmd.NodeKeyGenKESCmdArgs
      <$> pKeyOutputFormat
      <*> pVerificationKeyFileOut
      <*> pSigningKeyFileOut

pKeyGenVRF :: Parser (NodeCmds era)
pKeyGenVRF =
  fmap Cmd.NodeKeyGenVRFCmd $
    Cmd.NodeKeyGenVRFCmdArgs
      <$> pKeyOutputFormat
      <*> pVerificationKeyFileOut
      <*> pSigningKeyFileOut

pKeyHashVRF :: Parser (NodeCmds era)
pKeyHashVRF =
  fmap Cmd.NodeKeyHashVRFCmd $
    Cmd.NodeKeyHashVRFCmdArgs
      <$> pVerificationKeyOrFileIn AsVrfKey
      <*> pMaybeOutputFile

pNewCounter :: Parser (NodeCmds era)
pNewCounter =
  fmap Cmd.NodeNewCounterCmd $
    Cmd.NodeNewCounterCmdArgs
      <$> pColdVerificationKeyOrFile Nothing
      <*> pCounterValue
      <*> pOperatorCertIssueCounterFile

pCounterValue :: Parser Word
pCounterValue =
  Opt.option Opt.auto $
    mconcat
      [ Opt.long "counter-value"
      , Opt.metavar "INT"
      , Opt.help "The next certificate issue counter value to use."
      ]

pIssueOpCert :: Parser (NodeCmds era)
pIssueOpCert =
  fmap Cmd.NodeIssueOpCertCmd $
    Cmd.NodeIssueOpCertCmdArgs
      <$> pKesVerificationKeyOrFile
      <*> pColdSigningKeyFile
      <*> pOperatorCertIssueCounterFile
      <*> pKesPeriod
      <*> pOutputFile
