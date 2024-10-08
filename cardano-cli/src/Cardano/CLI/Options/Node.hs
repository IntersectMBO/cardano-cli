{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.Options.Node
  ( pNodeCmds
  )
where

import           Cardano.Api hiding (QueryInShelleyBasedEra (..))

import           Cardano.CLI.Commands.Node
import qualified Cardano.CLI.Commands.Node as Cmd
import           Cardano.CLI.EraBased.Options.Common

import           Data.Foldable
import           Options.Applicative hiding (help, str)
import qualified Options.Applicative as Opt

{- HLINT ignore "Use <$>" -}
{- HLINT ignore "Move brackets to avoid $" -}

pNodeCmds :: Parser NodeCmds
pNodeCmds =
  let nodeCmdParsers =
        asum
          [ subParser "key-gen" $
              Opt.info pKeyGenOperator $
                Opt.progDesc $
                  mconcat
                    [ "Create a key pair for a node operator's offline "
                    , "key and a new certificate issue counter"
                    ]
          , subParser "key-gen-KES" $
              Opt.info pKeyGenKES $
                Opt.progDesc $
                  mconcat
                    [ "Create a key pair for a node KES operational key"
                    ]
          , subParser "key-gen-VRF" $
              Opt.info pKeyGenVRF $
                Opt.progDesc $
                  mconcat
                    [ "Create a key pair for a node VRF operational key"
                    ]
          , subParser "key-hash-VRF" . Opt.info pKeyHashVRF $
              Opt.progDesc $
                mconcat
                  [ "Print hash of a node's operational VRF key."
                  ]
          , subParser "new-counter" $
              Opt.info pNewCounter $
                Opt.progDesc $
                  mconcat
                    [ "Create a new certificate issue counter"
                    ]
          , subParser "issue-op-cert" $
              Opt.info pIssueOpCert $
                Opt.progDesc $
                  mconcat
                    [ "Issue a node operational certificate"
                    ]
          ]
   in subParser
        "node"
        $ Opt.info
          nodeCmdParsers
          ( Opt.progDesc $
              mconcat
                [ "Node operation commands."
                ]
          )

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
      <$> pVerificationKeyOrFileIn AsVrfKey
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
