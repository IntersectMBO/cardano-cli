{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- HLINT ignore "Use <$>" -}
{- HLINT ignore "Move brackets to avoid $" -}

module Cardano.CLI.Legacy.Options.Node
  ( parseNodeCmds
  ) where

import           Cardano.Api hiding (QueryInShelleyBasedEra (..))

import           Cardano.CLI.EraBased.Options.Common
import           Cardano.CLI.Legacy.Commands.Node

import           Data.Foldable
import           Options.Applicative hiding (help, str)
import qualified Options.Applicative as Opt

parseNodeCmds :: Parser LegacyNodeCmds
parseNodeCmds =
  asum
    [ subParser "key-gen" . Opt.info pKeyGenOperator . Opt.progDesc $ mconcat
      [ "Create a key pair for a node operator's offline "
      , "key and a new certificate issue counter"
      ]
    , subParser "key-gen-KES" . Opt.info pKeyGenKES . Opt.progDesc $ mconcat
      [ "Create a key pair for a node KES operational key"
      ]
    , subParser "key-gen-VRF" . Opt.info pKeyGenVRF . Opt.progDesc $ mconcat
      [ "Create a key pair for a node VRF operational key"
      ]
    , subParser "key-hash-VRF". Opt.info pKeyHashVRF . Opt.progDesc $ mconcat
      [ "Print hash of a node's operational VRF key."
      ]
    , subParser "new-counter" . Opt.info pNewCounter . Opt.progDesc $ mconcat
      [ "Create a new certificate issue counter"
      ]
    , subParser "issue-op-cert" . Opt.info pIssueOpCert . Opt.progDesc $ mconcat
      [ "Issue a node operational certificate"
      ]
    ]
  where
    pKeyGenOperator :: Parser LegacyNodeCmds
    pKeyGenOperator =
      NodeKeyGenCold
        <$> pKeyOutputFormat
        <*> pColdVerificationKeyFile
        <*> pColdSigningKeyFile
        <*> pOperatorCertIssueCounterFile

    pKeyGenKES :: Parser LegacyNodeCmds
    pKeyGenKES =
      NodeKeyGenKES
        <$> pKeyOutputFormat
        <*> pVerificationKeyFileOut
        <*> pSigningKeyFileOut

    pKeyGenVRF :: Parser LegacyNodeCmds
    pKeyGenVRF =
      NodeKeyGenVRF
        <$> pKeyOutputFormat
        <*> pVerificationKeyFileOut
        <*> pSigningKeyFileOut

    pKeyHashVRF :: Parser LegacyNodeCmds
    pKeyHashVRF =
      NodeKeyHashVRF
        <$> pVerificationKeyOrFile AsVrfKey
        <*> pMaybeOutputFile

    pNewCounter :: Parser LegacyNodeCmds
    pNewCounter =
      NodeNewCounter
        <$> pColdVerificationKeyOrFile
        <*> pCounterValue
        <*> pOperatorCertIssueCounterFile

    pCounterValue :: Parser Word
    pCounterValue =
      Opt.option Opt.auto $ mconcat
        [ Opt.long "counter-value"
        , Opt.metavar "INT"
        , Opt.help "The next certificate issue counter value to use."
        ]

    pIssueOpCert :: Parser LegacyNodeCmds
    pIssueOpCert =
      NodeIssueOpCert
        <$> pKesVerificationKeyOrFile
        <*> pColdSigningKeyFile
        <*> pOperatorCertIssueCounterFile
        <*> pKesPeriod
        <*> pOutputFile
