{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.CLI.EraBased.Governance.Committee.Option
  ( pGovernanceCommitteeCmds
  )
where

import Cardano.Api
import Cardano.Api.Experimental qualified as Exp
import Cardano.Api.Ledger qualified as L

import Cardano.CLI.EraBased.Common.Option hiding (pAnchorUrl)
import Cardano.CLI.EraBased.Governance.Committee.Command
import Cardano.CLI.Parser
import Cardano.CLI.Read
import Cardano.CLI.Type.Key

import Data.Foldable (asum)
import Options.Applicative (Parser, optional)
import Options.Applicative qualified as Opt

pGovernanceCommitteeCmds
  :: Exp.IsEra era => Maybe (Parser (GovernanceCommitteeCmds era))
pGovernanceCommitteeCmds =
  subInfoParser
    "committee"
    ( Opt.progDesc $
        mconcat
          [ "Committee member commands."
          ]
    )
    [ pGovernanceCommitteeKeyGenColdCmd
    , pGovernanceCommitteeKeyGenHotCmd
    , pGovernanceCommitteeKeyHashCmd
    , pGovernanceCommitteeCreateHotKeyAuthorizationCertificateCmd
    , pGovernanceCommitteeCreateColdKeyResignationCertificateCmd
    ]

pGovernanceCommitteeKeyGenColdCmd
  :: Exp.IsEra era => Maybe (Parser (GovernanceCommitteeCmds era))
pGovernanceCommitteeKeyGenColdCmd = do
  pure $
    Opt.hsubparser $
      commandWithMetavar "key-gen-cold" $
        Opt.info (pCmd Exp.useEra) $
          Opt.progDesc $
            mconcat
              [ "Create a cold key pair for a Constitutional Committee Member"
              ]
 where
  pCmd w =
    fmap GovernanceCommitteeKeyGenColdCmd $
      GovernanceCommitteeKeyGenColdCmdArgs w
        <$> pColdVerificationKeyFile
        <*> pColdSigningKeyFile

pGovernanceCommitteeKeyGenHotCmd
  :: Exp.IsEra era => Maybe (Parser (GovernanceCommitteeCmds era))
pGovernanceCommitteeKeyGenHotCmd = do
  pure $
    Opt.hsubparser $
      commandWithMetavar "key-gen-hot" $
        Opt.info (pCmd Exp.useEra) $
          Opt.progDesc $
            mconcat
              [ "Create a hot key pair for a Constitutional Committee Member"
              ]
 where
  pCmd w =
    fmap GovernanceCommitteeKeyGenHotCmd $
      GovernanceCommitteeKeyGenHotCmdArgs w
        <$> pVerificationKeyFileOut
        <*> pSigningKeyFileOut

pGovernanceCommitteeKeyHashCmd
  :: Exp.IsEra era => Maybe (Parser (GovernanceCommitteeCmds era))
pGovernanceCommitteeKeyHashCmd = do
  pure
    $ Opt.hsubparser
    $ commandWithMetavar "key-hash"
    $ Opt.info
      ( fmap GovernanceCommitteeKeyHashCmd $
          GovernanceCommitteeKeyHashCmdArgs Exp.useEra
            <$> pAnyVerificationKeySource "Constitutional Committee Member key (hot or cold)"
      )
    $ Opt.progDesc
    $ mconcat
      [ "Print the identifier (hash) of a public key"
      ]

pGovernanceCommitteeCreateHotKeyAuthorizationCertificateCmd
  :: Exp.IsEra era => Maybe (Parser (GovernanceCommitteeCmds era))
pGovernanceCommitteeCreateHotKeyAuthorizationCertificateCmd = do
  pure
    $ Opt.hsubparser
    $ commandWithMetavar "create-hot-key-authorization-certificate"
    $ Opt.info
      ( fmap GovernanceCommitteeCreateHotKeyAuthorizationCertificateCmd $
          GovernanceCommitteeCreateHotKeyAuthorizationCertificateCmdArgs Exp.useEra
            <$> pColdCredential
            <*> pHotCredential
            <*> pOutputFile
      )
    $ Opt.progDesc
    $ mconcat
      [ "Create hot key authorization certificate for a Constitutional Committee Member"
      ]

pGovernanceCommitteeCreateColdKeyResignationCertificateCmd
  :: Exp.IsEra era => Maybe (Parser (GovernanceCommitteeCmds era))
pGovernanceCommitteeCreateColdKeyResignationCertificateCmd = do
  pure $
    Opt.hsubparser $
      commandWithMetavar "create-cold-key-resignation-certificate" $
        Opt.info (mkParser Exp.useEra) $
          Opt.progDesc $
            mconcat
              [ "Create cold key resignation certificate for a Constitutional Committee Member"
              ]
 where
  mkParser w =
    GovernanceCommitteeCreateColdKeyResignationCertificateCmd
      <$> ( GovernanceCommitteeCreateColdKeyResignationCertificateCmdArgs w
              <$> pColdCredential
              <*> optional
                ( pPotentiallyCheckedAnchorData
                    pMustCheckResignationMetadataHash
                    pAnchor
                )
              <*> pOutputFile
          )

pColdCredential :: Parser (VerificationKeySource CommitteeColdKey)
pColdCredential =
  asum
    [ VksKeyHashFile . VerificationKeyOrFile <$> pCommitteeColdVerificationKeyOrFile
    , VksKeyHashFile . VerificationKeyHash <$> pCommitteeColdVerificationKeyHash
    , VksScriptHash
        <$> pScriptHash
          "cold-script-hash"
          "Committee cold Native or Plutus script file hash (hex-encoded). Obtain it with \"cardano-cli hash script ...\"."
    , VksScript <$> pScriptFor "cold-script-file" Nothing "Cold Native or Plutus script file"
    ]

pHotCredential :: Parser (VerificationKeySource CommitteeHotKey)
pHotCredential =
  asum
    [ VksKeyHashFile . VerificationKeyOrFile <$> pCommitteeHotVerificationKeyOrFile
    , VksKeyHashFile . VerificationKeyHash <$> pCommitteeHotVerificationKeyHash
    , VksScriptHash
        <$> pScriptHash
          "hot-script-hash"
          "Committee hot Native or Plutus script file hash (hex-encoded). Obtain it with \"cardano-cli hash script ...\"."
    , VksScript <$> pScriptFor "hot-script-file" Nothing "Hot Native or Plutus script file"
    ]

pAnchor :: Parser L.Anchor
pAnchor =
  L.Anchor
    <$> fmap unAnchorUrl pAnchorUrl
    <*> pSafeHash

pAnchorUrl :: Parser AnchorUrl
pAnchorUrl =
  AnchorUrl
    <$> pUrl "resignation-metadata-url" "Constitutional Committee cold key resignation certificate URL"

pSafeHash :: Parser (L.SafeHash L.AnchorData)
pSafeHash =
  Opt.option readSafeHash $
    mconcat
      [ Opt.long "resignation-metadata-hash"
      , Opt.metavar "HASH"
      , Opt.help "Constitutional Committee cold key resignation certificate metadata hash"
      ]
