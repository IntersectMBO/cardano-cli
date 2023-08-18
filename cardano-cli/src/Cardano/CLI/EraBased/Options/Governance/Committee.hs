{-# LANGUAGE DataKinds #-}

module Cardano.CLI.EraBased.Options.Governance.Committee
  ( pGovernanceCommitteeCmds
  ) where

import           Cardano.Api

import           Cardano.CLI.EraBased.Commands.Governance.Committee
import           Cardano.CLI.EraBased.Options.Common

import           Options.Applicative (Parser)
import qualified Options.Applicative as Opt

pGovernanceCommitteeCmds :: ()
  => CardanoEra era
  -> Maybe (Parser (GovernanceCommitteeCmds era))
pGovernanceCommitteeCmds era =
  subInfoParser "committee"
    ( Opt.progDesc
        $ mconcat
          [ "Committee member commands."
          ]
    )
    [ pGovernanceCommitteeKeyGenCold era
    , pGovernanceCommitteeKeyGenHot era
    , pGovernanceCommitteeKeyHash era
    , pGovernanceCommitteeCreateHotKeyAuthorizationCertificate era
    , pGovernanceCommitteeCreateColdKeyResignationCertificate era
    ]

pGovernanceCommitteeKeyGenCold :: ()
  => CardanoEra era
  -> Maybe (Parser (GovernanceCommitteeCmds era))
pGovernanceCommitteeKeyGenCold era = do
  w <- maybeFeatureInEra era
  pure
    $ subParser "key-gen-cold"
    $ Opt.info (pCmd w)
    $ Opt.progDesc
    $ mconcat
        [ "Create a cold key pair for a Constitutional Committee Member"
        ]
  where
    pCmd :: ()
      => ConwayEraOnwards era
      -> Parser (GovernanceCommitteeCmds era)
    pCmd w =
      GovernanceCommitteeKeyGenCold w
        <$> pColdVerificationKeyFile
        <*> pColdSigningKeyFile

pGovernanceCommitteeKeyGenHot :: ()
  => CardanoEra era
  -> Maybe (Parser (GovernanceCommitteeCmds era))
pGovernanceCommitteeKeyGenHot era = do
  w <- maybeFeatureInEra era
  pure
    $ subParser "key-gen-hot"
    $ Opt.info (pCmd w)
    $ Opt.progDesc
    $ mconcat
        [ "Create a cold key pair for a Constitutional Committee Member"
        ]
  where
    pCmd :: ()
      => ConwayEraOnwards era
      -> Parser (GovernanceCommitteeCmds era)
    pCmd w =
      GovernanceCommitteeKeyGenHot w
        <$> pVerificationKeyFileOut
        <*> pSigningKeyFileOut

pGovernanceCommitteeKeyHash :: ()
  => CardanoEra era
  -> Maybe (Parser (GovernanceCommitteeCmds era))
pGovernanceCommitteeKeyHash era = do
  w <- maybeFeatureInEra era
  pure
    $ subParser "key-hash"
    $ Opt.info
        ( GovernanceCommitteeKeyHash w
            <$> pAnyVerificationKeySource "Constitutional Committee Member key (hot or cold)"
        )
    $ Opt.progDesc
    $ mconcat
        [ "Print the identifier (hash) of a public key"
        ]

pGovernanceCommitteeCreateHotKeyAuthorizationCertificate :: ()
  => CardanoEra era
  -> Maybe (Parser (GovernanceCommitteeCmds era))
pGovernanceCommitteeCreateHotKeyAuthorizationCertificate era = do
  w <- maybeFeatureInEra era
  pure
    $ subParser "create-hot-key-authorization-certificate"
    $ Opt.info
        ( GovernanceCommitteeCreateHotKeyAuthorizationCertificate w
            <$> pCommitteeColdVerificationKeyOrHashOrFile
            <*> pCommitteeHotKeyOrHashOrFile
            <*> pOutputFile
        )
    $ Opt.progDesc
    $ mconcat
        [ "Create hot key authorization certificate for a Constitutional Committee Member"
        ]

pGovernanceCommitteeCreateColdKeyResignationCertificate :: ()
  => CardanoEra era
  -> Maybe (Parser (GovernanceCommitteeCmds era))
pGovernanceCommitteeCreateColdKeyResignationCertificate era = do
  w <- maybeFeatureInEra era
  pure
    $ subParser "create-cold-key-resignation-certificate"
    $ Opt.info
        ( GovernanceCommitteeCreateColdKeyResignationCertificate w
            <$> pCommitteeColdVerificationKeyOrHashOrFile
            <*> pOutputFile
        )
    $ Opt.progDesc
    $ mconcat
        [ "Create cold key resignation certificate for a Constitutional Committee Member"
        ]
