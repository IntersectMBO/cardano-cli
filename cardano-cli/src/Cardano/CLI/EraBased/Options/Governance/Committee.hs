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
    [ pGovernanceCommitteeKeyGenColdCmd era
    , pGovernanceCommitteeKeyGenHotCmd era
    , pGovernanceCommitteeKeyHashCmd era
    , pGovernanceCommitteeCreateHotKeyAuthorizationCertificateCmd era
    , pGovernanceCommitteeCreateColdKeyResignationCertificateCmd era
    ]

pGovernanceCommitteeKeyGenColdCmd :: ()
  => CardanoEra era
  -> Maybe (Parser (GovernanceCommitteeCmds era))
pGovernanceCommitteeKeyGenColdCmd era = do
  w <- forEraMaybeEon era
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
      fmap GovernanceCommitteeKeyGenColdCmd $
        GovernanceCommitteeKeyGenColdCmdArgs w
          <$> pColdVerificationKeyFile
          <*> pColdSigningKeyFile

pGovernanceCommitteeKeyGenHotCmd :: ()
  => CardanoEra era
  -> Maybe (Parser (GovernanceCommitteeCmds era))
pGovernanceCommitteeKeyGenHotCmd era = do
  w <- forEraMaybeEon era
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
      fmap GovernanceCommitteeKeyGenHotCmd $
        GovernanceCommitteeKeyGenHotCmdArgs w
          <$> pVerificationKeyFileOut
          <*> pSigningKeyFileOut

pGovernanceCommitteeKeyHashCmd :: ()
  => CardanoEra era
  -> Maybe (Parser (GovernanceCommitteeCmds era))
pGovernanceCommitteeKeyHashCmd era = do
  w <- forEraMaybeEon era
  pure
    $ subParser "key-hash"
    $ Opt.info
        ( fmap GovernanceCommitteeKeyHashCmd $
            GovernanceCommitteeKeyHashCmdArgs w
              <$> pAnyVerificationKeySource "Constitutional Committee Member key (hot or cold)"
        )
    $ Opt.progDesc
    $ mconcat
        [ "Print the identifier (hash) of a public key"
        ]

pGovernanceCommitteeCreateHotKeyAuthorizationCertificateCmd :: ()
  => CardanoEra era
  -> Maybe (Parser (GovernanceCommitteeCmds era))
pGovernanceCommitteeCreateHotKeyAuthorizationCertificateCmd era = do
  w <- forEraMaybeEon era
  pure
    $ subParser "create-hot-key-authorization-certificate"
    $ Opt.info
        ( fmap GovernanceCommitteeCreateHotKeyAuthorizationCertificateCmd $
            GovernanceCommitteeCreateHotKeyAuthorizationCertificateCmdArgs w
              <$> pCommitteeColdVerificationKeyOrHashOrFile
              <*> pCommitteeHotKeyOrHashOrFile
              <*> pOutputFile
        )
    $ Opt.progDesc
    $ mconcat
        [ "Create hot key authorization certificate for a Constitutional Committee Member"
        ]

pGovernanceCommitteeCreateColdKeyResignationCertificateCmd :: ()
  => CardanoEra era
  -> Maybe (Parser (GovernanceCommitteeCmds era))
pGovernanceCommitteeCreateColdKeyResignationCertificateCmd era = do
  w <- forEraMaybeEon era
  pure
    $ subParser "create-cold-key-resignation-certificate"
    $ Opt.info
        ( fmap GovernanceCommitteeCreateColdKeyResignationCertificateCmd $
            GovernanceCommitteeCreateColdKeyResignationCertificateCmdArgs w
              <$> pCommitteeColdVerificationKeyOrHashOrFile
              <*> pOutputFile
        )
    $ Opt.progDesc
    $ mconcat
        [ "Create cold key resignation certificate for a Constitutional Committee Member"
        ]
