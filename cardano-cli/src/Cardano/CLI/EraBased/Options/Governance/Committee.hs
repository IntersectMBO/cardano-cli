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
    ]

pGovernanceCommitteeKeyGenCold :: ()
  => CardanoEra era
  -> Maybe (Parser (GovernanceCommitteeCmds era))
pGovernanceCommitteeKeyGenCold =
  featureInEra
    Nothing
    ( \w ->
        Just
          $ subParser "key-gen-cold"
          $ Opt.info (pCmd w)
          $ Opt.progDesc
          $ mconcat
              [ "Create a cold key pair for a Constitutional Committee Member"
              ]
    )
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
pGovernanceCommitteeKeyGenHot =
  featureInEra
    Nothing
    ( \w ->
        Just
          $ subParser "key-gen-hot"
          $ Opt.info (pCmd w)
          $ Opt.progDesc
          $ mconcat
              [ "Create a cold key pair for a Constitutional Committee Member"
              ]
    )
  where
    pCmd :: ()
      => ConwayEraOnwards era
      -> Parser (GovernanceCommitteeCmds era)
    pCmd w =
      GovernanceCommitteeKeyGenHot w
        <$> pVerificationKeyFileOut
        <*> pSigningKeyFileOut
