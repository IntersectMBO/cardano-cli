{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.Compatible.Governance.Option
  ( pCompatibleGovernanceCmds
  )
where

import Cardano.Api
import Cardano.Api.Experimental (obtainCommonConstraints)

import Cardano.CLI.Compatible.Governance.Command
import Cardano.CLI.EraBased.Common.Option
import Cardano.CLI.EraBased.Governance.Actions.Command
import Cardano.CLI.EraBased.Governance.Actions.Option
  ( dpGovActionProtocolParametersUpdate
  , pCostModelsFile
  , pProtocolParametersUpdateGenesisKeys
  , pUpdateProtocolParametersPostConway
  )
import Cardano.CLI.EraBased.Governance.Command
import Cardano.CLI.EraBased.Governance.Option (pCreateMirCertificatesCmds)
import Cardano.CLI.EraBased.Governance.Option qualified as Latest
import Cardano.CLI.Parser

import Data.Foldable
import Data.Maybe
import Options.Applicative
import Options.Applicative qualified as Opt

pCompatibleGovernanceCmds :: ShelleyBasedEra era -> Parser (CompatibleGovernanceCmds era)
pCompatibleGovernanceCmds sbe =
  asum $ catMaybes [fmap CreateCompatibleProtocolUpdateCmd <$> pGovernanceCmds sbe]

pGovernanceCmds
  :: ShelleyBasedEra era
  -> Maybe (Parser (GovernanceCmds era))
pGovernanceCmds sbe =
  caseShelleyToBabbageOrConwayEraOnwards
    ( const $
        subInfoParser
          "governance"
          ( Opt.progDesc $
              mconcat
                [ "Governance commands."
                ]
          )
          [ pCreateMirCertificatesCmds sbe
          , pGovernanceGenesisKeyDelegationCertificate sbe
          , fmap GovernanceActionCmds <$> pGovernanceActionCmds sbe
          ]
    )
    (\w -> obtainCommonConstraints (convert w) Latest.pGovernanceCmds)
    sbe

pGovernanceActionCmds :: ShelleyBasedEra era -> Maybe (Parser (GovernanceActionCmds era))
pGovernanceActionCmds sbe =
  subInfoParser
    "action"
    ( Opt.progDesc $
        mconcat
          [ "Governance action commands."
          ]
    )
    [ pGovernanceActionProtocolParametersUpdateCmd sbe
    ]

pGovernanceActionProtocolParametersUpdateCmd
  :: ()
  => ShelleyBasedEra era
  -> Maybe (Parser (GovernanceActionCmds era))
pGovernanceActionProtocolParametersUpdateCmd sbe = do
  w <- forShelleyBasedEraMaybeEon sbe
  pure $
    GovernanceActionProtocolParametersUpdateCmd
      <$> pUpdateProtocolParametersCmd w

pUpdateProtocolParametersCmd
  :: ShelleyBasedEra era -> Parser (GovernanceActionProtocolParametersUpdateCmdArgs era)
pUpdateProtocolParametersCmd =
  caseShelleyToBabbageOrConwayEraOnwards
    ( \shelleyToBab ->
        let sbe = convert shelleyToBab
         in Opt.hsubparser
              $ commandWithMetavar "create-protocol-parameters-update"
              $ Opt.info
                ( GovernanceActionProtocolParametersUpdateCmdArgs
                    (convert shelleyToBab)
                    <$> fmap Just (pUpdateProtocolParametersPreConway shelleyToBab)
                    <*> pure Nothing
                    <*> dpGovActionProtocolParametersUpdate sbe
                    <*> pCostModelsFile sbe
                    <*> pOutputFile
                )
              $ Opt.progDesc "Create a protocol parameters update."
    )
    ( \conwayOnwards ->
        let sbe = convert conwayOnwards
            ppup = fmap Just (obtainCommonConstraints (convert conwayOnwards) pUpdateProtocolParametersPostConway)
         in Opt.hsubparser
              $ commandWithMetavar "create-protocol-parameters-update"
              $ Opt.info
                ( GovernanceActionProtocolParametersUpdateCmdArgs
                    (convert conwayOnwards)
                    Nothing
                    <$> ppup
                    <*> dpGovActionProtocolParametersUpdate sbe
                    <*> pCostModelsFile sbe
                    <*> pOutputFile
                )
              $ Opt.progDesc "Create a protocol parameters update."
    )

pUpdateProtocolParametersPreConway
  :: ShelleyToBabbageEra era -> Parser (UpdateProtocolParametersPreConway era)
pUpdateProtocolParametersPreConway shelleyToBab =
  UpdateProtocolParametersPreConway shelleyToBab
    <$> pEpochNoUpdateProp
    <*> pProtocolParametersUpdateGenesisKeys

pGovernanceGenesisKeyDelegationCertificate
  :: ()
  => ShelleyBasedEra era
  -> Maybe (Parser (GovernanceCmds era))
pGovernanceGenesisKeyDelegationCertificate sbe = do
  w <- forShelleyBasedEraMaybeEon sbe
  pure $
    Opt.hsubparser $
      commandWithMetavar "create-genesis-key-delegation-certificate" $
        Opt.info (parser w) $
          Opt.progDesc "Create a genesis key delegation certificate"
 where
  parser w =
    GovernanceGenesisKeyDelegationCertificate w
      <$> pGenesisVerificationKeyOrHashOrFile
      <*> pGenesisDelegateVerificationKeyOrHashOrFile
      <*> pVrfVerificationKeyOrHashOrFile
      <*> pOutputFile
