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
  ( pCostModelsFile
  , pGovActionProtocolParametersUpdate
  , pProtocolParametersUpdateGenesisKeys
  , pUpdateProtocolParametersPostConway
  )
import Cardano.CLI.EraBased.Governance.Command
import Cardano.CLI.EraBased.Governance.Option qualified as Latest
import Cardano.CLI.Parser

import Data.Foldable
import Data.Maybe
import Options.Applicative
import Options.Applicative qualified as Opt

pCompatibleGovernanceCmds
  :: ShelleyBasedEra era
  -> Parser (CompatibleGovernanceCmds era)
pCompatibleGovernanceCmds sbe =
  asum $
    catMaybes
      [ caseShelleyToBabbageOrConwayEraOnwards
          ( const $
              subInfoParser
                "governance"
                ( Opt.progDesc $
                    mconcat
                      [ "Governance commands."
                      ]
                )
                [ fmap CreateCompatibleMirCertificateCmd <$> pCreateMirCertificatesCmds sbe
                , fmap CreateCompatibleGenesisKeyDelegationCertificateCmd
                    <$> pGovernanceGenesisKeyDelegationCertificate sbe
                , fmap CreateCompatibleProtocolParametersUpdateCmd <$> pGovernanceActionCmds sbe
                ]
          )
          ( \w ->
              fmap LatestCompatibleGovernanceCmds <$> obtainCommonConstraints (convert w) Latest.pGovernanceCmds
          )
          sbe
      ]

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
                    <*> pGovActionProtocolParametersUpdate sbe
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
                    <*> pGovActionProtocolParametersUpdate sbe
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

pCreateMirCertificatesCmds :: ShelleyBasedEra era -> Maybe (Parser (GovernanceCmds era))
pCreateMirCertificatesCmds era' = do
  w <- forShelleyBasedEraMaybeEon era'
  pure $
    Opt.hsubparser $
      commandWithMetavar "create-mir-certificate" $
        Opt.info (pMIRPayStakeAddresses w <|> mirCertParsers w) $
          Opt.progDesc "Create an MIR (Move Instantaneous Rewards) certificate"

mirCertParsers
  :: ()
  => ShelleyToBabbageEra era
  -> Parser (GovernanceCmds era)
mirCertParsers w =
  asum
    [ Opt.hsubparser $
        commandWithMetavar "stake-addresses" $
          Opt.info (pMIRPayStakeAddresses w) $
            Opt.progDesc "Create an MIR certificate to pay stake addresses"
    , Opt.hsubparser $
        commandWithMetavar "transfer-to-treasury" $
          Opt.info (pGovernanceCreateMirCertificateTransferToTreasuryCmd w) $
            Opt.progDesc "Create an MIR certificate to transfer from the reserves pot to the treasury pot"
    , Opt.hsubparser $
        commandWithMetavar "transfer-to-rewards" $
          Opt.info (pGovernanceCreateMirCertificateTransferToReservesCmd w) $
            Opt.progDesc "Create an MIR certificate to transfer from the treasury pot to the reserves pot"
    ]

pMIRPayStakeAddresses
  :: ()
  => ShelleyToBabbageEra era
  -> Parser (GovernanceCmds era)
pMIRPayStakeAddresses w =
  GovernanceCreateMirCertificateStakeAddressesCmd w
    <$> pMIRPot
    <*> some (pStakeAddress Nothing)
    <*> some pRewardAmt
    <*> pOutputFile

pGovernanceCreateMirCertificateTransferToTreasuryCmd
  :: ()
  => ShelleyToBabbageEra era
  -> Parser (GovernanceCmds era)
pGovernanceCreateMirCertificateTransferToTreasuryCmd w =
  GovernanceCreateMirCertificateTransferToTreasuryCmd w
    <$> pTransferAmt
    <*> pOutputFile

pGovernanceCreateMirCertificateTransferToReservesCmd
  :: ()
  => ShelleyToBabbageEra era
  -> Parser (GovernanceCmds era)
pGovernanceCreateMirCertificateTransferToReservesCmd w =
  GovernanceCreateMirCertificateTransferToReservesCmd w
    <$> pTransferAmt
    <*> pOutputFile
