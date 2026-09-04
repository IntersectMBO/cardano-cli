{-# LANGUAGE GADTs #-}

module Cardano.CLI.Compatible.Governance.Option
  ( pCompatibleGovernanceCmds
  )
where

import Cardano.Api

import Cardano.CLI.Compatible.Governance.Command
import Cardano.CLI.Compatible.Governance.Types
import Cardano.CLI.EraBased.Common.Option
import Cardano.CLI.EraBased.Governance.Actions.Option
  ( pCostModelsFile
  , pGovActionProtocolParametersUpdate
  , pProtocolParametersUpdateGenesisKeys
  , pUpdateProtocolParametersPostConway
  )
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
      [ case sbe of
          ShelleyBasedEraShelley -> preConway
          ShelleyBasedEraAllegra -> preConway
          ShelleyBasedEraMary -> preConway
          ShelleyBasedEraAlonzo -> preConway
          ShelleyBasedEraBabbage -> preConway
          ShelleyBasedEraConway ->
            fmap LatestCompatibleGovernanceCmds <$> Latest.pGovernanceCmds
          ShelleyBasedEraDijkstra ->
            fmap LatestCompatibleGovernanceCmds <$> Latest.pGovernanceCmds
      ]
 where
  preConway =
    subInfoParser
      "governance"
      ( Opt.progDesc $
          mconcat
            [ "Governance commands."
            ]
      )
      [ pCreateMirCertificatesCmds sbe
      , pGovernanceGenesisKeyDelegationCertificate
      , fmap CreateCompatibleProtocolParametersUpdateCmd <$> pGovernanceActionCmds sbe
      ]

pGovernanceActionCmds
  :: ShelleyBasedEra era -> Maybe (Parser (GovernanceActionProtocolParametersUpdateCmdArgs era))
pGovernanceActionCmds sbe =
  subInfoParser
    "action"
    ( Opt.progDesc $
        mconcat
          [ "Governance action commands."
          ]
    )
    [ Just $ pUpdateProtocolParametersCmd sbe
    ]

pUpdateProtocolParametersCmd
  :: ShelleyBasedEra era -> Parser (GovernanceActionProtocolParametersUpdateCmdArgs era)
pUpdateProtocolParametersCmd sbe =
  case sbe of
    ShelleyBasedEraShelley -> pPreConwayUpdateProtocolParametersCmd sbe
    ShelleyBasedEraAllegra -> pPreConwayUpdateProtocolParametersCmd sbe
    ShelleyBasedEraMary -> pPreConwayUpdateProtocolParametersCmd sbe
    ShelleyBasedEraAlonzo -> pPreConwayUpdateProtocolParametersCmd sbe
    ShelleyBasedEraBabbage -> pPreConwayUpdateProtocolParametersCmd sbe
    ShelleyBasedEraConway -> pPostConwayUpdateProtocolParametersCmd sbe
    ShelleyBasedEraDijkstra -> pPostConwayUpdateProtocolParametersCmd sbe

pPreConwayUpdateProtocolParametersCmd
  :: ShelleyBasedEra era -> Parser (GovernanceActionProtocolParametersUpdateCmdArgs era)
pPreConwayUpdateProtocolParametersCmd sbe =
  Opt.hsubparser
    $ commandWithMetavar "create-protocol-parameters-update"
    $ Opt.info
      ( GovernanceActionProtocolParametersUpdateCmdArgs sbe
          <$> fmap Just pUpdateProtocolParametersPreConway
          <*> pure Nothing
          <*> pGovActionProtocolParametersUpdate sbe
          <*> pCostModelsFile sbe
          <*> pOutputFile
      )
    $ Opt.progDesc "Create a protocol parameters update."

pPostConwayUpdateProtocolParametersCmd
  :: ShelleyBasedEra era -> Parser (GovernanceActionProtocolParametersUpdateCmdArgs era)
pPostConwayUpdateProtocolParametersCmd sbe =
  Opt.hsubparser
    $ commandWithMetavar "create-protocol-parameters-update"
    $ Opt.info
      ( GovernanceActionProtocolParametersUpdateCmdArgs sbe Nothing
          <$> pConwayOnwards
          <*> pGovActionProtocolParametersUpdate sbe
          <*> pCostModelsFile sbe
          <*> pOutputFile
      )
    $ Opt.progDesc "Create a protocol parameters update."
 where
  pConwayOnwards =
    case sbe of
      ShelleyBasedEraShelley -> pure Nothing
      ShelleyBasedEraAllegra -> pure Nothing
      ShelleyBasedEraMary -> pure Nothing
      ShelleyBasedEraAlonzo -> pure Nothing
      ShelleyBasedEraBabbage -> pure Nothing
      ShelleyBasedEraConway -> Just <$> pUpdateProtocolParametersPostConway
      ShelleyBasedEraDijkstra -> Just <$> pUpdateProtocolParametersPostConway

pUpdateProtocolParametersPreConway
  :: Parser (UpdateProtocolParametersPreConway era)
pUpdateProtocolParametersPreConway =
  UpdateProtocolParametersPreConway
    <$> pEpochNoUpdateProp
    <*> pProtocolParametersUpdateGenesisKeys

pGovernanceGenesisKeyDelegationCertificate
  :: Maybe (Parser (CompatibleGovernanceCmds era))
pGovernanceGenesisKeyDelegationCertificate = do
  pure $
    Opt.hsubparser $
      commandWithMetavar "create-genesis-key-delegation-certificate" $
        Opt.info parser $
          Opt.progDesc "Create a genesis key delegation certificate"
 where
  parser =
    CompatibleGenesisKeyDelegationCertificate
      <$> pGenesisVerificationKeyOrHashOrFile
      <*> pGenesisDelegateVerificationKeyOrHashOrFile
      <*> pVrfVerificationKeyOrHashOrFile
      <*> pOutputFile

pCreateMirCertificatesCmds :: ShelleyBasedEra era -> Maybe (Parser (CompatibleGovernanceCmds era))
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
  -> Parser (CompatibleGovernanceCmds era)
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
  -> Parser (CompatibleGovernanceCmds era)
pMIRPayStakeAddresses w =
  CompatibleCreateMirCertificateStakeAddressesCmd w
    <$> pMIRPot
    <*> some (pStakeAddress Nothing)
    <*> some pRewardAmt
    <*> pOutputFile

pGovernanceCreateMirCertificateTransferToTreasuryCmd
  :: ()
  => ShelleyToBabbageEra era
  -> Parser (CompatibleGovernanceCmds era)
pGovernanceCreateMirCertificateTransferToTreasuryCmd w =
  CompatibleCreateMirCertificateTransferToTreasuryCmd w
    <$> pTransferAmt
    <*> pOutputFile

pGovernanceCreateMirCertificateTransferToReservesCmd
  :: ()
  => ShelleyToBabbageEra era
  -> Parser (CompatibleGovernanceCmds era)
pGovernanceCreateMirCertificateTransferToReservesCmd w =
  CompatibleCreateMirCertificateTransferToReservesCmd w
    <$> pTransferAmt
    <*> pOutputFile
