module Cardano.CLI.Compatible.Governance.Option
  ( pCompatibleGovernanceCmds
  )
where

import Cardano.Api
import Cardano.Api.Experimental qualified as Exp

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
      [ inEonForShelleyBasedEra
          ( subInfoParser
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
          )
          ( \w ->
              fmap LatestCompatibleGovernanceCmds <$> Exp.obtainCommonConstraints w Latest.pGovernanceCmds
          )
          sbe
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
  inEonForShelleyBasedEra (preConway sbe) postConway sbe
 where
  -- The two branches build the same command and differ only in which of the two
  -- optional payloads they populate.
  mkCmd
    :: ShelleyBasedEra era'
    -> Parser (Maybe (UpdateProtocolParametersPreConway era'))
    -> Parser (Maybe (UpdateProtocolParametersConwayOnwards era'))
    -> Parser (GovernanceActionProtocolParametersUpdateCmdArgs era')
  mkCmd sbe' pPreConway pConwayOnwards =
    Opt.hsubparser
      $ commandWithMetavar "create-protocol-parameters-update"
      $ Opt.info
        ( GovernanceActionProtocolParametersUpdateCmdArgs sbe'
            <$> pPreConway
            <*> pConwayOnwards
            <*> pGovActionProtocolParametersUpdate sbe'
            <*> pCostModelsFile sbe'
            <*> pOutputFile
        )
      $ Opt.progDesc "Create a protocol parameters update."

  preConway
    :: ShelleyBasedEra era'
    -> Parser (GovernanceActionProtocolParametersUpdateCmdArgs era')
  preConway sbe' =
    mkCmd sbe' (Just <$> pUpdateProtocolParametersPreConway) (pure Nothing)

  postConway
    :: ConwayEraOnwards era'
    -> Parser (GovernanceActionProtocolParametersUpdateCmdArgs era')
  postConway conwayOnwards =
    mkCmd
      (convert conwayOnwards)
      (pure Nothing)
      ( Just
          <$> Exp.obtainCommonConstraints (convert conwayOnwards) pUpdateProtocolParametersPostConway
      )

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
