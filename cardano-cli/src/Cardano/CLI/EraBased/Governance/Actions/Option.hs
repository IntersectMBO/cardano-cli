{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.Governance.Actions.Option
  ( pGovernanceActionCmds
  )
where

import Cardano.Api
import Cardano.Api.Experimental (Era)
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Shelley

import Cardano.CLI.EraBased.Common.Option
import Cardano.CLI.EraBased.Governance.Actions.Command qualified as Cmd
import Cardano.CLI.Option.Flag (setDefault)
import Cardano.CLI.Parser
import Cardano.CLI.Type.Common

import Data.Foldable
import Data.Function ((&))
import GHC.Natural (Natural)
import Options.Applicative
import Options.Applicative qualified as Opt

pGovernanceActionCmds
  :: ()
  => ShelleyBasedEra era
  -> Maybe (Parser (Cmd.GovernanceActionCmds era))
pGovernanceActionCmds era =
  subInfoParser
    "action"
    ( Opt.progDesc $
        mconcat
          [ "Governance action commands."
          ]
    )
    [ pGovernanceActionNewConstitutionCmd era
    , pGovernanceActionUpdateCommitteeCmd era
    , pGovernanceActionNewInfoCmd era
    , pGovernanceActionNoConfidenceCmd era
    , pGovernanceActionProtocolParametersUpdateCmd era
    , pGovernanceActionTreasuryWithdrawalCmd era
    , pGovernanceActionHardforkInitCmd era
    , pGovernanceActionViewCmd era
    ]

pGovernanceActionViewCmd
  :: ShelleyBasedEra era
  -> Maybe (Parser (Cmd.GovernanceActionCmds era))
pGovernanceActionViewCmd era = do
  eon <- forShelleyBasedEraMaybeEon era
  return
    $ Opt.hsubparser
    $ commandWithMetavar "view"
    $ Opt.info
      ( fmap Cmd.GovernanceActionViewCmd $
          Cmd.GovernanceActionViewCmdArgs eon
            <$> pFileInDirection "action-file" "Path to action file."
            <*> pFormatFlags
              "governance action view output"
              [ flagFormatJson & setDefault
              , flagFormatYaml
              ]
            <*> pMaybeOutputFile
      )
    $ Opt.progDesc "View a governance action."

pGovernanceActionNewInfoCmd
  :: ShelleyBasedEra era
  -> Maybe (Parser (Cmd.GovernanceActionCmds era))
pGovernanceActionNewInfoCmd era = do
  eon <- forShelleyBasedEraMaybeEon era
  pure
    $ Opt.hsubparser
    $ commandWithMetavar "create-info"
    $ Opt.info
      ( fmap Cmd.GovernanceActionInfoCmd $
          Cmd.GovernanceActionInfoCmdArgs eon
            <$> pNetwork
            <*> pGovActionDeposit
            <*> pStakeIdentifier (Just "deposit-return")
            <*> pAnchorUrl
            <*> pAnchorDataHash
            <*> pMustCheckProposalHash
            <*> pFileOutDirection "out-file" "Path to action file to be used later on with build or build-raw "
      )
    $ Opt.progDesc "Create an info action."

pGovernanceActionNewConstitutionCmd
  :: ShelleyBasedEra era
  -> Maybe (Parser (Cmd.GovernanceActionCmds era))
pGovernanceActionNewConstitutionCmd era = do
  eon <- forShelleyBasedEraMaybeEon era
  pure
    $ Opt.hsubparser
    $ commandWithMetavar "create-constitution"
    $ Opt.info
      ( fmap Cmd.GovernanceActionCreateConstitutionCmd $
          Cmd.GovernanceActionCreateConstitutionCmdArgs eon
            <$> pNetwork
            <*> pGovActionDeposit
            <*> pStakeIdentifier (Just "deposit-return")
            <*> pPreviousGovernanceAction
            <*> pAnchorUrl
            <*> pAnchorDataHash
            <*> pMustCheckProposalHash
            <*> pConstitutionUrl
            <*> pConstitutionHash
            <*> pMustCheckConstitutionHash
            <*> optional pConstitutionScriptHash
            <*> pFileOutDirection "out-file" "Output filepath of the constitution."
      )
    $ Opt.progDesc "Create a constitution."

pGovernanceActionUpdateCommitteeCmd
  :: ShelleyBasedEra era
  -> Maybe (Parser (Cmd.GovernanceActionCmds era))
pGovernanceActionUpdateCommitteeCmd era = do
  eon <- forShelleyBasedEraMaybeEon era
  pure
    $ Opt.hsubparser
    $ commandWithMetavar "update-committee"
    $ Opt.info
      ( Cmd.GovernanceActionUpdateCommitteeCmd
          <$> pUpdateCommitteeCmd eon
      )
    $ Opt.progDesc "Create or update a new committee proposal."

pUpdateCommitteeCmd
  :: ()
  => Era era
  -> Parser (Cmd.GovernanceActionUpdateCommitteeCmdArgs era)
pUpdateCommitteeCmd eon =
  Cmd.GovernanceActionUpdateCommitteeCmdArgs eon
    <$> pNetwork
    <*> pGovActionDeposit
    <*> pStakeIdentifier (Just "deposit-return")
    <*> pAnchorUrl
    <*> pAnchorDataHash
    <*> pMustCheckProposalHash
    <*> many pRemoveCommitteeColdVerificationKeySource
    <*> many
      ( (,)
          <$> pAddCommitteeColdVerificationKeySource
          <*> pEpochNo "Committee member expiry epoch"
      )
    <*> pRational "threshold" "Threshold of YES votes that are necessary for approving a governance action."
    <*> pPreviousGovernanceAction
    <*> pOutputFile

pGovernanceActionNoConfidenceCmd
  :: ShelleyBasedEra era
  -> Maybe (Parser (Cmd.GovernanceActionCmds era))
pGovernanceActionNoConfidenceCmd era = do
  eon <- forShelleyBasedEraMaybeEon era
  pure
    $ Opt.hsubparser
    $ commandWithMetavar "create-no-confidence"
    $ Opt.info
      ( fmap Cmd.GovernanceActionCreateNoConfidenceCmd $
          Cmd.GovernanceActionCreateNoConfidenceCmdArgs eon
            <$> pNetwork
            <*> pGovActionDeposit
            <*> pStakeIdentifier (Just "deposit-return")
            <*> pAnchorUrl
            <*> pAnchorDataHash
            <*> pMustCheckProposalHash
            <*> pPreviousGovernanceAction
            <*> pFileOutDirection "out-file" "Output filepath of the no confidence proposal."
      )
    $ Opt.progDesc "Create a no confidence proposal."

pUpdateProtocolParametersPreConway
  :: ShelleyToBabbageEra era -> Parser (Cmd.UpdateProtocolParametersPreConway era)
pUpdateProtocolParametersPreConway shelleyToBab =
  Cmd.UpdateProtocolParametersPreConway shelleyToBab
    <$> pEpochNoUpdateProp
    <*> pProtocolParametersUpdateGenesisKeys

pUpdateProtocolParametersPostConway
  :: ConwayEraOnwards era -> Parser (Cmd.UpdateProtocolParametersConwayOnwards era)
pUpdateProtocolParametersPostConway conwayOnwards =
  Cmd.UpdateProtocolParametersConwayOnwards conwayOnwards
    <$> pNetwork
    <*> pGovActionDeposit
    <*> pStakeIdentifier (Just "deposit-return")
    <*> pAnchorUrl
    <*> pAnchorDataHash
    <*> pMustCheckProposalHash
    <*> pPreviousGovernanceAction
    <*> optional pConstitutionScriptHash

pUpdateProtocolParametersCmd
  :: ShelleyBasedEra era -> Parser (Cmd.GovernanceActionProtocolParametersUpdateCmdArgs era)
pUpdateProtocolParametersCmd =
  caseShelleyToBabbageOrConwayEraOnwards
    ( \shelleyToBab ->
        let sbe = convert shelleyToBab
         in Opt.hsubparser
              $ commandWithMetavar "create-protocol-parameters-update"
              $ Opt.info
                ( Cmd.GovernanceActionProtocolParametersUpdateCmdArgs
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
         in Opt.hsubparser
              $ commandWithMetavar "create-protocol-parameters-update"
              $ Opt.info
                ( Cmd.GovernanceActionProtocolParametersUpdateCmdArgs
                    (convert conwayOnwards)
                    Nothing
                    <$> fmap Just (pUpdateProtocolParametersPostConway conwayOnwards)
                    <*> dpGovActionProtocolParametersUpdate sbe
                    <*> pCostModelsFile sbe
                    <*> pOutputFile
                )
              $ Opt.progDesc "Create a protocol parameters update."
    )

-- | Cost models only makes sense in eras from Alonzo onwards. For earlier
-- eras, this parser doesn't show up in the command line and returns 'Nothing'.
pCostModelsFile :: ShelleyBasedEra era -> Parser (Maybe (Cmd.CostModelsFile era))
pCostModelsFile =
  caseShelleyToMaryOrAlonzoEraOnwards
    (const $ pure Nothing)
    ( \alonzoOnwards ->
        fmap (Cmd.CostModelsFile alonzoOnwards . File)
          <$> optional pCostModels
    )

pGovernanceActionProtocolParametersUpdateCmd
  :: ()
  => ShelleyBasedEra era
  -> Maybe (Parser (Cmd.GovernanceActionCmds era))
pGovernanceActionProtocolParametersUpdateCmd era = do
  w <- forShelleyBasedEraMaybeEon era
  pure $
    Cmd.GovernanceActionProtocolParametersUpdateCmd
      <$> pUpdateProtocolParametersCmd w

pMinFeeRefScriptCostPerByte :: Parser L.NonNegativeInterval
pMinFeeRefScriptCostPerByte =
  Opt.option (toNonNegativeIntervalOrErr <$> readRational) $
    mconcat
      [ Opt.long "ref-script-cost-per-byte"
      , Opt.metavar "RATIONAL"
      , Opt.help "Reference script cost per byte for the minimum fee calculation."
      ]

convertToLedger :: (a -> b) -> Parser (Maybe a) -> Parser (L.StrictMaybe b)
convertToLedger conv = fmap (L.maybeToStrictMaybe . fmap conv)

toNonNegativeIntervalOrErr :: Rational -> L.NonNegativeInterval
toNonNegativeIntervalOrErr r = case L.boundRational r of
  Nothing ->
    error $
      mconcat
        [ "toNonNegativeIntervalOrErr: "
        , "rational out of bounds " <> show r
        ]
  Just n -> n

mkProtocolVersionOrErr :: (Natural, Natural) -> L.ProtVer
mkProtocolVersionOrErr (majorProtVer, minorProtVer) =
  case (`L.ProtVer` minorProtVer) <$> L.mkVersion majorProtVer of
    Just v -> v
    Nothing ->
      error $ "mkProtocolVersionOrErr: invalid protocol version " <> show (majorProtVer, minorProtVer)

pCommonProtocolParameters :: Parser CommonProtocolParametersUpdate
pCommonProtocolParameters =
  CommonProtocolParametersUpdate
    <$> convertToLedger id (optional pMinFeePerByteFactor)
    <*> convertToLedger id (optional pMinFeeConstantFactor)
    <*> convertToLedger id (optional pMaxBodySize)
    <*> convertToLedger id (optional pMaxTransactionSize)
    <*> convertToLedger id (optional pMaxBlockHeaderSize)
    <*> convertToLedger id (optional pKeyRegistDeposit)
    <*> convertToLedger id (optional pPoolDeposit)
    <*> convertToLedger id (optional pEpochBoundRetirement)
    <*> convertToLedger id (optional pNumberOfPools)
    <*> convertToLedger toNonNegativeIntervalOrErr (optional pPoolInfluence)
    <*> convertToLedger toUnitIntervalOrErr (optional pTreasuryExpansion)
    <*> convertToLedger toUnitIntervalOrErr (optional pMonetaryExpansion)
    <*> convertToLedger id (optional pMinPoolCost)

pDeprecatedAfterMaryPParams :: Parser (DeprecatedAfterMaryPParams ledgerera)
pDeprecatedAfterMaryPParams =
  DeprecatedAfterMaryPParams
    <$> convertToLedger id (optional pMinUTxOValue)

pDeprecatedAfterBabbagePParams :: Parser (DeprecatedAfterBabbagePParams ledgerera)
pDeprecatedAfterBabbagePParams =
  DeprecatedAfterBabbagePParams
    <$> convertToLedger mkProtocolVersionOrErr (optional pProtocolVersion)

pShelleyToAlonzoPParams :: Parser (ShelleyToAlonzoPParams ledgerera)
pShelleyToAlonzoPParams =
  ShelleyToAlonzoPParams
    <$> convertToLedger id (optional $ toLedgerNonce <$> pExtraEntropy)
    <*> convertToLedger toUnitIntervalOrErr (optional pDecentralParam)

pAlonzoOnwardsPParams :: Parser (AlonzoOnwardsPParams ledgerera)
pAlonzoOnwardsPParams =
  AlonzoOnwardsPParams L.SNothing -- The cost models are read separately from a file, so we use 'SNothing' as the place holder here
    <$> convertToLedger
      (either (\e -> error $ "pAlonzoOnwardsPParams: " <> show e) id . toAlonzoPrices)
      (optional pExecutionUnitPrices)
    <*> convertToLedger toAlonzoExUnits (optional pMaxTxExecutionUnits)
    <*> convertToLedger toAlonzoExUnits (optional pMaxBlockExecutionUnits)
    <*> convertToLedger id (optional pMaxValueSize)
    <*> convertToLedger id (optional pCollateralPercent)
    <*> convertToLedger id (optional pMaxCollateralInputs)

pIntroducedInBabbagePParams :: Parser (IntroducedInBabbagePParams ledgerera)
pIntroducedInBabbagePParams =
  IntroducedInBabbagePParams
    <$> convertToLedger L.CoinPerByte (optional pUTxOCostPerByte)

pIntroducedInConwayPParams :: Parser (IntroducedInConwayPParams ledgerera)
pIntroducedInConwayPParams =
  IntroducedInConwayPParams
    <$> convertToLedger id (optional pPoolVotingThresholds)
    <*> convertToLedger id (optional pDRepVotingThresholds)
    <*> convertToLedger id (optional pMinCommitteeSize)
    <*> convertToLedger id (optional pCommitteeTermLength)
    <*> convertToLedger id (optional pGovActionLifetime)
    <*> convertToLedger id (optional pNewGovActionDeposit)
    <*> convertToLedger id (optional pDRepDeposit)
    <*> convertToLedger id (optional pDRepActivity)
    <*> convertToLedger id (optional pMinFeeRefScriptCostPerByte)

-- Not necessary in Conway era onwards
pProtocolParametersUpdateGenesisKeys :: Parser [VerificationKeyFile In]
pProtocolParametersUpdateGenesisKeys = some pGenesisVerificationKeyFile

dpGovActionProtocolParametersUpdate
  :: ShelleyBasedEra era -> Parser (EraBasedProtocolParametersUpdate era)
dpGovActionProtocolParametersUpdate = \case
  ShelleyBasedEraShelley ->
    ShelleyEraBasedProtocolParametersUpdate
      <$> pCommonProtocolParameters
      <*> pDeprecatedAfterMaryPParams
      <*> pDeprecatedAfterBabbagePParams
      <*> pShelleyToAlonzoPParams
  ShelleyBasedEraAllegra ->
    AllegraEraBasedProtocolParametersUpdate
      <$> pCommonProtocolParameters
      <*> pDeprecatedAfterMaryPParams
      <*> pShelleyToAlonzoPParams
      <*> pDeprecatedAfterBabbagePParams
  ShelleyBasedEraMary ->
    MaryEraBasedProtocolParametersUpdate
      <$> pCommonProtocolParameters
      <*> pDeprecatedAfterMaryPParams
      <*> pShelleyToAlonzoPParams
      <*> pDeprecatedAfterBabbagePParams
  ShelleyBasedEraAlonzo ->
    AlonzoEraBasedProtocolParametersUpdate
      <$> pCommonProtocolParameters
      <*> pShelleyToAlonzoPParams
      <*> pAlonzoOnwardsPParams
      <*> pDeprecatedAfterBabbagePParams
  ShelleyBasedEraBabbage ->
    BabbageEraBasedProtocolParametersUpdate
      <$> pCommonProtocolParameters
      <*> pAlonzoOnwardsPParams
      <*> pDeprecatedAfterBabbagePParams
      <*> pIntroducedInBabbagePParams
  ShelleyBasedEraConway ->
    ConwayEraBasedProtocolParametersUpdate
      <$> pCommonProtocolParameters
      <*> pAlonzoOnwardsPParams
      <*> pIntroducedInBabbagePParams
      <*> pIntroducedInConwayPParams

pGovernanceActionTreasuryWithdrawalCmd
  :: ShelleyBasedEra era -> Maybe (Parser (Cmd.GovernanceActionCmds era))
pGovernanceActionTreasuryWithdrawalCmd era = do
  eon <- forShelleyBasedEraMaybeEon era
  pure
    $ Opt.hsubparser
    $ commandWithMetavar "create-treasury-withdrawal"
    $ Opt.info
      ( fmap Cmd.GovernanceActionTreasuryWithdrawalCmd $
          Cmd.GovernanceActionTreasuryWithdrawalCmdArgs eon
            <$> pNetwork
            <*> pGovActionDeposit
            <*> pStakeIdentifier (Just "deposit-return")
            <*> pAnchorUrl
            <*> pAnchorDataHash
            <*> pMustCheckProposalHash
            <*> some ((,) <$> pStakeIdentifier (Just "funds-receiving") <*> pTreasuryWithdrawalAmt)
            <*> optional pConstitutionScriptHash
            <*> pFileOutDirection "out-file" "Output filepath of the treasury withdrawal."
      )
    $ Opt.progDesc "Create a treasury withdrawal."

pNetwork :: Parser L.Network
pNetwork =
  asum $
    mconcat
      [
        [ Opt.flag' L.Mainnet $
            mconcat
              [ Opt.long "mainnet"
              , Opt.help "Use the mainnet magic id."
              ]
        , Opt.flag' L.Testnet $
            mconcat
              [ Opt.long "testnet"
              , Opt.help "Use the testnet magic id."
              ]
        ]
      ]

pPV :: Parser L.ProtVer
pPV = mkProtocolVersionOrErr <$> pProtocolVersion

pGovernanceActionHardforkInitCmd
  :: ShelleyBasedEra era
  -> Maybe (Parser (Cmd.GovernanceActionCmds era))
pGovernanceActionHardforkInitCmd era = do
  eon <- forShelleyBasedEraMaybeEon era
  pure
    $ Opt.hsubparser
    $ commandWithMetavar "create-hardfork"
    $ Opt.info
      ( fmap Cmd.GovernanceActionHardforkInitCmd $
          Cmd.GovernanceActionHardforkInitCmdArgs eon
            <$> pNetwork
            <*> pGovActionDeposit
            <*> pStakeIdentifier (Just "deposit-return")
            <*> pPreviousGovernanceAction
            <*> pAnchorUrl
            <*> pAnchorDataHash
            <*> pMustCheckProposalHash
            <*> pPV
            <*> pFileOutDirection "out-file" "Output filepath of the hardfork proposal."
      )
    $ Opt.progDesc "Create a hardfork initiation proposal."
