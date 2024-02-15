{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.Options.Governance.Actions
  ( pGovernanceActionCmds
  ) where

import           Cardano.Api
import qualified Cardano.Api.Ledger as L
import           Cardano.Api.Shelley

import qualified Cardano.CLI.EraBased.Commands.Governance.Actions as Cmd
import           Cardano.CLI.EraBased.Options.Common
import           Cardano.CLI.Types.Common

import           Data.Foldable
import           GHC.Natural (Natural)
import           Options.Applicative
import qualified Options.Applicative as Opt

pGovernanceActionCmds :: ()
  => CardanoEra era
  -> Maybe (Parser (Cmd.GovernanceActionCmds era))
pGovernanceActionCmds era =
  subInfoParser "action"
    ( Opt.progDesc
        $ mconcat
          [ "Governance action commands."
          ]
    )
    [ pGovernanceActionNewConstitutionCmd era
    , pGovernanceActionUpdateCommitteeCmd era
    , pGovernanceActionNewInfoCmd era
    , pGovernanceActionNoConfidenceCmd era
    , pGovernanceActionProtocolParametersUpdateCmd era
    , pGovernanceActionTreasuryWithdrawalCmd era
    , pGovernanceActionViewCmd era
    ]

pGovernanceActionViewCmd
  :: CardanoEra era
  -> Maybe (Parser (Cmd.GovernanceActionCmds era))
pGovernanceActionViewCmd era = do
  eon <- forEraMaybeEon era
  return
    $ subParser "view"
    $ Opt.info
        ( fmap Cmd.GovernanceActionViewCmd
            $ Cmd.GovernanceActionViewCmdArgs eon
                <$> pFileInDirection "action-file" "Path to action file."
                <*> pGovernanceActionViewOutputFormat
                <*> pMaybeOutputFile
        )
    $ Opt.progDesc "View a governance action."

pGovernanceActionNewInfoCmd
  :: CardanoEra era
  -> Maybe (Parser (Cmd.GovernanceActionCmds era))
pGovernanceActionNewInfoCmd era = do
  eon <- forEraMaybeEon era
  pure
    $ subParser "create-info"
    $ Opt.info
        ( fmap Cmd.GovernanceActionInfoCmd $
            Cmd.GovernanceActionInfoCmdArgs eon
              <$> pNetwork
              <*> pGovActionDeposit
              <*> pStakeVerifier (Just "deposit-return")
              <*> pAnchorUrl
              <*> pAnchorDataHash
              <*> pFileOutDirection "out-file" "Path to action file to be used later on with build or build-raw "
        )
    $ Opt.progDesc "Create an info action."


pGovernanceActionNewConstitutionCmd
  :: CardanoEra era
  -> Maybe (Parser (Cmd.GovernanceActionCmds era))
pGovernanceActionNewConstitutionCmd era = do
  eon <- forEraMaybeEon era
  pure
    $ subParser "create-constitution"
    $ Opt.info
        ( fmap Cmd.GovernanceActionCreateConstitutionCmd $
            Cmd.GovernanceActionCreateConstitutionCmdArgs eon
              <$> pNetwork
              <*> pGovActionDeposit
              <*> pStakeIdentifier (Just "deposit-return")
              <*> pPreviousGovernanceAction
              <*> pAnchorUrl
              <*> pAnchorDataHash
              <*> pConstitutionUrl
              <*> pConstitutionHash
              <*> pFileOutDirection "out-file" "Output filepath of the constitution."
        )
    $ Opt.progDesc "Create a constitution."

pGovernanceActionUpdateCommitteeCmd
  :: CardanoEra era
  -> Maybe (Parser (Cmd.GovernanceActionCmds era))
pGovernanceActionUpdateCommitteeCmd era = do
  eon <- forEraMaybeEon era
  pure
    $ subParser "update-committee"
    $ Opt.info
        ( Cmd.GoveranceActionUpdateCommitteeCmd
            <$> pUpdateCommitteeCmd eon
        )
    $ Opt.progDesc "Create or update a new committee proposal."

pUpdateCommitteeCmd :: ()
  => ConwayEraOnwards era
  -> Parser (Cmd.GoveranceActionUpdateCommitteeCmdArgs era)
pUpdateCommitteeCmd eon =
  Cmd.GoveranceActionUpdateCommitteeCmdArgs eon
    <$> pNetwork
    <*> pGovActionDeposit
    <*> pStakeIdentifier (Just "deposit-return")
    <*> pAnchorUrl
    <*> pAnchorDataHash
    <*> many pRemoveCommitteeColdVerificationKeyOrHashOrFile
    <*> many
          ( (,)
              <$> pAddCommitteeColdVerificationKeyOrHashOrFile
              <*> pEpochNo "Committee member expiry epoch")
    <*> pRational "quorum" "Quorum of the committee that is necessary for a successful vote."
    <*> pPreviousGovernanceAction
    <*> pOutputFile


pGovernanceActionNoConfidenceCmd
  :: CardanoEra era
  -> Maybe (Parser (Cmd.GovernanceActionCmds era))
pGovernanceActionNoConfidenceCmd era = do
  eon <- forEraMaybeEon era
  pure
    $ subParser "create-no-confidence"
    $ Opt.info
        ( fmap Cmd.GovernanceActionCreateNoConfidenceCmd $
            Cmd.GovernanceActionCreateNoConfidenceCmdArgs eon
              <$> pNetwork
              <*> pGovActionDeposit
              <*> pStakeIdentifier (Just "deposit-return")
              <*> pAnchorUrl
              <*> pAnchorDataHash
              <*> pTxId "prev-governance-action-tx-id" "Txid of the previous governance action."
              <*> pWord32 "prev-governance-action-index" "Action index of the previous governance action."
              <*> pFileOutDirection "out-file" "Output filepath of the no confidence proposal."
        )
    $ Opt.progDesc "Create a no confidence proposal."

pUpdateProtocolParametersPreConway :: ShelleyToBabbageEra era -> Parser (Cmd.UpdateProtocolParametersPreConway era)
pUpdateProtocolParametersPreConway shelleyToBab =
  Cmd.UpdateProtocolParametersPreConway shelleyToBab
    <$> pEpochNoUpdateProp
    <*> pProtocolParametersUpdateGenesisKeys

pUpdateProtocolParametersPostConway :: ConwayEraOnwards era -> Parser (Cmd.UpdateProtocolParametersConwayOnwards era)
pUpdateProtocolParametersPostConway conwayOnwards =
  Cmd.UpdateProtocolParametersConwayOnwards conwayOnwards
    <$> pNetwork
    <*> pGovActionDeposit
    <*> pStakeIdentifier (Just "deposit-return")
    <*> pAnchorUrl
    <*> pAnchorDataHash
    <*> pPreviousGovernanceAction
    <*> optional pConstitutionScriptHash


pUpdateProtocolParametersCmd :: ShelleyBasedEra era -> Parser (Cmd.GovernanceActionProtocolParametersUpdateCmdArgs era)
pUpdateProtocolParametersCmd =
  caseShelleyToBabbageOrConwayEraOnwards
    (\shelleyToBab ->
        let sbe = shelleyToBabbageEraToShelleyBasedEra shelleyToBab
        in subParser "create-protocol-parameters-update"
                         $ Opt.info
                             ( Cmd.GovernanceActionProtocolParametersUpdateCmdArgs (shelleyToBabbageEraToShelleyBasedEra shelleyToBab)
                                 <$> fmap Just (pUpdateProtocolParametersPreConway shelleyToBab)
                                 <*> pure Nothing
                                 <*> dpGovActionProtocolParametersUpdate sbe
                                 <*> pCostModelsFile sbe
                                 <*> pOutputFile
                             )
                         $ Opt.progDesc "Create a protocol parameters update.")
    (\conwayOnwards ->
        let sbe = conwayEraOnwardsToShelleyBasedEra conwayOnwards
        in subParser "create-protocol-parameters-update"
                        $ Opt.info
                            ( Cmd.GovernanceActionProtocolParametersUpdateCmdArgs
                                (conwayEraOnwardsToShelleyBasedEra conwayOnwards) Nothing
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

pGovernanceActionProtocolParametersUpdateCmd :: ()
  => CardanoEra era
  -> Maybe (Parser (Cmd.GovernanceActionCmds era))
pGovernanceActionProtocolParametersUpdateCmd era = do
  w <- forEraMaybeEon era
  pure $ Cmd.GovernanceActionProtocolParametersUpdateCmd
    <$> pUpdateProtocolParametersCmd w


convertToLedger :: (a -> b) -> Parser (Maybe a) -> Parser (L.StrictMaybe b)
convertToLedger conv = fmap (L.maybeToStrictMaybe . fmap conv)

toNonNegativeIntervalOrErr :: Rational -> L.NonNegativeInterval
toNonNegativeIntervalOrErr r = case L.boundRational r of
                         Nothing ->
                           error $ mconcat [ "toNonNegativeIntervalOrErr: "
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
    <$> convertToLedger toShelleyLovelace (optional pMinFeeConstantFactor)
    <*> convertToLedger toShelleyLovelace (optional pMinFeePerByteFactor)
    <*> convertToLedger id (optional pMaxBodySize)
    <*> convertToLedger id (optional pMaxTransactionSize)
    <*> convertToLedger id (optional pMaxBlockHeaderSize)
    <*> convertToLedger toShelleyLovelace (optional pKeyRegistDeposit)
    <*> convertToLedger toShelleyLovelace (optional pPoolDeposit)
    <*> convertToLedger id (optional pEpochBoundRetirement)
    <*> convertToLedger id (optional pNumberOfPools)
    <*> convertToLedger toNonNegativeIntervalOrErr (optional pPoolInfluence)
    <*> convertToLedger toUnitIntervalOrErr (optional pTreasuryExpansion)
    <*> convertToLedger toUnitIntervalOrErr (optional pMonetaryExpansion)
    <*> convertToLedger toShelleyLovelace (optional pMinPoolCost)


pDeprecatedAfterMaryPParams :: Parser (DeprecatedAfterMaryPParams ledgerera)
pDeprecatedAfterMaryPParams =
  DeprecatedAfterMaryPParams
    <$> convertToLedger toShelleyLovelace (optional pMinUTxOValue)

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
    <$> convertToLedger (either (\e -> error $ "pAlonzoOnwardsPParams: " <> show e) id . toAlonzoPrices)
                        (optional pExecutionUnitPrices)
    <*> convertToLedger toAlonzoExUnits (optional pMaxTxExecutionUnits)
    <*> convertToLedger toAlonzoExUnits (optional pMaxBlockExecutionUnits)
    <*> convertToLedger id (optional pMaxValueSize)
    <*> convertToLedger id (optional pCollateralPercent)
    <*> convertToLedger id (optional pMaxCollateralInputs)


pIntroducedInBabbagePParams :: Parser (IntroducedInBabbagePParams ledgerera)
pIntroducedInBabbagePParams =
  IntroducedInBabbagePParams
    <$> convertToLedger (L.CoinPerByte . toShelleyLovelace) (optional pUTxOCostPerByte)

pIntroducedInConwayPParams :: Parser (IntroducedInConwayPParams ledgerera)
pIntroducedInConwayPParams =
  IntroducedInConwayPParams
    <$> convertToLedger id (optional pPoolVotingThresholds)
    <*> convertToLedger id (optional pDRepVotingThresholds)
    <*> convertToLedger id (optional pMinCommitteeSize)
    <*> convertToLedger id (optional pCommitteeTermLength)
    <*> convertToLedger id (optional pGovActionLifetime)
    <*> convertToLedger toShelleyLovelace (optional pNewGovActionDeposit)
    <*> convertToLedger toShelleyLovelace (optional pDRepDeposit)
    <*> convertToLedger id (optional pDRepActivity)

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

pGovernanceActionTreasuryWithdrawalCmd :: CardanoEra era -> Maybe (Parser (Cmd.GovernanceActionCmds era))
pGovernanceActionTreasuryWithdrawalCmd era = do
  eon <- forEraMaybeEon era
  pure
    $ subParser "create-treasury-withdrawal"
    $ Opt.info
        ( fmap Cmd.GovernanceActionTreasuryWithdrawalCmd $
            Cmd.GovernanceActionTreasuryWithdrawalCmdArgs eon
              <$> pNetwork
              <*> pGovActionDeposit
              <*> pStakeIdentifier (Just "deposit-return")
              <*> pAnchorUrl
              <*> pAnchorDataHash
              <*> many ((,) <$> pStakeVerificationKeyOrHashOrFile (Just "funds-receiving") <*> pTransferAmt)
              <*> optional pConstitutionScriptHash
              <*> pFileOutDirection "out-file" "Output filepath of the treasury withdrawal."
        )
    $ Opt.progDesc "Create a treasury withdrawal."

pNetwork :: Parser L.Network
pNetwork  = asum $ mconcat
  [ [ Opt.flag' L.Mainnet $ mconcat
      [ Opt.long "mainnet"
      , Opt.help "Use the mainnet magic id."
      ]
    , Opt.flag' L.Testnet $ mconcat
      [ Opt.long "testnet"
      , Opt.help "Use the testnet magic id."
      ]
    ]
  ]
