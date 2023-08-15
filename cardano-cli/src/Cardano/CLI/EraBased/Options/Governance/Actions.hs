{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.Options.Governance.Actions
  ( pGovernanceActionCmds
  ) where

import           Cardano.Api
import           Cardano.Api.Ledger
import           Cardano.Api.Shelley

import           Cardano.CLI.EraBased.Commands.Governance.Actions
import           Cardano.CLI.EraBased.Options.Common
import           Cardano.Ledger.BaseTypes (NonNegativeInterval)
import qualified Cardano.Ledger.BaseTypes as Ledger

import           Data.Foldable
import           GHC.Natural (Natural)
import           Options.Applicative
import qualified Options.Applicative as Opt

pGovernanceActionCmds :: ()
  => CardanoEra era
  -> Maybe (Parser (GovernanceActionCmds era))
pGovernanceActionCmds era =
  subInfoParser "action"
    ( Opt.progDesc
        $ mconcat
          [ "Governance action commands."
          ]
    )
    [ pGovernanceActionNewConstitution era
    , pGovernanceActionProtocolParametersUpdate era
    ]


pGovernanceActionNewConstitution
  :: CardanoEra era  -> Maybe (Parser (GovernanceActionCmds era))
pGovernanceActionNewConstitution =
  featureInEra Nothing (\cOn -> Just $
     subParser "create-constitution"
        $ Opt.info (pCmd cOn)
        $ Opt.progDesc "Create a constitution.")
 where
  pCmd :: ConwayEraOnwards era -> Parser (GovernanceActionCmds era)
  pCmd cOn =
    fmap (GovernanceActionCreateConstitution cOn) $
      EraBasedNewConstitution
        <$> pGovActionDeposit
        <*> pAnyStakeIdentifier
        <*> pConstitution
        <*> pFileOutDirection "out-file" "Output filepath of the constitution."

pAnyStakeIdentifier :: Parser AnyStakeIdentifier
pAnyStakeIdentifier =
  asum [ AnyStakePoolKey <$> pStakePoolVerificationKeyOrHashOrFile
       , AnyStakeKey <$> pStakeVerificationKeyOrHashOrFile
       ]

pGovernanceActionProtocolParametersUpdate
  :: CardanoEra era  -> Maybe (Parser (GovernanceActionCmds era))
pGovernanceActionProtocolParametersUpdate era =
   Just $ subParser "create-protocol-parameters-update"
        $ Opt.info (pCmd era)
        $ Opt.progDesc "Create a protocol parameters update."
 where
  pCmd :: CardanoEra era -> Parser (GovernanceActionCmds era)
  pCmd era' =
    case cardanoEraStyle era' of
      LegacyByronEra -> empty
      ShelleyBasedEra sbe ->
        case sbe of
         ShelleyBasedEraShelley ->
           GovernanceActionProtocolParametersUpdate sbe
             <$> dpGovActionProtocolParametersUpdate ShelleyBasedEraShelley
             <*> pOutputFile
         ShelleyBasedEraAllegra ->
           GovernanceActionProtocolParametersUpdate sbe
             <$> dpGovActionProtocolParametersUpdate ShelleyBasedEraAllegra
             <*> pOutputFile
         ShelleyBasedEraMary ->
           GovernanceActionProtocolParametersUpdate sbe
             <$> dpGovActionProtocolParametersUpdate ShelleyBasedEraMary
             <*> pOutputFile
         ShelleyBasedEraAlonzo ->
           GovernanceActionProtocolParametersUpdate sbe
             <$> dpGovActionProtocolParametersUpdate ShelleyBasedEraAlonzo
             <*> pOutputFile
         ShelleyBasedEraBabbage ->
           GovernanceActionProtocolParametersUpdate sbe
             <$> dpGovActionProtocolParametersUpdate ShelleyBasedEraBabbage
             <*> pOutputFile
         ShelleyBasedEraConway ->
           GovernanceActionProtocolParametersUpdate sbe
             <$> dpGovActionProtocolParametersUpdate ShelleyBasedEraConway
             <*> pOutputFile

convertToLedger :: (a -> b) -> Parser (Maybe a) -> Parser (StrictMaybe b)
convertToLedger conv = fmap (maybeToStrictMaybe . fmap conv)

toNonNegativeIntervalOrErr :: Rational -> NonNegativeInterval
toNonNegativeIntervalOrErr r = case Ledger.boundRational r of
                         Nothing ->
                           error $ mconcat [ "toNonNegativeIntervalOrErr: "
                                           , "rational out of bounds " <> show r
                                           ]
                         Just n -> n

toUnitIntervalOrErr :: Rational -> Ledger.UnitInterval
toUnitIntervalOrErr r = case Ledger.boundRational r of
                         Nothing ->
                           error $ mconcat [ "toUnitIntervalOrErr: "
                                           , "rational out of bounds " <> show r
                                           ]
                         Just n -> n

mkProtocolVersionOrErr :: (Natural, Natural) -> Ledger.ProtVer
mkProtocolVersionOrErr (majorProtVer, minorProtVer) =
  case (`Ledger.ProtVer` minorProtVer) <$> Ledger.mkVersion majorProtVer of
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
    <*> convertToLedger mkProtocolVersionOrErr (optional pProtocolVersion)
    <*> convertToLedger toShelleyLovelace (optional pMinPoolCost)


pDeprecatedAfterMaryPParams :: Parser (DeprecatedAfterMaryPParams ledgerera)
pDeprecatedAfterMaryPParams =
  DeprecatedAfterMaryPParams
    <$> convertToLedger toShelleyLovelace (optional pMinUTxOValue)

pShelleyToAlonzoPParams' :: Parser (ShelleyToAlonzoPParams' ledgerera)
pShelleyToAlonzoPParams' =
  ShelleyToAlonzoPParams'
    <$> convertToLedger id (optional $ toLedgerNonce <$> pExtraEntropy)
    <*> convertToLedger toUnitIntervalOrErr (optional pDecentralParam)

pShelleyToAlonzoPParams :: Parser (ShelleyToAlonzoPParams era)
pShelleyToAlonzoPParams =
  ShelleyToAlonzoPParams
    <$> convertToLedger (CoinPerWord . toShelleyLovelace) (optional pUTxOCostPerWord)


pAlonzoOnwardsPParams :: Parser (AlonzoOnwardsPParams ledgerera)
pAlonzoOnwardsPParams =
  AlonzoOnwardsPParams SNothing -- TODO: Conway era cost model
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
    <$> convertToLedger (CoinPerByte . toShelleyLovelace) (optional pUTxOCostPerByte)

dpGovActionProtocolParametersUpdate :: ShelleyBasedEra era -> Parser (EraBasedProtocolParametersUpdate era)
dpGovActionProtocolParametersUpdate = \case
  ShelleyBasedEraShelley ->
    ShelleyEraBasedProtocolParametersUpdate
      <$> pCommonProtocolParameters
      <*> pDeprecatedAfterMaryPParams
      <*> pShelleyToAlonzoPParams'
  ShelleyBasedEraAllegra ->
    AllegraEraBasedProtocolParametersUpdate
      <$> pCommonProtocolParameters
      <*> pDeprecatedAfterMaryPParams
      <*> pShelleyToAlonzoPParams'
  ShelleyBasedEraMary ->
    MaryEraBasedProtocolParametersUpdate
      <$> pCommonProtocolParameters
      <*> pDeprecatedAfterMaryPParams
      <*> pShelleyToAlonzoPParams'
  ShelleyBasedEraAlonzo ->
    AlonzoEraBasedProtocolParametersUpdate
      <$> pCommonProtocolParameters
      <*> pShelleyToAlonzoPParams'
      <*> pAlonzoOnwardsPParams
      <*> pShelleyToAlonzoPParams
  ShelleyBasedEraBabbage ->
    BabbageEraBasedProtocolParametersUpdate
      <$> pCommonProtocolParameters
      <*> pAlonzoOnwardsPParams
      <*> pIntroducedInBabbagePParams
  ShelleyBasedEraConway ->
    ConwayEraBasedProtocolParametersUpdate
      <$> pCommonProtocolParameters
      <*> pAlonzoOnwardsPParams
      <*> pIntroducedInBabbagePParams
