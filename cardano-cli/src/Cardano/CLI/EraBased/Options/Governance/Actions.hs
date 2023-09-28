{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.Options.Governance.Actions
  ( pGovernanceActionCmds
  ) where

import Cardano.Api
import Cardano.Api.Ledger
import Cardano.Api.Shelley

import Cardano.CLI.EraBased.Commands.Governance.Actions
import Cardano.CLI.EraBased.Options.Common
import Cardano.CLI.Types.Common
import Cardano.Ledger.BaseTypes (NonNegativeInterval)
import Cardano.Ledger.BaseTypes qualified as Ledger

import Data.Foldable
import GHC.Natural (Natural)
import Options.Applicative
import Options.Applicative qualified as Opt

pGovernanceActionCmds
  :: ()
  => CardanoEra era
  -> Maybe (Parser (GovernanceActionCmds era))
pGovernanceActionCmds era =
  subInfoParser
    "action"
    ( Opt.progDesc $
        mconcat
          [ "Governance action commands."
          ]
    )
    [ pGovernanceActionNewConstitutionCmd era
    , pGovernanceActionNewCommitteeCmd era
    , pGovernanceActionNewInfoCmd era
    , pGovernanceActionNoConfidenceCmd era
    , pGovernanceActionProtocolParametersUpdateCmd era
    , pGovernanceActionTreasuryWithdrawalCmd era
    ]

pGovernanceActionNewInfoCmd
  :: CardanoEra era
  -> Maybe (Parser (GovernanceActionCmds era))
pGovernanceActionNewInfoCmd era = do
  cOn <- maybeEonInEra era
  pure
    $ subParser "create-info"
    $ Opt.info
      ( fmap (GovernanceActionInfoCmd cOn) $
          NewInfoCmd
            <$> pNetwork
            <*> pGovActionDeposit
            <*> pAnyStakeIdentifier Nothing
            <*> pProposalUrl
            <*> pProposalHashSource
            <*> pFileOutDirection "out-file" "Path to action file to be used later on with build or build-raw "
      )
    $ Opt.progDesc "Create an info action."

pGovernanceActionNewConstitutionCmd
  :: CardanoEra era
  -> Maybe (Parser (GovernanceActionCmds era))
pGovernanceActionNewConstitutionCmd era = do
  cOn <- maybeEonInEra era
  pure
    $ subParser "create-constitution"
    $ Opt.info
      ( fmap (GovernanceActionCreateConstitutionCmd cOn) $
          NewConstitutionCmd
            <$> pNetwork
            <*> pGovActionDeposit
            <*> pAnyStakeIdentifier Nothing
            <*> pPreviousGovernanceAction
            <*> pProposalUrl
            <*> pProposalHashSource
            <*> pConstitutionUrl
            <*> pConstitutionHashSource
            <*> pFileOutDirection "out-file" "Output filepath of the constitution."
      )
    $ Opt.progDesc "Create a constitution."

pGovernanceActionNewCommitteeCmd
  :: CardanoEra era
  -> Maybe (Parser (GovernanceActionCmds era))
pGovernanceActionNewCommitteeCmd era = do
  cOn <- maybeEonInEra era
  pure
    $ subParser "create-new-committee"
    $ Opt.info
      ( GoveranceActionCreateNewCommitteeCmd cOn
          <$> pNewCommitteeCmd
      )
    $ Opt.progDesc "Create a new committee proposal."

pNewCommitteeCmd :: Parser NewCommitteeCmd
pNewCommitteeCmd =
  NewCommitteeCmd
    <$> pNetwork
    <*> pGovActionDeposit
    <*> pAnyStakeIdentifier Nothing
    <*> pProposalUrl
    <*> pProposalHashSource
    <*> many pRemoveCommitteeColdVerificationKeyOrHashOrFile
    <*> many
      ( (,)
          <$> pAddCommitteeColdVerificationKeyOrHashOrFile
          <*> pEpochNo "Committee member expiry epoch"
      )
    <*> pRational "quorum" "Quorum of the committee that is necessary for a successful vote."
    <*> pPreviousGovernanceAction
    <*> pOutputFile

pGovernanceActionNoConfidenceCmd
  :: CardanoEra era
  -> Maybe (Parser (GovernanceActionCmds era))
pGovernanceActionNoConfidenceCmd era = do
  cOn <- maybeEonInEra era
  pure
    $ subParser "create-no-confidence"
    $ Opt.info
      ( fmap (GovernanceActionCreateNoConfidenceCmd cOn) $
          NoConfidenceCmd
            <$> pNetwork
            <*> pGovActionDeposit
            <*> pAnyStakeIdentifier Nothing
            <*> pProposalUrl
            <*> pProposalHashSource
            <*> pTxId
              "governance-action-tx-id"
              "Previous txid of `NoConfidence` or `NewCommittee` governance action."
            <*> pWord32
              "governance-action-index"
              "Previous tx's governance action index of `NoConfidence` or `NewCommittee` governance action."
            <*> pFileOutDirection "out-file" "Output filepath of the no confidence proposal."
      )
    $ Opt.progDesc "Create a no confidence proposal."

-- | The first argument is the optional prefix.
pAnyStakeIdentifier :: Maybe String -> Parser AnyStakeIdentifier
pAnyStakeIdentifier prefix =
  asum
    [ AnyStakePoolKey <$> pStakePoolVerificationKeyOrHashOrFile prefix
    , AnyStakeKey <$> pStakeVerificationKeyOrHashOrFile prefix
    ]

pGovernanceActionProtocolParametersUpdateCmd
  :: ()
  => CardanoEra era
  -> Maybe (Parser (GovernanceActionCmds era))
pGovernanceActionProtocolParametersUpdateCmd era = do
  w <- maybeEonInEra era
  pure
    $ subParser "create-protocol-parameters-update"
    $ Opt.info
      ( GovernanceActionProtocolParametersUpdateCmd w
          <$> pEpochNoUpdateProp
          <*> pProtocolParametersUpdateGenesisKeys w
          <*> dpGovActionProtocolParametersUpdate w
          <*> pOutputFile
      )
    $ Opt.progDesc "Create a protocol parameters update."

convertToLedger :: (a -> b) -> Parser (Maybe a) -> Parser (StrictMaybe b)
convertToLedger conv = fmap (maybeToStrictMaybe . fmap conv)

toNonNegativeIntervalOrErr :: Rational -> NonNegativeInterval
toNonNegativeIntervalOrErr r = case Ledger.boundRational r of
  Nothing ->
    error $
      mconcat
        [ "toNonNegativeIntervalOrErr: "
        , "rational out of bounds " <> show r
        ]
  Just n -> n

toUnitIntervalOrErr :: Rational -> Ledger.UnitInterval
toUnitIntervalOrErr r = case Ledger.boundRational r of
  Nothing ->
    error $
      mconcat
        [ "toUnitIntervalOrErr: "
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
    <$> convertToLedger (CoinPerByte . toShelleyLovelace) (optional pUTxOCostPerByte)

-- Not necessary in Conway era onwards
pProtocolParametersUpdateGenesisKeys :: ShelleyBasedEra era -> Parser [VerificationKeyFile In]
pProtocolParametersUpdateGenesisKeys =
  caseShelleyToBabbageOrConwayEraOnwards
    (const (many pGenesisVerificationKeyFile))
    (const empty)

dpGovActionProtocolParametersUpdate
  :: ShelleyBasedEra era -> Parser (EraBasedProtocolParametersUpdate era)
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

pGovernanceActionTreasuryWithdrawalCmd
  :: CardanoEra era -> Maybe (Parser (GovernanceActionCmds era))
pGovernanceActionTreasuryWithdrawalCmd era = do
  cOn <- maybeEonInEra era
  pure
    $ subParser "create-treasury-withdrawal"
    $ Opt.info
      ( fmap (GovernanceActionTreasuryWithdrawalCmd cOn) $
          TreasuryWithdrawalCmd
            <$> pNetwork
            <*> pGovActionDeposit
            <*> pAnyStakeIdentifier Nothing
            <*> pProposalUrl
            <*> pProposalHashSource
            <*> many ((,) <$> pAnyStakeIdentifier Nothing <*> pTransferAmt) -- TODO we should likely pass a prefix here, becaus pAnyStakeIdentiefier is used twice
            <*> pFileOutDirection "out-file" "Output filepath of the treasury withdrawal."
      )
    $ Opt.progDesc "Create a treasury withdrawal."

pNetwork :: Parser Ledger.Network
pNetwork =
  asum $
    mconcat
      [
        [ Opt.flag' Ledger.Mainnet $
            mconcat
              [ Opt.long "mainnet"
              , Opt.help "Use the mainnet magic id."
              ]
        , Opt.flag' Ledger.Testnet $
            mconcat
              [ Opt.long "testnet"
              , Opt.help "Use the testnet magic id."
              ]
        ]
      ]
