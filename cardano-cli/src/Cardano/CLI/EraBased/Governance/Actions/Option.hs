{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.Governance.Actions.Option
  ( pGovActionProtocolParametersUpdate
  , pCostModelsFile
  , pGovernanceActionCmds
  , pProtocolParametersUpdateGenesisKeys
  , pUpdateProtocolParametersPostConway
  )
where

import Cardano.Api
import Cardano.Api.Experimental qualified as Exp
import Cardano.Api.Ledger qualified as L

import Cardano.CLI.Compatible.Governance.Types
import Cardano.CLI.EraBased.Common.Option
import Cardano.CLI.EraBased.Governance.Actions.Command qualified as Cmd
import Cardano.CLI.Option.Flag (setDefault)
import Cardano.CLI.Parser
import Cardano.CLI.Type.Common
import Cardano.Ledger.BaseTypes (Milliseconds32 (..), NonZero, PositiveInterval, nonZero)
import Cardano.Ledger.Plutus.ExUnits (OrdExUnits (..))

import Data.Foldable
import Data.Function ((&))
import Data.Word (Word16, Word32)
import GHC.Natural (Natural)
import Options.Applicative
import Options.Applicative qualified as Opt

pGovernanceActionCmds
  :: Exp.IsEra era
  => Maybe (Parser (Cmd.GovernanceActionCmds era))
pGovernanceActionCmds =
  subInfoParser
    "action"
    ( Opt.progDesc $
        mconcat
          [ "Governance action commands."
          ]
    )
    [ pGovernanceActionNewConstitutionCmd
    , pGovernanceActionUpdateCommitteeCmd
    , pGovernanceActionNewInfoCmd
    , pGovernanceActionNoConfidenceCmd
    , pGovernanceActionProtocolParametersUpdateCmd
    , pGovernanceActionTreasuryWithdrawalCmd
    , pGovernanceActionHardforkInitCmd
    , pGovernanceActionViewCmd
    ]

pGovernanceActionViewCmd
  :: Exp.IsEra era => Maybe (Parser (Cmd.GovernanceActionCmds era))
pGovernanceActionViewCmd = do
  return
    $ Opt.hsubparser
    $ commandWithMetavar "view"
    $ Opt.info
      ( fmap Cmd.GovernanceActionViewCmd $
          Cmd.GovernanceActionViewCmdArgs Exp.useEra
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
  :: Exp.IsEra era => Maybe (Parser (Cmd.GovernanceActionCmds era))
pGovernanceActionNewInfoCmd = do
  pure
    $ Opt.hsubparser
    $ commandWithMetavar "create-info"
    $ Opt.info
      ( fmap Cmd.GovernanceActionInfoCmd $
          Cmd.GovernanceActionInfoCmdArgs Exp.useEra
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
  :: Exp.IsEra era => Maybe (Parser (Cmd.GovernanceActionCmds era))
pGovernanceActionNewConstitutionCmd = do
  pure
    $ Opt.hsubparser
    $ commandWithMetavar "create-constitution"
    $ Opt.info
      ( fmap Cmd.GovernanceActionCreateConstitutionCmd $
          Cmd.GovernanceActionCreateConstitutionCmdArgs Exp.useEra
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
  :: Exp.IsEra era => Maybe (Parser (Cmd.GovernanceActionCmds era))
pGovernanceActionUpdateCommitteeCmd = do
  pure
    $ Opt.hsubparser
    $ commandWithMetavar "update-committee"
    $ Opt.info
      ( Cmd.GovernanceActionUpdateCommitteeCmd
          <$> pUpdateCommitteeCmd
      )
    $ Opt.progDesc "Create or update a new committee proposal."

pUpdateCommitteeCmd
  :: Exp.IsEra era => Parser (Cmd.GovernanceActionUpdateCommitteeCmdArgs era)
pUpdateCommitteeCmd =
  Cmd.GovernanceActionUpdateCommitteeCmdArgs Exp.useEra
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
  :: Exp.IsEra era => Maybe (Parser (Cmd.GovernanceActionCmds era))
pGovernanceActionNoConfidenceCmd = do
  pure
    $ Opt.hsubparser
    $ commandWithMetavar "create-no-confidence"
    $ Opt.info
      ( fmap Cmd.GovernanceActionCreateNoConfidenceCmd $
          Cmd.GovernanceActionCreateNoConfidenceCmdArgs Exp.useEra
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

pUpdateProtocolParametersPostConway
  :: Exp.IsEra era => Parser (Cmd.UpdateProtocolParametersConwayOnwards era)
pUpdateProtocolParametersPostConway =
  Cmd.UpdateProtocolParametersConwayOnwards Exp.useEra
    <$> pNetwork
    <*> pGovActionDeposit
    <*> pStakeIdentifier (Just "deposit-return")
    <*> pAnchorUrl
    <*> pAnchorDataHash
    <*> pMustCheckProposalHash
    <*> pPreviousGovernanceAction
    <*> optional pConstitutionScriptHash

pUpdateProtocolParametersCmd
  :: Exp.IsEra era => Parser (Cmd.GovernanceActionProtocolParametersUpdateCmdArgs era)
pUpdateProtocolParametersCmd =
  let sbe = convert Exp.useEra
   in Opt.hsubparser
        $ commandWithMetavar "create-protocol-parameters-update"
        $ Opt.info
          ( Cmd.GovernanceActionProtocolParametersUpdateCmdArgs
              Exp.useEra
              <$> pUpdateProtocolParametersPostConway
              <*> pGovActionProtocolParametersUpdate sbe
              <*> pCostModelsFile sbe
              <*> pOutputFile
          )
        $ Opt.progDesc "Create a protocol parameters update."

-- | Cost models only makes sense in eras from Alonzo onwards. For earlier
-- eras, this parser doesn't show up in the command line and returns 'Nothing'.
pCostModelsFile :: ShelleyBasedEra era -> Parser (Maybe (CostModelsFile era))
pCostModelsFile sbe =
  forEraInEon
    (convert sbe)
    (pure Nothing)
    ( \alonzoOnwards ->
        fmap (CostModelsFile alonzoOnwards . File)
          <$> optional pCostModels
    )

pGovernanceActionProtocolParametersUpdateCmd
  :: Exp.IsEra era => Maybe (Parser (Cmd.GovernanceActionCmds era))
pGovernanceActionProtocolParametersUpdateCmd = do
  pure $
    Cmd.GovernanceActionProtocolParametersUpdateCmd
      <$> pUpdateProtocolParametersCmd

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

mkProtocolVersionOrErr :: (Natural, Word32) -> L.ProtVer
mkProtocolVersionOrErr (majorProtVer, minorProtVer) =
  case (`L.ProtVer` fromIntegral minorProtVer) <$> L.mkVersion majorProtVer of
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
    <$> convertToLedger id (optional pUTxOCostPerByte)

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

pIntroducedInDijkstraPParams :: Parser (IntroducedInDijkstraPParams ledgerera)
pIntroducedInDijkstraPParams =
  IntroducedInDijkstraPParams
    <$> convertToLedger id (optional pMaxRefScriptSizePerBlock)
    <*> convertToLedger id (optional pMaxRefScriptSizePerTx)
    <*> convertToLedger id (optional pRefScriptCostStride)
    <*> convertToLedger id (optional pRefScriptCostMultiplier)
    <*> convertToLedger id (optional pLeiosAnnouncementPeriodLength)
    <*> convertToLedger id (optional pLeiosVotePeriodLength)
    <*> convertToLedger id (optional pLeiosDiffusionPeriodLength)
    <*> convertToLedger id (optional pLeiosCommitteeSize)
    <*> convertToLedger toUnitIntervalOrErr (optional pLeiosQuorumStakeThreshold)
    <*> convertToLedger id (optional pMaxEndorserBlockReferencesSize)
    <*> convertToLedger id (optional pMaxEndorserBlockTxsSize)
    <*> convertToLedger (OrdExUnits . toAlonzoExUnits) (optional pMaxEndorserBlockExecutionUnits)
    <*> convertToLedger id (optional pMaxRefScriptSizePerEndorserBlock)

pLeiosAnnouncementPeriodLength :: Parser Milliseconds32
pLeiosAnnouncementPeriodLength =
  Milliseconds32
    <$> Opt.option
      integralReader
      ( mconcat
          [ Opt.long "leios-announcement-period-length"
          , Opt.metavar "MILLISECONDS"
          , Opt.help "Length of the Leios announcement period, in milliseconds."
          ]
      )

pLeiosVotePeriodLength :: Parser Milliseconds32
pLeiosVotePeriodLength =
  Milliseconds32
    <$> Opt.option
      integralReader
      ( mconcat
          [ Opt.long "leios-vote-period-length"
          , Opt.metavar "MILLISECONDS"
          , Opt.help "Length of the Leios voting period, in milliseconds."
          ]
      )

pLeiosDiffusionPeriodLength :: Parser Milliseconds32
pLeiosDiffusionPeriodLength =
  Milliseconds32
    <$> Opt.option
      integralReader
      ( mconcat
          [ Opt.long "leios-diffusion-period-length"
          , Opt.metavar "MILLISECONDS"
          , Opt.help "Length of the Leios diffusion period, in milliseconds."
          ]
      )

pLeiosCommitteeSize :: Parser Word16
pLeiosCommitteeSize =
  Opt.option integralReader $
    mconcat
      [ Opt.long "leios-committee-size"
      , Opt.metavar "WORD16"
      , Opt.help "Number of seats on the Leios voting committee."
      ]

pLeiosQuorumStakeThreshold :: Parser Rational
pLeiosQuorumStakeThreshold =
  Opt.option readRational $
    mconcat
      [ Opt.long "leios-quorum-stake-threshold"
      , Opt.metavar "RATIONAL"
      , Opt.help "Fraction of committee stake required to certify an endorser block."
      ]

pMaxEndorserBlockReferencesSize :: Parser Word32
pMaxEndorserBlockReferencesSize =
  Opt.option integralReader $
    mconcat
      [ Opt.long "max-endorser-block-references-size"
      , Opt.metavar "WORD32"
      , Opt.help "Maximum total size of the transaction references in an endorser block."
      ]

pMaxEndorserBlockTxsSize :: Parser Word32
pMaxEndorserBlockTxsSize =
  Opt.option integralReader $
    mconcat
      [ Opt.long "max-endorser-block-txs-size"
      , Opt.metavar "WORD32"
      , Opt.help "Maximum total size of the transactions referenced by an endorser block."
      ]

pMaxEndorserBlockExecutionUnits :: Parser ExecutionUnits
pMaxEndorserBlockExecutionUnits =
  uncurry ExecutionUnits
    <$> Opt.option
      pairIntegralReader
      ( mconcat
          [ Opt.long "max-endorser-block-execution-units"
          , Opt.metavar "(INT, INT)"
          , Opt.help $
              mconcat
                [ "Max total script execution resource units allowed per endorser "
                , "block. They are denominated as follows (steps, memory)."
                ]
          ]
      )

pMaxRefScriptSizePerEndorserBlock :: Parser Word32
pMaxRefScriptSizePerEndorserBlock =
  Opt.option integralReader $
    mconcat
      [ Opt.long "max-ref-script-size-per-endorser-block"
      , Opt.metavar "WORD32"
      , Opt.help "Maximum total size of reference scripts per endorser block."
      ]

pMaxRefScriptSizePerBlock :: Parser Word32
pMaxRefScriptSizePerBlock =
  Opt.option integralReader $
    mconcat
      [ Opt.long "max-ref-script-size-per-block"
      , Opt.metavar "WORD32"
      , Opt.help "Maximum total size of reference scripts per block."
      ]

pMaxRefScriptSizePerTx :: Parser Word32
pMaxRefScriptSizePerTx =
  Opt.option integralReader $
    mconcat
      [ Opt.long "max-ref-script-size-per-tx"
      , Opt.metavar "WORD32"
      , Opt.help "Maximum total size of reference scripts per transaction."
      ]

pRefScriptCostStride :: Parser (NonZero Word32)
pRefScriptCostStride =
  Opt.option
    (integralReader >>= maybe (fail "ref-script-cost-stride must be non-zero") pure . nonZero)
    $ mconcat
      [ Opt.long "ref-script-cost-stride"
      , Opt.metavar "WORD32"
      , Opt.help "Reference script cost stride (non-zero) for fee calculation."
      ]

pRefScriptCostMultiplier :: Parser PositiveInterval
pRefScriptCostMultiplier =
  Opt.option (readRational >>= toPositiveInterval) $
    mconcat
      [ Opt.long "ref-script-cost-multiplier"
      , Opt.metavar "RATIONAL"
      , Opt.help "Reference script cost multiplier for fee calculation."
      ]

toPositiveInterval :: Rational -> Opt.ReadM PositiveInterval
toPositiveInterval r =
  maybe
    ( Opt.readerError $
        "expected a positive rational with numerator and denominator fitting in 64 bits, got: " <> show r
    )
    pure
    $ L.boundRational r

-- Not necessary in Conway era onwards
pProtocolParametersUpdateGenesisKeys :: Parser [VerificationKeyFile In]
pProtocolParametersUpdateGenesisKeys = some pGenesisVerificationKeyFile

pGovActionProtocolParametersUpdate
  :: ShelleyBasedEra era -> Parser (EraBasedProtocolParametersUpdate era)
pGovActionProtocolParametersUpdate = \case
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
  ShelleyBasedEraDijkstra ->
    DijkstraEraBasedProtocolParametersUpdate
      <$> pCommonProtocolParameters
      <*> pAlonzoOnwardsPParams
      <*> pIntroducedInBabbagePParams
      <*> pIntroducedInConwayPParams
      <*> pIntroducedInDijkstraPParams

pGovernanceActionTreasuryWithdrawalCmd
  :: Exp.IsEra era => Maybe (Parser (Cmd.GovernanceActionCmds era))
pGovernanceActionTreasuryWithdrawalCmd = do
  pure
    $ Opt.hsubparser
    $ commandWithMetavar "create-treasury-withdrawal"
    $ Opt.info
      ( fmap Cmd.GovernanceActionTreasuryWithdrawalCmd $
          Cmd.GovernanceActionTreasuryWithdrawalCmdArgs Exp.useEra
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
  :: Exp.IsEra era => Maybe (Parser (Cmd.GovernanceActionCmds era))
pGovernanceActionHardforkInitCmd = do
  pure
    $ Opt.hsubparser
    $ commandWithMetavar "create-hardfork"
    $ Opt.info
      ( fmap Cmd.GovernanceActionHardforkInitCmd $
          Cmd.GovernanceActionHardforkInitCmdArgs Exp.useEra
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
