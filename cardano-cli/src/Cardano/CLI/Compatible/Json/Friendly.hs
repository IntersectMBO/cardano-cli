{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- | User-friendly pretty-printing for textual user interfaces (TUI)
module Cardano.CLI.Compatible.Json.Friendly
  ( -- * Functions in IO

  --
  -- Use them when writing to stdout or to files.
    friendlyTx
  , friendlyTxBody
  , friendlyProposal
  , friendlyDRep

    -- * Functions that are not in IO

  --
  -- They are more low-level, but can be used in any context.
  -- The '*Impl' functions give you access to the Aeson representation
  -- of various structures.
  , friendlyTxImpl
  , friendlyTxBodyImpl
  , friendlyProposalImpl
  )
where

import Cardano.Api as Api
import Cardano.Api.Experimental (obtainCommonConstraints)
import Cardano.Api.Experimental qualified as Exp
import Cardano.Api.Ledger (ExUnits (..), extractHash, strictMaybeToMaybe)
import Cardano.Api.Ledger qualified as Alonzo
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Ledger qualified as Ledger

import Cardano.CLI.Json.Encode qualified as Json
import Cardano.CLI.Orphan ()
import Cardano.CLI.Type.Common (FormatJson (..), FormatYaml (..))
import Cardano.CLI.Type.MonadWarning (MonadWarning, runWarningIO)
import Cardano.Crypto.Hash (hashToTextAsHex)
import Cardano.Ledger.Core qualified as C

import Data.Aeson (Value (..), object, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types qualified as Aeson
import Data.ByteString.Char8 qualified as BSC
import Data.Char (isAscii)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Ratio (numerator)
import Data.Text qualified as T
import Data.Text qualified as Text
import Data.Typeable (Typeable)
import Data.Vector qualified as Vector
import Data.Yaml (array)
import GHC.Exts (IsList (..))
import GHC.Real (denominator)
import GHC.Unicode (isAlphaNum)
import Lens.Micro ((^.))
import Vary (Vary)
import Vary qualified

friendly
  :: (MonadIO m, Aeson.ToJSON a)
  => Vary [FormatJson, FormatYaml]
  -> Maybe (File () Out)
  -> a
  -> m (Either (FileError e) ())
friendly format mOutFile value = do
  output <-
    pure
      $ format
        & ( id
              . Vary.on (\FormatJson -> Json.encodeJson)
              . Vary.on (\FormatYaml -> Json.encodeYaml)
              $ Vary.exhaustiveCase
          )
      $ value

  writeLazyByteStringOutput mOutFile output

friendlyTx
  :: MonadIO m
  => Vary [FormatJson, FormatYaml]
  -> Maybe (File () Out)
  -> Exp.Era era
  -> Tx era
  -> m (Either (FileError e) ())
friendlyTx format mOutFile era tx = do
  pairs <- runWarningIO $ friendlyTxImpl era tx
  friendly format mOutFile $ object pairs

friendlyTxBody
  :: MonadIO m
  => Vary [FormatJson, FormatYaml]
  -> Maybe (File () Out)
  -> Exp.Era era
  -> TxBody era
  -> m (Either (FileError e) ())
friendlyTxBody format mOutFile era tx = do
  pairs <- runWarningIO $ friendlyTxBodyImpl era tx
  friendly format mOutFile $ object pairs

friendlyProposal
  :: MonadIO m
  => Exp.IsEra era
  => Vary [FormatJson, FormatYaml]
  -> Maybe (File () Out)
  -> Proposal era
  -> m (Either (FileError e) ())
friendlyProposal format mOutFile =
  friendly format mOutFile . object . friendlyProposalImpl

friendlyProposalImpl :: forall era. Exp.IsEra era => Proposal era -> [Aeson.Pair]
friendlyProposalImpl
  ( Proposal
      ( L.ProposalProcedure
          { L.pProcDeposit
          , L.pProcReturnAddr
          , L.pProcGovAction
          , L.pProcAnchor
          }
        )
    ) =
    Exp.obtainCommonConstraints
      (Exp.useEra @era)
      [ "deposit" .= pProcDeposit
      , "return address" .= pProcReturnAddr
      , "governance action" .= pProcGovAction
      , "anchor" .= pProcAnchor
      ]

friendlyTxImpl
  :: MonadWarning m
  => Exp.Era era
  -> Tx era
  -> m [Aeson.Pair]
friendlyTxImpl era tx =
  (("witnesses" .= map friendlyKeyWitness witnesses) :) <$> friendlyTxBodyImpl era body
 where
  (body, witnesses) = getTxBodyAndWitnesses tx

friendlyKeyWitness :: KeyWitness era -> Aeson.Value
friendlyKeyWitness =
  object
    . \case
      ByronKeyWitness txInWitness -> ["Byron witness" .= textShow txInWitness]
      ShelleyBootstrapWitness _era bootstrapWitness ->
        ["bootstrap witness" .= textShow bootstrapWitness]
      ShelleyKeyWitness _era (L.WitVKey key signature) ->
        ["key" .= textShow key, "signature" .= textShow signature]

friendlyTxBodyImpl
  :: forall m era
   . MonadWarning m
  => Exp.Era era
  -> TxBody era
  -> m [Aeson.Pair]
friendlyTxBodyImpl era tb = do
  let cEra = convert era :: CardanoEra era
      sbe = convert era :: ShelleyBasedEra era
  return
    ( mconcat
        [
          [ "auxiliary scripts" .= friendlyAuxScripts txAuxScripts
          , "certificates" .= forShelleyBasedEraInEon sbe Null (`friendlyCertificates` txCertificates)
          , "collateral inputs" .= friendlyCollateralInputs txInsCollateral
          , "era" .= cEra
          , "fee" .= friendlyFee txFee
          , "inputs" .= friendlyInputs txIns
          , "metadata" .= friendlyMetadata txMetadata
          , "mint" .= friendlyMintValue txMintValue
          , "outputs" .= map (friendlyTxOut era) txOuts
          , "reference inputs" .= friendlyReferenceInputs txInsReference
          , "total collateral" .= friendlyTotalCollateral txTotalCollateral
          , "return collateral" .= friendlyReturnCollateral era txReturnCollateral
          , "required signers (payment key hashes needed for scripts)"
              .= friendlyExtraKeyWits txExtraKeyWits
          , "update proposal" .= friendlyUpdateProposal txUpdateProposal
          , "validity range" .= friendlyValidityRange sbe (txValidityLowerBound, txValidityUpperBound)
          , "withdrawals" .= friendlyWithdrawals txWithdrawals
          ]
        , forShelleyBasedEraInEon
            sbe
            mempty
            (`getScriptWitnessDetails` tb)
        , forShelleyBasedEraInEon
            sbe
            mempty
            ( \cOnwards ->
                conwayEraOnwardsConstraints cOnwards $
                  case txProposalProcedures of
                    Nothing -> []
                    Just (Featured _ TxProposalProceduresNone) -> []
                    Just (Featured _ pp) -> do
                      let lProposals = toList $ convProposalProcedures pp
                      ["governance actions" .= friendlyLedgerProposals (convert cOnwards) lProposals]
            )
        , forShelleyBasedEraInEon
            sbe
            mempty
            ( \cOnwards ->
                case txVotingProcedures of
                  Nothing -> []
                  Just (Featured _ TxVotingProceduresNone) -> []
                  Just (Featured _ (TxVotingProcedures votes _witnesses)) ->
                    ["voters" .= friendlyVotingProcedures cOnwards votes]
            )
        , forShelleyBasedEraInEon @ConwayEraOnwards
            sbe
            mempty
            (const ["currentTreasuryValue" .= toJSON (unFeatured <$> txCurrentTreasuryValue)])
        , forShelleyBasedEraInEon @ConwayEraOnwards
            sbe
            mempty
            (const ["treasuryDonation" .= toJSON (unFeatured <$> txTreasuryDonation)])
        ]
    )
 where
  -- Enumerating the fields, so that we are warned by GHC when we add a new one
  TxBodyContent
    txIns
    txInsCollateral
    txInsReference
    txOuts
    txTotalCollateral
    txReturnCollateral
    txFee
    txValidityLowerBound
    txValidityUpperBound
    txMetadata
    txAuxScripts
    txExtraKeyWits
    _txProtocolParams
    txWithdrawals
    txCertificates
    txUpdateProposal
    txMintValue
    _txScriptValidity
    txProposalProcedures
    txVotingProcedures
    txCurrentTreasuryValue
    txTreasuryDonation = getTxBodyContent tb

friendlyLedgerProposals
  :: Typeable era => Exp.Era era -> [L.ProposalProcedure (ShelleyLedgerEra era)] -> Aeson.Value
friendlyLedgerProposals e proposalProcedures =
  Array $ fromList $ map (obtainCommonConstraints e friendlyLedgerProposal) proposalProcedures

friendlyLedgerProposal
  :: (Typeable era, Exp.IsEra era) => L.ProposalProcedure (ShelleyLedgerEra era) -> Aeson.Value
friendlyLedgerProposal proposalProcedure = object $ friendlyProposalImpl (Proposal proposalProcedure)

friendlyVotingProcedures
  :: ConwayEraOnwards era -> L.VotingProcedures (ShelleyLedgerEra era) -> Aeson.Value
friendlyVotingProcedures cOnwards x = conwayEraOnwardsConstraints cOnwards $ toJSON x

data EraIndependentPlutusScriptPurpose
  = Spending
  | Minting
  | Certifying
  | Rewarding
  | Voting
  | Proposing
  | Guarding

getScriptWitnessDetails
  :: forall era. Exp.Era era -> TxBody era -> [Aeson.Pair]
getScriptWitnessDetails era tb =
  let ShelleyTx _ ledgerTx = makeSignedTransaction [] tb
   in [ "redeemers" .= friendlyRedeemers ledgerTx
      , "scripts" .= friendlyScriptData ledgerTx
      , "datums" .= friendlyDats ledgerTx
      ]
 where
  aeo = convert era
  friendlyRedeemers
    :: Ledger.Tx (ShelleyLedgerEra era)
    -> Aeson.Value
  friendlyRedeemers tx =
    alonzoEraOnwardsConstraints aeo $ do
      let plutusScriptPurposeAndExUnits = Map.toList $ Ledger.unRedeemers $ tx ^. Ledger.witsTxL . Ledger.rdmrsTxWitsL
          redeemerList = map (uncurry $ friendlyRedeemerInfo tx) plutusScriptPurposeAndExUnits
      Aeson.Array $ Vector.fromList redeemerList

  friendlyRedeemerInfo
    :: Ledger.Tx (ShelleyLedgerEra era)
    -> Ledger.PlutusPurpose Ledger.AsIx (ShelleyLedgerEra era)
    -> (Ledger.Data (ShelleyLedgerEra era), ExUnits)
    -> Aeson.Value
  friendlyRedeemerInfo tx redeemerPurpose (redeemerData, exUnits) =
    alonzoEraOnwardsConstraints aeo $ do
      let inputNotFoundError =
            Aeson.object
              [ "error" .= Aeson.String (T.pack $ "Could not find corresponding input to " ++ show redeemerPurpose)
              ]
          mCorrespondingInput = strictMaybeToMaybe $ Ledger.redeemerPointerInverse (tx ^. Ledger.bodyTxL) redeemerPurpose
          mFriendlyPurposeResult = friendlyPurpose aeo <$> mCorrespondingInput
       in object
            [ "purpose" .= fromMaybe inputNotFoundError mFriendlyPurposeResult
            , "redeemer" .= friendlyRedeemer redeemerData exUnits
            ]

  friendlyRedeemer :: Ledger.Data (ShelleyLedgerEra era) -> ExUnits -> Aeson.Value
  friendlyRedeemer scriptData ExUnits{exUnitsSteps = exSteps, exUnitsMem = exMemUnits} =
    object
      [ "data" .= Aeson.String (T.pack $ show $ Ledger.unData scriptData)
      , "execution units"
          .= object
            [ "steps" .= Aeson.Number (fromIntegral exSteps)
            , "memory" .= Aeson.Number (fromIntegral exMemUnits)
            ]
      ]

  friendlyPurpose
    :: AlonzoEraOnwards era -> Ledger.PlutusPurpose L.AsIxItem (ShelleyLedgerEra era) -> Aeson.Value
  friendlyPurpose AlonzoEraOnwardsAlonzo purpose =
    case purpose of
      Ledger.AlonzoSpending (L.AsIxItem _ sp) -> addLabelToPurpose Spending (friendlyInput sp)
      Ledger.AlonzoMinting (L.AsIxItem _ mp) -> addLabelToPurpose Minting mp
      Ledger.AlonzoCertifying (L.AsIxItem _ cp) -> addLabelToPurpose Certifying cp
      Ledger.AlonzoRewarding (L.AsIxItem _ rp) -> addLabelToPurpose Rewarding rp
  friendlyPurpose AlonzoEraOnwardsBabbage purpose =
    case purpose of
      Ledger.AlonzoSpending (L.AsIxItem _ sp) -> addLabelToPurpose Spending (friendlyInput sp)
      Ledger.AlonzoMinting (L.AsIxItem _ mp) -> addLabelToPurpose Minting mp
      Ledger.AlonzoCertifying (L.AsIxItem _ cp) -> addLabelToPurpose Certifying cp
      Ledger.AlonzoRewarding (L.AsIxItem _ rp) -> addLabelToPurpose Rewarding rp
  friendlyPurpose AlonzoEraOnwardsConway purpose =
    case purpose of
      Ledger.ConwaySpending (L.AsIxItem _ sp) -> addLabelToPurpose Spending (friendlyInput sp)
      Ledger.ConwayMinting (L.AsIxItem _ mp) -> addLabelToPurpose Minting mp
      Ledger.ConwayCertifying (L.AsIxItem _ cp) -> addLabelToPurpose Certifying cp
      Ledger.ConwayRewarding (L.AsIxItem _ rp) -> addLabelToPurpose Rewarding rp
      Ledger.ConwayVoting (L.AsIxItem _ vp) -> addLabelToPurpose Voting vp
      Ledger.ConwayProposing (L.AsIxItem _ pp) -> addLabelToPurpose Proposing pp
  friendlyPurpose AlonzoEraOnwardsDijkstra purpose = do
    let era' = fromJust $ forEraMaybeEon (convert era)
    obtainCommonConstraints era' $
      case purpose of
        Ledger.DijkstraSpending (L.AsIxItem _ sp) -> addLabelToPurpose Spending (friendlyInput sp)
        Ledger.DijkstraMinting (L.AsIxItem _ mp) -> addLabelToPurpose Minting mp
        Ledger.DijkstraCertifying (L.AsIxItem _ cp) -> addLabelToPurpose Certifying cp
        Ledger.DijkstraRewarding (L.AsIxItem _ rp) -> addLabelToPurpose Rewarding rp
        Ledger.DijkstraVoting (L.AsIxItem _ vp) -> addLabelToPurpose Voting vp
        Ledger.DijkstraProposing (L.AsIxItem _ pp) -> addLabelToPurpose Proposing pp
        Ledger.DijkstraGuarding (L.AsIxItem _ pp) -> addLabelToPurpose Guarding pp
  friendlyInput :: Ledger.TxIn -> Aeson.Value
  friendlyInput (Ledger.TxIn (Ledger.TxId txidHash) ix) =
    Aeson.String $
      T.pack $
        T.unpack (hashToTextAsHex (extractHash txidHash)) ++ "#" ++ show (Ledger.txIxToInt ix)

  addLabelToPurpose
    :: ToJSON v
    => EraIndependentPlutusScriptPurpose
    -> v
    -> Aeson.Value
  addLabelToPurpose Spending sp = Aeson.object ["spending script witnessed input" .= sp]
  addLabelToPurpose Minting mp = Aeson.object ["minting currency with policy id" .= mp]
  addLabelToPurpose Certifying cp = Aeson.object ["validating certificate with script credentials" .= cp]
  addLabelToPurpose Rewarding rp = Aeson.object ["withdrawing reward from script address" .= rp]
  addLabelToPurpose Voting vp = Aeson.object ["voting using script protected voter credentials" .= vp]
  addLabelToPurpose Proposing pp = Aeson.object ["submitting a proposal following proposal policy" .= pp]
  addLabelToPurpose Guarding _ = error "TODO Dijkstra"

  friendlyScriptData :: Ledger.Tx (ShelleyLedgerEra era) -> Aeson.Value
  friendlyScriptData tx =
    alonzoEraOnwardsConstraints aeo $ do
      Aeson.Array $
        Vector.fromList $
          [ Aeson.Object $
              KeyMap.fromList
                [ "script hash" .= scriptHash
                , "script data" .= friendlyScript scriptData
                ]
          | (scriptHash, scriptData) <- Map.toList $ tx ^. Ledger.witsTxL . Ledger.scriptTxWitsL
          ]

  friendlyDats :: Ledger.Tx (ShelleyLedgerEra era) -> Aeson.Value
  friendlyDats tx =
    alonzoEraOnwardsConstraints aeo $
      let Ledger.TxDats dats = tx ^. Ledger.witsTxL . Ledger.datsTxWitsL
       in Aeson.Array $
            Vector.fromList $
              [ Aeson.Object $
                  KeyMap.fromList
                    [ "datum hash" .= datHash
                    , "datum" .= friendlyDatum dat
                    ]
              | (datHash, dat) <- Map.toList dats
              ]

-- | Create a friendly JSON out of a script
friendlyScript
  :: AlonzoEraOnwardsConstraints era => Ledger.Script (ShelleyLedgerEra era) -> Aeson.Value
friendlyScript script = Aeson.Object $
  KeyMap.fromList $
    case Ledger.getNativeScript script of
      Just nativeScript ->
        [ ("type", "native")
        , ("script", Aeson.String $ T.pack $ Ledger.showTimelock nativeScript)
        ]
      Nothing ->
        ( case Ledger.toPlutusScript script of
            Just plutusScript ->
              Ledger.withPlutusScript plutusScript $
                friendlyPlutusScript $
                  Ledger.plutusScriptLanguage plutusScript
            Nothing -> [("error", Aeson.String "Unsupported script type")]
        )
 where
  friendlyPlutusScript :: Ledger.Language -> Ledger.Plutus l -> [(KeyMap.Key, Aeson.Value)]
  friendlyPlutusScript language plutusScript =
    [ ("type", "plutus")
    , ("plutus version", Aeson.String $ Ledger.languageToText language)
    , ("script", Aeson.String $ Ledger.serializeAsHexText $ Ledger.plutusBinary plutusScript)
    ]

-- | Create a friendly JSON out of a datum
friendlyDatum
  :: AlonzoEraOnwardsConstraints era => Alonzo.Data (ShelleyLedgerEra era) -> Aeson.Value
friendlyDatum (Alonzo.Data datum) = Aeson.String (T.pack $ show datum)

friendlyTotalCollateral :: TxTotalCollateral era -> Aeson.Value
friendlyTotalCollateral TxTotalCollateralNone = Aeson.Null
friendlyTotalCollateral (TxTotalCollateral _ coll) = toJSON coll

friendlyReturnCollateral
  :: ()
  => Exp.Era era
  -> TxReturnCollateral CtxTx era
  -> Aeson.Value
friendlyReturnCollateral era = \case
  TxReturnCollateralNone -> Aeson.Null
  TxReturnCollateral _ collOut -> friendlyTxOut era collOut

friendlyExtraKeyWits :: TxExtraKeyWitnesses era -> Aeson.Value
friendlyExtraKeyWits = \case
  TxExtraKeyWitnessesNone -> Null
  TxExtraKeyWitnesses _supported paymentKeyHashes -> toJSON paymentKeyHashes

friendlyValidityRange
  :: ShelleyBasedEra era
  -> (TxValidityLowerBound era, TxValidityUpperBound era)
  -> Aeson.Value
friendlyValidityRange era = \case
  (lowerBound, upperBound)
    | isLowerBoundSupported || isUpperBoundSupported ->
        object
          [ "lower bound"
              .= case lowerBound of
                TxValidityNoLowerBound -> Null
                TxValidityLowerBound _ s -> toJSON s
          , "upper bound"
              .= case upperBound of
                TxValidityUpperBound _ s -> toJSON s
          ]
    | otherwise -> Null
 where
  isLowerBoundSupported = isJust $ forShelleyBasedEraInEonMaybe era TxValidityLowerBound
  isUpperBoundSupported = isJust $ forShelleyBasedEraInEonMaybe era TxValidityUpperBound

friendlyWithdrawals :: TxWithdrawals ViewTx era -> Aeson.Value
friendlyWithdrawals TxWithdrawalsNone = Null
friendlyWithdrawals (TxWithdrawals _ withdrawals) =
  array
    [ object $
        "address" .= serialiseAddress addr
          : "amount" .= friendlyLovelace amount
          : friendlyStakeAddress addr
    | (addr, amount, _) <- withdrawals
    ]

friendlyStakeAddress :: StakeAddress -> [Aeson.Pair]
friendlyStakeAddress (StakeAddress net cred) =
  [ "network" .= net
  , friendlyStakeCredential cred
  ]

friendlyTxOut :: Exp.Era era -> TxOut CtxTx era -> Aeson.Value
friendlyTxOut era (TxOut addr amount mdatum script) =
  obtainCommonConstraints era $
    object $
      case addr of
        AddressInEra ByronAddressInAnyEra byronAdr ->
          [ "address era" .= String "Byron"
          , "address" .= serialiseAddress byronAdr
          , "amount" .= friendlyTxOutValue amount
          ]
        AddressInEra (ShelleyAddressInEra _) saddr@(ShelleyAddress net cred stake) ->
          let preAlonzo =
                friendlyPaymentCredential (fromShelleyPaymentCredential cred)
                  : [ "address era" .= Aeson.String "Shelley"
                    , "network" .= net
                    , "address" .= serialiseAddress saddr
                    , "amount" .= friendlyTxOutValue amount
                    , "stake reference" .= friendlyStakeReference (fromShelleyStakeReference stake)
                    ]
              datum = ["datum" .= d | d <- maybeToList $ renderDatum mdatum]
              sinceAlonzo = ["reference script" .= script]
           in preAlonzo ++ datum ++ sinceAlonzo
 where
  renderDatum :: TxOutDatum CtxTx era -> Maybe Aeson.Value
  renderDatum = \case
    TxOutDatumNone -> Nothing
    TxOutDatumHash _ h -> Just $ toJSON h
    TxOutSupplementalDatum _ sData -> Just $ scriptDataToJson ScriptDataJsonDetailedSchema sData
    TxOutDatumInline _ sData -> Just $ scriptDataToJson ScriptDataJsonDetailedSchema sData

friendlyStakeReference :: StakeAddressReference -> Aeson.Value
friendlyStakeReference = \case
  NoStakeAddress -> Null
  StakeAddressByPointer ptr -> String (textShow ptr)
  StakeAddressByValue cred -> object [friendlyStakeCredential $ toShelleyStakeCredential cred]

friendlyUpdateProposal :: TxUpdateProposal era -> Aeson.Value
friendlyUpdateProposal = \case
  TxUpdateProposalNone -> Null
  TxUpdateProposal _ (UpdateProposal parameterUpdates epoch) ->
    object
      [ "epoch" .= epoch
      , "updates"
          .= [ object
                 [ "genesis key hash" .= genesisKeyHash
                 , "update" .= friendlyProtocolParametersUpdate parameterUpdate
                 ]
             | (genesisKeyHash, parameterUpdate) <- Map.assocs parameterUpdates
             ]
      ]

friendlyProtocolParametersUpdate :: ProtocolParametersUpdate -> Aeson.Value
friendlyProtocolParametersUpdate
  ProtocolParametersUpdate
    { protocolUpdateProtocolVersion
    , protocolUpdateDecentralization
    , protocolUpdateExtraPraosEntropy
    , protocolUpdateMaxBlockHeaderSize
    , protocolUpdateMaxBlockBodySize
    , protocolUpdateMaxTxSize
    , protocolUpdateTxFeeFixed
    , protocolUpdateTxFeePerByte
    , protocolUpdateMinUTxOValue
    , protocolUpdateStakeAddressDeposit
    , protocolUpdateStakePoolDeposit
    , protocolUpdateMinPoolCost
    , protocolUpdatePoolRetireMaxEpoch
    , protocolUpdateStakePoolTargetNum
    , protocolUpdatePoolPledgeInfluence
    , protocolUpdateMonetaryExpansion
    , protocolUpdateTreasuryCut
    , protocolUpdateCollateralPercent
    , protocolUpdateMaxBlockExUnits
    , protocolUpdateMaxCollateralInputs
    , protocolUpdateMaxTxExUnits
    , protocolUpdateMaxValueSize
    , protocolUpdatePrices
    , protocolUpdateUTxOCostPerByte
    } =
    object . catMaybes $
      [ protocolUpdateProtocolVersion <&> \(major, minor) ->
          "protocol version" .= (textShow major <> "." <> textShow minor)
      , protocolUpdateDecentralization
          <&> ("decentralization parameter" .=) . friendlyRational
      , protocolUpdateExtraPraosEntropy
          <&> ("extra entropy" .=) . maybe "reset" toJSON
      , protocolUpdateMaxBlockHeaderSize <&> ("max block header size" .=)
      , protocolUpdateMaxBlockBodySize <&> ("max block body size" .=)
      , protocolUpdateMaxTxSize <&> ("max transaction size" .=)
      , protocolUpdateTxFeeFixed <&> ("transaction fee constant" .=)
      , protocolUpdateTxFeePerByte <&> ("transaction fee linear per byte" .=)
      , protocolUpdateMinUTxOValue <&> ("min UTxO value" .=) . friendlyLovelace
      , protocolUpdateStakeAddressDeposit
          <&> ("key registration deposit" .=) . friendlyLovelace
      , protocolUpdateStakePoolDeposit
          <&> ("pool registration deposit" .=) . friendlyLovelace
      , protocolUpdateMinPoolCost <&> ("min pool cost" .=) . friendlyLovelace
      , protocolUpdatePoolRetireMaxEpoch <&> ("pool retirement epoch boundary" .=)
      , protocolUpdateStakePoolTargetNum <&> ("number of pools" .=)
      , protocolUpdatePoolPledgeInfluence
          <&> ("pool influence" .=) . friendlyRational
      , protocolUpdateMonetaryExpansion
          <&> ("monetary expansion" .=) . friendlyRational
      , protocolUpdateTreasuryCut <&> ("treasury expansion" .=) . friendlyRational
      , protocolUpdateCollateralPercent
          <&> ("collateral inputs share" .=) . (<> "%") . textShow
      , protocolUpdateMaxBlockExUnits <&> ("max block execution units" .=)
      , protocolUpdateMaxCollateralInputs <&> ("max collateral inputs" .=)
      , protocolUpdateMaxTxExUnits <&> ("max transaction execution units" .=)
      , protocolUpdateMaxValueSize <&> ("max value size" .=)
      , protocolUpdatePrices <&> ("execution prices" .=) . friendlyPrices
      , protocolUpdateUTxOCostPerByte
          <&> ("UTxO storage cost per byte" .=) . friendlyLovelace
      ]

friendlyPrices :: ExecutionUnitPrices -> Aeson.Value
friendlyPrices ExecutionUnitPrices{priceExecutionMemory, priceExecutionSteps} =
  object
    [ "memory" .= friendlyRational priceExecutionMemory
    , "steps" .= friendlyRational priceExecutionSteps
    ]

friendlyCertificates :: ShelleyBasedEra era -> TxCertificates ViewTx era -> Aeson.Value
friendlyCertificates sbe = \case
  TxCertificatesNone -> Null
  TxCertificates _ cs -> array $ map (friendlyCertificate sbe . fst) $ toList cs

friendlyCertificate :: ShelleyBasedEra era -> Exp.Certificate (ShelleyLedgerEra era) -> Aeson.Value
friendlyCertificate sbe =
  shelleyBasedEraConstraints sbe $
    object . (: []) . renderCertificate sbe

renderCertificate
  :: ShelleyBasedEra era -> Exp.Certificate (ShelleyLedgerEra era) -> (Aeson.Key, Aeson.Value)
renderCertificate sbe (Exp.Certificate c) =
  case sbe of
    ShelleyBasedEraShelley -> renderShelleyCertificate sbe c
    ShelleyBasedEraAllegra -> renderShelleyCertificate sbe c
    ShelleyBasedEraMary -> renderShelleyCertificate sbe c
    ShelleyBasedEraAlonzo -> renderShelleyCertificate sbe c
    ShelleyBasedEraBabbage -> renderShelleyCertificate sbe c
    ShelleyBasedEraConway -> renderConwayCertificate c
    ShelleyBasedEraDijkstra -> error "renderCertificate: TODO Dijkstra era not supported"

renderDrepCredential
  :: ()
  => L.Credential 'L.DRepRole
  -> Aeson.Value
renderDrepCredential =
  object . \case
    L.ScriptHashObj sHash -> ["scriptHash" .= sHash]
    L.KeyHashObj keyHash -> ["keyHash" .= keyHash]

delegateeJson
  :: L.Delegatee
  -> Aeson.Value
delegateeJson =
  object . \case
    L.DelegStake hk@L.KeyHash{} ->
      [ "delegatee type" .= String "stake"
      , "key hash" .= hk
      ]
    L.DelegVote drep -> do
      ["delegatee type" .= String "vote", "DRep" .= drep]
    L.DelegStakeVote kh drep ->
      [ "delegatee type" .= String "stake vote"
      , "key hash" .= kh
      , "DRep" .= drep
      ]

renderShelleyCertificate
  :: ShelleyBasedEra era -> Ledger.ShelleyTxCert (ShelleyLedgerEra era) -> (Aeson.Key, Aeson.Value)
renderShelleyCertificate sbe c =
  case c of
    L.ShelleyTxCertDelegCert (L.ShelleyRegCert cred) ->
      "stake address registration" .= cred
    L.ShelleyTxCertDelegCert (L.ShelleyUnRegCert cred) ->
      "stake address deregistration" .= cred
    L.ShelleyTxCertDelegCert (L.ShelleyDelegCert cred poolId) ->
      "stake address delegation"
        .= object
          [ "credential" .= cred
          , "pool" .= poolId
          ]
    L.ShelleyTxCertPool (L.RetirePool poolId retirementEpoch) ->
      "stake pool retirement"
        .= object
          [ "pool" .= StakePoolKeyHash poolId
          , "epoch" .= retirementEpoch
          ]
    L.ShelleyTxCertPool (L.RegPool poolParams) ->
      "stake pool registration" .= poolParams
    L.ShelleyTxCertGenesisDeleg (L.GenesisDelegCert genesisKeyHash delegateKeyHash vrfKeyHash) ->
      "genesis key delegation"
        .= object
          [ "genesis key hash" .= genesisKeyHash
          , "delegate key hash" .= delegateKeyHash
          , "VRF key hash" .= vrfKeyHash
          ]
    L.ShelleyTxCertMir (L.MIRCert pot target) ->
      "MIR"
        .= object
          [ "pot" .= friendlyMirPot pot
          , friendlyMirTarget sbe target
          ]

renderConwayCertificate
  :: Ledger.ConwayTxCert (ShelleyLedgerEra ConwayEra) -> (Aeson.Key, Aeson.Value)
renderConwayCertificate cert =
  case cert of
    L.RegDRepTxCert credential coin mAnchor ->
      "Drep registration certificate"
        .= object
          [ "deposit" .= coin
          , "certificate" .= renderDrepCredential credential
          , "anchor" .= mAnchor
          ]
    L.UnRegDRepTxCert credential coin ->
      "Drep unregistration certificate"
        .= object
          [ "refund" .= coin
          , "certificate" .= renderDrepCredential credential
          ]
    L.AuthCommitteeHotKeyTxCert coldCred hotCred
      | L.ScriptHashObj sh <- coldCred ->
          "Cold committee authorization"
            .= object
              ["script hash" .= sh]
      | L.ScriptHashObj sh <- hotCred ->
          "Hot committee authorization"
            .= object
              ["script hash" .= sh]
      | L.KeyHashObj ck@L.KeyHash{} <- coldCred
      , L.KeyHashObj hk@L.KeyHash{} <- hotCred ->
          "Constitutional committee member hot key registration"
            .= object
              [ "cold key hash" .= ck
              , "hot key hash" .= hk
              ]
    L.ResignCommitteeColdTxCert cred anchor -> case cred of
      L.ScriptHashObj sh ->
        "Cold committee resignation"
          .= object
            [ "script hash" .= sh
            , "anchor" .= anchor
            ]
      L.KeyHashObj ck@L.KeyHash{} ->
        "Constitutional committee cold key resignation"
          .= object
            [ "cold key hash" .= ck
            ]
    L.RegTxCert stakeCredential ->
      "Stake address registration"
        .= object
          [ "stake credential" .= stakeCredential
          ]
    L.UnRegTxCert stakeCredential ->
      "Stake address deregistration"
        .= object
          [ "stake credential" .= stakeCredential
          ]
    L.RegDepositTxCert stakeCredential deposit ->
      "Stake address registration"
        .= object
          [ "stake credential" .= stakeCredential
          , "deposit" .= deposit
          ]
    L.UnRegDepositTxCert stakeCredential refund ->
      "Stake address deregistration"
        .= object
          [ "stake credential" .= stakeCredential
          , "refund" .= refund
          ]
    L.DelegTxCert stakeCredential delegatee ->
      "Stake address delegation"
        .= object
          [ "stake credential" .= stakeCredential
          , "delegatee" .= delegateeJson delegatee
          ]
    L.RegDepositDelegTxCert stakeCredential delegatee deposit ->
      "Stake address registration and delegation"
        .= object
          [ "stake credential" .= stakeCredential
          , "delegatee" .= delegateeJson delegatee
          , "deposit" .= deposit
          ]
    L.RegPoolTxCert poolParams ->
      "Pool registration"
        .= object
          [ "pool params" .= poolParams
          ]
    L.RetirePoolTxCert kh@L.KeyHash{} epoch ->
      "Pool retirement"
        .= object
          [ "stake pool key hash" .= kh
          , "epoch" .= epoch
          ]
    L.UpdateDRepTxCert drepCredential mbAnchor ->
      "Drep certificate update"
        .= object
          [ "Drep credential" .= drepCredential
          , "anchor " .= mbAnchor
          ]
    _ -> "unsupported certificate" .= String (T.pack $ show cert)

friendlyMirTarget
  :: ShelleyBasedEra era -> L.MIRTarget -> Aeson.Pair
friendlyMirTarget sbe = \case
  L.StakeAddressesMIR addresses ->
    "target stake addresses"
      .= [ object
             [ friendlyStakeCredential credential
             , "amount" .= friendlyLovelace (L.Coin 0 `L.addDeltaCoin` lovelace)
             ]
         | (credential, lovelace) <- shelleyBasedEraConstraints sbe $ toList addresses
         ]
  L.SendToOppositePotMIR amount -> "MIR amount" .= friendlyLovelace amount

friendlyStakeCredential
  :: L.Credential L.Staking -> Aeson.Pair
friendlyStakeCredential = \case
  L.KeyHashObj keyHash ->
    "stake credential key hash" .= keyHash
  L.ScriptHashObj scriptHash ->
    "stake credential script hash" .= scriptHash

friendlyPaymentCredential :: PaymentCredential -> Aeson.Pair
friendlyPaymentCredential = \case
  PaymentCredentialByKey keyHash ->
    "payment credential key hash" .= keyHash
  PaymentCredentialByScript scriptHash ->
    "payment credential script hash" .= scriptHash

friendlyMirPot :: L.MIRPot -> Aeson.Value
friendlyMirPot = \case
  L.ReservesMIR -> "reserves"
  L.TreasuryMIR -> "treasury"

friendlyRational :: Rational -> Aeson.Value
friendlyRational r =
  String $
    case d of
      1 -> textShow n
      _ -> textShow n <> "/" <> textShow d
 where
  n = numerator r
  d = denominator r

friendlyFee :: TxFee era -> Aeson.Value
friendlyFee = \case
  TxFeeExplicit _ fee -> friendlyLovelace fee

friendlyLovelace :: Lovelace -> Aeson.Value
friendlyLovelace value = String $ docToText (pretty value)

friendlyMintValue :: forall era. TxMintValue ViewTx era -> Aeson.Value
friendlyMintValue = \case
  TxMintNone -> Null
  txMintValue@(TxMintValue w _) -> friendlyValue @era (convert w) $ txMintValueToValue txMintValue

friendlyTxOutValue :: TxOutValue era -> Aeson.Value
friendlyTxOutValue = \case
  TxOutValueByron lovelace -> friendlyLovelace lovelace
  TxOutValueShelleyBased sbe v -> friendlyLedgerValue sbe v

friendlyLedgerValue
  :: ()
  => ShelleyBasedEra era
  -> L.Value (ShelleyLedgerEra era)
  -> Aeson.Value
friendlyLedgerValue sbe v = friendlyValue sbe $ Api.fromLedgerValue sbe v

friendlyValue
  :: ()
  => ShelleyBasedEra era
  -> Api.Value
  -> Aeson.Value
friendlyValue _ v =
  object
    [ case bundle of
        ValueNestedBundleAda q -> "lovelace" .= q
        ValueNestedBundle policy assets ->
          Aeson.fromText (friendlyPolicyId policy) .= friendlyAssets assets
    | bundle <- bundles
    ]
 where
  ValueNestedRep bundles = valueToNestedRep v

  friendlyPolicyId = ("policy " <>) . serialiseToRawBytesHexText

  friendlyAssets = Map.mapKeys friendlyAssetName

  friendlyAssetName = \case
    UnsafeAssetName "" -> "default asset"
    name@(UnsafeAssetName nameBS) ->
      "asset " <> serialiseToRawBytesHexText name <> nameAsciiSuffix
     where
      nameAsciiSuffix
        | nameIsAscii = " (" <> nameAscii <> ")"
        | otherwise = ""
      nameIsAscii = BSC.all (\c -> isAscii c && isAlphaNum c) nameBS
      nameAscii = Text.pack $ BSC.unpack nameBS

friendlyMetadata :: TxMetadataInEra era -> Aeson.Value
friendlyMetadata = \case
  TxMetadataNone -> Null
  TxMetadataInEra _ (TxMetadata m) -> toJSON $ friendlyMetadataValue <$> m

friendlyMetadataValue :: TxMetadataValue -> Aeson.Value
friendlyMetadataValue = \case
  TxMetaNumber int -> toJSON int
  TxMetaBytes bytes -> String $ textShow bytes
  TxMetaList lst -> array $ map friendlyMetadataValue lst
  TxMetaMap m ->
    array
      [array [friendlyMetadataValue k, friendlyMetadataValue v] | (k, v) <- m]
  TxMetaText text -> toJSON text

friendlyAuxScripts :: TxAuxScripts era -> Aeson.Value
friendlyAuxScripts = \case
  TxAuxScriptsNone -> Null
  TxAuxScripts _ scripts -> String $ textShow scripts

friendlyReferenceInputs :: TxInsReference era build -> Aeson.Value
friendlyReferenceInputs TxInsReferenceNone = Null
friendlyReferenceInputs (TxInsReference _ txins _) = toJSON txins

friendlyInputs :: [(TxIn, build)] -> Aeson.Value
friendlyInputs = toJSON . map fst

friendlyCollateralInputs :: TxInsCollateral era -> Aeson.Value
friendlyCollateralInputs = \case
  TxInsCollateralNone -> Null
  TxInsCollateral _ txins -> toJSON txins

friendlyDRep :: L.DRep -> Aeson.Value
friendlyDRep L.DRepAlwaysAbstain = "alwaysAbstain"
friendlyDRep L.DRepAlwaysNoConfidence = "alwaysNoConfidence"
friendlyDRep (L.DRepCredential sch@(L.ScriptHashObj (C.ScriptHash _))) =
  Aeson.object
    [ "scriptHashHex" .= UsingRawBytesHex sch
    ]
friendlyDRep (L.DRepCredential kh@(L.KeyHashObj (C.KeyHash _))) =
  Aeson.object
    [ "keyHashHex" .= UsingRawBytesHex kh
    , "keyHashBech32" .= serialiseToBech32Cip129 kh
    ]
