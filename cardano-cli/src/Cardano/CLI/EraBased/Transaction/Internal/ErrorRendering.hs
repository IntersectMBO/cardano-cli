{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.CLI.EraBased.Transaction.Internal.ErrorRendering
  ( renderApplyTxErrors
  , renderScriptWitnessIndexShort
  , renderScriptExecutionError
  )
where

import Cardano.Api
import Cardano.Api.Experimental qualified as Exp
import Cardano.Api.Ledger qualified as L

import Data.Foldable qualified as Foldable
import Data.Map.NonEmpty qualified as NEMap
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Set.NonEmpty (NonEmptySet, toSet)
import Data.Text qualified as Text
import Lens.Micro ((^.))

showT :: Show a => a -> Text
showT = Text.pack . show

showCoin :: L.Coin -> Text
showCoin (L.Coin n) = Text.pack (show n)

showDeltaCoin :: L.DeltaCoin -> Text
showDeltaCoin (L.DeltaCoin n) = Text.pack (show n)

renderLedgerTxIn :: L.TxIn -> Text
renderLedgerTxIn (L.TxIn (L.TxId h) ix) =
  L.hashToTextAsHex (L.extractHash h) <> "#" <> Text.pack (show (fromEnum ix))

renderLedgerTxIns :: Set.Set L.TxIn -> Text
renderLedgerTxIns = Text.intercalate ", " . map renderLedgerTxIn . Set.toList

renderScriptHash :: L.ScriptHash -> Text
renderScriptHash (L.ScriptHash h) = L.hashToTextAsHex h

renderScriptHashes :: Set.Set L.ScriptHash -> Text
renderScriptHashes = Text.intercalate ", " . map renderScriptHash . Set.toList

renderKeyHash :: L.KeyHash r -> Text
renderKeyHash = L.hashToTextAsHex . L.unKeyHash

class RelationSymbol (r :: L.Relation) where
  relationSymbol :: Text

instance RelationSymbol L.RelEQ where relationSymbol = "="
instance RelationSymbol L.RelLT where relationSymbol = "<"
instance RelationSymbol L.RelGT where relationSymbol = ">"
instance RelationSymbol L.RelLTEQ where relationSymbol = "≤"
instance RelationSymbol L.RelGTEQ where relationSymbol = "≥"
instance RelationSymbol L.RelSubset where relationSymbol = "⊆"

renderMismatch :: forall r a. RelationSymbol r => Text -> (a -> Text) -> L.Mismatch r a -> [Text]
renderMismatch name renderVal L.Mismatch{L.mismatchSupplied, L.mismatchExpected} =
  [ name
      <> ": supplied "
      <> renderVal mismatchSupplied
      <> ", expected "
      <> relationSymbol @r
      <> " "
      <> renderVal mismatchExpected
  ]

renderMismatchInline :: forall r a. RelationSymbol r => (a -> Text) -> L.Mismatch r a -> Text
renderMismatchInline renderVal L.Mismatch{L.mismatchSupplied, L.mismatchExpected} =
  "supplied "
    <> renderVal mismatchSupplied
    <> ", expected "
    <> relationSymbol @r
    <> " "
    <> renderVal mismatchExpected

showEpochNo :: L.EpochNo -> Text
showEpochNo (L.EpochNo n) = Text.pack (show n)

renderVKeyWitnesses :: NonEmptySet (L.KeyHash r) -> [Text]
renderVKeyWitnesses keyHashes =
  let hashes = [renderKeyHash kh | kh <- Set.toList (toSet keyHashes)]
   in ["MissingVKeyWitnessesUTXOW: " <> Text.intercalate ", " hashes]

renderWithdrawals :: L.Withdrawals -> [Text]
renderWithdrawals (L.Withdrawals ws) =
  [ "  " <> showT addr <> ": " <> showCoin coin <> " lovelace"
  | (addr, coin) <- Map.toList ws
  ]

renderTxOutCoins :: Foldable f => f (L.TxOut (ShelleyLedgerEra ConwayEra)) -> [Text]
renderTxOutCoins outs =
  [ "  output with " <> showCoin (out ^. L.coinTxOutL) <> " lovelace"
  | out <- Foldable.toList outs
  ]

renderSafeHash :: L.SafeHash c -> Text
renderSafeHash = L.hashToTextAsHex . L.extractHash

renderDataHashes :: Foldable f => f L.DataHash -> Text
renderDataHashes = Text.intercalate ", " . map renderSafeHash . Foldable.toList

renderStrictMaybeHash :: L.StrictMaybe (L.SafeHash c) -> Text
renderStrictMaybeHash L.SNothing = "none"
renderStrictMaybeHash (L.SJust h) = renderSafeHash h

renderPlutusPurposeAsItem :: L.ConwayPlutusPurpose L.AsItem (ShelleyLedgerEra ConwayEra) -> Text
renderPlutusPurposeAsItem = \case
  L.ConwaySpending (L.AsItem txIn) -> "Spending " <> renderLedgerTxIn txIn
  L.ConwayMinting (L.AsItem (L.PolicyID sh)) -> "Minting " <> renderScriptHash sh
  L.ConwayCertifying (L.AsItem cert) -> "Certifying " <> showT cert
  L.ConwayRewarding (L.AsItem addr) -> "Rewarding " <> showT addr
  L.ConwayVoting (L.AsItem voter) -> "Voting " <> showT voter
  L.ConwayProposing (L.AsItem prop) -> "Proposing " <> showT prop

renderPlutusPurposeAsIx :: L.ConwayPlutusPurpose L.AsIx (ShelleyLedgerEra ConwayEra) -> Text
renderPlutusPurposeAsIx = \case
  L.ConwaySpending (L.AsIx ix) -> "Spending:" <> showT ix
  L.ConwayMinting (L.AsIx ix) -> "Minting:" <> showT ix
  L.ConwayCertifying (L.AsIx ix) -> "Certifying:" <> showT ix
  L.ConwayRewarding (L.AsIx ix) -> "Rewarding:" <> showT ix
  L.ConwayVoting (L.AsIx ix) -> "Voting:" <> showT ix
  L.ConwayProposing (L.AsIx ix) -> "Proposing:" <> showT ix

renderApplyTxErrors :: Exp.Era era -> L.ApplyTxError (Exp.LedgerEra era) -> [Text]
renderApplyTxErrors = \case
  Exp.ConwayEra -> renderConwayErrors
  Exp.DijkstraEra -> error "TODO Dijkstra: renderApplyTxErrors"

-- Conway top-level

renderConwayErrors :: L.ApplyTxError (ShelleyLedgerEra ConwayEra) -> [Text]
renderConwayErrors (L.ConwayApplyTxError failures) =
  concatMap conwayLedgerFailure failures

conwayLedgerFailure :: L.ConwayLedgerPredFailure (ShelleyLedgerEra ConwayEra) -> [Text]
conwayLedgerFailure = \case
  L.ConwayUtxowFailure f -> conwayUtxowFailure f
  L.ConwayCertsFailure f -> conwayCertsFailure f
  L.ConwayGovFailure f -> conwayGovFailure f
  L.ConwayWdrlNotDelegatedToDRep keyHashes ->
    [ "ConwayWdrlNotDelegatedToDRep: "
        <> Text.intercalate ", " (map renderKeyHash (Foldable.toList keyHashes))
    ]
  L.ConwayTreasuryValueMismatch m ->
    renderMismatch "ConwayTreasuryValueMismatch" (\c -> showCoin c <> " lovelace") m
  L.ConwayTxRefScriptsSizeTooBig m ->
    renderMismatch "ConwayTxRefScriptsSizeTooBig" (\n -> showT n <> " bytes") m
  L.ConwayMempoolFailure txt -> ["ConwayMempoolFailure: " <> txt]
  L.ConwayWithdrawalsMissingAccounts ws ->
    "ConwayWithdrawalsMissingAccounts:" : renderWithdrawals ws
  L.ConwayIncompleteWithdrawals m ->
    ["ConwayIncompleteWithdrawals:"]
      <> [ "  "
             <> showT addr
             <> ": "
             <> renderMismatchInline (\c -> showCoin c <> " lovelace") mm
         | (addr, mm) <- NEMap.toList m
         ]

-- Conway UTXOW

conwayUtxowFailure :: L.ConwayUtxowPredFailure (ShelleyLedgerEra ConwayEra) -> [Text]
conwayUtxowFailure = \case
  L.UtxoFailure f -> conwayUtxoFailure f
  L.InvalidWitnessesUTXOW ws ->
    ["InvalidWitnessesUTXOW:"]
      <> ["  " <> showT w | w <- Foldable.toList ws]
  L.MissingVKeyWitnessesUTXOW keyHashes -> renderVKeyWitnesses keyHashes
  L.MissingScriptWitnessesUTXOW hs ->
    ["MissingScriptWitnessesUTXOW: " <> renderScriptHashes (toSet hs)]
  L.ScriptWitnessNotValidatingUTXOW hs ->
    ["ScriptWitnessNotValidatingUTXOW: " <> renderScriptHashes (toSet hs)]
  L.MissingTxBodyMetadataHash (L.TxAuxDataHash h) -> ["MissingTxBodyMetadataHash: " <> renderSafeHash h]
  L.MissingTxMetadata (L.TxAuxDataHash h) -> ["MissingTxMetadata: " <> renderSafeHash h]
  L.ConflictingMetadataHash m ->
    renderMismatch "ConflictingMetadataHash" (\(L.TxAuxDataHash h) -> renderSafeHash h) m
  L.InvalidMetadata -> ["InvalidMetadata"]
  L.ExtraneousScriptWitnessesUTXOW hs ->
    ["ExtraneousScriptWitnessesUTXOW: " <> renderScriptHashes (toSet hs)]
  L.MissingRedeemers rs ->
    ["MissingRedeemers:"]
      <> [ "  " <> renderPlutusPurposeAsItem purpose <> " -> " <> renderScriptHash sh
         | (purpose, sh) <- Foldable.toList rs
         ]
  L.MissingRequiredDatums missing received ->
    [ "MissingRequiredDatums: missing "
        <> renderDataHashes (toSet missing)
        <> ", received "
        <> renderDataHashes received
    ]
  L.NotAllowedSupplementalDatums unallowed acceptable ->
    [ "NotAllowedSupplementalDatums: unallowed "
        <> renderDataHashes (toSet unallowed)
        <> ", acceptable "
        <> renderDataHashes acceptable
    ]
  L.PPViewHashesDontMatch m -> renderMismatch "PPViewHashesDontMatch" renderStrictMaybeHash m
  L.UnspendableUTxONoDatumHash txins ->
    ["UnspendableUTxONoDatumHash: " <> renderLedgerTxIns (toSet txins)]
  L.ExtraRedeemers rs ->
    ["ExtraRedeemers:"]
      <> ["  " <> renderPlutusPurposeAsIx r | r <- Foldable.toList rs]
  L.MalformedScriptWitnesses hs ->
    ["MalformedScriptWitnesses: " <> renderScriptHashes (toSet hs)]
  L.MalformedReferenceScripts hs ->
    ["MalformedReferenceScripts: " <> renderScriptHashes (toSet hs)]
  L.ScriptIntegrityHashMismatch m _bs -> renderMismatch "ScriptIntegrityHashMismatch" renderStrictMaybeHash m

-- Conway UTxO

conwayUtxoFailure :: L.ConwayUtxoPredFailure (ShelleyLedgerEra ConwayEra) -> [Text]
conwayUtxoFailure = \case
  L.UtxosFailure f -> conwayUtxosFailure f
  L.BadInputsUTxO ins -> ["BadInputsUTxO: " <> renderLedgerTxIns (toSet ins)]
  L.OutsideValidityIntervalUTxO interval slot ->
    ["OutsideValidityIntervalUTxO: validity interval " <> showT interval <> " at slot " <> showT slot]
  L.MaxTxSizeUTxO m ->
    renderMismatch "MaxTxSizeUTxO" (\n -> showT n <> " bytes") m
  L.InputSetEmptyUTxO -> ["InputSetEmptyUTxO"]
  L.FeeTooSmallUTxO L.Mismatch{L.mismatchSupplied, L.mismatchExpected} ->
    [ "FeeTooSmallUTxO: minimum fee is "
        <> showCoin mismatchExpected
        <> " lovelace"
        <> ", transaction specifies "
        <> showCoin mismatchSupplied
        <> " lovelace"
    ]
  L.ValueNotConservedUTxO m ->
    renderMismatch "ValueNotConservedUTxO" (\v -> showCoin (L.coin v) <> " lovelace") m
  L.WrongNetwork network addrs -> ["WrongNetwork: expected " <> showT network <> ", addresses " <> showT addrs]
  L.WrongNetworkWithdrawal network addrs ->
    ["WrongNetworkWithdrawal: expected " <> showT network <> ", addresses " <> showT addrs]
  L.OutputTooSmallUTxO outs ->
    "OutputTooSmallUTxO:" : renderTxOutCoins outs
  L.OutputBootAddrAttrsTooBig outs ->
    "OutputBootAddrAttrsTooBig:" : renderTxOutCoins outs
  L.OutputTooBigUTxO outs ->
    ["OutputTooBigUTxO:"]
      <> [ "  output: actual size " <> showT actual <> ", limit " <> showT limit
         | (actual, limit, _) <- Foldable.toList outs
         ]
  L.InsufficientCollateral actualBal required ->
    [ "InsufficientCollateral: actual collateral is "
        <> showDeltaCoin actualBal
        <> " lovelace"
        <> ", required collateral is "
        <> showCoin required
        <> " lovelace"
    ]
  L.ScriptsNotPaidUTxO utxos ->
    ["ScriptsNotPaidUTxO:"]
      <> [ "  " <> renderLedgerTxIn txIn <> ": " <> showCoin (out ^. L.coinTxOutL) <> " lovelace"
         | (txIn, out) <- NEMap.toList utxos
         ]
  L.ExUnitsTooBigUTxO m ->
    renderMismatch "ExUnitsTooBigUTxO" showT m
  L.CollateralContainsNonADA val -> ["CollateralContainsNonADA: " <> showT val]
  L.WrongNetworkInTxBody m -> renderMismatch "WrongNetworkInTxBody" showT m
  L.OutsideForecast slot -> ["OutsideForecast: slot " <> showT slot]
  L.TooManyCollateralInputs m ->
    renderMismatch "TooManyCollateralInputs" showT m
  L.NoCollateralInputs -> ["NoCollateralInputs"]
  L.IncorrectTotalCollateralField actualBal declaredTotal ->
    [ "IncorrectTotalCollateralField: declared total collateral is "
        <> showCoin declaredTotal
        <> " lovelace"
        <> ", actual total collateral is "
        <> showDeltaCoin actualBal
        <> " lovelace"
    ]
  L.BabbageOutputTooSmallUTxO outs ->
    ["BabbageOutputTooSmallUTxO:"]
      <> [ "  output has "
             <> showCoin (out ^. L.coinTxOutL)
             <> " lovelace, minimum is "
             <> showCoin minCoin
             <> " lovelace"
         | (out, minCoin) <- Foldable.toList outs
         ]
  L.BabbageNonDisjointRefInputs ins ->
    [ "BabbageNonDisjointRefInputs: "
        <> Text.intercalate ", " (map renderLedgerTxIn (Foldable.toList ins))
    ]

-- Conway UTxOS (phase-2 script validation errors)

conwayUtxosFailure :: L.ConwayUtxosPredFailure (ShelleyLedgerEra ConwayEra) -> [Text]
conwayUtxosFailure = \case
  L.ValidationTagMismatch (L.IsValid expected) desc ->
    ["ValidationTagMismatch: isValid=" <> showT expected <> ", " <> renderTagMismatch desc]
  L.CollectErrors errs ->
    ["CollectErrors:"] <> concatMap (\e -> ["  " <> renderCollectError e]) errs

renderTagMismatch :: L.TagMismatchDescription -> Text
renderTagMismatch = \case
  L.PassedUnexpectedly -> "script passed unexpectedly (expected failure)"
  L.FailedUnexpectedly descs ->
    "script failed unexpectedly: "
      <> Text.intercalate "; " (map renderFailureDescription (Foldable.toList descs))

renderFailureDescription :: L.FailureDescription -> Text
renderFailureDescription = \case
  L.PlutusFailure msg _bs -> msg

renderCollectError :: L.CollectError (ShelleyLedgerEra ConwayEra) -> Text
renderCollectError = \case
  L.NoRedeemer purpose -> "NoRedeemer: " <> renderPlutusPurposeAsItem purpose
  L.NoWitness scriptHash -> "NoWitness: " <> renderScriptHash scriptHash
  L.NoCostModel lang -> "NoCostModel: " <> showT lang
  L.BadTranslation err -> "BadTranslation: " <> showT err

-- Conway CERTS (certificate validation)

conwayCertsFailure :: L.ConwayCertsPredFailure (ShelleyLedgerEra ConwayEra) -> [Text]
conwayCertsFailure = \case
  L.WithdrawalsNotInRewardsCERTS ws ->
    "WithdrawalsNotInRewardsCERTS:" : renderWithdrawals ws
  L.CertFailure f -> conwayCertFailure f

conwayCertFailure :: L.ConwayCertPredFailure (ShelleyLedgerEra ConwayEra) -> [Text]
conwayCertFailure = \case
  L.DelegFailure f -> conwayDelegFailure f
  L.PoolFailure f -> shelleyPoolFailure f
  L.GovCertFailure f -> conwayGovCertFailure f

conwayDelegFailure :: L.ConwayDelegPredFailure (ShelleyLedgerEra ConwayEra) -> [Text]
conwayDelegFailure = \case
  L.IncorrectDepositDELEG coin -> ["IncorrectDepositDELEG: " <> showCoin coin <> " lovelace"]
  L.StakeKeyRegisteredDELEG cred -> ["StakeKeyRegisteredDELEG: " <> showT cred]
  L.StakeKeyNotRegisteredDELEG cred -> ["StakeKeyNotRegisteredDELEG: " <> showT cred]
  L.StakeKeyHasNonZeroAccountBalanceDELEG coin ->
    ["StakeKeyHasNonZeroAccountBalanceDELEG: " <> showCoin coin <> " lovelace"]
  L.DelegateeDRepNotRegisteredDELEG cred -> ["DelegateeDRepNotRegisteredDELEG: " <> showT cred]
  L.DelegateeStakePoolNotRegisteredDELEG kh -> ["DelegateeStakePoolNotRegisteredDELEG: " <> renderKeyHash kh]
  L.DepositIncorrectDELEG m ->
    renderMismatch "DepositIncorrectDELEG" (\c -> showCoin c <> " lovelace") m
  L.RefundIncorrectDELEG m ->
    renderMismatch "RefundIncorrectDELEG" (\c -> showCoin c <> " lovelace") m

shelleyPoolFailure :: L.ShelleyPoolPredFailure (ShelleyLedgerEra ConwayEra) -> [Text]
shelleyPoolFailure = \case
  L.StakePoolNotRegisteredOnKeyPOOL kh -> ["StakePoolNotRegisteredOnKeyPOOL: " <> renderKeyHash kh]
  L.StakePoolRetirementWrongEpochPOOL
    (tooEarly :: L.Mismatch L.RelGT L.EpochNo)
    (tooLate :: L.Mismatch L.RelLTEQ L.EpochNo) ->
      [ "StakePoolRetirementWrongEpochPOOL: requested epoch "
          <> showEpochNo (L.mismatchSupplied tooEarly)
          <> ", must be " <> relationSymbol @L.RelGT <> " "
          <> showEpochNo (L.mismatchExpected tooEarly)
          <> " and " <> relationSymbol @L.RelLTEQ <> " "
          <> showEpochNo (L.mismatchExpected tooLate)
      ]
  L.StakePoolCostTooLowPOOL m ->
    renderMismatch "StakePoolCostTooLowPOOL" (\c -> showCoin c <> " lovelace") m
  L.WrongNetworkPOOL m kh ->
    [ "WrongNetworkPOOL: pool "
        <> renderKeyHash kh
        <> ", "
        <> renderMismatchInline showT m
    ]
  L.PoolMedataHashTooBig kh sz ->
    ["PoolMedataHashTooBig: pool " <> renderKeyHash kh <> ", size " <> showT sz]
  L.VRFKeyHashAlreadyRegistered kh vrfHash ->
    ["VRFKeyHashAlreadyRegistered: pool " <> renderKeyHash kh <> ", VRF " <> showT vrfHash]

conwayGovCertFailure :: L.ConwayGovCertPredFailure (ShelleyLedgerEra ConwayEra) -> [Text]
conwayGovCertFailure = \case
  L.ConwayDRepAlreadyRegistered cred -> ["ConwayDRepAlreadyRegistered: " <> showT cred]
  L.ConwayDRepNotRegistered cred -> ["ConwayDRepNotRegistered: " <> showT cred]
  L.ConwayDRepIncorrectDeposit m ->
    renderMismatch "ConwayDRepIncorrectDeposit" (\c -> showCoin c <> " lovelace") m
  L.ConwayCommitteeHasPreviouslyResigned cred ->
    ["ConwayCommitteeHasPreviouslyResigned: " <> showT cred]
  L.ConwayDRepIncorrectRefund m ->
    renderMismatch "ConwayDRepIncorrectRefund" (\c -> showCoin c <> " lovelace") m
  L.ConwayCommitteeIsUnknown cred -> ["ConwayCommitteeIsUnknown: " <> showT cred]

-- Conway GOV (governance action validation)

conwayGovFailure :: L.ConwayGovPredFailure (ShelleyLedgerEra ConwayEra) -> [Text]
conwayGovFailure = \case
  L.GovActionsDoNotExist ids -> ["GovActionsDoNotExist: " <> showT (Foldable.toList ids)]
  L.MalformedProposal act -> ["MalformedProposal: " <> showT act]
  L.ProposalProcedureNetworkIdMismatch addr network ->
    [ "ProposalProcedureNetworkIdMismatch: address "
        <> showT addr
        <> ", expected network "
        <> showT network
    ]
  L.TreasuryWithdrawalsNetworkIdMismatch addrs network ->
    [ "TreasuryWithdrawalsNetworkIdMismatch: expected network "
        <> showT network
        <> ", addresses "
        <> showT addrs
    ]
  L.ProposalDepositIncorrect m ->
    renderMismatch "ProposalDepositIncorrect" (\c -> showCoin c <> " lovelace") m
  L.DisallowedVoters voters ->
    ["DisallowedVoters: " <> showT (Foldable.toList voters)]
  L.ConflictingCommitteeUpdate creds ->
    ["ConflictingCommitteeUpdate: " <> showT creds]
  L.ExpirationEpochTooSmall m ->
    ["ExpirationEpochTooSmall:"]
      <> ["  " <> showT cred <> ": epoch " <> showT epoch | (cred, epoch) <- NEMap.toList m]
  L.InvalidPrevGovActionId prop -> ["InvalidPrevGovActionId: " <> showT prop]
  L.VotingOnExpiredGovAction voters ->
    ["VotingOnExpiredGovAction: " <> showT (Foldable.toList voters)]
  L.ProposalCantFollow prevId m ->
    renderMismatch "ProposalCantFollow" showT m
      <> ["  previous gov action id: " <> showT prevId]
  L.InvalidGuardrailsScriptHash got expected ->
    ["InvalidGuardrailsScriptHash: expected " <> showT expected <> ", got " <> showT got]
  L.DisallowedProposalDuringBootstrap prop -> ["DisallowedProposalDuringBootstrap: " <> showT prop]
  L.DisallowedVotesDuringBootstrap voters ->
    ["DisallowedVotesDuringBootstrap: " <> showT (Foldable.toList voters)]
  L.VotersDoNotExist voters ->
    ["VotersDoNotExist: " <> showT (Foldable.toList voters)]
  L.ZeroTreasuryWithdrawals act -> ["ZeroTreasuryWithdrawals: " <> showT act]
  L.ProposalReturnAccountDoesNotExist addr ->
    ["ProposalReturnAccountDoesNotExist: " <> showT addr]
  L.TreasuryWithdrawalReturnAccountsDoNotExist addrs ->
    ["TreasuryWithdrawalReturnAccountsDoNotExist: " <> showT (Foldable.toList addrs)]
  L.UnelectedCommitteeVoters voters ->
    ["UnelectedCommitteeVoters: " <> showT (Foldable.toList voters)]

-- Script witness rendering (used by phase-2 output)

renderScriptWitnessIndexShort :: ScriptWitnessIndex -> Text
renderScriptWitnessIndexShort = \case
  ScriptWitnessIndexTxIn n -> "Spend:" <> Text.pack (show n)
  ScriptWitnessIndexMint n -> "Mint:" <> Text.pack (show n)
  ScriptWitnessIndexCertificate n -> "Cert:" <> Text.pack (show n)
  ScriptWitnessIndexWithdrawal n -> "Reward:" <> Text.pack (show n)
  ScriptWitnessIndexVoting n -> "Vote:" <> Text.pack (show n)
  ScriptWitnessIndexProposing n -> "Propose:" <> Text.pack (show n)

renderScriptExecutionError :: ScriptExecutionError -> Text
renderScriptExecutionError = \case
  ScriptErrorEvaluationFailed dpf ->
    let evalErr = dpfEvaluationError dpf
        logs = dpfExecutionLogs dpf
     in Text.intercalate "\n" $
          [Text.pack (show evalErr)]
            <> ["Logs: " <> Text.intercalate ", " logs | not (null logs)]
  err -> Text.pack (docToString (prettyError err))
