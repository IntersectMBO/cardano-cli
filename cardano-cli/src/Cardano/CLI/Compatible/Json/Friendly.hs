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
import Cardano.Api.Experimental.Tx qualified as Exp
import Cardano.Api.Ledger (ExUnits (..), extractHash, strictMaybeToMaybe)
import Cardano.Api.Ledger qualified as Alonzo
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Ledger qualified as Ledger

import Cardano.CLI.Json.Encode qualified as Json
import Cardano.CLI.Orphan ()
import Cardano.CLI.Type.Common (FormatJson (..), FormatYaml (..))
import Cardano.CLI.Type.MonadWarning (MonadWarning, runWarningIO)
import Cardano.Crypto.Hash (hashToTextAsHex)
import Cardano.Ledger.Api.Tx qualified as L
import Cardano.Ledger.Core qualified as C
import Cardano.Ledger.Credential (credKeyHash, credScriptHash)
import Cardano.Ledger.Keys (coerceKeyRole)

import Control.Applicative ((<|>))
import Data.Aeson (Value (..), object, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types qualified as Aeson
import Data.ByteString.Char8 qualified as BSC
import Data.Char (isAscii)
import Data.Foldable (asum)
import Data.Function ((&))
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text qualified as Text
import Data.Typeable (Typeable)
import Data.Vector qualified as Vector
import Data.Yaml (array)
import GHC.Exts (IsList (..))
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
  -> Exp.SignedTx era
  -> m (Either (FileError e) ())
friendlyTx format mOutFile era tx = do
  pairs <- runWarningIO $ friendlyTxImpl era tx
  friendly format mOutFile $ object pairs

friendlyTxBody
  :: MonadIO m
  => Vary [FormatJson, FormatYaml]
  -> Maybe (File () Out)
  -> Exp.Era era
  -> Exp.UnsignedTx (Exp.LedgerEra era)
  -> m (Either (FileError e) ())
friendlyTxBody format mOutFile era unsignedTx = do
  pairs <- runWarningIO $ friendlyTxBodyImpl era unsignedTx
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
  :: forall m era
   . MonadWarning m
  => Exp.Era era
  -> Exp.SignedTx era
  -> m [Aeson.Pair]
friendlyTxImpl era (Exp.SignedTx ledgerTx) =
  Exp.obtainCommonConstraints era $
    let sbe = convert era :: ShelleyBasedEra era
        witnesses = getTxWitnesses (ShelleyTx sbe ledgerTx)
     in (("witnesses" .= map friendlyKeyWitness witnesses) :)
          <$> friendlyTxBodyImpl era (Exp.UnsignedTx ledgerTx)

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
  -> Exp.UnsignedTx (Exp.LedgerEra era)
  -> m [Aeson.Pair]
friendlyTxBodyImpl era (Exp.UnsignedTx ledgerTx) =
  pure $
    Exp.obtainCommonConstraints era $
      let body = ledgerTx ^. L.bodyTxL
          mAuxData = strictMaybeToMaybe (ledgerTx ^. L.auxDataTxL)
       in basePairs era body mAuxData
            <> [ "validity range" .= renderValidityInterval (body ^. L.vldtTxBodyL)
               , "mint" .= renderMaryMint (body ^. L.mintTxBodyL)
               , "collateral inputs" .= renderCollateralInputs body
               , renderReqSigners body
               , "reference inputs" .= renderReferenceInputs body
               , "total collateral" .= toJSON (strictMaybeToMaybe (body ^. L.totalCollateralTxBodyL))
               , "return collateral" .= friendlyReturnCollateral era (body ^. L.collateralReturnTxBodyL)
               ]
            <> alonzoScriptWitnessPairs era ledgerTx
            <> conwayBodyPairs body

renderCollateralInputs
  :: L.AlonzoEraTxBody (Exp.LedgerEra era)
  => L.TxBody C.TopTx (Exp.LedgerEra era)
  -> Aeson.Value
renderCollateralInputs body =
  toJSON (map fromShelleyTxIn (toList (body ^. L.collateralInputsTxBodyL)))

renderReqSigners
  :: L.AlonzoEraTxBody (Exp.LedgerEra era)
  => L.TxBody C.TopTx (Exp.LedgerEra era)
  -> Aeson.Pair
renderReqSigners body =
  "required signers (payment key hashes needed for scripts)"
    .= friendlyExtraKeyWits (body ^. L.reqSignerHashesTxBodyG)

renderReferenceInputs
  :: L.BabbageEraTxBody (Exp.LedgerEra era)
  => L.TxBody C.TopTx (Exp.LedgerEra era)
  -> Aeson.Value
renderReferenceInputs body =
  toJSON (map fromShelleyTxIn (toList (body ^. L.referenceInputsTxBodyL)))

-- | Pairs that are present in every Conway-onwards era.
basePairs
  :: forall era
   . Exp.Era era
  -> L.TxBody C.TopTx (Exp.LedgerEra era)
  -> Maybe (L.TxAuxData (Exp.LedgerEra era))
  -> [Aeson.Pair]
basePairs era body mAuxData =
  Exp.obtainCommonConstraints era $
    let certs = toList (body ^. L.certsTxBodyL)
     in [ "auxiliary scripts" .= friendlyAuxScripts era mAuxData
        , "certificates"
            .= if null certs
              then Null
              else array [friendlyCertificate era (Exp.Certificate cert) | cert <- certs]
        , "era" .= (convert era :: CardanoEra era)
        , "fee" .= friendlyLovelace (body ^. L.feeTxBodyL)
        , "inputs" .= toJSON (map fromShelleyTxIn (toList (body ^. L.inputsTxBodyL)))
        , "metadata" .= friendlyMetadata mAuxData
        , "outputs"
            .= map
              (friendlyTxOut era . Exp.TxOut)
              (toList (body ^. L.outputsTxBodyL))
        , "withdrawals" .= friendlyWithdrawals (body ^. L.withdrawalsTxBodyL)
        ]

renderValidityInterval :: L.ValidityInterval -> Aeson.Value
renderValidityInterval (L.ValidityInterval invalidBefore invalidHereafter) =
  object
    [ "lower bound" .= toJSON (strictMaybeToMaybe invalidBefore)
    , "upper bound" .= toJSON (strictMaybeToMaybe invalidHereafter)
    ]

renderMaryMint :: L.MultiAsset -> Aeson.Value
renderMaryMint ma
  | ma == mempty = Null
  | otherwise = friendlyValue (fromMultiAsset ma)

alonzoScriptWitnessPairs
  :: L.AlonzoEraTx (Exp.LedgerEra era)
  => Exp.Era era
  -> Ledger.Tx C.TopTx (Exp.LedgerEra era)
  -> [Aeson.Pair]
alonzoScriptWitnessPairs era tx =
  [ "redeemers" .= renderRedeemers era tx
  , "scripts" .= renderScriptData tx
  , "datums" .= renderDats tx
  ]

conwayBodyPairs
  :: forall era
   . ( L.ConwayEraTxBody (Exp.LedgerEra era)
     , Typeable era
     , Exp.IsEra era
     , L.EraTx (Exp.LedgerEra era)
     )
  => L.TxBody C.TopTx (Exp.LedgerEra era)
  -> [Aeson.Pair]
conwayBodyPairs body =
  [ "governance actions"
      .= friendlyLedgerProposals
        (Exp.useEra @era)
        (toList (body ^. L.proposalProceduresTxBodyL))
  , "voters" .= toJSON (body ^. L.votingProceduresTxBodyL)
  , "currentTreasuryValue"
      .= toJSON (strictMaybeToMaybe (body ^. L.currentTreasuryValueTxBodyL))
  , "treasuryDonation" .= toJSON (body ^. L.treasuryDonationTxBodyL)
  ]

friendlyLedgerProposals
  :: Typeable era => Exp.Era era -> [L.ProposalProcedure (Exp.LedgerEra era)] -> Aeson.Value
friendlyLedgerProposals e proposalProcedures =
  Array $ fromList $ map (obtainCommonConstraints e friendlyLedgerProposal) proposalProcedures

friendlyLedgerProposal
  :: forall era. (Typeable era, Exp.IsEra era) => L.ProposalProcedure (Exp.LedgerEra era) -> Aeson.Value
friendlyLedgerProposal proposalProcedure =
  Exp.obtainCommonConstraints (Exp.useEra @era) $
    object $
      friendlyProposalImpl (Proposal proposalProcedure)

renderRedeemers
  :: L.AlonzoEraTx (Exp.LedgerEra era)
  => Exp.Era era
  -> Ledger.Tx C.TopTx (Exp.LedgerEra era)
  -> Aeson.Value
renderRedeemers era tx =
  let plutusScriptPurposeAndExUnits = Map.toList $ Ledger.unRedeemers $ tx ^. Ledger.witsTxL . Ledger.rdmrsTxWitsL
      redeemerList = map (uncurry (renderRedeemerInfo era tx)) plutusScriptPurposeAndExUnits
   in Aeson.Array $ Vector.fromList redeemerList

renderRedeemerInfo
  :: L.AlonzoEraTx (Exp.LedgerEra era)
  => Exp.Era era
  -> Ledger.Tx C.TopTx (Exp.LedgerEra era)
  -> Ledger.PlutusPurpose Ledger.AsIx (Exp.LedgerEra era)
  -> (Ledger.Data (Exp.LedgerEra era), ExUnits)
  -> Aeson.Value
renderRedeemerInfo era tx redeemerPurpose (redeemerData, exUnits) =
  let inputNotFoundError =
        Aeson.object
          [ "error"
              .= Aeson.String (T.pack $ "Could not find corresponding input to " ++ show redeemerPurpose)
          ]
      mCorrespondingInput = strictMaybeToMaybe $ Ledger.redeemerPointerInverse (tx ^. Ledger.bodyTxL) redeemerPurpose
      mPurposeRendered = renderPurpose era <$> mCorrespondingInput
   in object
        [ "purpose" .= fromMaybe inputNotFoundError mPurposeRendered
        , "redeemer" .= renderRedeemer redeemerData exUnits
        ]

renderRedeemer :: Ledger.Data era -> ExUnits -> Aeson.Value
renderRedeemer scriptData ExUnits{exUnitsSteps = exSteps, exUnitsMem = exMemUnits} =
  object
    [ "data" .= Aeson.String (T.pack $ show $ Ledger.unData scriptData)
    , "execution units"
        .= object
          [ "steps" .= Aeson.Number (fromIntegral exSteps)
          , "memory" .= Aeson.Number (fromIntegral exMemUnits)
          ]
    ]

renderLedgerInput :: Ledger.TxIn -> Aeson.Value
renderLedgerInput (Ledger.TxIn (Ledger.TxId txidHash) ix) =
  Aeson.String $
    T.pack $
      T.unpack (hashToTextAsHex (extractHash txidHash)) ++ "#" ++ show (Ledger.txIxToInt ix)

-- | Render a Plutus purpose. Dispatches on the experimental era to pick the
-- right ledger constructor set.
renderPurpose
  :: Exp.Era era
  -> Ledger.PlutusPurpose L.AsIxItem (Exp.LedgerEra era)
  -> Aeson.Value
renderPurpose era purpose = case era of
  Exp.ConwayEra ->
    fromMaybe Aeson.Null (alonzoView purpose <|> conwayView purpose)
  Exp.DijkstraEra ->
    fromMaybe Aeson.Null (alonzoView purpose <|> conwayView purpose <|> dijkstraView purpose)
 where
  spendingLabel
    , mintingLabel
    , certifyingLabel
    , rewardingLabel
    , votingLabel
    , proposingLabel
      :: Aeson.Key
  spendingLabel = "spending script witnessed input"
  mintingLabel = "minting currency with policy id"
  certifyingLabel = "validating certificate with script credentials"
  rewardingLabel = "withdrawing reward from script address"
  votingLabel = "voting using script protected voter credentials"
  proposingLabel = "submitting a proposal following proposal policy"

  labelPurpose :: ToJSON v => Aeson.Key -> v -> Aeson.Value
  labelPurpose k v = Aeson.object [k .= v]

  unAsIxItem :: L.AsIxItem ix it -> it
  unAsIxItem (L.AsIxItem _ it) = it

  alonzoView
    :: ( L.AlonzoEraScript ledgerEra
       , ToJSON (C.TxCert ledgerEra)
       )
    => Ledger.PlutusPurpose L.AsIxItem ledgerEra -> Maybe Aeson.Value
  alonzoView p =
    asum
      [ labelPurpose spendingLabel . renderLedgerInput . unAsIxItem <$> L.toSpendingPurpose p
      , labelPurpose mintingLabel . unAsIxItem <$> L.toMintingPurpose p
      , labelPurpose certifyingLabel . unAsIxItem <$> L.toCertifyingPurpose p
      , labelPurpose rewardingLabel . unAsIxItem <$> L.toRewardingPurpose p
      ]

  conwayView
    :: ( L.ConwayEraScript ledgerEra
       , C.EraPParams ledgerEra
       )
    => Ledger.PlutusPurpose L.AsIxItem ledgerEra -> Maybe Aeson.Value
  conwayView p =
    asum
      [ labelPurpose votingLabel . unAsIxItem <$> L.toVotingPurpose p
      , labelPurpose proposingLabel . unAsIxItem <$> L.toProposingPurpose p
      ]

  dijkstraView
    :: Ledger.PlutusPurpose L.AsIxItem ledgerEra -> Maybe Aeson.Value
  dijkstraView _ = error "TODO Dijkstra"

renderScriptData
  :: ( L.AlonzoEraTxWits (Exp.LedgerEra era)
     , L.EraTx (Exp.LedgerEra era)
     )
  => Ledger.Tx C.TopTx (Exp.LedgerEra era) -> Aeson.Value
renderScriptData tx =
  Aeson.Array $
    Vector.fromList
      [ Aeson.Object $
          KeyMap.fromList
            [ "script hash" .= scriptHash
            , "script data" .= friendlyScript scriptData
            ]
      | (scriptHash, scriptData) <- Map.toList $ tx ^. Ledger.witsTxL . Ledger.scriptTxWitsL
      ]

renderDats
  :: ( L.AlonzoEraTxWits (Exp.LedgerEra era)
     , L.EraTx (Exp.LedgerEra era)
     )
  => Ledger.Tx C.TopTx (Exp.LedgerEra era) -> Aeson.Value
renderDats tx =
  let Ledger.TxDats dats = tx ^. Ledger.witsTxL . Ledger.datsTxWitsL
   in Aeson.Array $
        Vector.fromList
          [ Aeson.Object $
              KeyMap.fromList
                [ "datum hash" .= datHash
                , "datum" .= friendlyDatum dat
                ]
          | (datHash, dat) <- Map.toList dats
          ]

-- | Create a friendly JSON out of a script
friendlyScript
  :: L.AlonzoEraScript (Exp.LedgerEra era)
  => Ledger.Script (Exp.LedgerEra era) -> Aeson.Value
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
friendlyDatum :: L.Era era => Alonzo.Data era -> Aeson.Value
friendlyDatum (Alonzo.Data datum) = Aeson.String (T.pack $ show datum)

friendlyReturnCollateral
  :: forall era
   . Exp.Era era
  -> L.StrictMaybe (L.TxOut (Exp.LedgerEra era))
  -> Aeson.Value
friendlyReturnCollateral era = \case
  L.SNothing -> Aeson.Null
  L.SJust collOut ->
    Exp.obtainCommonConstraints era $
      friendlyTxOut era (Exp.TxOut collOut)

friendlyExtraKeyWits :: Set.Set (L.KeyHash L.Guard) -> Aeson.Value
friendlyExtraKeyWits keyhashes
  | Set.null keyhashes = Null
  | otherwise = toJSON [PaymentKeyHash (coerceKeyRole kh) | kh <- Set.toList keyhashes]

friendlyWithdrawals :: L.Withdrawals -> Aeson.Value
friendlyWithdrawals (L.Withdrawals ws)
  | Map.null ws = Null
  | otherwise =
      array
        [ object $
            "address" .= serialiseAddress addr
              : "amount" .= friendlyLovelace amount
              : friendlyStakeAddress addr
        | (rewardAccount, amount) <- Map.toList ws
        , let addr = fromShelleyStakeAddr rewardAccount
        ]

friendlyStakeAddress :: StakeAddress -> [Aeson.Pair]
friendlyStakeAddress (StakeAddress net cred) =
  [ "network" .= net
  , friendlyStakeCredential cred
  ]

friendlyTxOut
  :: forall era
   . Exp.Era era
  -> Exp.TxOut (Exp.LedgerEra era)
  -> Aeson.Value
friendlyTxOut era (Exp.TxOut ledgerTxOut) =
  Exp.obtainCommonConstraints era $
    babbageEraOnwardsConstraints beo $
      let addr = fromShelleyAddr sbe (ledgerTxOut ^. L.addrTxOutL)
          ledgerValue = ledgerTxOut ^. L.valueTxOutL
          refScript = case ledgerTxOut ^. L.referenceScriptTxOutL of
            L.SNothing -> ReferenceScriptNone
            L.SJust s -> fromShelleyScriptToReferenceScript sbe s
       in object $
            case addr of
              AddressInEra ByronAddressInAnyEra byronAdr ->
                [ "address era" .= String "Byron"
                , "address" .= serialiseAddress byronAdr
                , "amount" .= friendlyLedgerValue era ledgerValue
                ]
              AddressInEra (ShelleyAddressInEra _) saddr@(ShelleyAddress net cred stake) ->
                friendlyPaymentCredential (fromShelleyPaymentCredential cred)
                  : [ "address era" .= Aeson.String "Shelley"
                    , "network" .= net
                    , "address" .= serialiseAddress saddr
                    , "amount" .= friendlyLedgerValue era ledgerValue
                    , "stake reference"
                        .= friendlyStakeReference (fromShelleyStakeReference stake)
                    , "datum" .= case ledgerTxOut ^. L.datumTxOutL of
                        L.NoDatum -> Aeson.Null
                        L.DatumHash h -> toJSON h
                        L.Datum bd ->
                          scriptDataToJson ScriptDataJsonDetailedSchema $
                            fromAlonzoData (L.binaryDataToData bd)
                    , "reference script" .= refScript
                    ]
 where
  beo = convert era :: BabbageEraOnwards era
  sbe = convert era :: ShelleyBasedEra era

friendlyStakeReference :: StakeAddressReference -> Aeson.Value
friendlyStakeReference = \case
  NoStakeAddress -> Null
  StakeAddressByPointer ptr -> String (textShow ptr)
  StakeAddressByValue cred -> object [friendlyStakeCredential $ toShelleyStakeCredential cred]

friendlyCertificate :: Exp.Era era -> Exp.Certificate (Exp.LedgerEra era) -> Aeson.Value
friendlyCertificate era =
  Exp.obtainCommonConstraints era $
    object . (: []) . renderCertificate era

renderCertificate
  :: Exp.Era era -> Exp.Certificate (Exp.LedgerEra era) -> (Aeson.Key, Aeson.Value)
renderCertificate era (Exp.Certificate c) =
  case era of
    Exp.ConwayEra -> renderConwayCertificate c
    Exp.DijkstraEra -> error "renderCertificate: TODO Dijkstra era not supported"

renderDrepCredential
  :: ()
  => L.Credential L.DRepRole
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

renderConwayCertificate
  :: Ledger.ConwayTxCert (Exp.LedgerEra ConwayEra) -> (Aeson.Key, Aeson.Value)
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

friendlyLovelace :: Lovelace -> Aeson.Value
friendlyLovelace value = String $ docToText (pretty value)

friendlyLedgerValue
  :: ()
  => Exp.Era era
  -> L.Value (Exp.LedgerEra era)
  -> Aeson.Value
friendlyLedgerValue era v =
  Exp.obtainCommonConstraints era $
    friendlyValue (Api.fromLedgerValue (convert era) v)

friendlyValue :: Api.Value -> Aeson.Value
friendlyValue v =
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

friendlyMetadata
  :: forall era
   . L.EraTxAuxData era
  => Maybe (L.TxAuxData era) -> Aeson.Value
friendlyMetadata Nothing = Null
friendlyMetadata (Just auxData) =
  let m = auxData ^. L.metadataTxAuxDataL
   in if Map.null m
        then Null
        else toJSON $ friendlyMetadatum <$> m

friendlyMetadatum :: L.Metadatum -> Aeson.Value
friendlyMetadatum = \case
  L.I int -> toJSON int
  L.B bytes -> String $ textShow bytes
  L.List lst -> array $ map friendlyMetadatum lst
  L.Map m ->
    array
      [array [friendlyMetadatum k, friendlyMetadatum v] | (k, v) <- m]
  L.S text -> toJSON text

-- | Render aux-data scripts. Conway+ aux data exposes native (timelock) and
-- Plutus scripts. Matches the old API's @textShow scripts@ shape.
friendlyAuxScripts
  :: Exp.Era era
  -> Maybe (L.TxAuxData (Exp.LedgerEra era))
  -> Aeson.Value
friendlyAuxScripts _ Nothing = Null
friendlyAuxScripts era (Just aux) = case era of
  Exp.ConwayEra -> renderAux aux
  Exp.DijkstraEra -> renderAux aux
 where
  renderAux
    :: ( L.AlonzoEraTxAuxData ledgerEra
       , Show (C.NativeScript ledgerEra)
       )
    => L.TxAuxData ledgerEra -> Aeson.Value
  renderAux ad =
    let nat = ad ^. L.nativeScriptsTxAuxDataL
        plut = ad ^. L.plutusScriptsTxAuxDataL
     in if null nat && Map.null plut
          then Null
          else String (textShow (nat, plut))

friendlyDRep :: L.DRep -> Aeson.Value
friendlyDRep L.DRepAlwaysAbstain = "alwaysAbstain"
friendlyDRep L.DRepAlwaysNoConfidence = "alwaysNoConfidence"
friendlyDRep (L.DRepCredential sch@(L.ScriptHashObj (C.ScriptHash _))) =
  Aeson.object
    [ "scriptHash" .= credScriptHash sch
    , "cip129Hex" .= cip129SerialiseRaw sch
    , "cip129Bech32" .= serialiseToBech32Cip129 sch
    ]
friendlyDRep (L.DRepCredential kh@(L.KeyHashObj (C.KeyHash _))) =
  Aeson.object
    [ "keyHash" .= credKeyHash kh
    , "cip129Hex" .= cip129SerialiseRaw kh
    , "cip129Bech32" .= serialiseToBech32Cip129 kh
    ]
