{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
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
  , friendlySubTx
  , IsSigned (..)
  , friendlyProposal
  , friendlyDRep
  , friendlyCertificate

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
import Cardano.Api.Ledger (strictMaybeToMaybe)
import Cardano.Api.Ledger qualified as Alonzo
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Ledger qualified as Ledger

import Cardano.CLI.Compatible.Exception (CIO, fromEitherIOCli)
import Cardano.CLI.Json.Encode qualified as Json
import Cardano.CLI.Orphan ()
import Cardano.CLI.Type.Common (FormatJson (..), FormatYaml (..))
import Cardano.CLI.Type.MonadWarning (MonadWarning, runWarningIO)
import Cardano.Ledger.Address qualified as L
import Cardano.Ledger.Api.Tx qualified as L
import Cardano.Ledger.BaseTypes (Exclusive (..), Inclusive (..))
import Cardano.Ledger.Core qualified as C
import Cardano.Ledger.Credential (credKeyHash, credScriptHash)
import Cardano.Ledger.Keys qualified as L (coerceKeyRole)
import Cardano.Ledger.Plutus.Language qualified as Plutus (plutusFromRunnable, plutusLanguage)

import Data.Aeson (Value (..), object, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.ByteString.Char8 qualified as BSC
import Data.Char (isAscii)
import Data.Function ((&))
import Data.List (sort)
import Data.Map.Strict qualified as Map
import Data.Ratio (denominator, numerator)
import Data.Text qualified as T
import Data.Text qualified as Text
import Data.Typeable (Typeable)
import Data.Yaml (array)
import GHC.Exts (IsList (..))
import GHC.Unicode (isAlphaNum)
import Lens.Micro ((^.))
import Vary (Vary)
import Vary qualified

-- | Write a value in a user-friendly format. Throws if the output cannot be
-- written.
friendly
  :: Aeson.ToJSON a
  => Vary [FormatJson, FormatYaml]
  -> Maybe (File () Out)
  -> a
  -> CIO e ()
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

  fromEitherIOCli @(FileError ()) $ writeLazyByteStringOutput mOutFile output

friendlyTx
  :: Vary [FormatJson, FormatYaml]
  -> Maybe (File () Out)
  -> Exp.Era era
  -> Exp.SignedTx era
  -> CIO e ()
friendlyTx format mOutFile era tx = do
  pairs <- runWarningIO $ friendlyTxImpl era tx
  friendly format mOutFile $ object pairs

friendlyTxBody
  :: Vary [FormatJson, FormatYaml]
  -> Maybe (File () Out)
  -> Exp.Era era
  -> Exp.UnsignedTx (Exp.LedgerEra era)
  -> CIO e ()
friendlyTxBody format mOutFile era unsignedTx = do
  pairs <- runWarningIO $ friendlyTxBodyImpl era unsignedTx
  friendly format mOutFile $ object pairs

-- | Write a sub-transaction in a user-friendly format.
friendlySubTx
  :: Vary [FormatJson, FormatYaml]
  -- ^ The output format, JSON or YAML
  -> Maybe (File () Out)
  -- ^ The output file, or 'Nothing' for stdout
  -> IsSigned
  -- ^ Whether to show the witnesses: If 'Signed', we show the witnesses or null if it does not have any; if 'Unsigned', we ignore the witnesses
  -> L.Tx C.SubTx (Exp.LedgerEra Exp.DijkstraEra)
  -- ^ The sub-transaction to show
  -> CIO e ()
friendlySubTx format mOutFile signedness subTx =
  friendly format mOutFile . renderSubTransaction signedness $
    case signedness of
      Signed -> Exp.viewSignedSubTx (Exp.SignedSubTx subTx)
      Unsigned -> Exp.viewUnsignedSubTx (Exp.UnsignedSubTx subTx)

friendlyProposal
  :: Exp.IsEra era
  => Vary [FormatJson, FormatYaml]
  -> Maybe (File () Out)
  -> Proposal era
  -> CIO e ()
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
friendlyTxImpl era signedTx =
  let view = Exp.viewSignedTx era signedTx
   in pure $ keyWitnessPairs Signed (Exp.tvWitnesses view) <> topTxPairs era view

-- | Whether a transaction must be treated as signed or unsigned.
data IsSigned = Signed | Unsigned

keyWitnessPairs :: IsSigned -> Exp.TxWitnessesView era -> [Aeson.Pair]
keyWitnessPairs Unsigned _ = []
keyWitnessPairs Signed wits =
  [ "witnesses"
      .= ( [object ["bootstrap witness" .= textShow witness] | witness <- Exp.twvBootstrapWitnesses wits]
             <> [ object ["key" .= textShow key, "signature" .= textShow signature]
                | L.WitVKey key signature <- Exp.twvKeyWitnesses wits
                ]
         )
  ]

friendlyTxBodyImpl
  :: forall m era
   . MonadWarning m
  => Exp.Era era
  -> Exp.UnsignedTx (Exp.LedgerEra era)
  -> m [Aeson.Pair]
friendlyTxBodyImpl era unsignedTx =
  pure $ topTxPairs era (Exp.viewUnsignedTx era unsignedTx)

-- | Pairs for a top-level transaction, without its key witnesses.
topTxPairs :: Exp.Era era -> Exp.TxView C.TopTx (Exp.LedgerEra era) -> [Aeson.Pair]
topTxPairs era view =
  let top = Exp.bvTopTxOnlyFields (Exp.tvBody view)
   in [ "fee" .= friendlyLovelace (Exp.ttvFee top)
      , "collateral inputs" .= Exp.ttvInsCollateral top
      , "total collateral" .= fmap Exp.unTxTotalCollateral (Exp.ttvTotalCollateral top)
      , "return collateral" .= maybe Null (friendlyTxOut era) (Exp.ttvReturnCollateral top)
      ]
        <> sharedTxPairs era view
        <> dijkstraTopLevelPairs era top

-- | Pairs for both top-level transactions and sub-transactions.
sharedTxPairs
  :: Exp.Era era
  -> Exp.TxView l (Exp.LedgerEra era)
  -> [Aeson.Pair]
sharedTxPairs era view =
  Exp.obtainCommonConstraints era $
    let body = Exp.tvBody view
     in basePairs era body
          <> [ "validity range"
                 .= renderValidityInterval (Exp.bvValidityLowerBound body) (Exp.bvValidityUpperBound body)
             , "mint" .= renderMint (Exp.bvMintValue body)
             , "required signers (payment key hashes needed for scripts)"
                 .= friendlyExtraKeyWits (requiredSigners era body)
             , "reference inputs" .= Exp.bvInsReference body
             ]
          <> alonzoScriptWitnessPairs era (Exp.tvWitnesses view)
          <> conwayBodyPairs body
          <> dijkstraBodyPairs era body

dijkstraBodyPairs
  :: Exp.Era era
  -> Exp.BodyView l (Exp.LedgerEra era)
  -> [Aeson.Pair]
dijkstraBodyPairs era body = case era of
  Exp.ConwayEra -> []
  Exp.DijkstraEra ->
    [ "direct deposits" .= friendlyAccountAmounts (L.unDirectDeposits $ Exp.bvDirectDeposits body)
    , "account balance intervals"
        .= friendlyAccountBalanceIntervals (Exp.bvAccountBalanceIntervals body)
    , "guards" .= case Exp.bvGuards body of
        [] -> Null
        guards -> toJSON guards
    , "required top-level guards"
        .= friendlyRequiredTopLevelGuards (Exp.bvRequiredTopLevelGuards body)
    ]

friendlyRequiredTopLevelGuards
  :: L.Era era => Map.Map (L.Credential L.Guard) (L.StrictMaybe (L.Data era)) -> Aeson.Value
friendlyRequiredTopLevelGuards guards
  | Map.null guards = Null
  | otherwise =
      array
        [ object ["credential" .= credential, "datum" .= fmap friendlyDatum (strictMaybeToMaybe datum)]
        | (credential, datum) <- Map.toList guards
        ]

-- | Only a top-level transaction has starting account balance intervals and sub-transactions.
dijkstraTopLevelPairs
  :: Exp.Era era
  -> Exp.TopTxOnlyFieldsView C.TopTx (Exp.LedgerEra era)
  -> [Aeson.Pair]
dijkstraTopLevelPairs era top = case era of
  Exp.ConwayEra -> []
  Exp.DijkstraEra ->
    [ "starting account balance intervals"
        .= friendlyAccountBalanceIntervals (Exp.ttvStartingAccountBalanceIntervals top)
    , "sub-transactions" .= case Exp.ttvSubTransactions top of
        [] -> Null
        subTransactions -> array $ map (renderSubTransaction Signed) subTransactions
    ]

renderSubTransaction
  :: IsSigned
  -> Exp.TxView C.SubTx (Exp.LedgerEra Exp.DijkstraEra)
  -> Aeson.Value
renderSubTransaction signedness view =
  object $
    [ "id" .= serialiseToRawBytesHexText (Exp.tvId view)
    ]
      <> keyWitnessPairs signedness (Exp.tvWitnesses view)
      <> sharedTxPairs Exp.DijkstraEra view

friendlyAccountBalanceIntervals :: L.AccountBalanceIntervals era -> Aeson.Value
friendlyAccountBalanceIntervals (L.AccountBalanceIntervals intervals)
  | Map.null intervals = Null
  | otherwise =
      array
        [ object $
            [ "address" .= serialiseAddress addr
            , "interval" .= friendlyAccountBalanceInterval interval
            ]
              <> friendlyStakeAddress addr
        | (account, interval) <- Map.toList intervals
        , let addr = fromShelleyStakeAddr account
        ]

friendlyAccountBalanceInterval :: L.AccountBalanceInterval era -> Aeson.Value
friendlyAccountBalanceInterval = \case
  L.AccountBalanceLowerBound (Inclusive lower) ->
    object
      [ "kind" .= String "lowerBound"
      , "lower" .= friendlyLovelace lower
      ]
  L.AccountBalanceUpperBound (Exclusive upper) ->
    object
      [ "kind" .= String "upperBound"
      , "upper" .= friendlyLovelace upper
      ]
  L.AccountBalanceBothBounds (Inclusive lower) (Exclusive upper) ->
    object
      [ "kind" .= String "bothBounds"
      , "lower" .= friendlyLovelace lower
      , "upper" .= friendlyLovelace upper
      ]
  L.AccountBalanceExact coin ->
    object
      [ "kind" .= String "exact"
      , "coin" .= friendlyLovelace coin
      ]

-- | Pairs that are present in every Conway-onwards era.
basePairs
  :: forall era l
   . Exp.Era era
  -> Exp.BodyView l (Exp.LedgerEra era)
  -> [Aeson.Pair]
basePairs era body =
  [ "auxiliary scripts" .= friendlyAuxScripts era body
  , "certificates" .= case Exp.bvCertificates body of
      [] -> Null
      certs -> array [friendlyCertificateImpl era cert | cert <- certs]
  , "era" .= (convert era :: CardanoEra era)
  , "inputs" .= Exp.bvIns body
  , "metadata" .= friendlyMetadata (Exp.bvMetadata body)
  , "outputs" .= map (friendlyTxOut era) (Exp.bvOuts body)
  , "withdrawals" .= friendlyWithdrawals (Exp.bvWithdrawals body)
  ]

renderValidityInterval :: Maybe L.SlotNo -> Maybe L.SlotNo -> Aeson.Value
renderValidityInterval lowerBound upperBound =
  object
    [ "lower bound" .= lowerBound
    , "upper bound" .= upperBound
    ]

renderMint :: Map.Map PolicyId PolicyAssets -> Aeson.Value
renderMint mint
  | Map.null mint = Null
  | otherwise =
      friendlyValue
        (mconcat [policyAssetsToValue policyId assets | (policyId, assets) <- Map.toList mint])

alonzoScriptWitnessPairs
  :: Exp.Era era
  -> Exp.TxWitnessesView (Exp.LedgerEra era)
  -> [Aeson.Pair]
alonzoScriptWitnessPairs era wits =
  Exp.obtainCommonConstraints
    era
    [ "redeemers" .= map (renderRedeemer era) (Exp.twvRedeemers wits)
    , "scripts"
        .= [ object ["script hash" .= scriptHash, "script data" .= friendlyScript era script]
           | (scriptHash, script) <- Map.toList (Exp.twvScripts wits)
           ]
    , "datums"
        .= [ object ["datum hash" .= datumHash, "datum" .= friendlyDatum datum]
           | (datumHash, datum) <- Map.toList (Exp.twvDatums wits)
           ]
    ]

conwayBodyPairs
  :: forall era l
   . ( Typeable era
     , Exp.IsEra era
     , L.EraTx (Exp.LedgerEra era)
     )
  => Exp.BodyView l (Exp.LedgerEra era)
  -> [Aeson.Pair]
conwayBodyPairs body =
  [ "governance actions"
      .= friendlyLedgerProposals (Exp.useEra @era) (Exp.bvProposalProcedures body)
  , "voters" .= toJSON (Exp.bvVotingProcedures body)
  , "currentTreasuryValue" .= toJSON (Exp.bvCurrentTreasuryValue body)
  , "treasuryDonation" .= toJSON (Exp.bvTreasuryDonation body)
  ]

friendlyLedgerProposals
  :: Typeable era => Exp.Era era -> [L.ProposalProcedure (Exp.LedgerEra era)] -> Aeson.Value
friendlyLedgerProposals e proposalProcedures =
  Array $ fromList $ map (obtainCommonConstraints e friendlyLedgerProposal) proposalProcedures

friendlyLedgerProposal
  :: forall era. (Typeable era, Exp.IsEra era) => L.ProposalProcedure (Exp.LedgerEra era) -> Aeson.Value
friendlyLedgerProposal proposalProcedure =
  Exp.obtainCommonConstraints (Exp.useEra @era)
    $ object
    $ friendlyProposalImpl (Proposal proposalProcedure)

renderRedeemer :: Exp.Era era -> Exp.TxRedeemerView (Exp.LedgerEra era) -> Aeson.Value
renderRedeemer era redeemer =
  object
    [ "purpose" .= renderPurpose era redeemer
    , "redeemer" .= renderRedeemerData (Exp.trvData redeemer) (Exp.trvExUnits redeemer)
    ]

renderRedeemerData :: HashableScriptData -> ExecutionUnits -> Aeson.Value
renderRedeemerData redeemerData ExecutionUnits{executionSteps, executionMemory} =
  object
    [ "data" .= Aeson.String (T.pack $ show $ toPlutusData $ getScriptData redeemerData)
    , "execution units"
        .= object
          [ "steps" .= Aeson.Number (fromIntegral executionSteps)
          , "memory" .= Aeson.Number (fromIntegral executionMemory)
          ]
    ]

-- | Render what a redeemer is for: the item it witnesses, or an error when the
-- body has no item at the redeemer's index.
renderPurpose :: Exp.Era era -> Exp.TxRedeemerView (Exp.LedgerEra era) -> Aeson.Value
renderPurpose era redeemer =
  maybe missingItem renderItem $ Exp.trvItem redeemer
 where
  renderItem (Exp.AnyWitnessable item) = Exp.obtainCommonConstraints era $ renderWitnessable item

  missingItem =
    Aeson.object
      [ "error"
          .= Aeson.String
            ( "Could not find the "
                <> redeemerItemName (Exp.trvPurpose redeemer)
                <> " at index "
                <> textShow (Exp.trvIndex redeemer)
            )
      ]

  renderWitnessable
    :: (ToJSON (C.TxCert ledgerEra), ToJSON (L.ProposalProcedure ledgerEra))
    => Exp.Witnessable thing ledgerEra -> Aeson.Value
  renderWitnessable = \case
    Exp.WitTxIn txIn -> labelPurpose "spending script witnessed input" txIn
    Exp.WitMint policyId _ -> labelPurpose "minting currency with policy id" policyId
    Exp.WitTxCert cert -> labelPurpose "validating certificate with script credentials" cert
    Exp.WitWithdrawal stakeAddress _ ->
      labelPurpose "withdrawing reward from script address" (toShelleyStakeAddr stakeAddress)
    Exp.WitVote voter -> labelPurpose "voting using script protected voter credentials" voter
    Exp.WitProposal proposal -> labelPurpose "submitting a proposal following proposal policy" proposal
    Exp.WitGuard credential -> labelPurpose "guarding script" (guardHash credential)

  labelPurpose :: ToJSON v => Aeson.Key -> v -> Aeson.Value
  labelPurpose k v = Aeson.object [k .= v]

  -- The kind of body item a redeemer of each purpose points at.
  redeemerItemName :: Exp.PlutusScriptPurpose -> T.Text
  redeemerItemName = \case
    Exp.SpendingScript -> "input"
    Exp.MintingScript -> "minting policy"
    Exp.CertifyingScript -> "certificate"
    Exp.WithdrawingScript -> "withdrawal"
    Exp.VotingScript -> "voter"
    Exp.ProposingScript -> "proposal"
    Exp.GuardingScript -> "guard"

  -- A guard with a redeemer is a script guard: print its script hash.
  guardHash :: L.Credential L.Guard -> Aeson.Value
  guardHash = \case
    L.ScriptHashObj scriptHash -> toJSON scriptHash
    L.KeyHashObj keyHash -> toJSON keyHash

-- | Create a friendly JSON out of a script
friendlyScript
  :: Exp.Era era
  -> Exp.TxScriptView (Exp.LedgerEra era)
  -> Aeson.Value
friendlyScript era = \case
  Exp.UndecodedScript undecoded -> friendlyUndecodedPlutusScript undecoded
  Exp.DecodedScript (Exp.AnySimpleScript (Exp.SimpleScript nativeScript)) -> friendlyNativeScript era nativeScript
  Exp.DecodedScript (Exp.AnyPlutusScript (Exp.PlutusScriptInEra runnable)) ->
    let plutus = Plutus.plutusFromRunnable runnable
     in friendlyPlutusScript (Plutus.plutusLanguage plutus) (L.plutusBinary plutus)

-- | A native script, in the JSON format of script files.
friendlyNativeScript :: Exp.Era era -> L.NativeScript (Exp.LedgerEra era) -> Aeson.Value
friendlyNativeScript era nativeScript =
  object
    [ "type" .= Aeson.String "native"
    , "script" .= simpleScript
    ]
 where
  simpleScript :: SimpleScript
  simpleScript = case era of
    Exp.ConwayEra -> fromAllegraTimelock nativeScript
    Exp.DijkstraEra -> fromDijkstraNativeScript nativeScript

friendlyPlutusScript :: L.Language -> L.PlutusBinary -> Aeson.Value
friendlyPlutusScript language binary =
  object
    [ "type" .= Aeson.String "plutus"
    , "plutus version" .= L.languageToText language
    , "script" .= L.serializeAsHexText binary
    ]

-- | Create a friendly JSON out of a datum
friendlyDatum :: L.Era era => Alonzo.Data era -> Aeson.Value
friendlyDatum (Alonzo.Data datum) = Aeson.String (T.pack $ show datum)

-- | Keys that must sign the transaction: the required signers in Conway, the
-- key guards from Dijkstra on, sorted like the ledger's set of required signer
-- hashes.
requiredSigners :: Exp.Era era -> Exp.BodyView l (Exp.LedgerEra era) -> [Hash PaymentKey]
requiredSigners era body = case era of
  Exp.ConwayEra -> case Exp.bvTopTxOnlyFields body of
    Exp.TopTxView{Exp.ttvExtraKeyWits = Exp.TxExtraKeyWitnesses keys} -> keys
    Exp.NoTopTxFields -> []
  Exp.DijkstraEra ->
    [ PaymentKeyHash (L.coerceKeyRole keyHash)
    | keyHash <- sort [kh | L.KeyHashObj kh <- Exp.bvGuards body]
    ]

friendlyExtraKeyWits :: [Hash PaymentKey] -> Aeson.Value
friendlyExtraKeyWits [] = Null
friendlyExtraKeyWits keyHashes = toJSON keyHashes

friendlyWithdrawals :: [(StakeAddress, Lovelace)] -> Aeson.Value
friendlyWithdrawals [] = Null
friendlyWithdrawals withdrawals =
  array [friendlyAccountAmount addr amount | (addr, amount) <- withdrawals]

friendlyAccountAmounts :: Map.Map L.AccountAddress Lovelace -> Aeson.Value
friendlyAccountAmounts ws
  | Map.null ws = Null
  | otherwise =
      array
        [ friendlyAccountAmount (fromShelleyStakeAddr rewardAccount) amount
        | (rewardAccount, amount) <- Map.toList ws
        ]

friendlyAccountAmount :: StakeAddress -> Lovelace -> Aeson.Value
friendlyAccountAmount addr amount =
  object $
    "address" .= serialiseAddress addr
      : "amount" .= friendlyLovelace amount
      : friendlyStakeAddress addr

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
  Exp.obtainCommonConstraints era
    $ babbageEraOnwardsConstraints beo
    $ let addr = fromShelleyAddr sbe (ledgerTxOut ^. L.addrTxOutL)
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

friendlyCertificate
  :: Vary [FormatJson, FormatYaml]
  -> Maybe (File () Out)
  -> Exp.Era era
  -> Exp.Certificate (Exp.LedgerEra era)
  -> CIO e ()
friendlyCertificate format mOutFile era cert = do
  let certValue = friendlyCertificateImpl era cert
  friendly format mOutFile certValue

friendlyCertificateImpl :: Exp.Era era -> Exp.Certificate (Exp.LedgerEra era) -> Aeson.Value
friendlyCertificateImpl era =
  Exp.obtainCommonConstraints era $
    object . (: []) . renderCertificate era

renderCertificate
  :: Exp.Era era -> Exp.Certificate (Exp.LedgerEra era) -> (Aeson.Key, Aeson.Value)
renderCertificate era (Exp.Certificate c) =
  case era of
    Exp.ConwayEra -> renderConwayCertificate c
    Exp.DijkstraEra -> renderDijkstraCertificate c

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

friendlyPoolParams :: L.StakePoolParams era -> Aeson.Value
friendlyPoolParams params =
  object $
    [ "pool id" .= serialiseToBech32 (StakePoolKeyHash (L.sppId params))
    , "vrf key hash" .= L.sppVrf params
    , "pledge" .= L.sppPledge params
    , "cost" .= L.sppCost params
    , "margin" .= (textShow (numerator margin) <> "/" <> textShow (denominator margin))
    , "reward account" .= serialiseAddress (fromShelleyStakeAddr (L.sppAccountAddress params))
    , "owners" .= L.sppOwners params
    , "relays" .= L.sppRelays params
    , "metadata" .= L.sppMetadata params
    ]
      <> ["bls key" .= blsKey | L.SJust blsKey <- [L.sppBlsKey params]]
 where
  margin = L.unboundRational (L.sppMargin params)

renderConwayCertificate
  :: Ledger.ConwayTxCert (Exp.LedgerEra ConwayEra) -> (Aeson.Key, Aeson.Value)
renderConwayCertificate cert =
  case cert of
    L.RegDRepTxCert credential coin mAnchor ->
      "DRep registration certificate"
        .= object
          [ "deposit" .= coin
          , "DRep credential" .= renderDrepCredential credential
          , "anchor" .= mAnchor
          ]
    L.UnRegDRepTxCert credential coin ->
      "DRep unregistration certificate"
        .= object
          [ "refund" .= coin
          , "DRep credential" .= renderDrepCredential credential
          ]
    L.AuthCommitteeHotKeyTxCert coldCred hotCred
      | L.ScriptHashObj sh <- coldCred ->
          "Cold committee authorization"
            .= object
              ["script hash" .= sh, "hot credential" .= hotCred]
      | L.ScriptHashObj sh <- hotCred ->
          "Hot committee authorization"
            .= object
              ["script hash" .= sh, "cold credential" .= coldCred]
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
            , "anchor" .= anchor
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
          [ "pool params" .= friendlyPoolParams poolParams
          ]
    L.RetirePoolTxCert kh@L.KeyHash{} epoch ->
      "Pool retirement"
        .= object
          [ "stake pool key hash" .= kh
          , "epoch" .= epoch
          ]
    L.UpdateDRepTxCert drepCredential mbAnchor ->
      "DRep certificate update"
        .= object
          [ "DRep credential" .= drepCredential
          , "anchor" .= mbAnchor
          ]
    _ -> "unsupported certificate" .= String (T.pack $ show cert)

renderDijkstraCertificate
  :: Ledger.DijkstraTxCert (Exp.LedgerEra DijkstraEra) -> (Aeson.Key, Aeson.Value)
renderDijkstraCertificate cert =
  case cert of
    L.RegDRepTxCert credential coin mAnchor ->
      "DRep registration certificate"
        .= object
          [ "deposit" .= coin
          , "DRep credential" .= renderDrepCredential credential
          , "anchor" .= mAnchor
          ]
    L.UnRegDRepTxCert credential coin ->
      "DRep unregistration certificate"
        .= object
          [ "refund" .= coin
          , "DRep credential" .= renderDrepCredential credential
          ]
    L.AuthCommitteeHotKeyTxCert coldCred hotCred
      | L.ScriptHashObj sh <- coldCred ->
          "Cold committee authorization"
            .= object
              ["script hash" .= sh, "hot credential" .= hotCred]
      | L.ScriptHashObj sh <- hotCred ->
          "Hot committee authorization"
            .= object
              ["script hash" .= sh, "cold credential" .= coldCred]
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
            , "anchor" .= anchor
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
          [ "pool params" .= friendlyPoolParams poolParams
          ]
    L.RetirePoolTxCert kh@L.KeyHash{} epoch ->
      "Pool retirement"
        .= object
          [ "stake pool key hash" .= kh
          , "epoch" .= epoch
          ]
    L.UpdateDRepTxCert drepCredential mbAnchor ->
      "DRep certificate update"
        .= object
          [ "DRep credential" .= drepCredential
          , "anchor" .= mbAnchor
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

friendlyMetadata :: TxMetadata -> Aeson.Value
friendlyMetadata (TxMetadata metadata)
  | Map.null metadata = Null
  | otherwise = toJSON $ friendlyMetadatum <$> toShelleyMetadata metadata

friendlyMetadatum :: L.Metadatum -> Aeson.Value
friendlyMetadatum = \case
  L.I int -> toJSON int
  L.B bytes -> String $ textShow bytes
  L.List lst -> array $ map friendlyMetadatum lst
  L.Map m ->
    array
      [array [friendlyMetadatum k, friendlyMetadatum v] | (k, v) <- m]
  L.S text -> toJSON text

-- | Render aux-data scripts: one entry for each script, like the script data
-- of a witness script. Native scripts come first, then Plutus scripts by
-- language.
friendlyAuxScripts :: Exp.Era era -> Exp.BodyView l (Exp.LedgerEra era) -> Aeson.Value
friendlyAuxScripts era body =
  Exp.obtainCommonConstraints era $
    case [friendlyNativeScript era nativeScript | Exp.SimpleScript nativeScript <- Exp.bvAuxScripts body]
      <> map friendlyUndecodedPlutusScript (Exp.bvAuxPlutusScripts body) of
      [] -> Null
      scripts -> array scripts

friendlyUndecodedPlutusScript :: Exp.UndecodedPlutusScript -> Aeson.Value
friendlyUndecodedPlutusScript (Exp.UndecodedPlutusScript version (PlutusScriptSerialised bytes)) =
  friendlyPlutusScript (toAlonzoLanguage (AnyPlutusScriptVersion version)) (L.PlutusBinary bytes)

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
