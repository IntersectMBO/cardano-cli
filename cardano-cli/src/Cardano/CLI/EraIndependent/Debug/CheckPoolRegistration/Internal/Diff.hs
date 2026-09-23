{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | The pure core of @cardano-cli debug check-pool-registration@: comparing the
-- stake pool parameters carried by a registration certificate against the ones
-- the pool already has on chain, and rendering the result.
--
-- Nothing in this module talks to a node, so the whole comparison is testable
-- without one.
module Cardano.CLI.EraIndependent.Debug.CheckPoolRegistration.Internal.Diff
  ( -- * The report
    PoolRegistrationReport (..)
  , ReportSummary (..)
  , ParameterDiff (..)
  , Baseline (..)
  , FieldComparison (..)
  , Notice (..)
  , NoticeLevel (..)

    -- * Building a report
  , mkPoolRegistrationReport

    -- * Rendering
  , renderPoolRegistrationReport

    -- * Field coverage
  , comparedFieldNames
  , stakePoolParamsFieldNames
  )
where

import Cardano.Api
import Cardano.Api.Ledger qualified as L

import Cardano.Ledger.Shelley qualified as L (ShelleyEra)
import Cardano.Ledger.State qualified as L

import Data.Aeson (object, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy qualified as LBS
import Data.Kind (Type)
import Data.List qualified as List
import Data.Maybe (fromMaybe, mapMaybe, maybeToList)
import Data.Scientific qualified as Scientific
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import GHC.Generics

-- | The baseline a set of certificate parameters is compared against.
data Baseline
  = -- | The parameters the pool is running with right now.
    CurrentParameters
  | -- | An update already staged in @futureStakePoolParams@, which submitting
    -- the certificate would supersede.
    StagedUpdate
  | -- | The pool is not registered, so there is nothing to compare against and
    -- every parameter is new.
    NoRegistration
  deriving (Eq, Show)

baselineName :: Baseline -> Text
baselineName = \case
  CurrentParameters -> "currentParameters"
  StagedUpdate -> "stagedUpdate"
  NoRegistration -> "noRegistration"

instance ToJSON Baseline where
  toJSON = toJSON . baselineName

-- | One field of the pool parameters, as it stands on chain and as the
-- certificate would leave it.
data FieldComparison = FieldComparison
  { fcField :: !Text
  -- ^ The name the CLI prints for the field, which is the key the ledger's own
  -- JSON uses, so the output lines up with @query pool-state@.
  , fcChanged :: !Bool
  , fcBaseline :: !(Maybe Aeson.Value)
  -- ^ 'Nothing' when there is no baseline, i.e. a first registration.
  , fcCertificate :: !Aeson.Value
  , fcBaselineText :: !(Maybe Text)
  -- ^ Compact rendering of 'fcBaseline', for the human readable output.
  , fcCertificateText :: !Text
  -- ^ Compact rendering of 'fcCertificate'.
  }
  deriving (Eq, Show)

instance ToJSON FieldComparison where
  toJSON FieldComparison{fcField, fcChanged, fcBaseline, fcCertificate} =
    object
      [ "field" .= fcField
      , "changed" .= fcChanged
      , "baseline" .= fcBaseline
      , "certificate" .= fcCertificate
      ]

-- | A field by field comparison against one baseline.
data ParameterDiff = ParameterDiff
  { pdBaseline :: !Baseline
  , pdFields :: ![FieldComparison]
  }
  deriving (Eq, Show)

instance ToJSON ParameterDiff where
  toJSON ParameterDiff{pdBaseline, pdFields} =
    object
      [ "baseline" .= pdBaseline
      , "changedFields" .= changedFieldsOf pdFields
      , "fields" .= pdFields
      ]

-- | How much the operator has to look at before signing.
data NoticeLevel
  = -- | A fact confirmed, nothing to look at. Rendered @[OK]@.
    Confirmed
  | -- | A fact that needs the operator's eyes before signing. The command
    -- cannot tell whether it was intended. Rendered @[WARNING]@.
    Attention
  deriving (Eq, Show)

instance ToJSON NoticeLevel where
  toJSON = \case
    Confirmed -> "confirmed"
    Attention -> "attention"

data Notice = Notice
  { nLevel :: !NoticeLevel
  , nMessage :: !Text
  }
  deriving (Eq, Show)

instance ToJSON Notice where
  toJSON Notice{nLevel, nMessage} =
    object ["level" .= nLevel, "message" .= nMessage]

-- | The machine readable answers to the questions the command exists to ask.
data ReportSummary = ReportSummary
  { rsFirstRegistration :: !Bool
  , rsChangedFields :: ![Text]
  -- ^ Fields that differ from the pool's current parameters. Every field for a
  -- first registration.
  , rsOnlyBlsKeyChanges :: !Bool
  -- ^ The expected rotation case: the certificate changes the BLS voting key
  -- and nothing else.
  , rsNoChanges :: !Bool
  , rsBlsKeyAlreadyRegistered :: !Bool
  -- ^ The certificate's BLS voting key is the one already on chain, so no
  -- rotation takes place.
  , rsBlsKeyRemoved :: !Bool
  -- ^ The certificate drops a BLS voting key the pool has registered. That is
  -- a change in the same field as a rotation, but the opposite of one.
  , rsSupersedesStagedUpdate :: !Bool
  }
  deriving (Eq, Show)

instance ToJSON ReportSummary where
  toJSON rs =
    object
      [ "firstRegistration" .= rsFirstRegistration rs
      , "changedFields" .= rsChangedFields rs
      , "changedFieldCount" .= length (rsChangedFields rs)
      , "onlyBlsKeyChanges" .= rsOnlyBlsKeyChanges rs
      , "noChanges" .= rsNoChanges rs
      , "blsKeyAlreadyRegistered" .= rsBlsKeyAlreadyRegistered rs
      , "blsKeyRemoved" .= rsBlsKeyRemoved rs
      , "supersedesStagedUpdate" .= rsSupersedesStagedUpdate rs
      ]

-- | Everything the command has to say about one certificate.
data PoolRegistrationReport = PoolRegistrationReport
  { prPoolId :: !Text
  , prRegisteredOnChain :: !Bool
  , prSummary :: !ReportSummary
  , prNotices :: ![Notice]
  , prDiffs :: ![ParameterDiff]
  }
  deriving (Eq, Show)

instance ToJSON PoolRegistrationReport where
  toJSON pr =
    object
      [ "poolId" .= prPoolId pr
      , "registeredOnChain" .= prRegisteredOnChain pr
      , "summary" .= prSummary pr
      , "notices" .= prNotices pr
      , "diffs" .= prDiffs pr
      ]

--------------------------------------------------------------------------------
-- The fields being compared
--------------------------------------------------------------------------------

-- | One field of 'L.StakePoolParams': its record selector, the name the CLI
-- prints for it, and how to read, compare and render it.
data PoolParamField era = PoolParamField
  { ppfSelector :: !Text
  -- ^ The ledger record selector, checked against the type's 'Generic'
  -- representation so that a field added in a future era cannot escape the
  -- comparison. See 'stakePoolParamsFieldNames'.
  , ppfName :: !Text
  , ppfJson :: L.StakePoolParams era -> Aeson.Value
  , ppfText :: L.StakePoolParams era -> Text
  , ppfEq :: L.StakePoolParams era -> L.StakePoolParams era -> Bool
  }

mkField
  :: (ToJSON a, Eq a)
  => Text
  -> Text
  -> (L.StakePoolParams era -> a)
  -> PoolParamField era
mkField selector name get =
  mkFieldWith selector name get (compactValue . toJSON)

mkFieldWith
  :: (ToJSON a, Eq a)
  => Text
  -> Text
  -> (L.StakePoolParams era -> a)
  -> (a -> Text)
  -> PoolParamField era
mkFieldWith selector name get render =
  PoolParamField
    { ppfSelector = selector
    , ppfName = name
    , ppfJson = toJSON . get
    , ppfText = render . get
    , ppfEq = \a b -> get a == get b
    }

-- | Every field of 'L.StakePoolParams', in the order the ledger declares them.
-- The names match the keys of the ledger's own JSON, so the output lines up
-- with @query pool-state@.
poolParamFields :: forall era. [PoolParamField era]
poolParamFields =
  [ mkField "sppId" "poolId" L.sppId
  , mkField "sppVrf" "vrf" L.sppVrf
  , mkFieldWith "sppBlsKey" "blsKey" L.sppBlsKey renderBlsKey
  , mkField "sppPledge" "pledge" L.sppPledge
  , mkField "sppCost" "cost" L.sppCost
  , mkField "sppMargin" "margin" L.sppMargin
  , mkFieldWith "sppAccountAddress" "accountAddress" L.sppAccountAddress renderAccountAddress
  , mkField "sppOwners" "owners" L.sppOwners
  , mkField "sppRelays" "relays" L.sppRelays
  , mkFieldWith "sppMetadata" "metadata" L.sppMetadata renderMetadata
  ]

-- | The record selectors this module compares, in order.
comparedFieldNames :: [Text]
comparedFieldNames = map ppfSelector (poolParamFields @L.ShelleyEra)

-- | The record selectors of 'L.StakePoolParams', read off the type's 'Generic'
-- representation. A field added to the ledger type shows up here without any
-- edit to this module, which is what lets the coverage test notice it.
--
-- 'L.StakePoolParams' is phantom in its era parameter, so the choice of era
-- here does not affect the answer.
stakePoolParamsFieldNames :: [Text]
stakePoolParamsFieldNames = genericSelectorNames (Proxy @(L.StakePoolParams L.ShelleyEra))

--------------------------------------------------------------------------------
-- Building the report
--------------------------------------------------------------------------------

-- | Compare the parameters a registration certificate carries against the
-- pool's live parameters.
mkPoolRegistrationReport
  :: L.StakePoolParams era
  -- ^ The parameters carried by the certificate.
  -> Maybe (L.StakePoolParams era)
  -- ^ The pool's current on chain parameters, absent when it is not registered.
  -> Maybe (L.StakePoolParams era)
  -- ^ An update already staged in @futureStakePoolParams@, if any.
  -> Maybe EpochNo
  -- ^ The epoch the pool is scheduled to retire in, if any.
  -> PoolRegistrationReport
mkPoolRegistrationReport certParams mCurrent mStaged mRetiring =
  PoolRegistrationReport
    { prPoolId = renderPoolId (L.sppId certParams)
    , prRegisteredOnChain = registered
    , prSummary = summary
    , prNotices = notices
    , prDiffs = currentDiff : maybeToList stagedDiff
    }
 where
  registered = case mCurrent of
    Nothing -> False
    Just _ -> True

  currentDiff = case mCurrent of
    Nothing -> ParameterDiff NoRegistration (map (newFieldComparison certParams) poolParamFields)
    Just current ->
      ParameterDiff CurrentParameters (map (fieldComparison current certParams) poolParamFields)

  stagedDiff =
    (\staged -> ParameterDiff StagedUpdate (map (fieldComparison staged certParams) poolParamFields))
      <$> mStaged

  changedFields = changedFieldsOf (pdFields currentDiff)

  -- The certificate's BLS voting key is the one already registered, so
  -- submitting it rotates nothing.
  blsKeyAlreadyRegistered =
    case (mCurrent, L.sppBlsKey certParams) of
      (Just current, L.SJust certKey) -> L.sppBlsKey current == L.SJust certKey
      _ -> False

  -- The certificate drops a voting key the pool has registered.
  blsKeyRemoved =
    case (mCurrent, L.sppBlsKey certParams) of
      (Just current, L.SNothing) -> L.sppBlsKey current /= L.SNothing
      _ -> False

  summary =
    ReportSummary
      { rsFirstRegistration = not registered
      , rsChangedFields = changedFields
      , rsOnlyBlsKeyChanges = registered && changedFields == ["blsKey"]
      , rsNoChanges = registered && null changedFields
      , rsBlsKeyAlreadyRegistered = blsKeyAlreadyRegistered
      , rsBlsKeyRemoved = blsKeyRemoved
      , rsSupersedesStagedUpdate = case mStaged of
          Nothing -> False
          Just _ -> True
      }

  notices =
    concat
      [ registrationNotice
      , changeNotice
      , blsKeyNotices
      , stagedUpdateNotice
      , retirementNotice
      ]

  registrationNotice
    | registered = [Notice Confirmed "registered on chain"]
    | otherwise =
        [ Notice
            Attention
            "not registered on chain: this is a first registration, every parameter below is new"
        ]

  -- A confirmation would be misleading while an update is staged: even a
  -- certificate that matches the current parameters exactly still supersedes
  -- that staged update, so there is something to look at either way.
  changeLevel
    | rsSupersedesStagedUpdate summary = Attention
    | otherwise = Confirmed

  changeNotice
    | not registered = []
    | rsNoChanges summary =
        [ Notice
            changeLevel
            "the certificate changes nothing: it matches the pool's current parameters"
        ]
    | blsKeyRemoved && changedFields == ["blsKey"] =
        [Notice Attention "the certificate removes the pool's BLS voting key"]
    | rsOnlyBlsKeyChanges summary =
        [Notice changeLevel "the certificate changes only the BLS voting key"]
    | otherwise =
        [ Notice Attention $
            pluralise (length changedFields) "parameter" "parameters"
              <> " change, review each one before signing"
        ]

  blsKeyNotices =
    [ Notice
        Attention
        "the BLS voting key in the certificate is already registered: no rotation takes place, though submitting the certificate restarts the key's age clock"
    | blsKeyAlreadyRegistered
    ]
      <> [ Notice Attention "the certificate carries no BLS voting key"
         | L.SNothing <- [L.sppBlsKey certParams]
         ]

  stagedUpdateNotice =
    [ Notice
        Attention
        "an update is already staged for this pool: submitting this certificate supersedes it"
    | Just _ <- [mStaged]
    ]

  retirementNotice =
    [ Notice Attention $
        "a retirement is scheduled for this pool in epoch "
          <> textShow (unEpochNo epoch)
          <> ": submitting this certificate cancels it"
    | Just epoch <- [mRetiring]
    ]

fieldComparison
  :: L.StakePoolParams era
  -- ^ Baseline.
  -> L.StakePoolParams era
  -- ^ Certificate.
  -> PoolParamField era
  -> FieldComparison
fieldComparison baseline certParams field =
  FieldComparison
    { fcField = ppfName field
    , fcChanged = not (ppfEq field baseline certParams)
    , fcBaseline = Just (ppfJson field baseline)
    , fcCertificate = ppfJson field certParams
    , fcBaselineText = Just (ppfText field baseline)
    , fcCertificateText = ppfText field certParams
    }

-- | A field of a first registration: there is no baseline, so it is new.
newFieldComparison :: L.StakePoolParams era -> PoolParamField era -> FieldComparison
newFieldComparison certParams field =
  FieldComparison
    { fcField = ppfName field
    , fcChanged = True
    , fcBaseline = Nothing
    , fcCertificate = ppfJson field certParams
    , fcBaselineText = Nothing
    , fcCertificateText = ppfText field certParams
    }

changedFieldsOf :: [FieldComparison] -> [Text]
changedFieldsOf = map fcField . filter fcChanged

--------------------------------------------------------------------------------
-- Rendering
--------------------------------------------------------------------------------

-- | The human readable diff. Follows the convention of @query kes-period-info@:
-- the level labels carry colour annotations, but 'docToString' drops
-- annotations, which is what keeps escape codes out of pipes and files.
renderPoolRegistrationReport :: PoolRegistrationReport -> Doc Ann
renderPoolRegistrationReport pr =
  vsep $
    concat
      [ ["Pool" <+> pretty (prPoolId pr)]
      , map renderNotice (prNotices pr)
      , concatMap renderDiff (prDiffs pr)
      ]

-- | The level is spelled out rather than marked with a glyph: a parameter the
-- operator has to look at is the whole point of the command, and a bare @!@ is
-- easy to miss above a wall of table rows.
renderNotice :: Notice -> Doc Ann
renderNotice Notice{nLevel, nMessage} =
  colour (pretty (pad labelWidth levelLabel)) <+> pretty nMessage
 where
  (levelLabel, colour) = case nLevel of
    Confirmed -> ("[OK]" :: Text, green)
    Attention -> ("[WARNING]", yellow)

-- | Every label is padded to this width, so that the messages line up.
labelWidth :: Int
labelWidth = Text.length "[WARNING]"

-- | One line of the table.
data Row = Row
  { rowName :: !Text
  , rowBaseline :: !Text
  -- ^ The value on the left. For a row with no baseline to compare against,
  -- the certificate's own value goes here.
  , rowCertificate :: !(Maybe Text)
  -- ^ The value on the right, present only when there is something to compare.
  , rowStatus :: !Text
  }

renderDiff :: ParameterDiff -> [Doc Ann]
renderDiff ParameterDiff{pdBaseline, pdFields} =
  concat
    [ [""]
    , [pretty header | not (Text.null header)]
    , ["" | not (Text.null header)]
    , map (pretty . renderRow) rows
    ]
 where
  header = case pdBaseline of
    CurrentParameters -> ""
    NoRegistration -> ""
    StagedUpdate -> "Against the update already staged for this pool:"

  -- What changed is what the operator is here for, so it goes first. The pool
  -- id cannot differ, since it is the key the on-chain parameters were looked
  -- up by, and it is already in the header; it stays in the comparison so that
  -- the field coverage check still sees it.
  rows = uncurry (<>) (List.partition ((== "changed") . rowStatus) (map toRow shown))
  shown = filter ((/= "poolId") . fcField) pdFields

  nameWidth = maximum (1 : map (Text.length . rowName) rows)
  baselineWidth = maximum (1 : map (Text.length . rowBaseline) rows)
  certificateWidth = maximum (1 : mapMaybe (fmap Text.length . rowCertificate) rows)

  renderRow row =
    Text.stripEnd $
      Text.concat
        [ "  "
        , pad nameWidth (rowName row)
        , "  "
        , pad baselineWidth (rowBaseline row)
        , maybe "      " (const "  ->  ") (rowCertificate row)
        , pad certificateWidth (fromMaybe "" (rowCertificate row))
        , "   "
        , rowStatus row
        ]

toRow :: FieldComparison -> Row
toRow f = case fcBaselineText f of
  -- No baseline: a first registration, where every parameter is new.
  Nothing -> Row (fcField f) (fcCertificateText f) Nothing "new"
  Just baselineText
    | not (fcChanged f) -> Row (fcField f) baselineText Nothing "unchanged"
    -- Two different values can share a compact rendering: one relay swapped for
    -- another still reads as "3 entries". Fall back to the whole value, so that
    -- the operator can see what moved.
    | baselineText == fcCertificateText f ->
        Row
          (fcField f)
          (maybe baselineText renderCompactJson (fcBaseline f))
          (Just (renderCompactJson (fcCertificate f)))
          "changed"
    | otherwise -> Row (fcField f) baselineText (Just (fcCertificateText f)) "changed"

pad :: Int -> Text -> Text
pad width t = t <> Text.replicate (max 0 (width - Text.length t)) " "

--------------------------------------------------------------------------------
-- Value rendering
--------------------------------------------------------------------------------

renderPoolId :: L.KeyHash L.StakePool -> Text
renderPoolId poolId = case toJSON poolId of
  Aeson.String t -> t
  other -> renderCompactJson other

-- | A BLS key is a public key plus a possession proof; the public key is what
-- identifies it, so that is what the compact rendering shows.
renderBlsKey :: L.StrictMaybe L.BlsKey -> Text
renderBlsKey = \case
  L.SNothing -> "none"
  L.SJust blsKey -> case toJSON blsKey of
    Aeson.Object o -> case KeyMap.lookup "blsPubKey" o of
      Just (Aeson.String t) -> abbreviate t
      _ -> abbreviate (renderCompactJson (toJSON blsKey))
    other -> abbreviate (renderCompactJson other)

renderAccountAddress :: L.AccountAddress -> Text
renderAccountAddress = abbreviateBech32 . serialiseToBech32 . fromShelleyStakeAddr

renderMetadata :: L.StrictMaybe L.PoolMetadata -> Text
renderMetadata = \case
  L.SNothing -> "none"
  L.SJust metadata -> abbreviate (L.urlToText (L.pmUrl metadata))

-- | The compact rendering used for a field with no more specific one: a scalar
-- is shown as is (abbreviated when it is a long hash), a collection as a count.
compactValue :: Aeson.Value -> Text
compactValue = \case
  Aeson.Null -> "none"
  Aeson.Bool b -> if b then "true" else "false"
  Aeson.String t -> abbreviate t
  Aeson.Number n -> renderNumber n
  Aeson.Array xs -> renderCount (length xs)
  value@(Aeson.Object _) -> abbreviate (renderCompactJson value)

renderCount :: Int -> Text
renderCount n = textShow n <> " " <> pluralise' n "entry" "entries"

-- | Plain decimal notation: a margin of 0.02 reads as @0.02@, not @2.0e-2@.
renderNumber :: Scientific.Scientific -> Text
renderNumber n = case Scientific.floatingOrInteger n :: Either Double Integer of
  Right i -> textShow i
  Left _ -> Text.pack (Scientific.formatScientific Scientific.Fixed Nothing n)

renderCompactJson :: Aeson.Value -> Text
renderCompactJson = Text.decodeUtf8 . LBS.toStrict . Aeson.encode

-- | Long hashes are unreadable in a diff and only the ends carry signal, so
-- show enough of both to tell two keys apart.
abbreviate :: Text -> Text
abbreviate t
  | Text.length t > 24 = Text.take 8 t <> "…" <> Text.takeEnd 6 t
  | otherwise = t

-- | Abbreviating a bech32 string from the front would only eat its human
-- readable prefix, which is the part that carries no information, so keep the
-- prefix and abbreviate what follows it.
abbreviateBech32 :: Text -> Text
abbreviateBech32 t
  | Text.null prefix = abbreviate t
  | otherwise = prefix <> abbreviate body
 where
  -- The bech32 separator is the last @1@ in the string.
  (prefix, body) = Text.breakOnEnd "1" t

pluralise :: Int -> Text -> Text -> Text
pluralise n singular plural = textShow n <> " " <> pluralise' n singular plural

pluralise' :: Int -> Text -> Text -> Text
pluralise' 1 singular _ = singular
pluralise' _ _ plural = plural

--------------------------------------------------------------------------------
-- Generic field name enumeration
--------------------------------------------------------------------------------

genericSelectorNames :: forall a. GSelectorNames (Rep a) => Proxy a -> [Text]
genericSelectorNames _ = gSelectorNames (Proxy @(Rep a))

class GSelectorNames (f :: Type -> Type) where
  gSelectorNames :: Proxy f -> [Text]

instance GSelectorNames f => GSelectorNames (D1 c f) where
  gSelectorNames _ = gSelectorNames (Proxy @f)

instance GSelectorNames f => GSelectorNames (C1 c f) where
  gSelectorNames _ = gSelectorNames (Proxy @f)

instance Selector c => GSelectorNames (S1 c f) where
  gSelectorNames _ = [Text.pack (selName (undefined :: S1 c f p))]

instance (GSelectorNames f, GSelectorNames g) => GSelectorNames (f :*: g) where
  gSelectorNames _ = gSelectorNames (Proxy @f) <> gSelectorNames (Proxy @g)
