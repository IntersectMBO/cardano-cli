{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

-- | Types that are used when writing to standard output or to files.
-- These types (and their encodings) are typically consumed by users of @cardano-cli@.
module Cardano.CLI.Type.Output
  ( PlutusScriptCostError
  , QueryDRepStateOutput (..)
  , QueryKesPeriodInfoOutput (..)
  , QueryTipLocalState (..)
  , QueryTipLocalStateOutput (..)
  , ScriptCostOutput (..)
  , createOpCertIntervalInfo
  , renderScriptCostsWithScriptHashesMap
  )
where

import Cardano.Api
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Shelley

import Cardano.CLI.Type.Common

import Prelude

import Data.Aeson
import Data.Aeson.Key qualified as Aeson
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time.Clock (UTCTime)
import Data.Word

data QueryKesPeriodInfoOutput
  = QueryKesPeriodInfoOutput
  { qKesOpCertIntervalInformation :: OpCertIntervalInformation
  , qKesInfoKesKeyExpiry :: Maybe UTCTime
  -- ^ Date of KES key expiry.
  , qKesInfoNodeStateOperationalCertNo :: Maybe OpCertNodeStateCounter
  -- ^ The latest operational certificate number in the node's state
  -- i.e how many times a new KES key has been generated.
  , qKesInfoOnDiskOperationalCertNo :: OpCertOnDiskCounter
  -- ^ The on disk operational certificate number.
  , qKesInfoMaxKesKeyEvolutions :: Word64
  -- ^ The maximum number of KES key evolutions permitted per KES period.
  , qKesInfoSlotsPerKesPeriod :: Word64
  }
  deriving (Eq, Show)

instance ToJSON QueryKesPeriodInfoOutput where
  toJSON
    ( QueryKesPeriodInfoOutput
        opCertIntervalInfo
        kesKeyExpiryTime
        nodeStateOpCertNo
        (OpCertOnDiskCounter onDiskOpCertNo)
        maxKesKeyOps
        slotsPerKesPeriod
      ) = do
      let (sKes, eKes, cKes, slotsTillExp) =
            case opCertIntervalInfo of
              OpCertWithinInterval startKes endKes currKes sUntilExp ->
                ( unOpCertStartingKesPeriod startKes
                , unOpCertEndingKesPeriod endKes
                , unCurrentKesPeriod currKes
                , Just sUntilExp
                )
              OpCertStartingKesPeriodIsInTheFuture startKes endKes currKes ->
                ( unOpCertStartingKesPeriod startKes
                , unOpCertEndingKesPeriod endKes
                , unCurrentKesPeriod currKes
                , Nothing
                )
              OpCertExpired startKes endKes currKes ->
                ( unOpCertStartingKesPeriod startKes
                , unOpCertEndingKesPeriod endKes
                , unCurrentKesPeriod currKes
                , Nothing
                )
              OpCertSomeOtherError startKes endKes currKes ->
                ( unOpCertStartingKesPeriod startKes
                , unOpCertEndingKesPeriod endKes
                , unCurrentKesPeriod currKes
                , Nothing
                )

      object
        [ "qKesCurrentKesPeriod" .= cKes
        , "qKesStartKesInterval" .= sKes
        , "qKesEndKesInterval" .= eKes
        , "qKesRemainingSlotsInKesPeriod" .= slotsTillExp
        , "qKesOnDiskOperationalCertificateNumber" .= onDiskOpCertNo
        , "qKesNodeStateOperationalCertificateNumber" .= nodeStateOpCertNo
        , "qKesMaxKESEvolutions" .= maxKesKeyOps
        , "qKesSlotsPerKesPeriod" .= slotsPerKesPeriod
        , "qKesKesKeyExpiry" .= kesKeyExpiryTime
        ]

instance FromJSON QueryKesPeriodInfoOutput where
  parseJSON = withObject "QueryKesPeriodInfoOutput" $ \o -> do
    currentKesPeriod <- o .: "qKesCurrentKesPeriod"
    startKesInterval <- o .: "qKesStartKesInterval"
    endKesInterval <- o .: "qKesEndKesInterval"
    remainingSlotsInKesPeriod <- o .: "qKesRemainingSlotsInKesPeriod"
    onDiskOperationalCertificateNumber <- o .: "qKesOnDiskOperationalCertificateNumber"
    nodeStateOperationalCertificateNumber <- o .: "qKesNodeStateOperationalCertificateNumber"
    maxKESEvolutions <- o .: "qKesMaxKESEvolutions"
    slotsPerKesPeriod <- o .: "qKesSlotsPerKesPeriod"
    kesKeyExpiry <- o .: "qKesKesKeyExpiry"
    let opCertIntervalInfo =
          createOpCertIntervalInfo
            currentKesPeriod
            startKesInterval
            endKesInterval
            remainingSlotsInKesPeriod
    return $
      QueryKesPeriodInfoOutput
        { qKesOpCertIntervalInformation = opCertIntervalInfo
        , qKesInfoKesKeyExpiry = kesKeyExpiry
        , qKesInfoNodeStateOperationalCertNo = nodeStateOperationalCertificateNumber
        , qKesInfoOnDiskOperationalCertNo = onDiskOperationalCertificateNumber
        , qKesInfoMaxKesKeyEvolutions = maxKESEvolutions
        , qKesInfoSlotsPerKesPeriod = slotsPerKesPeriod
        }

createOpCertIntervalInfo
  :: CurrentKesPeriod
  -> OpCertStartingKesPeriod
  -> OpCertEndingKesPeriod
  -> Maybe SlotsTillKesKeyExpiry
  -> OpCertIntervalInformation
createOpCertIntervalInfo
  c@(CurrentKesPeriod cKesPeriod)
  s@(OpCertStartingKesPeriod oCertStart)
  e@(OpCertEndingKesPeriod oCertEnd)
  (Just tillExp)
    | oCertStart <= cKesPeriod && cKesPeriod < oCertEnd =
        OpCertWithinInterval s e c tillExp
    | oCertStart > cKesPeriod = OpCertStartingKesPeriodIsInTheFuture s e c
    | cKesPeriod >= oCertEnd = OpCertExpired s e c
    | otherwise = OpCertSomeOtherError s e c
createOpCertIntervalInfo
  c@(CurrentKesPeriod cKesPeriod)
  s@(OpCertStartingKesPeriod oCertStart)
  e@(OpCertEndingKesPeriod oCertEnd)
  Nothing
    | oCertStart > cKesPeriod = OpCertStartingKesPeriodIsInTheFuture s e c
    | cKesPeriod >= oCertEnd = OpCertExpired s e c
    | otherwise = OpCertSomeOtherError s e c

data QueryTipLocalState mode = QueryTipLocalState
  { era :: AnyCardanoEra
  , eraHistory :: EraHistory
  , mSystemStart :: Maybe SystemStart
  , mChainTip :: Maybe ChainTip
  }

data QueryTipLocalStateOutput = QueryTipLocalStateOutput
  { localStateChainTip :: ChainTip
  , mEra :: Maybe AnyCardanoEra
  , mEpoch :: Maybe EpochNo
  , mSlotInEpoch :: Maybe Word64
  , mSlotsToEpochEnd :: Maybe Word64
  , mSyncProgress :: Maybe Text
  }
  deriving Show

data QueryDRepStateOutput
  = -- Not a record, because we want exhaustive warnings in the code of ToJSON below,
    -- if we ever add more fields.
    QueryDRepStateOutput
      (L.Credential L.DRepRole L.StandardCrypto)
      -- ^ Credential
      EpochNo
      -- ^ Expiry
      (Maybe (L.Anchor L.StandardCrypto))
      -- ^ Anchor
      Lovelace
      -- ^ Deposit
      IncludeStake
      (Maybe Lovelace)
      -- ^ Stake

instance ToJSON QueryDRepStateOutput where
  toJSON (QueryDRepStateOutput credential expiry anchor deposit includeStake stake) =
    toJSON
      ( credential
      , object $
          [ "expiry" .= expiry
          , "anchor" .= anchor
          , "deposit" .= deposit
          ]
            <> ( case includeStake of
                   WithStake -> ["stake" .= stake]
                   NoStake -> []
               )
      )

-- | A key-value pair difference list for encoding a JSON object.
(..=) :: (KeyValue e kv, ToJSON v) => Aeson.Key -> v -> [kv] -> [kv]
(..=) n v = (n .= v :)

-- | A key-value pair difference list for encoding a JSON object where Nothing encodes absence of the key-value pair.
(..=?) :: (KeyValue e kv, ToJSON v) => Aeson.Key -> Maybe v -> [kv] -> [kv]
(..=?) n mv = case mv of
  Just v -> (n .= v :)
  Nothing -> id

instance ToJSON QueryTipLocalStateOutput where
  toJSON a = case localStateChainTip a of
    ChainTipAtGenesis ->
      object $
        ( ("era" ..=? mEra a)
            . ("epoch" ..=? mEpoch a)
            . ("slotInEpoch" ..=? mSlotInEpoch a)
            . ("slotsToEpochEnd" ..=? mSlotsToEpochEnd a)
            . ("syncProgress" ..=? mSyncProgress a)
        )
          []
    ChainTip slotNo blockHeader blockNo ->
      object $
        ( ("slot" ..= slotNo)
            . ("hash" ..= serialiseToRawBytesHexText blockHeader)
            . ("block" ..= blockNo)
            . ("era" ..=? mEra a)
            . ("epoch" ..=? mEpoch a)
            . ("slotInEpoch" ..=? mSlotInEpoch a)
            . ("slotsToEpochEnd" ..=? mSlotsToEpochEnd a)
            . ("syncProgress" ..=? mSyncProgress a)
        )
          []
  toEncoding a = case localStateChainTip a of
    ChainTipAtGenesis ->
      pairs $
        mconcat $
          ( ("era" ..=? mEra a)
              . ("epoch" ..=? mEpoch a)
              . ("slotInEpoch" ..=? mSlotInEpoch a)
              . ("slotsToEpochEnd" ..=? mSlotsToEpochEnd a)
              . ("syncProgress" ..=? mSyncProgress a)
          )
            []
    ChainTip slotNo blockHeader blockNo ->
      pairs $
        mconcat $
          ( ("slot" ..= slotNo)
              . ("hash" ..= serialiseToRawBytesHexText blockHeader)
              . ("block" ..= blockNo)
              . ("era" ..=? mEra a)
              . ("epoch" ..=? mEpoch a)
              . ("slotInEpoch" ..=? mSlotInEpoch a)
              . ("slotsToEpochEnd" ..=? mSlotsToEpochEnd a)
              . ("syncProgress" ..=? mSyncProgress a)
          )
            []

instance FromJSON QueryTipLocalStateOutput where
  parseJSON = withObject "QueryTipLocalStateOutput" $ \o -> do
    mEra' <- o .:? "era"
    mEpoch' <- o .:? "epoch"
    mSyncProgress' <- o .:? "syncProgress"

    mSlot <- o .:? "slot"
    mHash <- o .:? "hash"
    mBlock <- o .:? "block"
    mSlotInEpoch' <- o .:? "slotInEpoch"
    mSlotsToEpochEnd' <- o .:? "slotsToEpochEnd"
    case (mSlot, mHash, mBlock) of
      (Nothing, Nothing, Nothing) ->
        pure $
          QueryTipLocalStateOutput
            ChainTipAtGenesis
            mEra'
            mEpoch'
            mSlotInEpoch'
            mSlotsToEpochEnd'
            mSyncProgress'
      (Just slot, Just hash, Just block) ->
        pure $
          QueryTipLocalStateOutput
            (ChainTip slot hash block)
            mEra'
            mEpoch'
            mSlotInEpoch'
            mSlotsToEpochEnd'
            mSyncProgress'
      (_, _, _) ->
        fail $
          mconcat
            [ "QueryTipLocalStateOutput was incorrectly JSON encoded."
            , " Expected slot, header hash and block number (ChainTip)"
            , " or none (ChainTipAtGenesis)"
            ]

data ScriptCostOutput
  = ScriptCostOutput
  { scScriptHash :: ScriptHash
  , scExecutionUnits :: ExecutionUnits
  , scAda :: Lovelace
  }

instance ToJSON ScriptCostOutput where
  toJSON (ScriptCostOutput sHash execUnits llCost) =
    object
      [ "scriptHash" .= sHash
      , "executionUnits" .= execUnits
      , "lovelaceCost" .= llCost
      ]

data PlutusScriptCostError
  = PlutusScriptCostErrPlutusScriptNotFound ScriptWitnessIndex
  | PlutusScriptCostErrExecError ScriptWitnessIndex (Maybe ScriptHash) ScriptExecutionError
  | PlutusScriptCostErrRationalExceedsBound
      [Text]
      -- ^ Execution logs
      L.Prices
      ExecutionUnits
  | PlutusScriptCostErrRefInputNoScript TxIn
  | PlutusScriptCostErrRefInputNotInUTxO TxIn
  deriving Show

instance Error PlutusScriptCostError where
  prettyError = \case
    PlutusScriptCostErrPlutusScriptNotFound sWitIndex ->
      "No Plutus script was found at: " <> pshow sWitIndex
    PlutusScriptCostErrExecError sWitIndex sHash sExecErro ->
      "Plutus script at: "
        <> pshow sWitIndex
        <> " with hash: "
        <> pshow sHash
        <> " errored with: "
        <> prettyError sExecErro
    PlutusScriptCostErrRationalExceedsBound executionLogs eUnitPrices eUnits ->
      let firstLine =
            mconcat
              [ "Either the execution unit prices: "
              , pshow eUnitPrices
              , " or the execution units: "
              , pshow eUnits
              , " or both are either too precise or not within bounds"
              ]
       in vsep
            [ firstLine
            , "Execution logs: " <> pretty (Text.unlines executionLogs)
            ]
    PlutusScriptCostErrRefInputNoScript txin ->
      "No reference script found at input: " <> pretty (renderTxIn txin)
    PlutusScriptCostErrRefInputNotInUTxO txin ->
      "Reference input was not found in utxo: " <> pretty (renderTxIn txin)

renderScriptCostsWithScriptHashesMap
  :: L.Prices
  -> Map ScriptWitnessIndex ScriptHash
  -- ^ Initial mapping of script witness index to script hash.
  -- We need this in order to know which script corresponds to the
  -- calculated execution units.
  -> Map ScriptWitnessIndex (Either ScriptExecutionError ([Text], ExecutionUnits))
  -- ^ Post execution cost calculation mapping of script witness
  -- index to execution units.
  -> Either PlutusScriptCostError [ScriptCostOutput]
renderScriptCostsWithScriptHashesMap eUnitPrices scriptMap executionCostMapping =
  sequenceA $
    Map.foldlWithKey
      ( \accum sWitInd eExecUnits -> do
          case Map.lookup sWitInd scriptMap of
            Just scriptHash -> do
              case eExecUnits of
                Right (logs, execUnits) ->
                  case calculateExecutionUnitsLovelace eUnitPrices execUnits of
                    Just llCost ->
                      Right (ScriptCostOutput scriptHash execUnits llCost)
                        : accum
                    Nothing ->
                      Left (PlutusScriptCostErrRationalExceedsBound logs eUnitPrices execUnits)
                        : accum
                Left err -> Left (PlutusScriptCostErrExecError sWitInd (Just scriptHash) err) : accum
            Nothing -> Left (PlutusScriptCostErrPlutusScriptNotFound sWitInd) : accum
      )
      []
      executionCostMapping
