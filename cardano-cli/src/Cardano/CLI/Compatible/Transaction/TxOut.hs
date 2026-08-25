{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.CLI.Compatible.Transaction.TxOut
  ( mkTxOut
  , toTxOutInAnyEra
  )
where

import Cardano.Api
import Cardano.Api.Experimental.Tx qualified as Exp
import Cardano.Api.Ledger qualified as L

import Cardano.CLI.Compatible.Exception
import Cardano.CLI.EraBased.Script.Read.Common
import Cardano.CLI.Orphan ()
import Cardano.CLI.Read
import Cardano.CLI.Type.Common
import Cardano.Ledger.Api.Tx qualified as L
import Cardano.Ledger.Hashes (DataHash)

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Lens.Micro

toTxOutInAnyEra
  :: ShelleyBasedEra era
  -> TxOutAnyEra
  -> CIO e (Exp.TxOut (ShelleyLedgerEra era), Map DataHash (L.Data (ShelleyLedgerEra era)))
toTxOutInAnyEra era (TxOutAnyEra addr' val' mDatumHash refScriptFp) = do
  let addr = anyAddressInShelleyBasedEra era addr'
  mkTxOut era addr val' mDatumHash refScriptFp

mkTxOut
  :: ShelleyBasedEra era
  -> AddressInEra era
  -> Value
  -> TxOutDatumAnyEra
  -> ReferenceScriptAnyEra
  -> CIO e (Exp.TxOut (ShelleyLedgerEra era), Map DataHash (L.Data (ShelleyLedgerEra era)))
mkTxOut sbe addr val' mDatumAnyEra refScriptFp = do
  txVal <- toTxOutValueInShelleyBasedEra sbe val'
  let ledgerAddr = toShelleyAddr addr
  shelleyBasedEraConstraints sbe $
    case txVal of
      TxOutValueShelleyBased _ ledgerVal ->
        case sbe of
          ShelleyBasedEraShelley -> pure (Exp.TxOut (L.mkBasicTxOut ledgerAddr ledgerVal), mempty)
          ShelleyBasedEraAllegra -> pure (Exp.TxOut (L.mkBasicTxOut ledgerAddr ledgerVal), mempty)
          ShelleyBasedEraMary -> pure (Exp.TxOut (L.mkBasicTxOut ledgerAddr ledgerVal), mempty)
          ShelleyBasedEraAlonzo -> do
            (mDH, suppl) <- alonzoDatumFields mDatumAnyEra
            pure
              ( Exp.TxOut (L.mkBasicTxOut ledgerAddr ledgerVal & L.dataHashTxOutL .~ mDH)
              , suppl
              )
          ShelleyBasedEraBabbage -> do
            (dat, suppl) <- babbageDatumFields mDatumAnyEra
            refScript <- readRefScript sbe refScriptFp
            pure
              ( Exp.TxOut
                  ( L.mkBasicTxOut ledgerAddr ledgerVal
                      & L.datumTxOutL .~ dat
                      & L.referenceScriptTxOutL .~ refScript
                  )
              , suppl
              )
          ShelleyBasedEraConway -> do
            (dat, suppl) <- babbageDatumFields mDatumAnyEra
            refScript <- readRefScript sbe refScriptFp
            pure
              ( Exp.TxOut
                  ( L.mkBasicTxOut ledgerAddr ledgerVal
                      & L.datumTxOutL .~ dat
                      & L.referenceScriptTxOutL .~ refScript
                  )
              , suppl
              )
          ShelleyBasedEraDijkstra -> do
            (dat, suppl) <- babbageDatumFields mDatumAnyEra
            refScript <- readRefScript sbe refScriptFp
            pure
              ( Exp.TxOut
                  ( L.mkBasicTxOut ledgerAddr ledgerVal
                      & L.datumTxOutL .~ dat
                      & L.referenceScriptTxOutL .~ refScript
                  )
              , suppl
              )

alonzoDatumFields
  :: L.Era ledgerera
  => TxOutDatumAnyEra
  -> CIO e (L.StrictMaybe DataHash, Map DataHash (L.Data ledgerera))
alonzoDatumFields = \case
  TxOutDatumByNone ->
    pure (L.SNothing, mempty)
  TxOutDatumByHashOnly h ->
    pure (L.SJust (unScriptDataHash h), mempty)
  TxOutDatumByHashOf sDataOrFile -> do
    sData <- fromExceptTCli $ readScriptDataOrFile sDataOrFile
    pure (L.SJust (unScriptDataHash (hashScriptDataBytes sData)), mempty)
  TxOutDatumByValue sDataOrFile -> do
    sData <- fromExceptTCli $ readScriptDataOrFile sDataOrFile
    let ld = toAlonzoData sData
        dh = L.hashData ld
    pure (L.SJust dh, Map.singleton dh ld)
  TxOutInlineDatumByValue _ ->
    throwCliError $ TxCmdTxFeatureMismatch (AnyCardanoEra AlonzoEra) TxFeatureInlineDatums

babbageDatumFields
  :: L.Era ledgerera
  => TxOutDatumAnyEra
  -> CIO e (L.Datum ledgerera, Map DataHash (L.Data ledgerera))
babbageDatumFields = \case
  TxOutDatumByNone ->
    pure (L.NoDatum, mempty)
  TxOutDatumByHashOnly h ->
    pure (L.DatumHash (unScriptDataHash h), mempty)
  TxOutDatumByHashOf sDataOrFile -> do
    sData <- fromExceptTCli $ readScriptDataOrFile sDataOrFile
    pure (L.DatumHash (unScriptDataHash (hashScriptDataBytes sData)), mempty)
  TxOutDatumByValue sDataOrFile -> do
    sData <- fromExceptTCli $ readScriptDataOrFile sDataOrFile
    let ld = toAlonzoData sData
        dh = L.hashData ld
    pure (L.DatumHash dh, Map.singleton dh ld)
  TxOutInlineDatumByValue sDataOrFile -> do
    sData <- fromExceptTCli $ readScriptDataOrFile sDataOrFile
    pure (scriptDataToInlineDatum sData, mempty)

readRefScript
  :: ShelleyBasedEra era
  -> ReferenceScriptAnyEra
  -> CIO e (L.StrictMaybe (L.Script (ShelleyLedgerEra era)))
readRefScript sbe = \case
  ReferenceScriptAnyEraNone -> pure L.SNothing
  ReferenceScriptAnyEra fp -> do
    script <- readFileScriptInAnyLang fp
    pure $ maybe L.SNothing (L.SJust . toShelleyScript) (toScriptInEra sbe script)

toTxOutValueInShelleyBasedEra
  :: ShelleyBasedEra era
  -> Value
  -> CIO e (TxOutValue era)
toTxOutValueInShelleyBasedEra sbe val =
  forEraInEon
    (convert sbe)
    ( case valueToLovelace val of
        Just l -> return (lovelaceToTxOutValue sbe l)
        Nothing -> txFeatureMismatch sbe TxFeatureMultiAssetOutputs
    )
    (\w -> maryEraOnwardsConstraints w $ return (TxOutValueShelleyBased sbe (toLedgerValue w val)))

-- | An enumeration of era-dependent features where we have to check that it
-- is permissible to use this feature in this era.
data TxFeature
  = TxFeatureMultiAssetOutputs
  | TxFeatureInlineDatums
  deriving Show

renderFeature :: TxFeature -> Text
renderFeature = \case
  TxFeatureMultiAssetOutputs -> "Multi-Asset outputs"
  TxFeatureInlineDatums -> "Inline datums"

data TxCmdTxFeatureMismatch = TxCmdTxFeatureMismatch !AnyCardanoEra !TxFeature deriving Show

instance Error TxCmdTxFeatureMismatch where
  prettyError (TxCmdTxFeatureMismatch (AnyCardanoEra era) feature) =
    pretty $
      mconcat
        [ renderFeature feature
        , " cannot be used for "
        , eraToStringKey era
        , " era transactions."
        ]

txFeatureMismatch
  :: ()
  => ToCardanoEra eon
  => eon era
  -> TxFeature
  -> CIO e a
txFeatureMismatch eon feature =
  throwCliError $ TxCmdTxFeatureMismatch (anyCardanoEra $ toCardanoEra eon) feature

eraToStringKey :: CardanoEra a -> Text
eraToStringKey = docToText . pretty
