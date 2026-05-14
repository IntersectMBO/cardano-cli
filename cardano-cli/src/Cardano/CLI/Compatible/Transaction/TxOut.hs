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
import Cardano.Ledger.Hashes (DataHash)
import Cardano.Ledger.Plutus.Data qualified as L

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

toTxOutInAnyEra
  :: ShelleyBasedEra era
  -> TxOutAnyEra
  -> CIO e (Exp.TxOut (ShelleyLedgerEra era), Map DataHash (L.Data (ShelleyLedgerEra era)))
toTxOutInAnyEra era (TxOutAnyEra addr' val' mDatumHash refScriptFp) = do
  let addr = anyAddressInShelleyBasedEra era addr'
  mkTxOut era addr val' mDatumHash refScriptFp

-- | Build an output for a transaction body. Produces the experimental
-- 'Exp.TxOut' plus any supplemental datum bodies that the caller-supplied
-- datum carries. The legacy 'TxOut CtxTx era' bundled supplemental datums
-- inside outputs; 'Exp.TxOut' only carries the datum hash, so callers thread
-- the full datum bodies in separately (e.g. via 'createCompatibleTx').
--
-- The legacy 'TxOut CtxTx era' is used internally as a stepping stone to
-- reuse the api's 'toShelleyTxOutAny' field-level conversion logic; it is
-- not exposed.
mkTxOut
  :: ShelleyBasedEra era
  -> AddressInEra era
  -> Value
  -> TxOutDatumAnyEra
  -> ReferenceScriptAnyEra
  -> CIO e (Exp.TxOut (ShelleyLedgerEra era), Map DataHash (L.Data (ShelleyLedgerEra era)))
mkTxOut sbe addr val' mDatumHash refScriptFp = do
  let era = toCardanoEra sbe
  val <- toTxOutValueInShelleyBasedEra sbe val'

  datum <-
    inEonForEra
      (pure TxOutDatumNone)
      (`toTxAlonzoDatum` mDatumHash)
      era

  refScript <-
    inEonForEra
      (pure ReferenceScriptNone)
      (`getReferenceScript` refScriptFp)
      era

  let legacyTxOut = TxOut addr val datum refScript
  pure $
    shelleyBasedEraConstraints sbe $
      (Exp.TxOut (toShelleyTxOutAny sbe legacyTxOut), supplementalsOf datum)
 where
  supplementalsOf
    :: L.Era (ShelleyLedgerEra era)
    => TxOutDatum CtxTx era
    -> Map DataHash (L.Data (ShelleyLedgerEra era))
  supplementalsOf (TxOutSupplementalDatum _ h) =
    let ld = toAlonzoData h
     in Map.singleton (L.hashData ld) ld
  supplementalsOf _ = mempty

toTxOutValueInShelleyBasedEra
  :: ShelleyBasedEra era
  -> Value
  -> CIO e (TxOutValue era)
toTxOutValueInShelleyBasedEra sbe val =
  caseShelleyToAllegraOrMaryEraOnwards
    ( \_ -> case valueToLovelace val of
        Just l -> return (TxOutValueShelleyBased sbe l)
        Nothing -> txFeatureMismatch sbe TxFeatureMultiAssetOutputs
    )
    (\w -> return (TxOutValueShelleyBased sbe (toLedgerValue w val)))
    sbe

toTxAlonzoDatum
  :: ()
  => AlonzoEraOnwards era
  -> TxOutDatumAnyEra
  -> CIO e (TxOutDatum CtxTx era)
toTxAlonzoDatum supp cliDatum =
  case cliDatum of
    TxOutDatumByNone -> pure TxOutDatumNone
    TxOutDatumByHashOnly h -> pure (TxOutDatumHash supp h)
    TxOutDatumByHashOf sDataOrFile -> do
      sData <- fromExceptTCli $ readScriptDataOrFile sDataOrFile
      pure (TxOutDatumHash supp $ hashScriptDataBytes sData)
    TxOutDatumByValue sDataOrFile -> do
      sData <- fromExceptTCli $ readScriptDataOrFile sDataOrFile
      pure (TxOutSupplementalDatum supp sData)
    TxOutInlineDatumByValue sDataOrFile -> do
      let cEra = toCardanoEra supp
      forEraInEon cEra (txFeatureMismatch cEra TxFeatureInlineDatums) $ \babbageOnwards -> do
        sData <- fromExceptTCli $ readScriptDataOrFile sDataOrFile
        pure $ TxOutDatumInline babbageOnwards sData

getReferenceScript
  :: BabbageEraOnwards era
  -> ReferenceScriptAnyEra
  -> CIO e (ReferenceScript era)
getReferenceScript w = \case
  ReferenceScriptAnyEraNone -> return ReferenceScriptNone
  ReferenceScriptAnyEra fp -> ReferenceScript w <$> readFileScriptInAnyLang fp

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
