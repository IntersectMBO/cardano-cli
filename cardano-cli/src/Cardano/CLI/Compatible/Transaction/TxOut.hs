{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.CLI.Compatible.Transaction.TxOut
  ( mkTxOut
  , toTxOutInAnyEra
  )
where

import Cardano.Api

import Cardano.CLI.Compatible.Exception
import Cardano.CLI.EraBased.Script.Read.Common
import Cardano.CLI.Orphan ()
import Cardano.CLI.Read
import Cardano.CLI.Type.Common

toTxOutInAnyEra
  :: ShelleyBasedEra era
  -> TxOutAnyEra
  -> CIO e (TxOut CtxTx era)
toTxOutInAnyEra era (TxOutAnyEra addr' val' mDatumHash refScriptFp) = do
  let addr = anyAddressInShelleyBasedEra era addr'
  mkTxOut era addr val' mDatumHash refScriptFp

mkTxOut
  :: ShelleyBasedEra era
  -> AddressInEra era
  -> Value
  -> TxOutDatumAnyEra
  -> ReferenceScriptAnyEra
  -> CIO e (TxOut CtxTx era)
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

  pure $ TxOut addr val datum refScript

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
