{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Types.TxFeature
  ( TxFeature (..)
  , renderFeature
  )
where

import Data.Text (Text)

-- | An enumeration of era-dependent features where we have to check that it
-- is permissible to use this feature in this era.
data TxFeature
  = TxFeatureShelleyAddresses
  | TxFeatureExplicitFees
  | TxFeatureImplicitFees
  | TxFeatureValidityLowerBound
  | TxFeatureValidityUpperBound
  | TxFeatureValidityNoUpperBound
  | TxFeatureTxMetadata
  | TxFeatureAuxScripts
  | TxFeatureWithdrawals
  | TxFeatureCertificates
  | TxFeatureMintValue
  | TxFeatureMultiAssetOutputs
  | TxFeatureScriptWitnesses
  | TxFeatureShelleyKeys
  | TxFeatureCollateral
  | TxFeatureProtocolParameters
  | TxFeatureTxOutDatum
  | TxFeatureScriptValidity
  | TxFeatureExtraKeyWits
  | TxFeatureInlineDatums
  | TxFeatureTotalCollateral
  | TxFeatureReferenceInputs
  | TxFeatureReturnCollateral
  deriving Show

renderFeature :: TxFeature -> Text
renderFeature = \case
  TxFeatureShelleyAddresses -> "Shelley addresses"
  TxFeatureExplicitFees -> "Explicit fees"
  TxFeatureImplicitFees -> "Implicit fees"
  TxFeatureValidityLowerBound -> "A validity lower bound"
  TxFeatureValidityUpperBound -> "A validity upper bound"
  TxFeatureValidityNoUpperBound -> "An absent validity upper bound"
  TxFeatureTxMetadata -> "Transaction metadata"
  TxFeatureAuxScripts -> "Auxiliary scripts"
  TxFeatureWithdrawals -> "Reward account withdrawals"
  TxFeatureCertificates -> "Certificates"
  TxFeatureMintValue -> "Asset minting"
  TxFeatureMultiAssetOutputs -> "Multi-Asset outputs"
  TxFeatureScriptWitnesses -> "Script witnesses"
  TxFeatureShelleyKeys -> "Shelley keys"
  TxFeatureCollateral -> "Collateral inputs"
  TxFeatureProtocolParameters -> "Protocol parameters"
  TxFeatureTxOutDatum -> "Transaction output datums"
  TxFeatureScriptValidity -> "Script validity"
  TxFeatureExtraKeyWits -> "Required signers"
  TxFeatureInlineDatums -> "Inline datums"
  TxFeatureTotalCollateral -> "Total collateral"
  TxFeatureReferenceInputs -> "Reference inputs"
  TxFeatureReturnCollateral -> "Return collateral"
