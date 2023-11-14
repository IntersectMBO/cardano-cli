{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Legacy.Commands.Transaction
  ( LegacyTransactionCmds (..)
  , renderLegacyTransactionCmds
  ) where

import           Cardano.Api.Shelley

import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Governance

import           Data.Text (Text)

data LegacyTransactionCmds
  = TransactionBuildRawCmd
      AnyCardanoEra
      (Maybe ScriptValidity)
      -- ^ Mark script as expected to pass or fail validation
      [(TxIn, Maybe (ScriptWitnessFiles WitCtxTxIn))]
      -- ^ Transaction inputs with optional spending scripts
      [TxIn]
      -- ^ Read only reference inputs
      [TxIn]
      -- ^ Transaction inputs for collateral, only key witnesses, no scripts.
      (Maybe TxOutAnyEra)
      -- ^ Return collateral
      (Maybe Lovelace)
      -- ^ Total collateral
      [RequiredSigner]
      -- ^ Required signers
      [TxOutAnyEra]
      (Maybe (Value, [ScriptWitnessFiles WitCtxMint]))
      -- ^ Multi-Asset value with script witness
      (Maybe SlotNo)
      -- ^ Transaction lower bound
      (Maybe SlotNo)
      -- ^ Transaction upper bound
      (Maybe Lovelace)
      -- ^ Tx fee
      [(CertificateFile, Maybe (ScriptWitnessFiles WitCtxStake))]
      -- ^ Certificates with potential script witness
      [(StakeAddress, Lovelace, Maybe (ScriptWitnessFiles WitCtxStake))]
      TxMetadataJsonSchema
      [ScriptFile]
      -- ^ Auxiliary scripts
      [MetadataFile]
      (Maybe ProtocolParamsFile)
      (Maybe UpdateProposalFile)
      (TxBodyFile Out)

    -- | Like 'TransactionBuildRaw' but without the fee, and with a change output.
  | TransactionBuildCmd
      SocketPath
      (EraInEon ShelleyBasedEra)
      ConsensusModeParams
      NetworkId
      (Maybe ScriptValidity) -- ^ Mark script as expected to pass or fail validation
      (Maybe Word)
      -- ^ Override the required number of tx witnesses
      [(TxIn, Maybe (ScriptWitnessFiles WitCtxTxIn))]
      -- ^ Transaction inputs with optional spending scripts
      [TxIn]
      -- ^ Read only reference inputs
      [RequiredSigner]
      -- ^ Required signers
      [TxIn]
      -- ^ Transaction inputs for collateral, only key witnesses, no scripts.
      (Maybe TxOutAnyEra)
      -- ^ Return collateral
      (Maybe Lovelace)
      -- ^ Total collateral
      [TxOutAnyEra]
      -- ^ Normal outputs
      TxOutChangeAddress
      -- ^ A change output
      (Maybe (Value, [ScriptWitnessFiles WitCtxMint]))
      -- ^ Multi-Asset value with script witness
      (Maybe SlotNo)
      -- ^ Transaction lower bound
      (Maybe SlotNo)
      -- ^ Transaction upper bound
      [(CertificateFile, Maybe (ScriptWitnessFiles WitCtxStake))]
      -- ^ Certificates with potential script witness
      [(StakeAddress, Lovelace, Maybe (ScriptWitnessFiles WitCtxStake))]
      -- ^ Withdrawals with potential script witness
      TxMetadataJsonSchema
      [ScriptFile]
      -- ^ Auxiliary scripts
      [MetadataFile]
      (Maybe UpdateProposalFile)
      [VoteFile In]
      [ProposalFile In]
      TxBuildOutputOptions
  | TransactionSignCmd
      InputTxBodyOrTxFile
      [WitnessSigningData]
      (Maybe NetworkId)
      (TxFile Out)
  | TransactionWitnessCmd
      (TxBodyFile In)
      WitnessSigningData
      (Maybe NetworkId)
      (File () Out)
  | TransactionSignWitnessCmd
      (TxBodyFile In)
      [WitnessFile]
      (File () Out)
  | TransactionSubmitCmd
      SocketPath
      ConsensusModeParams
      NetworkId
      FilePath
  | TransactionPolicyIdCmd
      ScriptFile
  | TransactionCalculateMinFeeCmd
      (TxBodyFile In)
      NetworkId
      ProtocolParamsFile
      TxInCount
      TxOutCount
      TxShelleyWitnessCount
      TxByronWitnessCount
  | TransactionCalculateMinValueCmd
      AnyCardanoEra
      ProtocolParamsFile
      TxOutShelleyBasedEra
  | TransactionHashScriptDataCmd
      ScriptDataOrFile
  | TransactionTxIdCmd
      InputTxBodyOrTxFile
  | TransactionViewCmd
      TxViewOutputFormat
      (Maybe (File () Out))
      InputTxBodyOrTxFile

renderLegacyTransactionCmds :: LegacyTransactionCmds -> Text
renderLegacyTransactionCmds = \case
  TransactionBuildCmd                     {} -> "transaction build"
  TransactionBuildRawCmd                  {} -> "transaction build-raw"
  TransactionSignCmd                      {} -> "transaction sign"
  TransactionWitnessCmd                   {} -> "transaction witness"
  TransactionSignWitnessCmd               {} -> "transaction sign-witness"
  TransactionSubmitCmd                    {} -> "transaction submit"
  TransactionPolicyIdCmd                  {} -> "transaction policyid"
  TransactionCalculateMinFeeCmd           {} -> "transaction calculate-min-fee"
  TransactionCalculateMinValueCmd         {} -> "transaction calculate-min-value"
  TransactionHashScriptDataCmd            {} -> "transaction hash-script-data"
  TransactionTxIdCmd                      {} -> "transaction txid"
  TransactionViewCmd                      {} -> "transaction view"
