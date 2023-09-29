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
  = TxBuildRaw
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

    -- | Like 'TxBuildRaw' but without the fee, and with a change output.
  | TxBuild
      SocketPath
      AnyCardanoEra
      AnyConsensusModeParams
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
  | TxSign
      InputTxBodyOrTxFile
      [WitnessSigningData]
      (Maybe NetworkId)
      (TxFile Out)
  | TxCreateWitness
      (TxBodyFile In)
      WitnessSigningData
      (Maybe NetworkId)
      (File () Out)
  | TxAssembleTxBodyWitness
      (TxBodyFile In)
      [WitnessFile]
      (File () Out)
  | TxSubmit
      SocketPath
      AnyConsensusModeParams
      NetworkId
      FilePath
  | TxMintedPolicyId
      ScriptFile
  | TxCalculateMinFee
      (TxBodyFile In)
      NetworkId
      ProtocolParamsFile
      TxInCount
      TxOutCount
      TxShelleyWitnessCount
      TxByronWitnessCount
  | TxCalculateMinRequiredUTxO
      AnyCardanoEra
      ProtocolParamsFile
      TxOutAnyEra
  | TxHashScriptData
      ScriptDataOrFile
  | TxGetTxId
      InputTxBodyOrTxFile
  | TxView
      TxViewOutputFormat
      (Maybe (File () Out))
      InputTxBodyOrTxFile

renderLegacyTransactionCmds :: LegacyTransactionCmds -> Text
renderLegacyTransactionCmds = \case
  TxBuild {} -> "transaction build"
  TxBuildRaw {} -> "transaction build-raw"
  TxSign {} -> "transaction sign"
  TxCreateWitness {} -> "transaction witness"
  TxAssembleTxBodyWitness {} -> "transaction sign-witness"
  TxSubmit {} -> "transaction submit"
  TxMintedPolicyId {} -> "transaction policyid"
  TxCalculateMinFee {} -> "transaction calculate-min-fee"
  TxCalculateMinRequiredUTxO {} -> "transaction calculate-min-value"
  TxHashScriptData {} -> "transaction hash-script-data"
  TxGetTxId {} -> "transaction txid"
  TxView {} -> "transaction view"
