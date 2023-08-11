{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Shelley CLI command types
module Cardano.CLI.Commands.Legacy
  ( -- * CLI command types
    Command (..)
  , AddressCmds (..)
  , StakeAddressCmds (..)
  , KeyCmds (..)
  , TransactionCmds (..)
  , NodeCmds (..)
  , PoolCmds (..)
  , QueryCmds (..)
  , GovernanceCmds (..)
  , GenesisCmds (..)
  , TextViewCmds (..)
  , VoteCmd(..)
  , renderLegacyCommand

    -- * CLI flag types
  , AddressKeyType (..)
  , ByronKeyType (..)
  , ByronKeyFormat (..)
  , CardanoAddressKeyType (..)
  , GenesisDir (..)
  , OpCertCounter
  , TxInCount (..)
  , TxOutCount (..)
  , TxShelleyWitnessCount (..)
  , TxByronWitnessCount (..)
  , SomeKeyFile (..)
  , OpCertCounterFile
  , ProtocolParamsFile (..)
  , WitnessFile (..)
  , TxFile
  , InputTxBodyOrTxFile (..)
  , VerificationKeyBase64 (..)
  , GenesisKeyFile (..)
  , MetadataFile (..)
  , StakePoolMetadataFile
  , PrivKeyFile (..)
  , BlockId (..)
  , WitnessSigningData (..)
  , ColdVerificationKeyOrFile (..)
  ) where

import           Cardano.Api.Shelley hiding (QueryInShelleyBasedEra (..))

import           Cardano.Chain.Common (BlockCount)
import           Cardano.CLI.EraBased.Governance
import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Governance
import           Cardano.CLI.Types.Key
import           Cardano.CLI.Types.Legacy

import           Prelude

import           Data.Text (Text)
import           Data.Time.Clock
--
-- Shelley CLI command data types
--

-- | All the CLI subcommands under \"shelley\".
--
data Command
  = AddressCmds       AddressCmds
  | StakeAddressCmds  StakeAddressCmds
  | KeyCmds           KeyCmds
  | TransactionCmds   TransactionCmds
  | NodeCmds          NodeCmds
  | PoolCmds          PoolCmds
  | QueryCmds         QueryCmds
  | GovernanceCmds    GovernanceCmds
  | GenesisCmds       GenesisCmds
  | TextViewCmds      TextViewCmds

renderLegacyCommand :: Command -> Text
renderLegacyCommand sc =
  case sc of
    AddressCmds cmd -> renderAddressCmds cmd
    StakeAddressCmds cmd -> renderStakeAddressCmds cmd
    KeyCmds cmd -> renderKeyCmds cmd
    TransactionCmds cmd -> renderTransactionCmds cmd
    NodeCmds cmd -> renderNodeCmds cmd
    PoolCmds cmd -> renderPoolCmds cmd
    QueryCmds cmd -> renderQueryCmds cmd
    GovernanceCmds cmd -> renderGovernanceCmds cmd
    GenesisCmds cmd -> renderGenesisCmds cmd
    TextViewCmds cmd -> renderTextViewCmds cmd

data AddressCmds
  = AddressKeyGen KeyOutputFormat AddressKeyType (VerificationKeyFile Out) (SigningKeyFile Out)
  | AddressKeyHash VerificationKeyTextOrFile (Maybe (File () Out))
  | AddressBuild
      PaymentVerifier
      (Maybe StakeIdentifier)
      NetworkId
      (Maybe (File () Out))
  | AddressInfo Text (Maybe (File () Out))
  deriving Show


renderAddressCmds :: AddressCmds -> Text
renderAddressCmds cmd =
  case cmd of
    AddressKeyGen {} -> "address key-gen"
    AddressKeyHash {} -> "address key-hash"
    AddressBuild {} -> "address build"
    AddressInfo {} -> "address info"

data StakeAddressCmds
  = StakeAddressKeyGen KeyOutputFormat (VerificationKeyFile Out) (SigningKeyFile Out)
  | StakeAddressKeyHash (VerificationKeyOrFile StakeKey) (Maybe (File () Out))
  | StakeAddressBuild StakeVerifier NetworkId (Maybe (File () Out))
  | StakeRegistrationCert
      AnyShelleyBasedEra
      StakeIdentifier
      (Maybe Lovelace)
      (File () Out)
  | StakeCredentialDelegationCert
      AnyShelleyBasedEra
      StakeIdentifier
      DelegationTarget
      (File () Out)
  | StakeCredentialDeRegistrationCert
      AnyShelleyBasedEra
      StakeIdentifier
      (Maybe Lovelace)
      (File () Out)
  deriving Show

renderStakeAddressCmds :: StakeAddressCmds -> Text
renderStakeAddressCmds cmd =
  case cmd of
    StakeAddressKeyGen {} -> "stake-address key-gen"
    StakeAddressKeyHash {} -> "stake-address key-hash"
    StakeAddressBuild {} -> "stake-address build"
    StakeRegistrationCert {} -> "stake-address registration-certificate"
    StakeCredentialDelegationCert {} -> "stake-address delegation-certificate"
    StakeCredentialDeRegistrationCert {} -> "stake-address deregistration-certificate"

data KeyCmds
  = KeyGetVerificationKey (SigningKeyFile In) (VerificationKeyFile Out)
  | KeyNonExtendedKey  (VerificationKeyFile In) (VerificationKeyFile Out)
  | KeyConvertByronKey (Maybe Text) ByronKeyType (SomeKeyFile In) (File () Out)
  | KeyConvertByronGenesisVKey VerificationKeyBase64 (File () Out)
  | KeyConvertITNStakeKey (SomeKeyFile In) (File () Out)
  | KeyConvertITNExtendedToStakeKey (SomeKeyFile In) (File () Out)
  | KeyConvertITNBip32ToStakeKey (SomeKeyFile In) (File () Out)
  | KeyConvertCardanoAddressSigningKey CardanoAddressKeyType (SigningKeyFile In) (File () Out)
  deriving Show

renderKeyCmds :: KeyCmds -> Text
renderKeyCmds cmd =
  case cmd of
    KeyGetVerificationKey {} -> "key verification-key"
    KeyNonExtendedKey {} -> "key non-extended-key"
    KeyConvertByronKey {} -> "key convert-byron-key"
    KeyConvertByronGenesisVKey {} -> "key convert-byron-genesis-key"
    KeyConvertITNStakeKey {} -> "key convert-itn-key"
    KeyConvertITNExtendedToStakeKey {} -> "key convert-itn-extended-key"
    KeyConvertITNBip32ToStakeKey {} -> "key convert-itn-bip32-key"
    KeyConvertCardanoAddressSigningKey {} -> "key convert-cardano-address-signing-key"

data TransactionCmds
  = TxBuildRaw
      AnyCardanoEra
      (Maybe ScriptValidity) -- ^ Mark script as expected to pass or fail validation
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
      [NewConstitutionFile In]
      TxBuildOutputOptions
  | TxSign InputTxBodyOrTxFile [WitnessSigningData] (Maybe NetworkId) (TxFile Out)
  | TxCreateWitness (TxBodyFile In) WitnessSigningData (Maybe NetworkId) (File () Out)
  | TxAssembleTxBodyWitness (TxBodyFile In) [WitnessFile] (File () Out)
  | TxSubmit SocketPath AnyConsensusModeParams NetworkId FilePath
  | TxMintedPolicyId ScriptFile
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
  | TxGetTxId InputTxBodyOrTxFile
  | TxView InputTxBodyOrTxFile

data InputTxBodyOrTxFile = InputTxBodyFile (TxBodyFile In) | InputTxFile (TxFile In)
  deriving Show

renderTransactionCmds :: TransactionCmds -> Text
renderTransactionCmds cmd =
  case cmd of
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

data NodeCmds
  = NodeKeyGenCold KeyOutputFormat (VerificationKeyFile Out) (SigningKeyFile Out) (OpCertCounterFile Out)
  | NodeKeyGenKES  KeyOutputFormat (VerificationKeyFile Out) (SigningKeyFile Out)
  | NodeKeyGenVRF  KeyOutputFormat (VerificationKeyFile Out) (SigningKeyFile Out)
  | NodeKeyHashVRF  (VerificationKeyOrFile VrfKey) (Maybe (File () Out))
  | NodeNewCounter ColdVerificationKeyOrFile Word (OpCertCounterFile InOut)
  | NodeIssueOpCert (VerificationKeyOrFile KesKey) (SigningKeyFile In) (OpCertCounterFile InOut)
                    KESPeriod (File () Out)
  deriving Show

renderNodeCmds :: NodeCmds -> Text
renderNodeCmds cmd = do
  case cmd of
    NodeKeyGenCold {} -> "node key-gen"
    NodeKeyGenKES {} -> "node key-gen-KES"
    NodeKeyGenVRF {} -> "node key-gen-VRF"
    NodeKeyHashVRF {} -> "node key-hash-VRF"
    NodeNewCounter {} -> "node new-counter"
    NodeIssueOpCert{} -> "node issue-op-cert"

data PoolCmds
  = PoolRegistrationCert
      AnyShelleyBasedEra
      -- ^ Era in which to register the stake pool.
      (VerificationKeyOrFile StakePoolKey)
      -- ^ Stake pool verification key.
      (VerificationKeyOrFile VrfKey)
      -- ^ VRF Verification key.
      Lovelace
      -- ^ Pool pledge.
      Lovelace
      -- ^ Pool cost.
      Rational
      -- ^ Pool margin.
      (VerificationKeyOrFile StakeKey)
      -- ^ Reward account verification staking key.
      [VerificationKeyOrFile StakeKey]
      -- ^ Pool owner verification staking key(s).
      [StakePoolRelay]
      -- ^ Stake pool relays.
      (Maybe StakePoolMetadataReference)
      -- ^ Stake pool metadata.
      NetworkId
      (File () Out)
  | PoolRetirementCert
      AnyShelleyBasedEra
      -- ^ Era in which to retire the stake pool.
      (VerificationKeyOrFile StakePoolKey)
      -- ^ Stake pool verification key.
      EpochNo
      -- ^ Epoch in which to retire the stake pool.
      (File () Out)
  | PoolGetId (VerificationKeyOrFile StakePoolKey) PoolIdOutputFormat (Maybe (File () Out))
  | PoolMetadataHash (StakePoolMetadataFile In) (Maybe (File () Out))
  deriving Show

renderPoolCmds :: PoolCmds -> Text
renderPoolCmds cmd =
  case cmd of
    PoolRegistrationCert {} -> "stake-pool registration-certificate"
    PoolRetirementCert {} -> "stake-pool deregistration-certificate"
    PoolGetId {} -> "stake-pool id"
    PoolMetadataHash {} -> "stake-pool metadata-hash"

data QueryCmds =
    QueryLeadershipSchedule
      SocketPath
      AnyConsensusModeParams
      NetworkId
      GenesisFile
      (VerificationKeyOrHashOrFile StakePoolKey)
      (SigningKeyFile In)
      EpochLeadershipSchedule
      (Maybe (File () Out))
  | QueryProtocolParameters' SocketPath AnyConsensusModeParams NetworkId (Maybe (File () Out))
  | QueryConstitutionHash SocketPath AnyConsensusModeParams NetworkId (Maybe (File () Out))
  | QueryTip SocketPath AnyConsensusModeParams NetworkId (Maybe (File () Out))
  | QueryStakePools' SocketPath AnyConsensusModeParams NetworkId (Maybe (File () Out))
  | QueryStakeDistribution' SocketPath AnyConsensusModeParams NetworkId (Maybe (File () Out))
  | QueryStakeAddressInfo SocketPath AnyConsensusModeParams StakeAddress NetworkId (Maybe (File () Out))
  | QueryUTxO' SocketPath AnyConsensusModeParams QueryUTxOFilter NetworkId (Maybe (File () Out))
  | QueryDebugLedgerState' SocketPath AnyConsensusModeParams NetworkId (Maybe (File () Out))
  | QueryProtocolState' SocketPath AnyConsensusModeParams NetworkId (Maybe (File () Out))
  | QueryStakeSnapshot'
      SocketPath
      AnyConsensusModeParams
      NetworkId
      (AllOrOnly [Hash StakePoolKey])
      (Maybe (File () Out))
  | QueryKesPeriodInfo
      SocketPath
      AnyConsensusModeParams
      NetworkId
      (File () In)
      -- ^ Node operational certificate
      (Maybe (File () Out))
  | QueryPoolState' SocketPath AnyConsensusModeParams NetworkId [Hash StakePoolKey]
  | QueryTxMempool SocketPath AnyConsensusModeParams NetworkId TxMempoolQuery (Maybe (File () Out))
  | QuerySlotNumber SocketPath AnyConsensusModeParams NetworkId UTCTime
  deriving Show

renderQueryCmds :: QueryCmds -> Text
renderQueryCmds cmd =
  case cmd of
    QueryLeadershipSchedule {} -> "query leadership-schedule"
    QueryProtocolParameters' {} -> "query protocol-parameters "
    QueryConstitutionHash {} -> "query constitution-hash "
    QueryTip {} -> "query tip"
    QueryStakePools' {} -> "query stake-pools"
    QueryStakeDistribution' {} -> "query stake-distribution"
    QueryStakeAddressInfo {} -> "query stake-address-info"
    QueryUTxO' {} -> "query utxo"
    QueryDebugLedgerState' {} -> "query ledger-state"
    QueryProtocolState' {} -> "query protocol-state"
    QueryStakeSnapshot' {} -> "query stake-snapshot"
    QueryKesPeriodInfo {} -> "query kes-period-info"
    QueryPoolState' {} -> "query pool-state"
    QueryTxMempool _ _ _ query _ -> "query tx-mempool" <> renderTxMempoolQuery query
    QuerySlotNumber {} -> "query slot-number"
  where
    renderTxMempoolQuery query =
      case query of
        TxMempoolQueryTxExists tx -> "tx-exists " <> serialiseToRawBytesHexText tx
        TxMempoolQueryNextTx -> "next-tx"
        TxMempoolQueryInfo -> "info"


data TextViewCmds
  = TextViewInfo !FilePath (Maybe (File () Out))
  deriving Show


renderTextViewCmds :: TextViewCmds -> Text
renderTextViewCmds (TextViewInfo _ _) = "text-view decode-cbor"

data GenesisCmds
  = GenesisCreate
      KeyOutputFormat
      GenesisDir
      Word
      Word
      (Maybe SystemStart)
      (Maybe Lovelace)
      NetworkId
  | GenesisCreateCardano GenesisDir Word Word (Maybe SystemStart) (Maybe Lovelace) BlockCount Word Rational NetworkId FilePath FilePath FilePath FilePath (Maybe FilePath)
  | GenesisCreateStaked
      KeyOutputFormat
      GenesisDir
      Word
      Word
      Word
      Word
      (Maybe SystemStart)
      (Maybe Lovelace)
      Lovelace
      NetworkId
      Word
      Word
      Word
      (Maybe FilePath) -- ^ Relay specification filepath
  | GenesisKeyGenGenesis (VerificationKeyFile Out) (SigningKeyFile Out)
  | GenesisKeyGenDelegate (VerificationKeyFile Out) (SigningKeyFile Out) (OpCertCounterFile Out)
  | GenesisKeyGenUTxO (VerificationKeyFile Out) (SigningKeyFile Out)
  | GenesisCmdKeyHash (VerificationKeyFile In)
  | GenesisVerKey (VerificationKeyFile Out) (SigningKeyFile In)
  | GenesisTxIn (VerificationKeyFile In) NetworkId (Maybe (File () Out))
  | GenesisAddr (VerificationKeyFile In) NetworkId (Maybe (File () Out))
  | GenesisHashFile GenesisFile
  deriving Show

renderGenesisCmds :: GenesisCmds -> Text
renderGenesisCmds cmd =
  case cmd of
    GenesisCreate {} -> "genesis create"
    GenesisCreateCardano {} -> "genesis create-cardano"
    GenesisCreateStaked {} -> "genesis create-staked"
    GenesisKeyGenGenesis {} -> "genesis key-gen-genesis"
    GenesisKeyGenDelegate {} -> "genesis key-gen-delegate"
    GenesisKeyGenUTxO {} -> "genesis key-gen-utxo"
    GenesisCmdKeyHash {} -> "genesis key-hash"
    GenesisVerKey {} -> "genesis get-ver-key"
    GenesisTxIn {} -> "genesis initial-txin"
    GenesisAddr {} -> "genesis initial-addr"
    GenesisHashFile {} -> "genesis hash"

--
-- Shelley CLI flag/option data types
--

newtype ProtocolParamsFile
  = ProtocolParamsFile FilePath
  deriving (Show, Eq)

newtype TxInCount
  = TxInCount Int
  deriving Show

newtype TxOutCount
  = TxOutCount Int
  deriving Show

newtype TxShelleyWitnessCount
  = TxShelleyWitnessCount Int
  deriving Show

newtype TxByronWitnessCount
  = TxByronWitnessCount Int
  deriving Show

newtype BlockId
  = BlockId String -- Probably not a String
  deriving Show

newtype GenesisKeyFile
  = GenesisKeyFile FilePath
  deriving Show

data MetadataFile = MetadataFileJSON (File () In)
                  | MetadataFileCBOR (File () In)

  deriving Show

type StakePoolMetadataFile = File StakePoolMetadata

newtype GenesisDir
  = GenesisDir FilePath
  deriving Show

-- | Either a verification or signing key, used for conversions and other
-- commands that make sense for both.
--
data SomeKeyFile direction
  = AVerificationKeyFile (VerificationKeyFile direction)
  | ASigningKeyFile (SigningKeyFile direction)
  deriving Show

data AddressKeyType
  = AddressKeyShelley
  | AddressKeyShelleyExtended
  | AddressKeyByron
  deriving Show

data ByronKeyType
  = ByronPaymentKey  ByronKeyFormat
  | ByronGenesisKey  ByronKeyFormat
  | ByronDelegateKey ByronKeyFormat
  deriving Show

data ByronKeyFormat = NonLegacyByronKeyFormat
                    | LegacyByronKeyFormat
  deriving Show

-- | The type of @cardano-address@ key.
data CardanoAddressKeyType
  = CardanoAddressShelleyPaymentKey
  | CardanoAddressShelleyStakeKey
  | CardanoAddressIcarusPaymentKey
  | CardanoAddressByronPaymentKey
  deriving Show

type OpCertCounterFile = File OpCertCounter

newtype PrivKeyFile
  = PrivKeyFile FilePath
  deriving Show

newtype WitnessFile
  = WitnessFile FilePath
  deriving Show

-- | A raw verification key given in Base64, and decoded into a ByteString.
newtype VerificationKeyBase64
  = VerificationKeyBase64 String
  deriving Show

-- | Data required to construct a witness.
data WitnessSigningData
  = KeyWitnessSigningData
      !(SigningKeyFile In)
      -- ^ Path to a file that should contain a signing key.
      !(Maybe (Address ByronAddr))
      -- ^ An optionally specified Byron address.
      --
      -- If specified, both the network ID and derivation path are extracted
      -- from the address and used in the construction of the Byron witness.
  deriving Show
