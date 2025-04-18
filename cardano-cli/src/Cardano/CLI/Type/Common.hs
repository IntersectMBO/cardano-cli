{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.CLI.Type.Common
  ( AllOrOnly (..)
  , AddressKeyType (..)
  , AnchorScheme (..)
  , AnyPlutusScriptVersion (..)
  , BalanceTxExecUnits (..)
  , BlockId (..)
  , ByronKeyFormat (..)
  , ByronKeyType (..)
  , CardanoAddressKeyType (..)
  , CBORObject (..)
  , CertificateFile (..)
  , ConstitutionHashSource (..)
  , ConstitutionText (..)
  , ConstitutionUrl (..)
  , CredentialGenerationMode (..)
  , CurrentKesPeriod (..)
  , DRepCredentials (..)
  , EpochLeadershipSchedule (..)
  , File (..)
  , FileDirection (..)
  , FormatBech32 (..)
  , FormatCbor (..)
  , FormatHex (..)
  , FormatJson (..)
  , FormatText (..)
  , FormatTextEnvelope (..)
  , FormatYaml (..)
  , GenesisDir (..)
  , GenesisFile (..)
  , GenesisKeyFile (..)
  , IncludeStake (..)
  , InputTxBodyOrTxFile (..)
  , MetadataFile (..)
  , MustCheckHash (..)
  , OpCertCounter
  , OpCertCounterFile
  , OpCertEndingKesPeriod (..)
  , OpCertIntervalInformation (..)
  , OpCertNodeAndOnDiskCounterInformation (..)
  , OpCertNodeStateCounter (..)
  , OpCertOnDiskCounter (..)
  , OpCertStartingKesPeriod (..)
  , Params (..)
  , ParserFileDirection (..)
  , PrivKeyFile (..)
  , ProposalBinary
  , ProposalFile
  , ProposalText
  , ProposalUrl (..)
  , ProtocolParamsFile (..)
  , ReferenceScriptAnyEra (..)
  , ReferenceScriptSize (..)
  , RequiredSigner (..)
  , ScriptDataOrFile (..)
  , ScriptFile
  , ScriptRedeemerOrFile
  , SigningKeyFile
  , SlotsTillKesKeyExpiry (..)
  , SomeKeyFile (..)
  , StakeDelegators (..)
  , StakePoolMetadataFile
  , SupportedSchemes
  , TransferDirection (..)
  , TxBodyFile
  , TxBuildOutputOptions (..)
  , TxByronWitnessCount (..)
  , TxFile
  , TxSubmissionResult (..)
  , TxTreasuryDonation (..)
  , TxInCount (..)
  , TxMempoolQuery (..)
  , TxOutAnyEra (..)
  , TxOutShelleyBasedEra (..)
  , TxOutChangeAddress (..)
  , TxOutCount (..)
  , TxOutDatumAnyEra (..)
  , TxShelleyWitnessCount (..)
  , UpdateProposalFile (..)
  , VerificationKeyBase64 (..)
  , VerificationKeyFile
  , VoteUrl (..)
  , VoteText (..)
  , VoteHashSource (..)
  , WitnessFile (..)
  , WitnessSigningData (..)
  , DRepMetadataFile
  , DRepMetadataUrl
  , ResignationMetadataUrl
  , PotentiallyCheckedAnchor (..)
  )
where

import Cardano.Api hiding (Script)
import Cardano.Api.Ledger qualified as L

import Data.Aeson (FromJSON (..), ToJSON (..), object, pairs, (.=))
import Data.Aeson qualified as Aeson
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word64)
import GHC.Generics (Generic)

-- | Determines the direction in which the MIR certificate will transfer ADA.
data TransferDirection
  = TransferToReserves
  | TransferToTreasury
  deriving Show

data OpCertCounter

newtype ConstitutionUrl = ConstitutionUrl
  { unConstitutionUrl :: L.Url
  }
  deriving (Eq, Show)

newtype ConstitutionText = ConstitutionText
  { unConstitutionText :: Text
  }
  deriving (Eq, Show)

data ConstitutionHashSource
  = ConstitutionHashSourceFile (File ConstitutionText In)
  | ConstitutionHashSourceText Text
  | ConstitutionHashSourceHash (L.SafeHash L.AnchorData)
  deriving Show

newtype ProposalUrl = ProposalUrl
  { unProposalUrl :: L.Url
  }
  deriving (Eq, Show)

-- | Specifies the schemes that are allowed to fetch anchor data.
type SupportedSchemes = [AnchorScheme]

-- | The different schemes that can be used to fetch anchor data.
data AnchorScheme = FileScheme | HttpScheme | HttpsScheme | IpfsScheme
  deriving (Show, Eq)

-- | Tag for tracking proposals submitted as 'Bytestring'
data ProposalBinary

-- | Tag for tracking proposals submitted as 'Text.Text'
data ProposalText

-- | Tag for differentiating between DRep metadata sources and
-- sources for other types of anchor data
data DRepMetadataUrl

-- | Tag for differentiating between resignation metadatata sources and
-- sources for other types of anchor data
data ResignationMetadataUrl

newtype VoteUrl = VoteUrl
  { unVoteUrl :: L.Url
  }
  deriving (Eq, Show)

newtype VoteText = VoteText
  { unVoteText :: Text
  }
  deriving (Eq, Show)

data VoteHashSource
  = VoteHashSourceFile (File VoteText In)
  | VoteHashSourceText Text
  | VoteHashSourceHash (L.SafeHash L.AnchorData)
  deriving Show

data StakeDelegators = StakeDelegators
  { stakeDelegatorsGenerationMode :: !CredentialGenerationMode
  -- ^ Whether to write them to disk
  , numOfStakeDelegators :: !Word
  -- ^ The number of stake credentials to generate
  }
  deriving Show

-- | Whether to include the stake, as queried by drep-stake-distribution, in
-- the output of drep-state. This is (computationally) expensive, but sometimes
-- convenient.
data IncludeStake = WithStake | NoStake deriving Show

data DRepCredentials = DRepCredentials
  { dRepCredentialGenerationMode :: !CredentialGenerationMode
  -- ^ Whether to write them to disk
  , numOfDRepCredentials :: !Word
  -- ^ The number of DRep credentials to generate
  }
  deriving Show

data CredentialGenerationMode
  = -- | Write credentials to disk
    OnDisk
  | -- | Don't write them to disk (process them in memory)
    Transient
  deriving (Show, Eq)

-- | Specify whether to render the script cost as JSON
-- in the cli's build command.
data TxBuildOutputOptions
  = OutputScriptCostOnly (File () Out)
  | OutputTxBodyOnly (TxBodyFile Out)
  deriving Show

-- | Specify what the CBOR file is
-- i.e a block, a tx, etc
data CBORObject
  = CBORBlockByron EpochSlots
  | CBORDelegationCertificateByron
  | CBORTxByron
  | CBORUpdateProposalByron
  | CBORVoteByron
  deriving Show

-- Encompasses stake certificates, stake pool certificates,
-- genesis delegate certificates and MIR certificates.
newtype CertificateFile = CertificateFile {unCertificateFile :: FilePath}
  deriving newtype (Eq, Show)

newtype CurrentKesPeriod = CurrentKesPeriod {unCurrentKesPeriod :: Word64} deriving (Eq, Show)

instance ToJSON CurrentKesPeriod where
  toJSON (CurrentKesPeriod k) = toJSON k

instance FromJSON CurrentKesPeriod where
  parseJSON v = CurrentKesPeriod <$> parseJSON v

newtype GenesisFile = GenesisFile
  {unGenesisFile :: FilePath}
  deriving stock (Eq, Ord)
  deriving newtype (IsString, Show)

data OpCertNodeAndOnDiskCounterInformation
  = -- | The on disk operational certificate has a counter
    -- that is equal to its corresponding counter in the
    -- node state. The on disk operational certificate therefore
    -- has a valid counter.
    OpCertOnDiskCounterEqualToNodeState
      OpCertOnDiskCounter
      OpCertNodeStateCounter
  | -- | The on disk operational certificate has a counter
    -- that is ahead of the counter in the node state by 1.
    -- The on disk operational certificate is invalid in
    -- this case.
    OpCertOnDiskCounterAheadOfNodeState
      OpCertOnDiskCounter
      OpCertNodeStateCounter
  | -- | The on disk operational certificate has a counter
    -- that is less than the counter in the node state. The
    -- on disk operational certificate is invalid in this case.
    OpCertOnDiskCounterTooFarAheadOfNodeState
      OpCertOnDiskCounter
      OpCertNodeStateCounter
  | -- | The corresponding counter for operational certificate
    -- was not found in the node state. This means the relevant
    -- stake pool has not minted a block yet. When the stake pool
    -- has minted a block the corresponding operational certificate's
    -- counter will be present in the node state.
    OpCertOnDiskCounterBehindNodeState
      OpCertOnDiskCounter
      OpCertNodeStateCounter
  | -- | The on disk operational certificate has a counter
    -- that is ahead of the counter in the node state by more
    -- than 1. The on disk operational certificate is invalid in
    -- this case.
    OpCertNoBlocksMintedYet
      OpCertOnDiskCounter
  deriving (Eq, Show)

newtype OpCertOnDiskCounter = OpCertOnDiskCounter {unOpCertOnDiskCounter :: Word64}
  deriving (Eq, Show)

instance ToJSON OpCertOnDiskCounter where
  toJSON (OpCertOnDiskCounter k) = toJSON k

instance FromJSON OpCertOnDiskCounter where
  parseJSON v = OpCertOnDiskCounter <$> parseJSON v

newtype OpCertNodeStateCounter = OpCertNodeStateCounter {unOpCertNodeStateCounter :: Word64}
  deriving (Eq, Show)

instance ToJSON OpCertNodeStateCounter where
  toJSON (OpCertNodeStateCounter k) = toJSON k

instance FromJSON OpCertNodeStateCounter where
  parseJSON v = OpCertNodeStateCounter <$> parseJSON v

newtype OpCertStartingKesPeriod = OpCertStartingKesPeriod {unOpCertStartingKesPeriod :: Word64}
  deriving (Eq, Show)

instance ToJSON OpCertStartingKesPeriod where
  toJSON (OpCertStartingKesPeriod k) = toJSON k

instance FromJSON OpCertStartingKesPeriod where
  parseJSON v = OpCertStartingKesPeriod <$> parseJSON v

newtype OpCertEndingKesPeriod = OpCertEndingKesPeriod {unOpCertEndingKesPeriod :: Word64}
  deriving (Eq, Show)

instance ToJSON OpCertEndingKesPeriod where
  toJSON (OpCertEndingKesPeriod k) = toJSON k

instance FromJSON OpCertEndingKesPeriod where
  parseJSON v = OpCertEndingKesPeriod <$> parseJSON v

data OpCertIntervalInformation
  = OpCertWithinInterval
      OpCertStartingKesPeriod
      OpCertEndingKesPeriod
      CurrentKesPeriod
      SlotsTillKesKeyExpiry
  | OpCertStartingKesPeriodIsInTheFuture
      OpCertStartingKesPeriod
      OpCertEndingKesPeriod
      CurrentKesPeriod
  | OpCertExpired
      OpCertStartingKesPeriod
      OpCertEndingKesPeriod
      CurrentKesPeriod
  | -- | Shouldn't be possible
    OpCertSomeOtherError
      OpCertStartingKesPeriod
      OpCertEndingKesPeriod
      CurrentKesPeriod
  deriving (Eq, Show)

instance FromJSON GenesisFile where
  parseJSON (Aeson.String genFp) = pure . GenesisFile $ Text.unpack genFp
  parseJSON invalid =
    error $
      "Parsing of GenesisFile failed due to type mismatch. "
        <> "Encountered: "
        <> show invalid

data AllOrOnly a = All | Only [a] deriving (Eq, Show)

-- | This data structure is used to allow nicely formatted output in the query pool-params command.
-- params are the current pool parameter settings, futureparams are new parameters, retiringEpoch is the
-- epoch that has been set for pool retirement.  Any of these may be Nothing.
data Params crypto = Params
  { poolParameters :: Maybe L.PoolParams
  , futurePoolParameters :: Maybe L.PoolParams
  , retiringEpoch :: Maybe EpochNo
  }
  deriving Show

-- | Pretty printing for pool parameters
instance L.Crypto crypto => ToJSON (Params crypto) where
  toJSON (Params p fp r) =
    object
      [ "poolParams" .= p
      , "futurePoolParams" .= fp
      , "retiring" .= r
      ]

  toEncoding (Params p fp r) =
    pairs $
      mconcat
        [ "poolParams" .= p
        , "futurePoolParams" .= fp
        , "retiring" .= r
        ]

type SigningKeyFile = File (SigningKey ())

type ProposalFile = File ()

newtype UpdateProposalFile = UpdateProposalFile {unUpdateProposalFile :: FilePath}
  deriving newtype (Eq, Show)

type VerificationKeyFile = File (VerificationKey ())

type ScriptFile = File ScriptInAnyLang In

data ScriptDataOrFile
  = -- | By reference to a CBOR file
    ScriptDataCborFile FilePath
  | -- | By reference to a JSON file
    ScriptDataJsonFile FilePath
  | -- | By value
    ScriptDataValue HashableScriptData
  deriving (Eq, Show)

type ScriptRedeemerOrFile = ScriptDataOrFile

newtype SlotsTillKesKeyExpiry = SlotsTillKesKeyExpiry {unSlotsTillKesKeyExpiry :: SlotNo}
  deriving (Eq, Show)

instance ToJSON SlotsTillKesKeyExpiry where
  toJSON (SlotsTillKesKeyExpiry k) = toJSON k

instance FromJSON SlotsTillKesKeyExpiry where
  parseJSON v = SlotsTillKesKeyExpiry <$> parseJSON v

data TxOutShelleyBasedEra
  = TxOutShelleyBasedEra
      !(Address ShelleyAddr)
      Value
      TxOutDatumAnyEra
      ReferenceScriptAnyEra
  deriving Show

-- | A TxOut value that is the superset of possibilities for any era: any
-- address type and allowing multi-asset values. This is used as the type for
-- values passed on the command line. It can be converted into the
-- era-dependent 'TxOutValue' type.
data TxOutAnyEra
  = TxOutAnyEra
      AddressAny
      Value
      TxOutDatumAnyEra
      ReferenceScriptAnyEra
  deriving (Eq, Show)

data TxOutDatumAnyEra
  = TxOutDatumByHashOnly (Hash ScriptData)
  | TxOutDatumByHashOf ScriptDataOrFile
  | TxOutDatumByValue ScriptDataOrFile
  | TxOutInlineDatumByValue ScriptDataOrFile
  | TxOutDatumByNone
  deriving (Eq, Show)

data ReferenceScriptAnyEra
  = ReferenceScriptAnyEraNone
  | ReferenceScriptAnyEra FilePath
  deriving (Eq, Show)

-- | A partially-specified transaction output indented to use as a change
-- output.
--
-- It does not specify a value, since this will be worked out automatically.
--
-- It does not use any script data hash, since that's generally not used for
-- change outputs.
newtype TxOutChangeAddress = TxOutChangeAddress AddressAny
  deriving (Eq, Show)

-- | A flag that differentiates between automatically
-- and manually balancing a tx.
data BalanceTxExecUnits = AutoBalance | ManualBalance

-- | Plutus script required signers
data RequiredSigner
  = RequiredSignerSkeyFile (SigningKeyFile In)
  | RequiredSignerHash (Hash PaymentKey)
  deriving Show

-- | Which leadership schedule we are interested in.
-- TODO: Implement Previous and Next epochs
data EpochLeadershipSchedule
  = CurrentEpoch
  | NextEpoch
  deriving Show

type TxBodyFile = File (TxBody ())

type TxFile = File (Tx ())

newtype TxTreasuryDonation = TxTreasuryDonation {unTxTreasuryDonation :: Lovelace}
  deriving Show

data TxMempoolQuery
  = TxMempoolQueryTxExists TxId
  | TxMempoolQueryNextTx
  | TxMempoolQueryInfo
  deriving Show

data FormatBech32 = FormatBech32
  deriving (Enum, Eq, Ord, Show)

data FormatHex = FormatHex
  deriving (Enum, Eq, Ord, Show)

data FormatCbor = FormatCbor
  deriving (Enum, Eq, Ord, Show)

data FormatJson = FormatJson
  deriving (Enum, Eq, Ord, Show)

data FormatText = FormatText
  deriving (Enum, Eq, Ord, Show)

data FormatTextEnvelope = FormatTextEnvelope
  deriving (Enum, Eq, Ord, Show)

data FormatYaml = FormatYaml
  deriving (Enum, Eq, Ord, Show)

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

newtype ReferenceScriptSize
  = ReferenceScriptSize {unReferenceScriptSize :: Int}
  deriving Show

newtype BlockId
  = BlockId String -- Probably not a String
  deriving Show

newtype GenesisKeyFile
  = GenesisKeyFile FilePath
  deriving Show

data MetadataFile
  = MetadataFileJSON (File () In)
  | MetadataFileCBOR (File () In)
  deriving Show

type StakePoolMetadataFile = File StakePoolMetadata

type DRepMetadataFile = File DRepMetadata

newtype GenesisDir
  = GenesisDir FilePath
  deriving Show

-- | Either a verification or signing key, used for conversions and other
-- commands that make sense for both.
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
  = ByronPaymentKey ByronKeyFormat
  | ByronGenesisKey ByronKeyFormat
  | ByronDelegateKey ByronKeyFormat
  deriving Show

data ByronKeyFormat
  = NonLegacyByronKeyFormat
  | LegacyByronKeyFormat
  deriving Show

-- | The type of @cardano-address@ key.
data CardanoAddressKeyType
  = CardanoAddressShelleyPaymentKey
  | CardanoAddressShelleyStakeKey
  | CardanoAddressIcarusPaymentKey
  | CardanoAddressByronPaymentKey
  | CardanoAddressCommitteeColdKey
  | CardanoAddressCommitteeHotKey
  | CardanoAddressDRepKey
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

data InputTxBodyOrTxFile = InputTxBodyFile (TxBodyFile In) | InputTxFile (TxFile In)
  deriving Show

data ParserFileDirection
  = Input
  | Output
  deriving (Eq, Show)

data MustCheckHash a
  = CheckHash
  | TrustHash
  deriving (Eq, Show)

data PotentiallyCheckedAnchor anchorType anchor
  = PotentiallyCheckedAnchor
  { pcaAnchor :: anchor
  -- ^ The anchor data whose hash is to be checked
  , pcaMustCheck :: MustCheckHash anchorType
  -- ^ Whether to check the hash or not (CheckHash for checking or TrustHash for not checking)
  }
  deriving (Eq, Show)

-- | Type used for serialization when printing the hash of a transaction
-- after having submitted it. Also used for printing JSON output of "transaction txid".
newtype TxSubmissionResult = TxSubmissionResult {txhash :: TxId}
  deriving (Show, Generic)

instance FromJSON TxSubmissionResult

instance ToJSON TxSubmissionResult
