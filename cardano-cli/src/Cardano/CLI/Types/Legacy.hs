{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cardano.CLI.Types.Legacy
  ( BalanceTxExecUnits (..)
  , CBORObject (..)
  , CertificateFile (..)
  , CurrentKesPeriod (..)
  , EpochLeadershipSchedule (..)
  , GenesisFile (..)
  , KeyOutputFormat(..)
  , OpCertEndingKesPeriod (..)
  , OpCertIntervalInformation (..)
  , OpCertOnDiskCounter (..)
  , OpCertNodeAndOnDiskCounterInformation (..)
  , OpCertNodeStateCounter (..)
  , OpCertStartingKesPeriod (..)
  , PoolIdOutputFormat (..)
  , TxBuildOutputOptions(..)
  , ReferenceScriptAnyEra (..)
  , SigningKeyFile
  , ScriptFile (..)
  , ScriptDataOrFile (..)
  , ScriptRedeemerOrFile
  , ScriptWitnessFiles (..)
  , ScriptDatumOrFile (..)
  , SlotsTillKesKeyExpiry (..)
  , TransferDirection(..)
  , TxBodyFile
  , TxOutAnyEra (..)
  , TxOutChangeAddress (..)
  , TxOutDatumAnyEra (..)
  , TxFile
  , TxMempoolQuery (..)
  , UpdateProposalFile (..)
  , VerificationKeyFile
  , Params (..)
  , RequiredSigner (..)
  , AllOrOnly(..)
  , File(..)
  , FileDirection (..)
  ) where

import           Cardano.Api (AddressAny, AnyScriptLanguage, EpochNo, ExecutionUnits, File (..),
                   FileDirection (..), Hash, HashableScriptData, Key (..), PaymentKey, PolicyId,
                   ScriptData, SlotNo (SlotNo), Tx, TxBody, TxId, TxIn, Value, WitCtxMint,
                   WitCtxStake, WitCtxTxIn)

import qualified Cardano.Chain.Slotting as Byron
import           Cardano.CLI.Types.Common
import qualified Cardano.Ledger.Crypto as Crypto
import           Cardano.Ledger.Shelley.TxBody (PoolParams (..))

import           Data.Aeson (FromJSON (..), ToJSON (..), object, pairs, (.=))
import qualified Data.Aeson as Aeson
import           Data.String (IsString)
import qualified Data.Text as Text
import           Data.Word (Word64)

-- | Specify whether to render the script cost as JSON
-- in the cli's build command.
data TxBuildOutputOptions = OutputScriptCostOnly (File () Out)
                          | OutputTxBodyOnly (TxBodyFile Out)
                          deriving Show


-- | Specify what the CBOR file is
-- i.e a block, a tx, etc
data CBORObject = CBORBlockByron Byron.EpochSlots
                | CBORDelegationCertificateByron
                | CBORTxByron
                | CBORUpdateProposalByron
                | CBORVoteByron
                deriving Show

-- Encompasses stake certificates, stake pool certificates,
-- genesis delegate certificates and MIR certificates.
newtype CertificateFile = CertificateFile { unCertificateFile :: FilePath }
                          deriving newtype (Eq, Show)

newtype CurrentKesPeriod = CurrentKesPeriod { unCurrentKesPeriod :: Word64 } deriving (Eq, Show)

instance ToJSON CurrentKesPeriod where
  toJSON (CurrentKesPeriod k) = toJSON k

instance FromJSON CurrentKesPeriod where
  parseJSON v = CurrentKesPeriod <$> parseJSON v

newtype GenesisFile = GenesisFile
  { unGenesisFile :: FilePath }
  deriving stock (Eq, Ord)
  deriving newtype (IsString, Show)

data OpCertNodeAndOnDiskCounterInformation
  -- | The on disk operational certificate has a counter
  -- that is equal to its corresponding counter in the
  -- node state. The on disk operational certificate therefore
  -- has a valid counter.
  = OpCertOnDiskCounterEqualToNodeState
      OpCertOnDiskCounter
      OpCertNodeStateCounter
  -- | The on disk operational certificate has a counter
  -- that is ahead of the counter in the node state by 1.
  -- The on disk operational certificate is invalid in
  -- this case.
  | OpCertOnDiskCounterAheadOfNodeState
      OpCertOnDiskCounter
      OpCertNodeStateCounter
  -- | The on disk operational certificate has a counter
  -- that is less than the counter in the node state. The
  -- on disk operational certificate is invalid in this case.
  | OpCertOnDiskCounterTooFarAheadOfNodeState
      OpCertOnDiskCounter
      OpCertNodeStateCounter
  -- | The corresponding counter for operational certificate
  -- was not found in the node state. This means the relevant
  -- stake pool has not minted a block yet. When the stake pool
  -- has minted a block the corresponding operational certificate's
  -- counter will be present in the node state.
  | OpCertOnDiskCounterBehindNodeState
      OpCertOnDiskCounter
      OpCertNodeStateCounter
  -- | The on disk operational certificate has a counter
  -- that is ahead of the counter in the node state by more
  -- than 1. The on disk operational certificate is invalid in
  -- this case.
  | OpCertNoBlocksMintedYet
      OpCertOnDiskCounter
  deriving (Eq, Show)

newtype OpCertOnDiskCounter = OpCertOnDiskCounter { unOpCertOnDiskCounter :: Word64 }
                              deriving (Eq, Show)

instance ToJSON OpCertOnDiskCounter where
  toJSON (OpCertOnDiskCounter k) = toJSON k

instance FromJSON OpCertOnDiskCounter where
  parseJSON v = OpCertOnDiskCounter <$> parseJSON v

newtype OpCertNodeStateCounter = OpCertNodeStateCounter { unOpCertNodeStateCounter :: Word64 }
                                 deriving (Eq, Show)

instance ToJSON OpCertNodeStateCounter where
  toJSON (OpCertNodeStateCounter k) = toJSON k

instance FromJSON OpCertNodeStateCounter where
  parseJSON v = OpCertNodeStateCounter <$> parseJSON v

newtype OpCertStartingKesPeriod = OpCertStartingKesPeriod { unOpCertStartingKesPeriod :: Word64 }
                                  deriving (Eq, Show)

instance ToJSON OpCertStartingKesPeriod where
  toJSON (OpCertStartingKesPeriod k) = toJSON k

instance FromJSON OpCertStartingKesPeriod where
  parseJSON v = OpCertStartingKesPeriod <$> parseJSON v

newtype OpCertEndingKesPeriod = OpCertEndingKesPeriod { unOpCertEndingKesPeriod :: Word64 }
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
  | OpCertSomeOtherError -- ^ Shouldn't be possible
      OpCertStartingKesPeriod
      OpCertEndingKesPeriod
      CurrentKesPeriod
  deriving (Eq, Show)

instance FromJSON GenesisFile where
  parseJSON (Aeson.String genFp) = pure . GenesisFile $ Text.unpack genFp
  parseJSON invalid = error $ "Parsing of GenesisFile failed due to type mismatch. "
                           <> "Encountered: " <> show invalid

-- | The desired output format.
data PoolIdOutputFormat
  = PoolIdOutputFormatHex
  | PoolIdOutputFormatBech32
  deriving (Eq, Show)

data KeyOutputFormat
  = KeyOutputFormatTextEnvelope
  | KeyOutputFormatBech32
  deriving (Eq, Show)

data AllOrOnly a = All | Only a deriving (Eq, Show)

-- | This data structure is used to allow nicely formatted output in the query pool-params command.
-- params are the current pool parameter settings, futureparams are new parameters, retiringEpoch is the
-- epoch that has been set for pool retirement.  Any of these may be Nothing.
data Params crypto = Params
  { poolParameters :: Maybe (PoolParams crypto)
  , futurePoolParameters :: Maybe (PoolParams crypto)
  , retiringEpoch :: Maybe EpochNo
  } deriving Show

-- | Pretty printing for pool parameters
instance Crypto.Crypto crypto =>  ToJSON (Params crypto) where
  toJSON (Params p fp r) = object
    [ "poolParams" .= p
    , "futurePoolParams" .= fp
    , "retiring" .= r
    ]

  toEncoding (Params p fp r) = pairs $ mconcat
    [ "poolParams" .= p
    , "futurePoolParams" .= fp
    , "retiring" .= r
    ]

type SigningKeyFile = File (SigningKey ())

newtype UpdateProposalFile = UpdateProposalFile { unUpdateProposalFile :: FilePath }
                             deriving newtype (Eq, Show)

type VerificationKeyFile = File (VerificationKey ())

newtype ScriptFile = ScriptFile { unScriptFile :: FilePath }
                     deriving (Eq, Show)

data ScriptDataOrFile = ScriptDataCborFile  FilePath   -- ^ By reference to a CBOR file
                      | ScriptDataJsonFile  FilePath   -- ^ By reference to a JSON file
                      | ScriptDataValue     HashableScriptData -- ^ By value
  deriving (Eq, Show)

type ScriptRedeemerOrFile = ScriptDataOrFile

-- | This type is like 'ScriptWitness', but the file paths from which to load
-- the script witness data representation.
--
-- It is era-independent, but witness context-dependent.
--
data ScriptWitnessFiles witctx where
     SimpleScriptWitnessFile  :: ScriptFile
                              -> ScriptWitnessFiles witctx

     PlutusScriptWitnessFiles :: ScriptFile
                              -> ScriptDatumOrFile witctx
                              -> ScriptRedeemerOrFile
                              -> ExecutionUnits
                              -> ScriptWitnessFiles witctx

     -- TODO: Need to figure out how to exclude PlutusV1 scripts at the type level
     PlutusReferenceScriptWitnessFiles
       :: TxIn
       -> AnyScriptLanguage
       -> ScriptDatumOrFile witctx
       -> ScriptRedeemerOrFile
       -> ExecutionUnits
       -> Maybe PolicyId -- ^ For minting reference scripts
       -> ScriptWitnessFiles witctx

     SimpleReferenceScriptWitnessFiles
       :: TxIn
       -> AnyScriptLanguage
       -> Maybe PolicyId -- ^ For minting reference scripts
       -> ScriptWitnessFiles witctx


deriving instance Show (ScriptWitnessFiles witctx)

data ScriptDatumOrFile witctx where
     ScriptDatumOrFileForTxIn    :: ScriptDataOrFile
                                 -> ScriptDatumOrFile WitCtxTxIn
     InlineDatumPresentAtTxIn    :: ScriptDatumOrFile WitCtxTxIn

     NoScriptDatumOrFileForMint  :: ScriptDatumOrFile WitCtxMint
     NoScriptDatumOrFileForStake :: ScriptDatumOrFile WitCtxStake

deriving instance Show (ScriptDatumOrFile witctx)

newtype SlotsTillKesKeyExpiry = SlotsTillKesKeyExpiry { unSlotsTillKesKeyExpiry :: SlotNo }
                                deriving (Eq, Show)

instance ToJSON SlotsTillKesKeyExpiry where
  toJSON (SlotsTillKesKeyExpiry k) = toJSON k

instance FromJSON SlotsTillKesKeyExpiry where
  parseJSON v = SlotsTillKesKeyExpiry <$> parseJSON v

-- | A TxOut value that is the superset of possibilities for any era: any
-- address type and allowing multi-asset values. This is used as the type for
-- values passed on the command line. It can be converted into the
-- era-dependent 'TxOutValue' type.
--
data TxOutAnyEra = TxOutAnyEra
                     AddressAny
                     Value
                     TxOutDatumAnyEra
                     ReferenceScriptAnyEra
  deriving (Eq, Show)

data TxOutDatumAnyEra = TxOutDatumByHashOnly (Hash ScriptData)
                      | TxOutDatumByHashOf    ScriptDataOrFile
                      | TxOutDatumByValue     ScriptDataOrFile
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
--
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

data TxMempoolQuery =
      TxMempoolQueryTxExists TxId
    | TxMempoolQueryNextTx
    | TxMempoolQueryInfo
  deriving Show
