{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.Commands.Genesis
  ( GenesisCmds (..)
  , GenesisCreateCmdArgs (..)
  , GenesisCreateCardanoCmdArgs (..)
  , GenesisCreateStakedCmdArgs (..)
  , GenesisCreateTestNetDataCmdArgs (..)
  , GenesisKeyGenGenesisCmdArgs (..)
  , GenesisKeyGenDelegateCmdArgs (..)
  , GenesisKeyGenUTxOCmdArgs (..)
  , GenesisVerKeyCmdArgs (..)
  , GenesisTxInCmdArgs (..)
  , GenesisAddrCmdArgs (..)
  , renderGenesisCmds
  ) where

import           Cardano.Api.Ledger (Coin)
import           Cardano.Api.Shelley

import           Cardano.Chain.Common (BlockCount)
import           Cardano.CLI.Types.Common

import           Data.Text (Text)

data GenesisCmds era
  = GenesisCreate !GenesisCreateCmdArgs
  | GenesisCreateCardano !GenesisCreateCardanoCmdArgs
  | GenesisCreateStaked !GenesisCreateStakedCmdArgs
  | GenesisCreateTestNetData !GenesisCreateTestNetDataCmdArgs
  | GenesisKeyGenGenesis !GenesisKeyGenGenesisCmdArgs
  | GenesisKeyGenDelegate !GenesisKeyGenDelegateCmdArgs
  | GenesisKeyGenUTxO !GenesisKeyGenUTxOCmdArgs
  | GenesisCmdKeyHash !(VerificationKeyFile In)
  | GenesisVerKey !GenesisVerKeyCmdArgs
  | GenesisTxIn !GenesisTxInCmdArgs
  | GenesisAddr !GenesisAddrCmdArgs
  | GenesisHashFile !GenesisFile
  deriving Show

data GenesisCreateCmdArgs = GenesisCreateCmdArgs
  { keyOutputFormat :: !KeyOutputFormat
  , genesisDir :: !GenesisDir
  , numGenesisKeys :: !Word
  , numUTxOKeys :: !Word
  , mSystemStart :: !(Maybe SystemStart)
  , mSupply :: !(Maybe Coin)
  , network :: !NetworkId
  } deriving Show

data GenesisCreateCardanoCmdArgs = GenesisCreateCardanoCmdArgs
  { genesisDir :: !GenesisDir
  , numGenesisKeys :: !Word
  , numUTxOKeys :: !Word
  , mSystemStart :: !(Maybe SystemStart)
  , mSupply :: !(Maybe Coin)
  , security :: !BlockCount
  , slotLength :: !Word
  , slotCoeff :: !Rational
  , network :: !NetworkId
  , byronGenesisTemplate :: !FilePath
  , shelleyGenesisTemplate :: !FilePath
  , alonzoGenesisTemplate :: !FilePath
  , conwayGenesisTemplate :: !FilePath
  , mNodeConfigTemplate :: !(Maybe FilePath)
  } deriving Show

data GenesisCreateStakedCmdArgs = GenesisCreateStakedCmdArgs
  { keyOutputFormat :: !KeyOutputFormat
  , genesisDir :: !GenesisDir
  , numGenesisKeys :: !Word
  , numUTxOKeys :: !Word
  , numPools :: !Word
  , numStakeDelegators :: !Word
  , mSystemStart :: !(Maybe SystemStart)
  , mNonDelegatedSupply :: !(Maybe Coin)
  , delegatedSupply :: !Coin
  , network :: !NetworkId
  , numBulkPoolCredFiles :: !Word
  , numBulkPoolsPerFile :: !Word
  , numStuffedUtxo :: !Word
  , mStakePoolRelaySpecFile :: !(Maybe FilePath) -- ^ Relay specification filepath
  } deriving Show

data GenesisCreateTestNetDataCmdArgs = GenesisCreateTestNetDataCmdArgs
  { specShelley :: !(Maybe FilePath) -- ^ Path to the @genesis-shelley@ file to use. If unspecified, a default one will be used if omitted.
  , numGenesisKeys :: !Word -- ^ The number of genesis keys credentials to create and write to disk.
  , numPools :: !Word -- ^ The number of stake pools credentials to create and write to disk.
  , stakeDelegators :: !StakeDelegators -- ^ The number of delegators to pools to create.
  , numDrepKeys :: !Word -- ^ The number of DRep keys to create. Right now they receive neither delegation nor are registrated. This will come later.
  , numStuffedUtxo :: !Word -- ^ The number of UTxO accounts to make. They are "stuffed" because the credentials are not written to disk.
  , numUtxoKeys :: !Word -- ^ The number of UTxO credentials to create and write to disk.
  , totalSupply :: !(Maybe Coin) -- ^ The total number of Lovelace
  , delegatedSupply :: !(Maybe Coin) -- ^ The number of Lovelace being delegated
  , networkId :: !(Maybe NetworkId) -- ^ The network ID to use. Overrides the network id supplied in the spec file.
  , relays :: !(Maybe FilePath) -- ^ Filepath of the specification of relays
  , systemStart :: !(Maybe SystemStart) -- ^ The genesis start time.
  , outputDir :: !FilePath -- ^ Directory where to write credentials and files.
  } deriving Show

data GenesisKeyGenGenesisCmdArgs = GenesisKeyGenGenesisCmdArgs
  { verificationKeyPath :: !(VerificationKeyFile Out)
  , signingKeyPath :: !(SigningKeyFile Out)
  } deriving Show

data GenesisKeyGenDelegateCmdArgs = GenesisKeyGenDelegateCmdArgs
  { verificationKeyPath :: !(VerificationKeyFile Out)
  , signingKeyPath :: !(SigningKeyFile Out)
  , opCertCounterPath :: !(OpCertCounterFile Out)
  } deriving Show

data GenesisKeyGenUTxOCmdArgs = GenesisKeyGenUTxOCmdArgs
  { verificationKeyPath :: !(VerificationKeyFile Out)
  , signingKeyPath :: !(SigningKeyFile Out)
  } deriving Show

data GenesisVerKeyCmdArgs = GenesisVerKeyCmdArgs
  { verificationKeyPath :: !(VerificationKeyFile Out)
  , signingKeyPath :: !(SigningKeyFile In)
  } deriving Show

data GenesisTxInCmdArgs = GenesisTxInCmdArgs
  { verificationKeyPath :: !(VerificationKeyFile In)
  , network :: !NetworkId
  , mOutFile :: !(Maybe (File () Out))
  } deriving Show

data GenesisAddrCmdArgs = GenesisAddrCmdArgs
  { verificationKeyPath :: !(VerificationKeyFile In)
  , network :: !NetworkId
  , mOutFile :: !(Maybe (File () Out))
  } deriving Show

renderGenesisCmds :: GenesisCmds era -> Text
renderGenesisCmds = \case
  GenesisCreate {} ->
    "genesis create"
  GenesisCreateCardano {} ->
    "genesis create-cardano"
  GenesisCreateStaked {} ->
    "genesis create-staked"
  GenesisCreateTestNetData {} ->
    "genesis create-testnet-data"
  GenesisKeyGenGenesis {} ->
    "genesis key-gen-genesis"
  GenesisKeyGenDelegate {} ->
    "genesis key-gen-delegate"
  GenesisKeyGenUTxO {} ->
    "genesis key-gen-utxo"
  GenesisCmdKeyHash {} ->
    "genesis key-hash"
  GenesisVerKey {} ->
    "genesis get-ver-key"
  GenesisTxIn {} ->
    "genesis initial-txin"
  GenesisAddr {} ->
    "genesis initial-addr"
  GenesisHashFile {} ->
    "genesis hash"
