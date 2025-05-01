{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.Genesis.Command
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
  )
where

import Cardano.Api.Experimental qualified as Exp
import Cardano.Api.Ledger (Coin)
import Cardano.Api.Shelley

import Cardano.CLI.Type.Common
import Cardano.Ledger.BaseTypes (NonZero)

import Data.Text (Text)
import Data.Word (Word64)
import Vary

data GenesisCmds era
  = GenesisCreate !(GenesisCreateCmdArgs era)
  | GenesisCreateCardano !(GenesisCreateCardanoCmdArgs era)
  | GenesisCreateStaked !(GenesisCreateStakedCmdArgs era)
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

data GenesisCreateCmdArgs era = GenesisCreateCmdArgs
  { eon :: !(ShelleyBasedEra era)
  , keyOutputFormat :: !(Vary [FormatBech32, FormatTextEnvelope])
  , genesisDir :: !GenesisDir
  , numGenesisKeys :: !Word
  , numUTxOKeys :: !Word
  , mSystemStart :: !(Maybe SystemStart)
  , mSupply :: !(Maybe Coin)
  , network :: !NetworkId
  }
  deriving Show

data GenesisCreateCardanoCmdArgs era = GenesisCreateCardanoCmdArgs
  { eon :: !(ShelleyBasedEra era)
  , genesisDir :: !GenesisDir
  , numGenesisKeys :: !Word
  , numUTxOKeys :: !Word
  , mSystemStart :: !(Maybe SystemStart)
  , mSupply :: !(Maybe Coin)
  , security :: !(NonZero Word64)
  , slotLength :: !Word
  , slotCoeff :: !Rational
  , network :: !NetworkId
  , byronGenesisTemplate :: !FilePath
  , shelleyGenesisTemplate :: !FilePath
  , alonzoGenesisTemplate :: !FilePath
  , conwayGenesisTemplate :: !FilePath
  , mNodeConfigTemplate :: !(Maybe FilePath)
  }
  deriving Show

data GenesisCreateStakedCmdArgs era = GenesisCreateStakedCmdArgs
  { eon :: !(ShelleyBasedEra era)
  , keyOutputFormat :: !(Vary [FormatBech32, FormatTextEnvelope])
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
  , mStakePoolRelaySpecFile :: !(Maybe FilePath)
  -- ^ Relay specification filepath
  }
  deriving Show

-- TODO This existential type parameter should become a regular type parameter
-- when we parameterize the parent type by the experimental era API.
data GenesisCreateTestNetDataCmdArgs = forall era. GenesisCreateTestNetDataCmdArgs
  { eon :: !(Exp.Era era)
  , specShelley :: !(Maybe FilePath)
  -- ^ Path to the @genesis-shelley@ file to use. If unspecified, a default one will be used.
  , specAlonzo :: !(Maybe FilePath)
  -- ^ Path to the @genesis-alonzo@ file to use. If unspecified, a default one will be used.
  , specConway :: !(Maybe FilePath)
  -- ^ Path to the @genesis-conway@ file to use. If unspecified, a default one will be used.
  , numGenesisKeys :: !Word
  -- ^ The number of genesis keys credentials to create and write to disk.
  , numPools :: !Word
  -- ^ The number of stake pools credentials to create and write to disk.
  , stakeDelegators :: !StakeDelegators
  -- ^ The number of members of the constitutional committee
  , numCommitteeKeys :: !Word
  -- ^ The number of delegators to pools and DReps to create.
  , numDRepKeys :: !DRepCredentials
  -- ^ The number of DRep keys to create. They are registered and get delegated to by stake delegators
  , numStuffedUtxo :: !Word
  -- ^ The number of UTxO accounts to make. They are "stuffed" because the credentials are not written to disk.
  , numUtxoKeys :: !Word
  -- ^ The number of UTxO credentials to create and write to disk.
  , totalSupply :: !(Maybe Coin)
  -- ^ The total number of Lovelace
  , delegatedSupply :: !(Maybe Coin)
  -- ^ The number of Lovelace being delegated
  , networkId :: !(Maybe NetworkId)
  -- ^ The network ID to use. Overrides the network id supplied in the spec file.
  , relays :: !(Maybe FilePath)
  -- ^ Filepath of the specification of relays
  , systemStart :: !(Maybe SystemStart)
  -- ^ The genesis start time.
  , outputDir :: !FilePath
  -- ^ Directory where to write credentials and files.
  }

instance Show GenesisCreateTestNetDataCmdArgs where
  show _ = "GenesisCreateTestNetDataCmdArgs"

data GenesisKeyGenGenesisCmdArgs = GenesisKeyGenGenesisCmdArgs
  { verificationKeyPath :: !(VerificationKeyFile Out)
  , signingKeyPath :: !(SigningKeyFile Out)
  }
  deriving Show

data GenesisKeyGenDelegateCmdArgs = GenesisKeyGenDelegateCmdArgs
  { verificationKeyPath :: !(VerificationKeyFile Out)
  , signingKeyPath :: !(SigningKeyFile Out)
  , opCertCounterPath :: !(OpCertCounterFile Out)
  }
  deriving Show

data GenesisKeyGenUTxOCmdArgs = GenesisKeyGenUTxOCmdArgs
  { verificationKeyPath :: !(VerificationKeyFile Out)
  , signingKeyPath :: !(SigningKeyFile Out)
  }
  deriving Show

data GenesisVerKeyCmdArgs = GenesisVerKeyCmdArgs
  { verificationKeyPath :: !(VerificationKeyFile Out)
  , signingKeyPath :: !(SigningKeyFile In)
  }
  deriving Show

data GenesisTxInCmdArgs = GenesisTxInCmdArgs
  { verificationKeyPath :: !(VerificationKeyFile In)
  , network :: !NetworkId
  , mOutFile :: !(Maybe (File () Out))
  }
  deriving Show

data GenesisAddrCmdArgs = GenesisAddrCmdArgs
  { verificationKeyPath :: !(VerificationKeyFile In)
  , network :: !NetworkId
  , mOutFile :: !(Maybe (File () Out))
  }
  deriving Show

renderGenesisCmds :: GenesisCmds era -> Text
renderGenesisCmds = \case
  GenesisCreate{} ->
    "genesis create"
  GenesisCreateCardano{} ->
    "genesis create-cardano"
  GenesisCreateStaked{} ->
    "genesis create-staked"
  GenesisCreateTestNetData{} ->
    "genesis create-testnet-data"
  GenesisKeyGenGenesis{} ->
    "genesis key-gen-genesis"
  GenesisKeyGenDelegate{} ->
    "genesis key-gen-delegate"
  GenesisKeyGenUTxO{} ->
    "genesis key-gen-utxo"
  GenesisCmdKeyHash{} ->
    "genesis key-hash"
  GenesisVerKey{} ->
    "genesis get-ver-key"
  GenesisTxIn{} ->
    "genesis initial-txin"
  GenesisAddr{} ->
    "genesis initial-addr"
  GenesisHashFile{} ->
    "genesis hash"
