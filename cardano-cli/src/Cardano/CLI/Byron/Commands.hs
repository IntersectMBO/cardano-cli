{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Cardano.CLI.Byron.Commands
  ( ByronCommand (..)
  , NodeCmds (..)
  , VerificationKeyFile
  , NewVerificationKeyFile (..)
  , CertificateFile (..)
  , NewCertificateFile (..)
  )
where

import Cardano.Api hiding (GenesisParameters)
import Cardano.Api.Byron qualified as Byron

import Cardano.CLI.Byron.Genesis
import Cardano.CLI.Byron.Key
import Cardano.CLI.Byron.Tx
import Cardano.CLI.Types.Common

import Data.String (IsString)

data ByronCommand
  = --- Node Related Commands ---
    NodeCmds
      NodeCmds
  | --- Genesis Related Commands ---
    Genesis
      NewDirectory
      GenesisParameters
  | PrintGenesisHash
      GenesisFile
  | --- Key Related Commands ---
    Keygen
      NewSigningKeyFile
  | ToVerification
      ByronKeyFormat
      (SigningKeyFile In)
      NewVerificationKeyFile
  | PrettySigningKeyPublic
      ByronKeyFormat
      (SigningKeyFile In)
  | MigrateDelegateKeyFrom
      (SigningKeyFile In)
      -- ^ Old key
      NewSigningKeyFile
      -- ^ New Key
  | PrintSigningKeyAddress
      ByronKeyFormat
      NetworkId
      (SigningKeyFile In)
  | -----------------------------------

    -- | Filepath of transaction to submit.
    SubmitTx
      SocketPath
      NetworkId
      (TxFile In)
  | SpendGenesisUTxO
      GenesisFile
      NetworkId
      ByronKeyFormat
      NewTxFile
      -- ^ Filepath of the newly created transaction.
      (SigningKeyFile In)
      -- ^ Signing key of genesis UTxO owner.
      (Address ByronAddr)
      -- ^ Genesis UTxO address.
      [TxOut CtxTx ByronEra]
      -- ^ Tx output.
  | SpendUTxO
      NetworkId
      ByronKeyFormat
      NewTxFile
      -- ^ Filepath of the newly created transaction.
      (SigningKeyFile In)
      -- ^ Signing key of Tx underwriter.
      [TxIn]
      -- ^ Inputs available for spending to the Tx underwriter's key.
      [TxOut CtxTx ByronEra]
      -- ^ Genesis UTxO output Address.
  | GetTxId (TxFile In)
  | --- Misc Commands ---

    ValidateCBOR
      CBORObject
      -- ^ Type of the CBOR object
      FilePath
  | PrettyPrintCBOR
      FilePath
  deriving Show

data NodeCmds
  = CreateVote
      NetworkId
      (SigningKeyFile In)
      FilePath
      -- ^ filepath to update proposal
      Bool
      FilePath
  | UpdateProposal
      NetworkId
      (SigningKeyFile In)
      Byron.ProtocolVersion
      Byron.SoftwareVersion
      Byron.SystemTag
      Byron.InstallerHash
      FilePath
      Byron.ByronProtocolParametersUpdate
  | -- | Update proposal filepath.
    SubmitUpdateProposal
      SocketPath
      NetworkId
      FilePath
  | -- | Vote filepath.
    SubmitVote
      SocketPath
      NetworkId
      FilePath
  deriving Show

newtype NewCertificateFile
  = NewCertificateFile {nFp :: FilePath}
  deriving (Eq, Show, IsString)
