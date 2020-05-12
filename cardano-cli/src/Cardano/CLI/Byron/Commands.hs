module Cardano.CLI.Byron.Commands
  ( ByronCommand (..)
  , NodeCmd(..)
  ) where

import           Cardano.Prelude

import           Cardano.Chain.Slotting (EpochNumber(..))
import           Cardano.Chain.Update
                   (InstallerHash(..), ProtocolVersion(..), SoftwareVersion(..),
                    SystemTag(..))

import           Cardano.Config.Types

import           Cardano.CLI.Byron.UpdateProposal

import           Cardano.CLI.Delegation
import           Cardano.CLI.Genesis
import           Cardano.CLI.Key
import           Cardano.CLI.Ops (CardanoEra(..))
import           Cardano.CLI.Tx

import           Cardano.Chain.Common (Address(..), NetworkMagic(..))
import           Cardano.Chain.UTxO (TxIn(..), TxOut(..))

data ByronCommand =

  --- Node Related Commands ---
    NodeCmd NodeCmd

  --- Genesis Related Commands ---
  | Genesis
        NewDirectory
        GenesisParameters
        CardanoEra
  | PrintGenesisHash
        GenesisFile

  --- Key Related Commands ---
  | Keygen
        CardanoEra
        NewSigningKeyFile
        PasswordRequirement
  | ToVerification
        CardanoEra
        SigningKeyFile
        NewVerificationKeyFile

  | PrettySigningKeyPublic
        CardanoEra
        SigningKeyFile

  | MigrateDelegateKeyFrom
        CardanoEra
        -- ^ Old CardanoEra
        SigningKeyFile
        -- ^ Old key
        CardanoEra
        -- ^ New CardanoEra
        NewSigningKeyFile
        -- ^ New Key

  | PrintSigningKeyAddress
        CardanoEra
        NetworkMagic  -- TODO:  consider deprecation in favor of ProtocolMagicId,
                      --        once Byron is out of the picture.
        SigningKeyFile

    --- Delegation Related Commands ---

  | IssueDelegationCertificate
        ConfigYamlFilePath
        EpochNumber
        -- ^ The epoch from which the delegation is valid.
        SigningKeyFile
        -- ^ The issuer of the certificate, who delegates their right to sign blocks.
        VerificationKeyFile
        -- ^ The delegate, who gains the right to sign blocks on behalf of the issuer.
        NewCertificateFile
        -- ^ Filepath of the newly created delegation certificate.
  | CheckDelegation
        ConfigYamlFilePath
        CertificateFile
        VerificationKeyFile
        VerificationKeyFile

  | GetLocalNodeTip
        ConfigYamlFilePath
        (Maybe SocketPath)

    -----------------------------------

  | SubmitTx
        TxFile
        -- ^ Filepath of transaction to submit.
        ConfigYamlFilePath
        (Maybe SocketPath)

  | SpendGenesisUTxO
        ConfigYamlFilePath
        NewTxFile
        -- ^ Filepath of the newly created transaction.
        SigningKeyFile
        -- ^ Signing key of genesis UTxO owner.
        Address
        -- ^ Genesis UTxO address.
        (NonEmpty TxOut)
        -- ^ Tx output.
  | SpendUTxO
        ConfigYamlFilePath
        NewTxFile
        -- ^ Filepath of the newly created transaction.
        SigningKeyFile
        -- ^ Signing key of Tx underwriter.
        (NonEmpty TxIn)
        -- ^ Inputs available for spending to the Tx underwriter's key.
        (NonEmpty TxOut)
        -- ^ Genesis UTxO output Address.

    --- Misc Commands ---

  | ValidateCBOR
        CBORObject
        -- ^ Type of the CBOR object
        FilePath

  | PrettyPrintCBOR
        FilePath
  deriving Show


data NodeCmd = CreateVote
               ConfigYamlFilePath
               SigningKeyFile
               FilePath -- filepath to update proposal
               Bool
               FilePath
             | UpdateProposal
               ConfigYamlFilePath
               SigningKeyFile
               ProtocolVersion
               SoftwareVersion
               SystemTag
               InstallerHash
               FilePath
               [ParametersToUpdate]
             | SubmitUpdateProposal
               ConfigYamlFilePath
               -- ^ Update proposal filepath.
               FilePath
               (Maybe SocketPath)
             | SubmitVote
               ConfigYamlFilePath
               FilePath
               -- ^ Vote filepath.
               (Maybe SocketPath)
              deriving Show

