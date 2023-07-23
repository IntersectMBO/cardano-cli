{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.EraBased.Options.StakeAddress
  ( StakeAddressCmd(..)
  , pStakeAddressCmd
  , renderStakeAddressCmd
  ) where

import           Cardano.Api hiding (QueryInShelleyBasedEra (..))

import           Cardano.CLI.Environment (EnvCli (..))
import           Cardano.CLI.EraBased.Options.Common
import           Cardano.CLI.Types.Key
import           Cardano.CLI.Types.Legacy

import           Data.Foldable
import           Data.Text (Text)
import           Options.Applicative hiding (help, str)
import qualified Options.Applicative as Opt

{- HLINT ignore "Use <$>" -}
{- HLINT ignore "Move brackets to avoid $" -}

data StakeAddressCmd era
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

renderStakeAddressCmd :: StakeAddressCmd era -> Text
renderStakeAddressCmd cmd =
  case cmd of
    StakeAddressKeyGen {} -> "stake-address key-gen"
    StakeAddressKeyHash {} -> "stake-address key-hash"
    StakeAddressBuild {} -> "stake-address build"
    StakeRegistrationCert {} -> "stake-address registration-certificate"
    StakeCredentialDelegationCert {} -> "stake-address delegation-certificate"
    StakeCredentialDeRegistrationCert {} -> "stake-address deregistration-certificate"

pStakeAddressCmd :: EnvCli -> Parser (StakeAddressCmd era)
pStakeAddressCmd envCli =
    asum
      [ subParser "key-gen"
          $ Opt.info pStakeAddressKeyGen
          $ Opt.progDesc "Create a stake address key pair"
      , subParser "build"
          $ Opt.info pStakeAddressBuild
          $ Opt.progDesc "Build a stake address"
      , subParser "key-hash"
          $ Opt.info pStakeAddressKeyHash
          $ Opt.progDesc "Print the hash of a stake address key."
      , subParser "registration-certificate"
          $ Opt.info pStakeAddressRegistrationCert
          $ Opt.progDesc "Create a stake address registration certificate"
      , subParser "deregistration-certificate"
          $ Opt.info pStakeAddressDeregistrationCert
          $ Opt.progDesc "Create a stake address deregistration certificate"
      , subParser "delegation-certificate"
          $ Opt.info pStakeAddressPoolDelegationCert
          $ Opt.progDesc "Create a stake address pool delegation certificate"
      ]
  where
    pStakeAddressKeyGen :: Parser (StakeAddressCmd era)
    pStakeAddressKeyGen =
      StakeAddressKeyGen
        <$> pKeyOutputFormat
        <*> pVerificationKeyFileOut
        <*> pSigningKeyFileOut

    pStakeAddressKeyHash :: Parser (StakeAddressCmd era)
    pStakeAddressKeyHash = StakeAddressKeyHash <$> pStakeVerificationKeyOrFile <*> pMaybeOutputFile

    pStakeAddressBuild :: Parser (StakeAddressCmd era)
    pStakeAddressBuild =
      StakeAddressBuild
        <$> pStakeVerifier
        <*> pNetworkId envCli
        <*> pMaybeOutputFile

    pStakeAddressRegistrationCert :: Parser (StakeAddressCmd era)
    pStakeAddressRegistrationCert =
      StakeRegistrationCert
        <$> pAnyShelleyBasedEra envCli
        <*> pStakeIdentifier
        <*> optional pKeyRegistDeposit
        <*> pOutputFile

    pStakeAddressDeregistrationCert :: Parser (StakeAddressCmd era)
    pStakeAddressDeregistrationCert =
      StakeCredentialDeRegistrationCert
        <$> pAnyShelleyBasedEra envCli
        <*> pStakeIdentifier
        <*> optional pKeyRegistDeposit
        <*> pOutputFile

    pStakeAddressPoolDelegationCert :: Parser (StakeAddressCmd era)
    pStakeAddressPoolDelegationCert =
      StakeCredentialDelegationCert
        <$> pAnyShelleyBasedEra envCli
        <*> pStakeIdentifier
        <*> pDelegationTarget
        <*> pOutputFile

pSigningKeyFileOut :: Parser (SigningKeyFile Out)
pSigningKeyFileOut =
  fmap File $ Opt.strOption $ mconcat
    [ Opt.long "signing-key-file"
    , Opt.metavar "FILE"
    , Opt.help "Output filepath of the signing key."
    , Opt.completer (Opt.bashCompleter "file")
    ]

pKeyOutputFormat :: Parser KeyOutputFormat
pKeyOutputFormat =
  Opt.option readKeyOutputFormat $ mconcat
    [ Opt.long "key-output-format"
    , Opt.metavar "STRING"
    , Opt.help $ mconcat
      [ "Optional key output format. Accepted output formats are \"text-envelope\" "
      , "and \"bech32\" (default is \"bech32\")."
      ]
    , Opt.value KeyOutputFormatTextEnvelope
    ]

pMaybeOutputFile :: Parser (Maybe (File content Out))
pMaybeOutputFile =
  optional $ fmap File $ Opt.strOption $ mconcat
    [ Opt.long "out-file"
    , Opt.metavar "FILE"
    , Opt.help "Optional output file. Default is to write to stdout."
    , Opt.completer (Opt.bashCompleter "file")
    ]

pVerificationKeyFileOut :: Parser (VerificationKeyFile Out)
pVerificationKeyFileOut =
  fmap File $ Opt.strOption $ mconcat
    [ Opt.long "verification-key-file"
    , Opt.metavar "FILE"
    , Opt.help "Output filepath of the verification key."
    , Opt.completer (Opt.bashCompleter "file")
    ]

pDelegationTarget
  :: Parser DelegationTarget
pDelegationTarget = StakePoolDelegationTarget <$> pStakePoolVerificationKeyOrHashOrFile
