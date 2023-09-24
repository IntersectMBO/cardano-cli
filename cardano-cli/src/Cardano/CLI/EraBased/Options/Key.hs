{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.EraBased.Options.Key
  ( pKeyCmds
  ) where

import           Cardano.Api hiding (QueryInShelleyBasedEra (..))

import           Cardano.CLI.EraBased.Commands.Key
import           Cardano.CLI.EraBased.Options.Common
import           Cardano.CLI.Types.Common

import           Data.Foldable
import           Data.Text (Text)
import           Options.Applicative hiding (help, str)
import qualified Options.Applicative as Opt

{- HLINT ignore "Use <$>" -}
{- HLINT ignore "Move brackets to avoid $" -}

pKeyCmds :: Maybe (Parser (KeyCmds era))
pKeyCmds =
  subInfoParser "key"
    ( Opt.progDesc
        $ mconcat
          [ "Key utility commands."
          ]
    )
    [ Just
        $ subParser "verification-key"
        $ Opt.info pKeyGetVerificationKey
        $ Opt.progDesc
        $ mconcat
            [ "Get a verification key from a signing key. This "
            , " supports all key types."
            ]
    , Just
        $ subParser "non-extended-key"
        $ Opt.info pKeyNonExtendedKey
        $ Opt.progDesc
        $ mconcat
            [ "Get a non-extended verification key from an "
            , "extended verification key. This supports all "
            , "extended key types."
            ]
    , Just
        $ subParser "convert-byron-key"
        $ Opt.info pKeyConvertByronKey
        $ Opt.progDesc
        $ mconcat
            [ "Convert a Byron payment, genesis or genesis "
            , "delegate key (signing or verification) to a "
            , "corresponding Shelley-format key."
            ]
    , Just
        $ subParser "convert-byron-genesis-vkey"
        $ Opt.info pKeyConvertByronGenesisVKey
        $ Opt.progDesc
        $ mconcat
            [ "Convert a Base64-encoded Byron genesis "
            , "verification key to a Shelley genesis "
            , "verification key"
            ]
    , Just
        $ subParser "convert-itn-key"
        $ Opt.info pKeyConvertITNKey
        $ Opt.progDesc
        $ mconcat
            [ "Convert an Incentivized Testnet (ITN) non-extended "
            , "(Ed25519) signing or verification key to a "
            , "corresponding Shelley stake key"
            ]
    , Just
        $ subParser "convert-itn-extended-key"
        $ Opt.info pKeyConvertITNExtendedKey
        $ Opt.progDesc
        $ mconcat
            [ "Convert an Incentivized Testnet (ITN) extended "
            , "(Ed25519Extended) signing key to a corresponding "
            , "Shelley stake signing key"
            ]
    , Just
        $ subParser "convert-itn-bip32-key"
        $ Opt.info pKeyConvertITNBip32Key
        $ Opt.progDesc
        $ mconcat
            [ "Convert an Incentivized Testnet (ITN) BIP32 "
            , "(Ed25519Bip32) signing key to a corresponding "
            , "Shelley stake signing key"
            ]
    , Just
        $ subParser "convert-cardano-address-key"
        $ Opt.info pKeyConvertCardanoAddressSigningKey
        $ Opt.progDesc
        $ mconcat
            [ "Convert a cardano-address extended signing key "
            , "to a corresponding Shelley-format key."
            ]
    ]

pKeyGetVerificationKey :: Parser (KeyCmds era)
pKeyGetVerificationKey =
  KeyGetVerificationKey
    <$> pSigningKeyFileIn
    <*> pVerificationKeyFileOut

pKeyNonExtendedKey :: Parser (KeyCmds era)
pKeyNonExtendedKey =
  KeyNonExtendedKey
    <$> pExtendedVerificationKeyFileIn
    <*> pVerificationKeyFileOut

pKeyConvertByronKey :: Parser (KeyCmds era)
pKeyConvertByronKey =
  KeyConvertByronKey
    <$> optional pPassword
    <*> pByronKeyType
    <*> pByronKeyFile
    <*> pOutputFile

pPassword :: Parser Text
pPassword =
  Opt.strOption $ mconcat
    [ Opt.long "password"
    , Opt.metavar "TEXT"
    , Opt.help "Password for signing key (if applicable)."
    ]

pByronKeyType :: Parser ByronKeyType
pByronKeyType =
  asum
    [ Opt.flag' (ByronPaymentKey NonLegacyByronKeyFormat) $ mconcat
        [ Opt.long "byron-payment-key-type"
        , Opt.help "Use a Byron-era payment key."
        ]
    , Opt.flag' (ByronPaymentKey LegacyByronKeyFormat) $ mconcat
        [ Opt.long "legacy-byron-payment-key-type"
        , Opt.help "Use a Byron-era payment key, in legacy SL format."
        ]
    , Opt.flag' (ByronGenesisKey NonLegacyByronKeyFormat) $ mconcat
        [ Opt.long "byron-genesis-key-type"
        , Opt.help "Use a Byron-era genesis key."
        ]
    , Opt.flag' (ByronGenesisKey LegacyByronKeyFormat) $ mconcat
        [ Opt.long "legacy-byron-genesis-key-type"
        , Opt.help "Use a Byron-era genesis key, in legacy SL format."
        ]
    , Opt.flag' (ByronDelegateKey NonLegacyByronKeyFormat) $ mconcat
        [ Opt.long "byron-genesis-delegate-key-type"
        , Opt.help "Use a Byron-era genesis delegate key."
        ]
    , Opt.flag' (ByronDelegateKey LegacyByronKeyFormat) $ mconcat
        [ Opt.long "legacy-byron-genesis-delegate-key-type"
        , Opt.help "Use a Byron-era genesis delegate key, in legacy SL format."
        ]
    ]

pByronKeyFile :: Parser (SomeKeyFile In)
pByronKeyFile =
  asum
    [ ASigningKeyFile      <$> pByronSigningKeyFile
    , AVerificationKeyFile <$> pByronVerificationKeyFile
    ]

pByronSigningKeyFile :: Parser (SigningKeyFile In)
pByronSigningKeyFile =
  fmap File $ Opt.strOption $ mconcat
    [ Opt.long "byron-signing-key-file"
    , Opt.metavar "FILE"
    , Opt.help "Input filepath of the Byron-format signing key."
    , Opt.completer (Opt.bashCompleter "file")
    ]

pByronVerificationKeyFile :: Parser (VerificationKeyFile In)
pByronVerificationKeyFile =
  fmap File $ Opt.strOption $ mconcat
    [ Opt.long "byron-verification-key-file"
    , Opt.metavar "FILE"
    , Opt.help "Input filepath of the Byron-format verification key."
    , Opt.completer (Opt.bashCompleter "file")
    ]

pKeyConvertByronGenesisVKey :: Parser (KeyCmds era)
pKeyConvertByronGenesisVKey =
  KeyConvertByronGenesisVKey
    <$> pByronGenesisVKeyBase64
    <*> pOutputFile

pByronGenesisVKeyBase64 :: Parser VerificationKeyBase64
pByronGenesisVKeyBase64 =
  fmap VerificationKeyBase64 $ Opt.strOption $ mconcat
    [ Opt.long "byron-genesis-verification-key"
    , Opt.metavar "BASE64"
    , Opt.help "Base64 string for the Byron genesis verification key."
    ]

pKeyConvertITNKey :: Parser (KeyCmds era)
pKeyConvertITNKey =
  KeyConvertITNStakeKey
    <$> pITNKeyFIle
    <*> pOutputFile

pKeyConvertITNExtendedKey :: Parser (KeyCmds era)
pKeyConvertITNExtendedKey =
  KeyConvertITNExtendedToStakeKey
    <$> pITNSigningKeyFile
    <*> pOutputFile

pKeyConvertITNBip32Key :: Parser (KeyCmds era)
pKeyConvertITNBip32Key =
  KeyConvertITNBip32ToStakeKey
    <$> pITNSigningKeyFile
    <*> pOutputFile

pITNKeyFIle :: Parser (SomeKeyFile direction)
pITNKeyFIle =
  asum
    [ pITNSigningKeyFile
    , pITNVerificationKeyFile
    ]

pITNSigningKeyFile :: Parser (SomeKeyFile direction)
pITNSigningKeyFile =
  fmap (ASigningKeyFile . File) $ Opt.strOption $ mconcat
    [ Opt.long "itn-signing-key-file"
    , Opt.metavar "FILE"
    , Opt.help "Filepath of the ITN signing key."
    , Opt.completer (Opt.bashCompleter "file")
    ]

pITNVerificationKeyFile :: Parser (SomeKeyFile direction)
pITNVerificationKeyFile =
  fmap (AVerificationKeyFile . File) $ Opt.strOption $ mconcat
    [ Opt.long "itn-verification-key-file"
    , Opt.metavar "FILE"
    , Opt.help "Filepath of the ITN verification key."
    , Opt.completer (Opt.bashCompleter "file")
    ]

pKeyConvertCardanoAddressSigningKey :: Parser (KeyCmds era)
pKeyConvertCardanoAddressSigningKey =
  KeyConvertCardanoAddressSigningKey
    <$> pCardanoAddressKeyType
    <*> pSigningKeyFileIn
    <*> pOutputFile

pCardanoAddressKeyType :: Parser CardanoAddressKeyType
pCardanoAddressKeyType =
  asum
    [ Opt.flag' CardanoAddressShelleyPaymentKey $ mconcat
        [ Opt.long "shelley-payment-key"
        , Opt.help "Use a Shelley-era extended payment key."
        ]
    , Opt.flag' CardanoAddressShelleyStakeKey $ mconcat
        [ Opt.long "shelley-stake-key"
        , Opt.help "Use a Shelley-era extended stake key."
        ]
    , Opt.flag' CardanoAddressIcarusPaymentKey $ mconcat
        [ Opt.long "icarus-payment-key"
        , Opt.help "Use a Byron-era extended payment key formatted in the Icarus style."
        ]
    , Opt.flag' CardanoAddressByronPaymentKey $ mconcat
        [ Opt.long "byron-payment-key"
        , Opt.help "Use a Byron-era extended payment key formatted in the deprecated Byron style."
        ]
    ]
