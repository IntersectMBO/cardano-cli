{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.Legacy.Options.Key
  ( pKeyCmds
  )
where

import           Cardano.Api hiding (QueryInShelleyBasedEra (..))

import           Cardano.CLI.EraBased.Options.Common
import           Cardano.CLI.Legacy.Commands.Key
import           Cardano.CLI.Types.Common

import           Data.Foldable
import           Data.Text (Text)
import           Options.Applicative hiding (help, str)
import qualified Options.Applicative as Opt

pKeyCmds :: Parser LegacyKeyCmds
pKeyCmds =
  asum
    [ subParser "verification-key" $
        Opt.info pKeyVerificationKeyCmd $
          Opt.progDesc $
            mconcat
              [ "Get a verification key from a signing key. This "
              , " supports all key types."
              ]
    , subParser "non-extended-key" $
        Opt.info pKeyNonExtendedKeyCmd $
          Opt.progDesc $
            mconcat
              [ "Get a non-extended verification key from an "
              , "extended verification key. This supports all "
              , "extended key types."
              ]
    , subParser "convert-byron-key" $
        Opt.info pKeyConvertByronKeyCmd $
          Opt.progDesc $
            mconcat
              [ "Convert a Byron payment, genesis or genesis "
              , "delegate key (signing or verification) to a "
              , "corresponding Shelley-format key."
              ]
    , subParser "convert-byron-genesis-vkey" $
        Opt.info pKeyConvertByronGenesisVKeyCmd $
          Opt.progDesc $
            mconcat
              [ "Convert a Base64-encoded Byron genesis "
              , "verification key to a Shelley genesis "
              , "verification key"
              ]
    , subParser "convert-itn-key" $
        Opt.info pKeyConvertITNKeyCmd $
          Opt.progDesc $
            mconcat
              [ "Convert an Incentivized Testnet (ITN) non-extended "
              , "(Ed25519) signing or verification key to a "
              , "corresponding Shelley stake key"
              ]
    , subParser "convert-itn-extended-key" $
        Opt.info pKeyConvertITNExtendedKeyCmd $
          Opt.progDesc $
            mconcat
              [ "Convert an Incentivized Testnet (ITN) extended "
              , "(Ed25519Extended) signing key to a corresponding "
              , "Shelley stake signing key"
              ]
    , subParser "convert-itn-bip32-key" $
        Opt.info pKeyConvertITNBip32KeyCmd $
          Opt.progDesc $
            mconcat
              [ "Convert an Incentivized Testnet (ITN) BIP32 "
              , "(Ed25519Bip32) signing key to a corresponding "
              , "Shelley stake signing key"
              ]
    , subParser "convert-cardano-address-key" $
        Opt.info pKeyConvertCardanoAddressKeyCmd $
          Opt.progDesc $
            mconcat
              [ "Convert a cardano-address extended signing key "
              , "to a corresponding Shelley-format key."
              ]
    ]

pKeyVerificationKeyCmd :: Parser LegacyKeyCmds
pKeyVerificationKeyCmd =
  KeyVerificationKeyCmd
    <$> pSigningKeyFileIn
    <*> pVerificationKeyFileOut

pKeyNonExtendedKeyCmd :: Parser LegacyKeyCmds
pKeyNonExtendedKeyCmd =
  KeyNonExtendedKeyCmd
    <$> pExtendedVerificationKeyFileIn
    <*> pVerificationKeyFileOut

pKeyConvertByronKeyCmd :: Parser LegacyKeyCmds
pKeyConvertByronKeyCmd =
  KeyConvertByronKeyCmd
    <$> optional pPassword
    <*> pByronKeyType
    <*> pByronKeyFile
    <*> pOutputFile

pPassword :: Parser Text
pPassword =
  Opt.strOption $
    mconcat
      [ Opt.long "password"
      , Opt.metavar "TEXT"
      , Opt.help "Password for signing key (if applicable)."
      ]

pByronKeyType :: Parser ByronKeyType
pByronKeyType =
  asum
    [ Opt.flag' (ByronPaymentKey NonLegacyByronKeyFormat) $
        mconcat
          [ Opt.long "byron-payment-key-type"
          , Opt.help "Use a Byron-era payment key."
          ]
    , Opt.flag' (ByronPaymentKey LegacyByronKeyFormat) $
        mconcat
          [ Opt.long "legacy-byron-payment-key-type"
          , Opt.help "Use a Byron-era payment key, in legacy SL format."
          ]
    , Opt.flag' (ByronGenesisKey NonLegacyByronKeyFormat) $
        mconcat
          [ Opt.long "byron-genesis-key-type"
          , Opt.help "Use a Byron-era genesis key."
          ]
    , Opt.flag' (ByronGenesisKey LegacyByronKeyFormat) $
        mconcat
          [ Opt.long "legacy-byron-genesis-key-type"
          , Opt.help "Use a Byron-era genesis key, in legacy SL format."
          ]
    , Opt.flag' (ByronDelegateKey NonLegacyByronKeyFormat) $
        mconcat
          [ Opt.long "byron-genesis-delegate-key-type"
          , Opt.help "Use a Byron-era genesis delegate key."
          ]
    , Opt.flag' (ByronDelegateKey LegacyByronKeyFormat) $
        mconcat
          [ Opt.long "legacy-byron-genesis-delegate-key-type"
          , Opt.help "Use a Byron-era genesis delegate key, in legacy SL format."
          ]
    ]

pByronKeyFile :: Parser (SomeKeyFile In)
pByronKeyFile =
  asum
    [ ASigningKeyFile <$> pByronSigningKeyFile
    , AVerificationKeyFile <$> pByronVerificationKeyFile
    ]

pByronSigningKeyFile :: Parser (SigningKeyFile In)
pByronSigningKeyFile =
  fmap File $
    Opt.strOption $
      mconcat
        [ Opt.long "byron-signing-key-file"
        , Opt.metavar "FILE"
        , Opt.help "Input filepath of the Byron-format signing key."
        , Opt.completer (Opt.bashCompleter "file")
        ]

pByronVerificationKeyFile :: Parser (VerificationKeyFile In)
pByronVerificationKeyFile =
  fmap File $
    Opt.strOption $
      mconcat
        [ Opt.long "byron-verification-key-file"
        , Opt.metavar "FILE"
        , Opt.help "Input filepath of the Byron-format verification key."
        , Opt.completer (Opt.bashCompleter "file")
        ]

pKeyConvertByronGenesisVKeyCmd :: Parser LegacyKeyCmds
pKeyConvertByronGenesisVKeyCmd =
  KeyConvertByronGenesisVKeyCmd
    <$> pByronGenesisVKeyBase64
    <*> pOutputFile

pByronGenesisVKeyBase64 :: Parser VerificationKeyBase64
pByronGenesisVKeyBase64 =
  fmap VerificationKeyBase64 $
    Opt.strOption $
      mconcat
        [ Opt.long "byron-genesis-verification-key"
        , Opt.metavar "BASE64"
        , Opt.help "Base64 string for the Byron genesis verification key."
        ]

pKeyConvertITNKeyCmd :: Parser LegacyKeyCmds
pKeyConvertITNKeyCmd =
  KeyConvertITNKeyCmd
    <$> pITNKeyFIle
    <*> pOutputFile

pKeyConvertITNExtendedKeyCmd :: Parser LegacyKeyCmds
pKeyConvertITNExtendedKeyCmd =
  KeyConvertITNExtendedKeyCmd
    <$> pITNSigningKeyFile
    <*> pOutputFile

pKeyConvertITNBip32KeyCmd :: Parser LegacyKeyCmds
pKeyConvertITNBip32KeyCmd =
  KeyConvertITNBip32KeyCmd
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
  fmap (ASigningKeyFile . File) $
    Opt.strOption $
      mconcat
        [ Opt.long "itn-signing-key-file"
        , Opt.metavar "FILE"
        , Opt.help "Filepath of the ITN signing key."
        , Opt.completer (Opt.bashCompleter "file")
        ]

pITNVerificationKeyFile :: Parser (SomeKeyFile direction)
pITNVerificationKeyFile =
  fmap (AVerificationKeyFile . File) $
    Opt.strOption $
      mconcat
        [ Opt.long "itn-verification-key-file"
        , Opt.metavar "FILE"
        , Opt.help "Filepath of the ITN verification key."
        , Opt.completer (Opt.bashCompleter "file")
        ]

pKeyConvertCardanoAddressKeyCmd :: Parser LegacyKeyCmds
pKeyConvertCardanoAddressKeyCmd =
  KeyConvertCardanoAddressKeyCmd
    <$> pCardanoAddressKeyType
    <*> pSigningKeyFileIn
    <*> pOutputFile

pCardanoAddressKeyType :: Parser CardanoAddressKeyType
pCardanoAddressKeyType =
  asum
    [ Opt.flag' CardanoAddressCommitteeColdKey $
        mconcat
          [ Opt.long "cc-cold-key"
          , Opt.help "Use a committee cold key."
          ]
    , Opt.flag' CardanoAddressCommitteeHotKey $
        mconcat
          [ Opt.long "cc-hot-key"
          , Opt.help "Use a committee hot key."
          ]
    , Opt.flag' CardanoAddressDRepKey $
        mconcat
          [ Opt.long "drep-key"
          , Opt.help "Use a DRep key."
          ]
    , Opt.flag' CardanoAddressShelleyPaymentKey $
        mconcat
          [ Opt.long "shelley-payment-key"
          , Opt.help "Use a Shelley-era extended payment key."
          ]
    , Opt.flag' CardanoAddressShelleyStakeKey $
        mconcat
          [ Opt.long "shelley-stake-key"
          , Opt.help "Use a Shelley-era extended stake key."
          ]
    , Opt.flag' CardanoAddressIcarusPaymentKey $
        mconcat
          [ Opt.long "icarus-payment-key"
          , Opt.help "Use a Byron-era extended payment key formatted in the Icarus style."
          ]
    , Opt.flag' CardanoAddressByronPaymentKey $
        mconcat
          [ Opt.long "byron-payment-key"
          , Opt.help "Use a Byron-era extended payment key formatted in the deprecated Byron style."
          ]
    ]
