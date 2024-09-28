{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.EraBased.Options.Key
  ( pKeyCmds
  )
where

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
  subInfoParser
    "key"
    ( Opt.progDesc $
        mconcat
          [ "Key utility commands."
          ]
    )
    [ Just $
        subParser "verification-key" $
          Opt.info pKeyVerificationKeyCmd $
            Opt.progDesc $
              mconcat
                [ "Get a verification key from a signing key. This "
                , " supports all key types."
                ]
    , Just $
        subParser "non-extended-key" $
          Opt.info pKeyNonExtendedKeyCmd $
            Opt.progDesc $
              mconcat
                [ "Get a non-extended verification key from an "
                , "extended verification key. This supports all "
                , "extended key types."
                ]
    , Just $
        subParser "convert-byron-key" $
          Opt.info pKeyConvertByronKeyCmd $
            Opt.progDesc $
              mconcat
                [ "Convert a Byron payment, genesis or genesis "
                , "delegate key (signing or verification) to a "
                , "corresponding Shelley-format key."
                ]
    , Just $
        subParser "convert-byron-genesis-vkey" $
          Opt.info pKeyConvertByronGenesisKeyCmd $
            Opt.progDesc $
              mconcat
                [ "Convert a Base64-encoded Byron genesis "
                , "verification key to a Shelley genesis "
                , "verification key"
                ]
    , Just $
        subParser "convert-itn-key" $
          Opt.info pKeyConvertITNKeyCmd $
            Opt.progDesc $
              mconcat
                [ "Convert an Incentivized Testnet (ITN) non-extended "
                , "(Ed25519) signing or verification key to a "
                , "corresponding Shelley stake key"
                ]
    , Just $
        subParser "convert-itn-extended-key" $
          Opt.info pKeyConvertITNExtendedKeyCmd $
            Opt.progDesc $
              mconcat
                [ "Convert an Incentivized Testnet (ITN) extended "
                , "(Ed25519Extended) signing key to a corresponding "
                , "Shelley stake signing key"
                ]
    , Just $
        subParser "convert-itn-bip32-key" $
          Opt.info pKeyConvertITNBip32KeyCmd $
            Opt.progDesc $
              mconcat
                [ "Convert an Incentivized Testnet (ITN) BIP32 "
                , "(Ed25519Bip32) signing key to a corresponding "
                , "Shelley stake signing key"
                ]
    , Just $
        subParser "convert-cardano-address-key" $
          Opt.info pKeyConvertCardanoAddressKeyCmd $
            Opt.progDesc $
              mconcat
                [ "Convert a cardano-address extended signing key "
                , "to a corresponding Shelley-format key."
                ]
    ]

pKeyVerificationKeyCmd :: Parser (KeyCmds era)
pKeyVerificationKeyCmd =
  fmap KeyVerificationKeyCmd $
    KeyVerificationKeyCmdArgs
      <$> pSigningKeyFileIn
      <*> pVerificationKeyFileOut

pKeyNonExtendedKeyCmd :: Parser (KeyCmds era)
pKeyNonExtendedKeyCmd =
  fmap KeyNonExtendedKeyCmd $
    KeyNonExtendedKeyCmdArgs
      <$> pExtendedVerificationKeyFileIn
      <*> pVerificationKeyFileOut

pKeyConvertByronKeyCmd :: Parser (KeyCmds era)
pKeyConvertByronKeyCmd =
  fmap KeyConvertByronKeyCmd $
    KeyConvertByronKeyCmdArgs
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
  File <$> parseFilePath "byron-signing-key-file" "Input filepath of the Byron-format signing key."

pByronVerificationKeyFile :: Parser (VerificationKeyFile In)
pByronVerificationKeyFile =
  File
    <$> parseFilePath "byron-verification-key-file" "Input filepath of the Byron-format verification key."

pKeyConvertByronGenesisKeyCmd :: Parser (KeyCmds era)
pKeyConvertByronGenesisKeyCmd =
  fmap KeyConvertByronGenesisVKeyCmd $
    KeyConvertByronGenesisVKeyCmdArgs
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

pKeyConvertITNKeyCmd :: Parser (KeyCmds era)
pKeyConvertITNKeyCmd =
  fmap KeyConvertITNKeyCmd $
    KeyConvertITNKeyCmdArgs
      <$> pITNKeyFIle
      <*> pOutputFile

pKeyConvertITNExtendedKeyCmd :: Parser (KeyCmds era)
pKeyConvertITNExtendedKeyCmd =
  fmap KeyConvertITNExtendedKeyCmd $
    KeyConvertITNExtendedKeyCmdArgs
      <$> pITNSigningKeyFile
      <*> pOutputFile

pKeyConvertITNBip32KeyCmd :: Parser (KeyCmds era)
pKeyConvertITNBip32KeyCmd =
  fmap KeyConvertITNBip32KeyCmd $
    KeyConvertITNBip32KeyCmdArgs
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
  ASigningKeyFile . File <$> parseFilePath "itn-signing-key-file" "Filepath of the ITN signing key."

pITNVerificationKeyFile :: Parser (SomeKeyFile direction)
pITNVerificationKeyFile =
  AVerificationKeyFile . File
    <$> parseFilePath "itn-verification-key-file" "Filepath of the ITN verification key."

pKeyConvertCardanoAddressKeyCmd :: Parser (KeyCmds era)
pKeyConvertCardanoAddressKeyCmd =
  fmap KeyConvertCardanoAddressKeyCmd $
    KeyConvertCardanoAddressKeyCmdArgs
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
