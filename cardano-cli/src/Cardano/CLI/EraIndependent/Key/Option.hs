{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.EraIndependent.Key.Option
  ( pKeyCmds
  )
where

import Cardano.Api hiding (QueryInShelleyBasedEra (..))

import Cardano.CLI.EraBased.Common.Option
import Cardano.CLI.EraIndependent.Key.Command
import Cardano.CLI.Parser
import Cardano.CLI.Type.Common

import Data.Foldable
import Data.Text (Text)
import GHC.Word (Word32)
import Options.Applicative hiding (help, str)
import Options.Applicative qualified as Opt
import Options.Applicative.Types (readerAsk)

pKeyCmds :: Parser KeyCmds
pKeyCmds =
  let keyCmdParsers =
        asum
          [ Opt.hsubparser $
              commandWithMetavar "verification-key" $
                Opt.info pKeyVerificationKeyCmd $
                  Opt.progDesc $
                    mconcat
                      [ "Get a verification key from a signing key. This "
                      , " supports all key types."
                      ]
          , Opt.hsubparser $
              commandWithMetavar "non-extended-key" $
                Opt.info pKeyNonExtendedKeyCmd $
                  Opt.progDesc $
                    mconcat
                      [ "Get a non-extended verification key from an "
                      , "extended verification key. This supports all "
                      , "extended key types."
                      ]
          , Opt.hsubparser $
              commandWithMetavar "generate-mnemonic" $
                Opt.info pKeyGenerateMnemonicCmd $
                  Opt.progDesc $
                    mconcat
                      [ "Generate a mnemonic sentence that can be used "
                      , "for key derivation."
                      ]
          , Opt.hsubparser $
              commandWithMetavar "derive-from-mnemonic" $
                Opt.info pKeyExtendedSigningKeyFromMnemonicCmd $
                  Opt.progDesc $
                    mconcat
                      [ "Derive an extended signing key from a mnemonic sentence. "
                      , "To ensure the safety of the mnemonic phrase, "
                      , "we recommend that key derivation is performed "
                      , "in an air-gapped environment."
                      ]
          , Opt.hsubparser $
              commandWithMetavar "convert-byron-key" $
                Opt.info pKeyConvertByronKeyCmd $
                  Opt.progDesc $
                    mconcat
                      [ "Convert a Byron payment, genesis or genesis "
                      , "delegate key (signing or verification) to a "
                      , "corresponding Shelley-format key."
                      ]
          , Opt.hsubparser $
              commandWithMetavar "convert-byron-genesis-vkey" $
                Opt.info pKeyConvertByronGenesisKeyCmd $
                  Opt.progDesc $
                    mconcat
                      [ "Convert a Base64-encoded Byron genesis "
                      , "verification key to a Shelley genesis "
                      , "verification key"
                      ]
          , Opt.hsubparser $
              commandWithMetavar "convert-itn-key" $
                Opt.info pKeyConvertITNKeyCmd $
                  Opt.progDesc $
                    mconcat
                      [ "Convert an Incentivized Testnet (ITN) non-extended "
                      , "(Ed25519) signing or verification key to a "
                      , "corresponding Shelley stake key"
                      ]
          , Opt.hsubparser $
              commandWithMetavar "convert-itn-extended-key" $
                Opt.info pKeyConvertITNExtendedKeyCmd $
                  Opt.progDesc $
                    mconcat
                      [ "Convert an Incentivized Testnet (ITN) extended "
                      , "(Ed25519Extended) signing key to a corresponding "
                      , "Shelley stake signing key"
                      ]
          , Opt.hsubparser $
              commandWithMetavar "convert-itn-bip32-key" $
                Opt.info pKeyConvertITNBip32KeyCmd $
                  Opt.progDesc $
                    mconcat
                      [ "Convert an Incentivized Testnet (ITN) BIP32 "
                      , "(Ed25519Bip32) signing key to a corresponding "
                      , "Shelley stake signing key"
                      ]
          , Opt.hsubparser $
              commandWithMetavar "convert-cardano-address-key" $
                Opt.info pKeyConvertCardanoAddressKeyCmd $
                  Opt.progDesc $
                    mconcat
                      [ "Convert a cardano-address extended signing key "
                      , "to a corresponding Shelley-format key."
                      ]
          ]
   in Opt.hsubparser $
        commandWithMetavar "key" $
          Opt.info keyCmdParsers $
            Opt.progDesc $
              mconcat
                [ "Key utility commands."
                ]

pKeyVerificationKeyCmd :: Parser KeyCmds
pKeyVerificationKeyCmd =
  fmap KeyVerificationKeyCmd $
    KeyVerificationKeyCmdArgs
      <$> pSigningKeyFileIn
      <*> pVerificationKeyFileOut

pKeyNonExtendedKeyCmd :: Parser KeyCmds
pKeyNonExtendedKeyCmd =
  fmap KeyNonExtendedKeyCmd $
    KeyNonExtendedKeyCmdArgs
      <$> pExtendedVerificationKeyFileIn
      <*> pVerificationKeyFileOut

pKeyGenerateMnemonicCmd :: Parser KeyCmds
pKeyGenerateMnemonicCmd =
  fmap KeyGenerateMnemonicCmd $
    KeyGenerateMnemonicCmdArgs
      <$> optional pOutputFile
      <*> pMnemonicSize

pMnemonicSize :: Parser MnemonicSize
pMnemonicSize = do
  option
    parseSize
    ( long "size"
        <> metavar "WORD32"
        <> Opt.help
          ( mconcat
              [ "Specify the desired number of words for the output"
              , "mnemonic sentence (valid options are: 12, 15, 18, 21, and 24)"
              ]
          )
    )
 where
  parseSize :: ReadM MnemonicSize
  parseSize =
    readerAsk
      >>= \case
        "12" -> return MS12
        "15" -> return MS15
        "18" -> return MS18
        "21" -> return MS21
        "24" -> return MS24
        invalidSize ->
          readerError $
            "Invalid mnemonic size " <> show invalidSize <> "! It must be one of: 12, 15, 18, 21, or 24."

pKeyExtendedSigningKeyFromMnemonicCmd :: Parser KeyCmds
pKeyExtendedSigningKeyFromMnemonicCmd =
  fmap KeyExtendedSigningKeyFromMnemonicCmd $
    KeyExtendedSigningKeyFromMnemonicArgs
      <$> pKeyOutputFormat
      <*> pDerivedExtendedSigningKeyType
      <*> pAccountNumber
      <*> pMnemonicSource
      <*> pSigningKeyFileOut

pDerivedExtendedSigningKeyType :: Parser ExtendedSigningType
pDerivedExtendedSigningKeyType =
  asum
    [ Opt.option (ExtendedSigningPaymentKey <$> integralReader) $
        mconcat
          [ Opt.long "payment-key-with-number"
          , Opt.metavar "WORD32"
          , Opt.help
              "Derive an extended payment key with the given payment address number from the derivation path."
          ]
    , Opt.option (ExtendedSigningStakeKey <$> integralReader) $
        mconcat
          [ Opt.long "stake-key-with-number"
          , Opt.metavar "WORD32"
          , Opt.help
              "Derive an extended stake key with the given stake address number from the derivation path."
          ]
    , Opt.flag' ExtendedSigningDRepKey $
        mconcat
          [ Opt.long "drep-key"
          , Opt.help "Derive an extended DRep key."
          ]
    , Opt.flag' ExtendedSigningCCColdKey $
        mconcat
          [ Opt.long "cc-cold-key"
          , Opt.help "Derive an extended committee cold key."
          ]
    , Opt.flag' ExtendedSigningCCHotKey $
        mconcat
          [ Opt.long "cc-hot-key"
          , Opt.help "Derive an extended committee hot key."
          ]
    ]

pMnemonicSource :: Parser MnemonicSource
pMnemonicSource =
  asum
    [ MnemonicFromFile . File <$> parseFilePath "mnemonic-from-file" "Input text file with the mnemonic."
    , Opt.flag' MnemonicFromInteractivePrompt $
        mconcat
          [ Opt.long "mnemonic-from-interactive-prompt"
          , Opt.help $
              "Input the mnemonic through an interactive prompt. "
                <> "This mode also accepts receiving the mnemonic through "
                <> "standard input directly, for example, by using a pipe."
          ]
    ]

pAccountNumber :: Parser Word32
pAccountNumber =
  Opt.option integralReader $
    mconcat
      [ Opt.long "account-number"
      , Opt.metavar "WORD32"
      , Opt.help "Account number in the derivation path."
      ]

pKeyConvertByronKeyCmd :: Parser KeyCmds
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

pKeyConvertByronGenesisKeyCmd :: Parser KeyCmds
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

pKeyConvertITNKeyCmd :: Parser KeyCmds
pKeyConvertITNKeyCmd =
  fmap KeyConvertITNKeyCmd $
    KeyConvertITNKeyCmdArgs
      <$> pITNKeyFIle
      <*> pOutputFile

pKeyConvertITNExtendedKeyCmd :: Parser KeyCmds
pKeyConvertITNExtendedKeyCmd =
  fmap KeyConvertITNExtendedKeyCmd $
    KeyConvertITNExtendedKeyCmdArgs
      <$> pITNSigningKeyFile
      <*> pOutputFile

pKeyConvertITNBip32KeyCmd :: Parser KeyCmds
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

pKeyConvertCardanoAddressKeyCmd :: Parser KeyCmds
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
