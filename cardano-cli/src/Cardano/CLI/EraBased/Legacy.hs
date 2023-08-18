{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.CLI.EraBased.Legacy
  ( -- * CLI command parser
    parseLegacyCommands

    -- * CLI command and flag types
  , module Cardano.CLI.Commands.Legacy

    -- * Field parser and renderers
  , parseTxIn

  , pKeyRegistDeposit
  , pStakePoolRegistrationParserRequirements
  , pStakePoolVerificationKeyOrHashOrFile
  ) where

import           Cardano.Api hiding (QueryInShelleyBasedEra (..))
import           Cardano.Api.Shelley hiding (QueryInShelleyBasedEra (..))

import           Cardano.Chain.Common (BlockCount (BlockCount))
import           Cardano.CLI.Commands.Legacy
import           Cardano.CLI.Environment (EnvCli (..))
import           Cardano.CLI.EraBased.Governance
import           Cardano.CLI.EraBased.Options.Common
import           Cardano.CLI.Types.Key (DelegationTarget (..), PaymentVerifier (..),
                   StakePoolRegistrationParserRequirements (..), VerificationKeyOrFile (..),
                   VerificationKeyOrHashOrFile (..), VerificationKeyTextOrFile (..))
import           Cardano.CLI.Types.Legacy
import qualified Cardano.Ledger.BaseTypes as Shelley
import           Cardano.Prelude (ConvertText (..))

import qualified Data.Aeson as Aeson
import qualified Data.Attoparsec.ByteString.Char8 as Atto
import           Data.Bifunctor
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BSC
import           Data.Foldable
import           Data.Functor (($>))
import qualified Data.IP as IP
import           Data.Maybe (fromMaybe)
import           Data.Ratio ((%))
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Data.Time.Clock (UTCTime)
import           Data.Time.Format (defaultTimeLocale, parseTimeOrError)
import           Data.Word (Word64)
import           GHC.Natural (Natural)
import           Network.Socket (PortNumber)
import           Options.Applicative hiding (help, str)
import qualified Options.Applicative as Opt
import qualified Options.Applicative.Help as H
import           Prettyprinter (line, pretty)
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.String as Parsec
import           Text.Read (readEither, readMaybe)

{- HLINT ignore "Use <$>" -}
{- HLINT ignore "Move brackets to avoid $" -}

--
-- Shelley CLI command parsers
--

parseLegacyCommands :: EnvCli -> Parser LegacyCommand
parseLegacyCommands envCli =
  Opt.hsubparser $ mconcat
    [ Opt.metavar "Legacy commands"
    , Opt.commandGroup "Legacy commands"
    , Opt.command "address"
        $ Opt.info (AddressCmd <$> pAddressCmd envCli)
        $ Opt.progDesc "Payment address commands"
    , Opt.command "stake-address"
        $ Opt.info (StakeAddressCmd <$> pStakeAddressCmd envCli)
        $ Opt.progDesc "Stake address commands"
    , Opt.command "key"
        $ Opt.info (KeyCmd <$> pKeyCmd)
        $ Opt.progDesc "Key utility commands"
    , Opt.command "transaction"
        $ Opt.info (TransactionCmd <$> pTransaction envCli)
        $ Opt.progDesc "Transaction commands"
    , Opt.command "node"
        $ Opt.info (NodeCmd <$> pNodeCmd)
        $ Opt.progDesc "Node operation commands"
    , Opt.command "stake-pool"
        $ Opt.info (PoolCmd <$> pPoolCmd envCli)
        $ Opt.progDesc "Stake pool commands"
    , Opt.command "query"
        $ Opt.info (QueryCmd <$> pQueryCmd envCli) . Opt.progDesc
        $ mconcat
            [ "Node query commands. Will query the local node whose Unix domain socket "
            , "is obtained from the CARDANO_NODE_SOCKET_PATH environment variable."
            ]
    , Opt.command "genesis"
        $ Opt.info (GenesisCmd <$> pGenesisCmd envCli)
        $ Opt.progDesc "Genesis block commands"
    , Opt.command "governance"
        $ Opt.info (GovernanceCmd' <$> pGovernanceCmd envCli)
        $ Opt.progDesc "Governance commands"
    , Opt.command "text-view"
        $ Opt.info (TextViewCmd <$> pTextViewCmd) . Opt.progDesc
        $ mconcat
            [ "Commands for dealing with Shelley TextView files. "
            , "Transactions, addresses etc are stored on disk as TextView files."
            ]
    ]

pTextViewCmd :: Parser TextViewCmd
pTextViewCmd =
  asum
    [ subParser "decode-cbor"
        (Opt.info (TextViewInfo <$> pCBORInFile <*> pMaybeOutputFile)
          $ Opt.progDesc "Print a TextView file as decoded CBOR."
          )
    ]

pCBORInFile :: Parser FilePath
pCBORInFile =
  asum
    [ Opt.strOption $ mconcat
        [ Opt.long "in-file"
        , Opt.metavar "FILE"
        , Opt.help "CBOR input file."
        , Opt.completer (Opt.bashCompleter "file")
        ]
    , Opt.strOption $ mconcat
        [ Opt.long "file"
        , Opt.internal
        ]
    ]


pAddressCmd :: EnvCli -> Parser AddressCmd
pAddressCmd envCli =
   asum
     [ subParser "key-gen"
         (Opt.info pAddressKeyGen $ Opt.progDesc "Create an address key pair.")
     , subParser "key-hash"
         (Opt.info pAddressKeyHash $ Opt.progDesc "Print the hash of an address key.")
     , subParser "build"
         (Opt.info pAddressBuild $ Opt.progDesc "Build a Shelley payment address, with optional delegation to a stake address.")
     , subParser "info"
         (Opt.info pAddressInfo $ Opt.progDesc "Print information about an address.")
     ]
  where
    pAddressKeyGen :: Parser AddressCmd
    pAddressKeyGen =
      AddressKeyGen
        <$> pKeyOutputFormat
        <*> pAddressKeyType
        <*> pVerificationKeyFileOut
        <*> pSigningKeyFileOut

    pAddressKeyHash :: Parser AddressCmd
    pAddressKeyHash =
      AddressKeyHash
        <$> pPaymentVerificationKeyTextOrFile
        <*> pMaybeOutputFile

    pAddressBuild :: Parser AddressCmd
    pAddressBuild = AddressBuild
      <$> pPaymentVerifier
      <*> Opt.optional pStakeIdentifier
      <*> pNetworkId envCli
      <*> pMaybeOutputFile

    pAddressInfo :: Parser AddressCmd
    pAddressInfo = AddressInfo <$> pAddress <*> pMaybeOutputFile

pPaymentVerifier :: Parser PaymentVerifier
pPaymentVerifier =
  asum
    [ PaymentVerifierKey <$> pPaymentVerificationKeyTextOrFile
    , PaymentVerifierScriptFile <$>
        pScriptFor "payment-script-file" Nothing "Filepath of the payment script."
    ]


pPaymentVerificationKeyTextOrFile :: Parser VerificationKeyTextOrFile
pPaymentVerificationKeyTextOrFile =
  asum
    [ VktofVerificationKeyText <$> pPaymentVerificationKeyText
    , VktofVerificationKeyFile <$> pPaymentVerificationKeyFile
    ]

pPaymentVerificationKeyText :: Parser Text
pPaymentVerificationKeyText =
  fmap Text.pack $ Opt.strOption $ mconcat
    [ Opt.long "payment-verification-key"
    , Opt.metavar "STRING"
    , Opt.help "Payment verification key (Bech32-encoded)"
    ]

pPaymentVerificationKeyFile :: Parser (VerificationKeyFile In)
pPaymentVerificationKeyFile =
  fmap File $ asum
    [ Opt.strOption $ mconcat
      [ Opt.long "payment-verification-key-file"
      , Opt.metavar "FILE"
      , Opt.help "Filepath of the payment verification key."
      , Opt.completer (Opt.bashCompleter "file")
      ]
    , Opt.strOption $ mconcat
      [ Opt.long "verification-key-file"
      , Opt.internal
      ]
    ]

pScript :: Parser ScriptFile
pScript = pScriptFor "script-file" Nothing "Filepath of the script."

pReferenceTxIn :: String -> String -> Parser TxIn
pReferenceTxIn prefix scriptType =
  Opt.option (readerFromParsecParser parseTxIn) $ mconcat
    [ Opt.long (prefix ++ "tx-in-reference")
    , Opt.metavar "TX-IN"
    , Opt.help $ mconcat
      [ "TxId#TxIx - Specify a reference input. The reference input must have"
      , " a " <> scriptType <> " reference script attached."
      ]
    ]

pReadOnlyReferenceTxIn :: Parser TxIn
pReadOnlyReferenceTxIn =
  Opt.option (readerFromParsecParser parseTxIn) $ mconcat
    [ Opt.long "read-only-tx-in-reference"
    , Opt.metavar "TX-IN"
    , Opt.help $ mconcat
      [ "Specify a read only reference input. This reference input is not witnessing anything "
      , "it is simply provided in the plutus script context."
      ]
    ]


pScriptWitnessFiles :: forall witctx.
                       WitCtx witctx
                    -> BalanceTxExecUnits -- ^ Use the @execution-units@ flag.
                    -> String -- ^ Script flag prefix
                    -> Maybe String
                    -> String
                    -> Parser (ScriptWitnessFiles witctx)
pScriptWitnessFiles witctx autoBalanceExecUnits scriptFlagPrefix scriptFlagPrefixDeprecated help =
    toScriptWitnessFiles
      <$> pScriptFor (scriptFlagPrefix ++ "-script-file")
                     ((++ "-script-file") <$> scriptFlagPrefixDeprecated)
                     ("The file containing the script to witness " ++ help)
      <*> optional ((,,) <$> pScriptDatumOrFile scriptFlagPrefix witctx
                         <*> pScriptRedeemerOrFile scriptFlagPrefix
                         <*> (case autoBalanceExecUnits of
                               AutoBalance -> pure (ExecutionUnits 0 0)
                               ManualBalance -> pExecutionUnits scriptFlagPrefix)
                   )
  where
    toScriptWitnessFiles :: ScriptFile
                         -> Maybe (ScriptDatumOrFile witctx,
                                   ScriptRedeemerOrFile,
                                   ExecutionUnits)
                         -> ScriptWitnessFiles witctx
    toScriptWitnessFiles sf Nothing        = SimpleScriptWitnessFile  sf
    toScriptWitnessFiles sf (Just (d,r, e)) = PlutusScriptWitnessFiles sf d r e


pExecutionUnits :: String -> Parser ExecutionUnits
pExecutionUnits scriptFlagPrefix =
  fmap (uncurry ExecutionUnits) $ Opt.option Opt.auto $ mconcat
    [ Opt.long (scriptFlagPrefix ++ "-execution-units")
    , Opt.metavar "(INT, INT)"
    , Opt.help "The time and space units needed by the script."
    ]

pScriptRedeemerOrFile :: String -> Parser ScriptDataOrFile
pScriptRedeemerOrFile scriptFlagPrefix =
  pScriptDataOrFile (scriptFlagPrefix ++ "-redeemer")
    "The script redeemer, in JSON syntax."
    "The script redeemer, in the given JSON file."


pScriptDatumOrFile :: String -> WitCtx witctx -> Parser (ScriptDatumOrFile witctx)
pScriptDatumOrFile scriptFlagPrefix witctx =
  case witctx of
    WitCtxTxIn  -> (ScriptDatumOrFileForTxIn <$>
                     pScriptDataOrFile
                       (scriptFlagPrefix ++ "-datum")
                       "The script datum, in JSON syntax."
                       "The script datum, in the given JSON file.") <|>
                    pInlineDatumPresent
    WitCtxMint  -> pure NoScriptDatumOrFileForMint
    WitCtxStake -> pure NoScriptDatumOrFileForStake
 where
  pInlineDatumPresent :: Parser (ScriptDatumOrFile WitCtxTxIn)
  pInlineDatumPresent  =
    flag' InlineDatumPresentAtTxIn $ mconcat
      [ long (scriptFlagPrefix ++ "-inline-datum-present")
      , Opt.help "Inline datum present at transaction input."
      ]

pScriptDataOrFile :: String -> String -> String -> Parser ScriptDataOrFile
pScriptDataOrFile dataFlagPrefix helpTextForValue helpTextForFile =
  asum
    [ pScriptDataCborFile
    , pScriptDataFile
    , pScriptDataValue
    ]
  where
    pScriptDataCborFile = fmap ScriptDataCborFile . Opt.strOption $ mconcat
      [ Opt.long (dataFlagPrefix ++ "-cbor-file")
      , Opt.metavar "CBOR FILE"
      , Opt.help $ mconcat
        [ helpTextForFile
        , " The file must follow the special JSON schema for script data."
        ]
      ]

    pScriptDataFile = fmap ScriptDataJsonFile . Opt.strOption $ mconcat
      [ Opt.long (dataFlagPrefix ++ "-file")
      , Opt.metavar "JSON FILE"
      , Opt.help $ mconcat
        [ helpTextForFile ++ " The file must follow the special "
        , "JSON schema for script data."
        ]
      ]

    pScriptDataValue = fmap ScriptDataValue . Opt.option readerScriptData $ mconcat
      [ Opt.long (dataFlagPrefix ++ "-value")
      , Opt.metavar "JSON VALUE"
      , Opt.help $ mconcat
        [ helpTextForValue
        , " There is no schema: (almost) any JSON value is supported, including "
        , "top-level strings and numbers."
        ]
      ]

    readerScriptData :: ReadM HashableScriptData
    readerScriptData = do
      v <- Opt.str
      case Aeson.eitherDecode v of
        Left e -> fail $ "readerScriptData: " <> e
        Right sDataValue ->
          case scriptDataJsonToHashable ScriptDataJsonNoSchema sDataValue of
            Left err -> fail (displayError err)
            Right sd -> return sd

pStakeAddressCmd :: EnvCli -> Parser StakeAddressCmd
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
    pStakeAddressKeyGen :: Parser StakeAddressCmd
    pStakeAddressKeyGen =
      StakeAddressKeyGen
        <$> pKeyOutputFormat
        <*> pVerificationKeyFileOut
        <*> pSigningKeyFileOut

    pStakeAddressKeyHash :: Parser StakeAddressCmd
    pStakeAddressKeyHash = StakeAddressKeyHash <$> pStakeVerificationKeyOrFile <*> pMaybeOutputFile

    pStakeAddressBuild :: Parser StakeAddressCmd
    pStakeAddressBuild =
      StakeAddressBuild
        <$> pStakeVerifier
        <*> pNetworkId envCli
        <*> pMaybeOutputFile

    pStakeAddressRegistrationCert :: Parser StakeAddressCmd
    pStakeAddressRegistrationCert =
      StakeRegistrationCert
        <$> pAnyShelleyBasedEra envCli
        <*> pStakeIdentifier
        <*> optional pKeyRegistDeposit
        <*> pOutputFile

    pStakeAddressDeregistrationCert :: Parser StakeAddressCmd
    pStakeAddressDeregistrationCert =
      StakeCredentialDeRegistrationCert
        <$> pAnyShelleyBasedEra envCli
        <*> pStakeIdentifier
        <*> optional pKeyRegistDeposit
        <*> pOutputFile

    pStakeAddressPoolDelegationCert :: Parser StakeAddressCmd
    pStakeAddressPoolDelegationCert =
      StakeCredentialDelegationCert
        <$> pAnyShelleyBasedEra envCli
        <*> pStakeIdentifier
        <*> pDelegationTarget
        <*> pOutputFile

pKeyCmd :: Parser KeyCmd
pKeyCmd =
  asum
    [ subParser "verification-key"
        $ Opt.info pKeyGetVerificationKey
        $ Opt.progDesc
        $ mconcat
            [ "Get a verification key from a signing key. This "
            , " supports all key types."
            ]
    , subParser "non-extended-key"
        $ Opt.info pKeyNonExtendedKey
        $ Opt.progDesc
        $ mconcat
            [ "Get a non-extended verification key from an "
            , "extended verification key. This supports all "
            , "extended key types."
            ]
    , subParser "convert-byron-key"
        $ Opt.info pKeyConvertByronKey
        $ Opt.progDesc
        $ mconcat
            [ "Convert a Byron payment, genesis or genesis "
            , "delegate key (signing or verification) to a "
            , "corresponding Shelley-format key."
            ]
    , subParser "convert-byron-genesis-vkey"
        $ Opt.info pKeyConvertByronGenesisVKey
        $ Opt.progDesc
        $ mconcat
            [ "Convert a Base64-encoded Byron genesis "
            , "verification key to a Shelley genesis "
            , "verification key"
            ]
    , subParser "convert-itn-key"
        $ Opt.info pKeyConvertITNKey
        $ Opt.progDesc
        $ mconcat
            [ "Convert an Incentivized Testnet (ITN) non-extended "
            , "(Ed25519) signing or verification key to a "
            , "corresponding Shelley stake key"
            ]
    , subParser "convert-itn-extended-key"
        $ Opt.info pKeyConvertITNExtendedKey
        $ Opt.progDesc
        $ mconcat
            [ "Convert an Incentivized Testnet (ITN) extended "
            , "(Ed25519Extended) signing key to a corresponding "
            , "Shelley stake signing key"
            ]
    , subParser "convert-itn-bip32-key"
        $ Opt.info pKeyConvertITNBip32Key
        $ Opt.progDesc
        $ mconcat
            [ "Convert an Incentivized Testnet (ITN) BIP32 "
            , "(Ed25519Bip32) signing key to a corresponding "
            , "Shelley stake signing key"
            ]
    , subParser "convert-cardano-address-key"
        $ Opt.info pKeyConvertCardanoAddressSigningKey
        $ Opt.progDesc
        $ mconcat
            [ "Convert a cardano-address extended signing key "
            , "to a corresponding Shelley-format key."
            ]
    ]
  where
    pKeyGetVerificationKey :: Parser KeyCmd
    pKeyGetVerificationKey =
      KeyGetVerificationKey
        <$> pSigningKeyFileIn
        <*> pVerificationKeyFileOut

    pKeyNonExtendedKey :: Parser KeyCmd
    pKeyNonExtendedKey =
      KeyNonExtendedKey
        <$> pExtendedVerificationKeyFileIn
        <*> pVerificationKeyFileOut

    pKeyConvertByronKey :: Parser KeyCmd
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

    pKeyConvertByronGenesisVKey :: Parser KeyCmd
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

    pKeyConvertITNKey :: Parser KeyCmd
    pKeyConvertITNKey =
      KeyConvertITNStakeKey
        <$> pITNKeyFIle
        <*> pOutputFile

    pKeyConvertITNExtendedKey :: Parser KeyCmd
    pKeyConvertITNExtendedKey =
      KeyConvertITNExtendedToStakeKey
        <$> pITNSigningKeyFile
        <*> pOutputFile

    pKeyConvertITNBip32Key :: Parser KeyCmd
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

    pKeyConvertCardanoAddressSigningKey :: Parser KeyCmd
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

pTransaction :: EnvCli -> Parser TransactionCmd
pTransaction envCli =
  asum
    [ subParser "build-raw"
        $ Opt.info pTransactionBuildRaw $ Opt.progDescDoc $ Just $ mconcat
          [ pretty @String "Build a transaction (low-level, inconvenient)"
          , line
          , line
          , H.yellow $ mconcat
            [ "Please note the order of some cmd options is crucial. If used incorrectly may produce "
            , "undesired tx body. See nested [] notation above for details."
            ]
          ]
    , subParser "build"
        $ Opt.info pTransactionBuild $ Opt.progDescDoc $ Just $ mconcat
          [ pretty @String "Build a balanced transaction (automatically calculates fees)"
          , line
          , line
          , H.yellow $ mconcat
            [ "Please note "
            , H.underline "the order"
            , " of some cmd options is crucial. If used incorrectly may produce "
            , "undesired tx body. See nested [] notation above for details."
            ]
          ]
    , subParser "sign"
        (Opt.info pTransactionSign $ Opt.progDesc "Sign a transaction")
    , subParser "witness"
        (Opt.info pTransactionCreateWitness $ Opt.progDesc "Create a transaction witness")
    , subParser "assemble"
        (Opt.info pTransactionAssembleTxBodyWit
          $ Opt.progDesc "Assemble a tx body and witness(es) to form a transaction")
    , pSignWitnessBackwardCompatible
    , subParser "submit"
        (Opt.info pTransactionSubmit . Opt.progDesc $
           mconcat
             [ "Submit a transaction to the local node whose Unix domain socket "
             , "is obtained from the CARDANO_NODE_SOCKET_PATH environment variable."
             ]
          )
    , subParser "policyid"
        (Opt.info pTransactionPolicyId $ Opt.progDesc "Calculate the PolicyId from the monetary policy script.")
    , subParser "calculate-min-fee"
        (Opt.info pTransactionCalculateMinFee $ Opt.progDesc "Calculate the minimum fee for a transaction.")
    , subParser "calculate-min-required-utxo"
        (Opt.info pTransactionCalculateMinReqUTxO $ Opt.progDesc "Calculate the minimum required UTxO for a transaction output.")
    , pCalculateMinRequiredUtxoBackwardCompatible
    , subParser "hash-script-data"
        (Opt.info pTxHashScriptData $ Opt.progDesc "Calculate the hash of script data.")
    , subParser "txid"
        (Opt.info pTransactionId $ Opt.progDesc "Print a transaction identifier.")
    , subParser "view" $
        Opt.info pTransactionView $ Opt.progDesc "Print a transaction."
    ]
 where
  -- Backwards compatible parsers
  calcMinValueInfo :: ParserInfo TransactionCmd
  calcMinValueInfo =
    Opt.info pTransactionCalculateMinReqUTxO
      $ Opt.progDesc "DEPRECATED: Use 'calculate-min-required-utxo' instead."

  pCalculateMinRequiredUtxoBackwardCompatible :: Parser TransactionCmd
  pCalculateMinRequiredUtxoBackwardCompatible =
    Opt.subparser
      $ Opt.command "calculate-min-value" calcMinValueInfo <> Opt.internal

  assembleInfo :: ParserInfo TransactionCmd
  assembleInfo =
    Opt.info pTransactionAssembleTxBodyWit
      $ Opt.progDesc "Assemble a tx body and witness(es) to form a transaction"

  pSignWitnessBackwardCompatible :: Parser TransactionCmd
  pSignWitnessBackwardCompatible =
    Opt.subparser
      $ Opt.command "sign-witness" assembleInfo <> Opt.internal

  pScriptValidity :: Parser ScriptValidity
  pScriptValidity = asum
    [ Opt.flag' ScriptValid $ mconcat
      [ Opt.long "script-valid"
      , Opt.help "Assertion that the script is valid. (default)"
      ]
    , Opt.flag' ScriptInvalid $ mconcat
      [ Opt.long "script-invalid"
      , Opt.help $ mconcat
        [ "Assertion that the script is invalid.  "
        , "If a transaction is submitted with such a script, "
        , "the script will fail and the collateral will be taken."
        ]
      ]
    ]

  pTransactionBuild :: Parser TransactionCmd
  pTransactionBuild =
    TxBuild
      <$> pSocketPath envCli
      <*> pCardanoEra envCli
      <*> pConsensusModeParams
      <*> pNetworkId envCli
      <*> optional pScriptValidity
      <*> optional pWitnessOverride
      <*> some (pTxIn AutoBalance)
      <*> many pReadOnlyReferenceTxIn
      <*> many pRequiredSigner
      <*> many pTxInCollateral
      <*> optional pReturnCollateral
      <*> optional pTotalCollateral
      <*> many pTxOut
      <*> pChangeAddress
      <*> optional (pMintMultiAsset AutoBalance)
      <*> optional pInvalidBefore
      <*> optional pInvalidHereafter
      <*> many (pCertificateFile AutoBalance)
      <*> many (pWithdrawal AutoBalance)
      <*> pTxMetadataJsonSchema
      <*> many (pScriptFor
                  "auxiliary-script-file"
                  Nothing
                  "Filepath of auxiliary script(s)")
      <*> many pMetadataFile
      <*> optional pUpdateProposalFile
      <*> many (pFileInDirection "vote-file" "Filepath of the vote.")
      <*> many (pFileInDirection "constitution-file" "Filepath of the constitution.")
      <*> (OutputTxBodyOnly <$> pTxBodyFileOut <|> pCalculatePlutusScriptCost)

  pChangeAddress :: Parser TxOutChangeAddress
  pChangeAddress =
    fmap TxOutChangeAddress $ Opt.option (readerFromParsecParser parseAddressAny) $ mconcat
      [ Opt.long "change-address"
      , Opt.metavar "ADDRESS"
      , Opt.help "Address where ADA in excess of the tx fee will go to."
      ]

  pTransactionBuildRaw :: Parser TransactionCmd
  pTransactionBuildRaw =
    TxBuildRaw
      <$> pCardanoEra envCli
      <*> optional pScriptValidity
      <*> some (pTxIn ManualBalance)
      <*> many pReadOnlyReferenceTxIn
      <*> many pTxInCollateral
      <*> optional pReturnCollateral
      <*> optional pTotalCollateral
      <*> many pRequiredSigner
      <*> many pTxOut
      <*> optional (pMintMultiAsset ManualBalance)
      <*> optional pInvalidBefore
      <*> optional pInvalidHereafter
      <*> optional pTxFee
      <*> many (pCertificateFile ManualBalance )
      <*> many (pWithdrawal ManualBalance)
      <*> pTxMetadataJsonSchema
      <*> many (pScriptFor "auxiliary-script-file" Nothing "Filepath of auxiliary script(s)")
      <*> many pMetadataFile
      <*> optional pProtocolParamsFile
      <*> optional pUpdateProposalFile
      <*> pTxBodyFileOut

  pTransactionSign  :: Parser TransactionCmd
  pTransactionSign =
    TxSign
      <$> pInputTxOrTxBodyFile
      <*> many pWitnessSigningData
      <*> optional (pNetworkId envCli)
      <*> pTxFileOut

  pTransactionCreateWitness :: Parser TransactionCmd
  pTransactionCreateWitness =
    TxCreateWitness
      <$> pTxBodyFileIn
      <*> pWitnessSigningData
      <*> optional (pNetworkId envCli)
      <*> pOutputFile

  pTransactionAssembleTxBodyWit :: Parser TransactionCmd
  pTransactionAssembleTxBodyWit =
    TxAssembleTxBodyWitness
      <$> pTxBodyFileIn
      <*> many pWitnessFile
      <*> pOutputFile

  pTransactionSubmit :: Parser TransactionCmd
  pTransactionSubmit =
    TxSubmit
      <$> pSocketPath envCli
      <*> pConsensusModeParams
      <*> pNetworkId envCli
      <*> pTxSubmitFile

  pTransactionPolicyId :: Parser TransactionCmd
  pTransactionPolicyId = TxMintedPolicyId <$> pScript

  pTransactionCalculateMinFee :: Parser TransactionCmd
  pTransactionCalculateMinFee =
    TxCalculateMinFee
      <$> pTxBodyFileIn
      <*> pNetworkId envCli
      <*> pProtocolParamsFile
      <*> pTxInCount
      <*> pTxOutCount
      <*> pTxShelleyWitnessCount
      <*> pTxByronWitnessCount

  pTransactionCalculateMinReqUTxO :: Parser TransactionCmd
  pTransactionCalculateMinReqUTxO = TxCalculateMinRequiredUTxO
    <$> pCardanoEra envCli
    <*> pProtocolParamsFile
    <*> pTxOut

  pTxHashScriptData :: Parser TransactionCmd
  pTxHashScriptData =
    fmap TxHashScriptData
      $ pScriptDataOrFile
          "script-data"
          "The script data, in JSON syntax."
          "The script data, in the given JSON file."

  pTransactionId  :: Parser TransactionCmd
  pTransactionId = TxGetTxId <$> pInputTxOrTxBodyFile

  pTransactionView :: Parser TransactionCmd
  pTransactionView = TxView <$> pInputTxOrTxBodyFile

pNodeCmd :: Parser NodeCmd
pNodeCmd =
  asum
    [ subParser "key-gen" . Opt.info pKeyGenOperator . Opt.progDesc $ mconcat
      [ "Create a key pair for a node operator's offline "
      , "key and a new certificate issue counter"
      ]
    , subParser "key-gen-KES" . Opt.info pKeyGenKES . Opt.progDesc $ mconcat
      [ "Create a key pair for a node KES operational key"
      ]
    , subParser "key-gen-VRF" . Opt.info pKeyGenVRF . Opt.progDesc $ mconcat
      [ "Create a key pair for a node VRF operational key"
      ]
    , subParser "key-hash-VRF". Opt.info pKeyHashVRF . Opt.progDesc $ mconcat
      [ "Print hash of a node's operational VRF key."
      ]
    , subParser "new-counter" . Opt.info pNewCounter . Opt.progDesc $ mconcat
      [ "Create a new certificate issue counter"
      ]
    , subParser "issue-op-cert" . Opt.info pIssueOpCert . Opt.progDesc $ mconcat
      [ "Issue a node operational certificate"
      ]
    ]
  where
    pKeyGenOperator :: Parser NodeCmd
    pKeyGenOperator =
      NodeKeyGenCold
        <$> pKeyOutputFormat
        <*> pColdVerificationKeyFile
        <*> pColdSigningKeyFile
        <*> pOperatorCertIssueCounterFile

    pKeyGenKES :: Parser NodeCmd
    pKeyGenKES =
      NodeKeyGenKES
        <$> pKeyOutputFormat
        <*> pVerificationKeyFileOut
        <*> pSigningKeyFileOut

    pKeyGenVRF :: Parser NodeCmd
    pKeyGenVRF =
      NodeKeyGenVRF
        <$> pKeyOutputFormat
        <*> pVerificationKeyFileOut
        <*> pSigningKeyFileOut

    pKeyHashVRF :: Parser NodeCmd
    pKeyHashVRF =
      NodeKeyHashVRF
        <$> pVerificationKeyOrFile AsVrfKey
        <*> pMaybeOutputFile

    pNewCounter :: Parser NodeCmd
    pNewCounter =
      NodeNewCounter
        <$> pColdVerificationKeyOrFile
        <*> pCounterValue
        <*> pOperatorCertIssueCounterFile

    pCounterValue :: Parser Word
    pCounterValue =
      Opt.option Opt.auto $ mconcat
        [ Opt.long "counter-value"
        , Opt.metavar "INT"
        , Opt.help "The next certificate issue counter value to use."
        ]

    pIssueOpCert :: Parser NodeCmd
    pIssueOpCert =
      NodeIssueOpCert
        <$> pKesVerificationKeyOrFile
        <*> pColdSigningKeyFile
        <*> pOperatorCertIssueCounterFile
        <*> pKesPeriod
        <*> pOutputFile

pPoolCmd :: EnvCli -> Parser PoolCmd
pPoolCmd  envCli =
  asum
    [ subParser "registration-certificate"
        $ Opt.info (pStakePoolRegistrationCert envCli)
        $ Opt.progDesc "Create a stake pool registration certificate"
    , subParser "deregistration-certificate"
        $ Opt.info (pStakePoolRetirementCert envCli)
        $ Opt.progDesc "Create a stake pool deregistration certificate"
    , subParser "id"
        $ Opt.info pId
        $ Opt.progDesc "Build pool id from the offline key"
    , subParser "metadata-hash"
        $ Opt.info pPoolMetadataHashSubCmd
        $ Opt.progDesc "Print the hash of pool metadata."
    ]
  where
    pId :: Parser PoolCmd
    pId = PoolGetId <$> pStakePoolVerificationKeyOrFile <*> pPoolIdOutputFormat <*> pMaybeOutputFile

    pPoolMetadataHashSubCmd :: Parser PoolCmd
    pPoolMetadataHashSubCmd = PoolMetadataHash <$> pPoolMetadataFile <*> pMaybeOutputFile

pQueryCmd :: EnvCli -> Parser QueryCmd
pQueryCmd envCli =
  asum
    [ subParser "protocol-parameters"
        $ Opt.info pQueryProtocolParameters
        $ Opt.progDesc "Get the node's current protocol parameters"
    , subParser "constitution-hash"
        $ Opt.info pQueryConstitutionHash
        $ Opt.progDesc "Get the constitution hash"
    , subParser "tip"
        $ Opt.info pQueryTip
        $ Opt.progDesc "Get the node's current tip (slot no, hash, block no)"
    , subParser "stake-pools"
        $ Opt.info pQueryStakePools
        $ Opt.progDesc "Get the node's current set of stake pool ids"
    , subParser "stake-distribution"
        $ Opt.info pQueryStakeDistribution
        $ Opt.progDesc "Get the node's current aggregated stake distribution"
    , subParser "stake-address-info"
        $ Opt.info pQueryStakeAddressInfo
        $ Opt.progDesc $ mconcat
            [ "Get the current delegations and reward accounts filtered by stake address."
            ]
    , subParser "utxo"
        $ Opt.info pQueryUTxO
        $ Opt.progDesc $ mconcat
            [ "Get a portion of the current UTxO: by tx in, by address or the whole."
            ]
    , subParser "ledger-state"
        $ Opt.info pQueryLedgerState
        $ Opt.progDesc $ mconcat
            [ "Dump the current ledger state of the node (Ledger.NewEpochState -- advanced command)"
            ]
    , subParser "protocol-state"
        $ Opt.info pQueryProtocolState
        $ Opt.progDesc $ mconcat
            [ "Dump the current protocol state of the node (Ledger.ChainDepState -- advanced command)"
            ]
    , subParser "stake-snapshot"
        $ Opt.info pQueryStakeSnapshot
        $ Opt.progDesc $ mconcat
            [ "Obtain the three stake snapshots for a pool, plus the total active stake (advanced command)"
            ]
    , hiddenSubParser "pool-params"
        $ Opt.info pQueryPoolState
        $ Opt.progDesc $ mconcat
            [ "DEPRECATED.  Use query pool-state instead.  Dump the pool parameters "
            , "(Ledger.NewEpochState.esLState._delegationState._pState._pParams -- advanced command)"
            ]
    , subParser "leadership-schedule"
        $ Opt.info pLeadershipSchedule
        $ Opt.progDesc "Get the slots the node is expected to mint a block in (advanced command)"
    , subParser "kes-period-info"
        $ Opt.info pKesPeriodInfo
        $ Opt.progDesc "Get information about the current KES period and your node's operational certificate."
    , subParser "pool-state"
        $ Opt.info pQueryPoolState
        $ Opt.progDesc "Dump the pool state"
    , subParser "tx-mempool"
        $ Opt.info pQueryTxMempool
        $ Opt.progDesc "Local Mempool info"
    , subParser "slot-number"
        $ Opt.info pQuerySlotNumber
        $ Opt.progDesc "Query slot number for UTC timestamp"
    ]
  where
    pQueryProtocolParameters :: Parser QueryCmd
    pQueryProtocolParameters =
      QueryProtocolParameters'
        <$> pSocketPath envCli
        <*> pConsensusModeParams
        <*> pNetworkId envCli
        <*> pMaybeOutputFile

    pQueryConstitutionHash :: Parser QueryCmd
    pQueryConstitutionHash =
      QueryConstitutionHash
        <$> pSocketPath envCli
        <*> pConsensusModeParams
        <*> pNetworkId envCli
        <*> pMaybeOutputFile

    pQueryTip :: Parser QueryCmd
    pQueryTip =
      QueryTip
        <$> pSocketPath envCli
        <*> pConsensusModeParams
        <*> pNetworkId envCli
        <*> pMaybeOutputFile

    pQueryUTxO :: Parser QueryCmd
    pQueryUTxO =
      QueryUTxO'
        <$> pSocketPath envCli
        <*> pConsensusModeParams
        <*> pQueryUTxOFilter
        <*> pNetworkId envCli
        <*> pMaybeOutputFile

    pQueryStakePools :: Parser QueryCmd
    pQueryStakePools =
      QueryStakePools'
        <$> pSocketPath envCli
        <*> pConsensusModeParams
        <*> pNetworkId envCli
        <*> pMaybeOutputFile

    pQueryStakeDistribution :: Parser QueryCmd
    pQueryStakeDistribution =
      QueryStakeDistribution'
        <$> pSocketPath envCli
        <*> pConsensusModeParams
        <*> pNetworkId envCli
        <*> pMaybeOutputFile

    pQueryStakeAddressInfo :: Parser QueryCmd
    pQueryStakeAddressInfo =
      QueryStakeAddressInfo
        <$> pSocketPath envCli
        <*> pConsensusModeParams
        <*> pFilterByStakeAddress
        <*> pNetworkId envCli
        <*> pMaybeOutputFile

    pQueryLedgerState :: Parser QueryCmd
    pQueryLedgerState =
      QueryDebugLedgerState'
        <$> pSocketPath envCli
        <*> pConsensusModeParams
        <*> pNetworkId envCli
        <*> pMaybeOutputFile

    pQueryProtocolState :: Parser QueryCmd
    pQueryProtocolState =
      QueryProtocolState'
        <$> pSocketPath envCli
        <*> pConsensusModeParams
        <*> pNetworkId envCli
        <*> pMaybeOutputFile

    pAllStakePoolsOrOnly :: Parser (AllOrOnly [Hash StakePoolKey])
    pAllStakePoolsOrOnly = pAll <|> pOnly
      where pAll :: Parser (AllOrOnly [Hash StakePoolKey])
            pAll = Opt.flag' All $ mconcat
              [ Opt.long "all-stake-pools"
              , Opt.help "Query for all stake pools"
              ]
            pOnly :: Parser (AllOrOnly [Hash StakePoolKey])
            pOnly = Only <$> many pStakePoolVerificationKeyHash

    pQueryStakeSnapshot :: Parser QueryCmd
    pQueryStakeSnapshot =
      QueryStakeSnapshot'
        <$> pSocketPath envCli
        <*> pConsensusModeParams
        <*> pNetworkId envCli
        <*> pAllStakePoolsOrOnly
        <*> pMaybeOutputFile

    pQueryPoolState :: Parser QueryCmd
    pQueryPoolState =
      QueryPoolState'
        <$> pSocketPath envCli
        <*> pConsensusModeParams
        <*> pNetworkId envCli
        <*> many pStakePoolVerificationKeyHash

    pQueryTxMempool :: Parser QueryCmd
    pQueryTxMempool =
      QueryTxMempool
        <$> pSocketPath envCli
        <*> pConsensusModeParams
        <*> pNetworkId envCli
        <*> pTxMempoolQuery
        <*> pMaybeOutputFile
      where
        pTxMempoolQuery :: Parser TxMempoolQuery
        pTxMempoolQuery = asum
          [ subParser "info"
              $ Opt.info (pure TxMempoolQueryInfo)
              $ Opt.progDesc "Ask the node about the current mempool's capacity and sizes"
          , subParser "next-tx"
              $ Opt.info (pure TxMempoolQueryNextTx)
              $ Opt.progDesc "Requests the next transaction from the mempool's current list"
          , subParser "tx-exists"
              $ Opt.info (TxMempoolQueryTxExists <$> argument Opt.str (metavar "TX_ID"))
              $ Opt.progDesc "Query if a particular transaction exists in the mempool"
          ]
    pLeadershipSchedule :: Parser QueryCmd
    pLeadershipSchedule =
      QueryLeadershipSchedule
        <$> pSocketPath envCli
        <*> pConsensusModeParams
        <*> pNetworkId envCli
        <*> pGenesisFile "Shelley genesis filepath"
        <*> pStakePoolVerificationKeyOrHashOrFile
        <*> pVrfSigningKeyFile
        <*> pWhichLeadershipSchedule
        <*> pMaybeOutputFile

    pKesPeriodInfo :: Parser QueryCmd
    pKesPeriodInfo =
      QueryKesPeriodInfo
        <$> pSocketPath envCli
        <*> pConsensusModeParams
        <*> pNetworkId envCli
        <*> pOperationalCertificateFile
        <*> pMaybeOutputFile

    pQuerySlotNumber :: Parser QueryCmd
    pQuerySlotNumber =
      QuerySlotNumber
        <$> pSocketPath envCli
        <*> pConsensusModeParams
        <*> pNetworkId envCli
        <*> pUtcTimestamp
          where
            pUtcTimestamp =
              convertTime <$> (Opt.strArgument . mconcat)
                [ Opt.metavar "TIMESTAMP"
                , Opt.help "UTC timestamp in YYYY-MM-DDThh:mm:ssZ format"
                ]


-- TODO: Conway era - move to Cardano.CLI.Conway.Parsers
pGovernanceCmd :: EnvCli -> Parser GovernanceCmd
pGovernanceCmd envCli =
  asum
    [ subParser "create-mir-certificate"
        $ Opt.info (pLegacyMIRPayStakeAddresses <|> mirCertParsers)
        $ Opt.progDesc "Create an MIR (Move Instantaneous Rewards) certificate"
    , subParser "create-genesis-key-delegation-certificate"
        $ Opt.info pGovernanceGenesisKeyDelegationCertificate
        $ Opt.progDesc "Create a genesis key delegation certificate"
    , subParser "create-update-proposal"
        $ Opt.info pUpdateProposal
        $ Opt.progDesc "Create an update proposal"
    , subParser "create-poll"
        $ Opt.info pGovernanceCreatePoll
        $ Opt.progDesc "Create an SPO poll"
    , subParser "answer-poll"
        $ Opt.info pGovernanceAnswerPoll
        $ Opt.progDesc "Answer an SPO poll"
    , subParser "verify-poll"
        $ Opt.info pGovernanceVerifyPoll
        $ Opt.progDesc "Verify an answer to a given SPO poll"
    , fmap GovernanceVoteCmd $ subParser "vote"
        $ Opt.info (pVoteCommmands envCli)
        $ Opt.progDesc "Vote related commands."
    , fmap GovernanceActionCmd $ subParser "action"
        $ Opt.info (pActionCommmands envCli)
        $ Opt.progDesc "Governance action related commands."
    ]
  where
    mirCertParsers :: Parser GovernanceCmd
    mirCertParsers = asum
      [ subParser "stake-addresses"
        $ Opt.info pLegacyMIRPayStakeAddresses
        $ Opt.progDesc "Create an MIR certificate to pay stake addresses"
      , subParser "transfer-to-treasury"
        $ Opt.info pLegacyMIRTransferToTreasury
        $ Opt.progDesc "Create an MIR certificate to transfer from the reserves pot to the treasury pot"
      , subParser "transfer-to-rewards"
        $ Opt.info pLegacyMIRTransferToReserves
        $ Opt.progDesc "Create an MIR certificate to transfer from the treasury pot to the reserves pot"
      ]

    pLegacyMIRPayStakeAddresses :: Parser GovernanceCmd
    pLegacyMIRPayStakeAddresses =
      GovernanceMIRPayStakeAddressesCertificate
        <$> pAnyShelleyToBabbageEra envCli
        <*> pMIRPot
        <*> some pStakeAddress
        <*> some pRewardAmt
        <*> pOutputFile

    pLegacyMIRTransferToTreasury :: Parser GovernanceCmd
    pLegacyMIRTransferToTreasury =
      GovernanceMIRTransfer
        <$> pAnyShelleyToBabbageEra envCli
        <*> pTransferAmt
        <*> pOutputFile
        <*> pure TransferToTreasury

    pLegacyMIRTransferToReserves :: Parser GovernanceCmd
    pLegacyMIRTransferToReserves =
      GovernanceMIRTransfer
        <$> pAnyShelleyToBabbageEra envCli
        <*> pTransferAmt
        <*> pOutputFile
        <*> pure TransferToReserves

    pGovernanceGenesisKeyDelegationCertificate :: Parser GovernanceCmd
    pGovernanceGenesisKeyDelegationCertificate =
      GovernanceGenesisKeyDelegationCertificate
        <$> pAnyShelleyBasedEra envCli
        <*> pGenesisVerificationKeyOrHashOrFile
        <*> pGenesisDelegateVerificationKeyOrHashOrFile
        <*> pVrfVerificationKeyOrHashOrFile
        <*> pOutputFile

    pUpdateProposal :: Parser GovernanceCmd
    pUpdateProposal =
      GovernanceUpdateProposal
        <$> pOutputFile
        <*> pEpochNoUpdateProp
        <*> some pGenesisVerificationKeyFile
        <*> pProtocolParametersUpdate
        <*> optional pCostModels

    pGovernanceCreatePoll :: Parser GovernanceCmd
    pGovernanceCreatePoll =
      GovernanceCreatePoll
        <$> pPollQuestion
        <*> some pPollAnswer
        <*> optional pPollNonce
        <*> pOutputFile

    pGovernanceAnswerPoll :: Parser GovernanceCmd
    pGovernanceAnswerPoll =
      GovernanceAnswerPoll
        <$> pPollFile
        <*> optional pPollAnswerIndex
        <*> optional pOutputFile

    pGovernanceVerifyPoll :: Parser GovernanceCmd
    pGovernanceVerifyPoll =
      GovernanceVerifyPoll
        <$> pPollFile
        <*> pPollTxFile
        <*> optional pOutputFile

pPollQuestion :: Parser Text
pPollQuestion =
  Opt.strOption $ mconcat
    [ Opt.long "question"
    , Opt.metavar "STRING"
    , Opt.help "The question for the poll."
    ]

pPollAnswer :: Parser Text
pPollAnswer =
  Opt.strOption $ mconcat
    [ Opt.long "answer"
    , Opt.metavar "STRING"
    , Opt.help "A possible choice for the poll. The option is repeatable."
    ]

pPollAnswerIndex :: Parser Word
pPollAnswerIndex =
  Opt.option auto $ mconcat
    [ Opt.long "answer"
    , Opt.metavar "INT"
    , Opt.help "The index of the chosen answer in the poll. Optional. Asked interactively if omitted."
    ]

pPollFile :: Parser (File GovernancePoll In)
pPollFile =
  Opt.strOption $ mconcat
    [ Opt.long "poll-file"
    , Opt.metavar "FILE"
    , Opt.help "Filepath to the ongoing poll."
    , Opt.completer (Opt.bashCompleter "file")
    ]

pPollTxFile :: Parser (TxFile In)
pPollTxFile =
  fmap File $ Opt.strOption $ mconcat
    [ Opt.long "tx-file"
    , Opt.metavar "FILE"
    , Opt.help "Filepath to the JSON TxBody or JSON Tx carrying a valid poll answer."
    , Opt.completer (Opt.bashCompleter "file")
    ]

pPollNonce :: Parser Word
pPollNonce =
  Opt.option auto $ mconcat
    [ Opt.long "nonce"
    , Opt.metavar "UINT"
    , Opt.help "An (optional) nonce for non-replayability."
    ]

pGenesisCmd :: EnvCli -> Parser GenesisCmd
pGenesisCmd envCli =
  asum
    [ subParser "key-gen-genesis"
        $ Opt.info pGenesisKeyGen
        $ Opt.progDesc "Create a Shelley genesis key pair"
    , subParser "key-gen-delegate"
        $ Opt.info pGenesisDelegateKeyGen
        $ Opt.progDesc "Create a Shelley genesis delegate key pair"
    , subParser "key-gen-utxo"
        $ Opt.info pGenesisUTxOKeyGen
        $ Opt.progDesc "Create a Shelley genesis UTxO key pair"
    , subParser "key-hash"
        $ Opt.info pGenesisKeyHash
        $ Opt.progDesc "Print the identifier (hash) of a public key"
    , subParser "get-ver-key"
        $ Opt.info pGenesisVerKey
        $ Opt.progDesc "Derive the verification key from a signing key"
    , subParser "initial-addr"
        $ Opt.info pGenesisAddr
        $ Opt.progDesc "Get the address for an initial UTxO based on the verification key"
    , subParser "initial-txin"
        $ Opt.info pGenesisTxIn
        $ Opt.progDesc "Get the TxIn for an initial UTxO based on the verification key"
    , subParser "create-cardano"
        $ Opt.info pGenesisCreateCardano
        $ Opt.progDesc
        $ mconcat
            [ "Create a Byron and Shelley genesis file from a genesis "
            , "template and genesis/delegation/spending keys."
            ]
    , subParser "create"
        $ Opt.info pGenesisCreate
        $ Opt.progDesc
        $ mconcat
            [ "Create a Shelley genesis file from a genesis "
            , "template and genesis/delegation/spending keys."
            ]
    , subParser "create-staked"
        $ Opt.info pGenesisCreateStaked
        $ Opt.progDesc
        $ mconcat
            [ "Create a staked Shelley genesis file from a genesis "
            , "template and genesis/delegation/spending keys."
            ]
    , subParser "hash"
        $ Opt.info pGenesisHash
        $ Opt.progDesc "Compute the hash of a genesis file"
    ]
  where
    pGenesisKeyGen :: Parser GenesisCmd
    pGenesisKeyGen =
      GenesisKeyGenGenesis
        <$> pVerificationKeyFileOut
        <*> pSigningKeyFileOut

    pGenesisDelegateKeyGen :: Parser GenesisCmd
    pGenesisDelegateKeyGen =
      GenesisKeyGenDelegate
        <$> pVerificationKeyFileOut
        <*> pSigningKeyFileOut
        <*> pOperatorCertIssueCounterFile

    pGenesisUTxOKeyGen :: Parser GenesisCmd
    pGenesisUTxOKeyGen =
      GenesisKeyGenUTxO
        <$> pVerificationKeyFileOut
        <*> pSigningKeyFileOut

    pGenesisKeyHash :: Parser GenesisCmd
    pGenesisKeyHash =
      GenesisCmdKeyHash
        <$> pVerificationKeyFileIn

    pGenesisVerKey :: Parser GenesisCmd
    pGenesisVerKey =
      GenesisVerKey
        <$> pVerificationKeyFileOut
        <*> pSigningKeyFileIn

    pGenesisAddr :: Parser GenesisCmd
    pGenesisAddr =
      GenesisAddr
        <$> pVerificationKeyFileIn
        <*> pNetworkId envCli
        <*> pMaybeOutputFile

    pGenesisTxIn :: Parser GenesisCmd
    pGenesisTxIn =
      GenesisTxIn
        <$> pVerificationKeyFileIn
        <*> pNetworkId envCli
        <*> pMaybeOutputFile

    pGenesisCreateCardano :: Parser GenesisCmd
    pGenesisCreateCardano =
      GenesisCreateCardano
        <$> pGenesisDir
        <*> pGenesisNumGenesisKeys
        <*> pGenesisNumUTxOKeys
        <*> pMaybeSystemStart
        <*> pInitialSupplyNonDelegated
        <*> (BlockCount <$> pSecurityParam)
        <*> pSlotLength
        <*> pSlotCoefficient
        <*> pNetworkId envCli
        <*> parseFilePath
              "byron-template"
              "JSON file with genesis defaults for each byron."
        <*> parseFilePath
              "shelley-template"
              "JSON file with genesis defaults for each shelley."
        <*> parseFilePath
              "alonzo-template"
              "JSON file with genesis defaults for alonzo."
        <*> parseFilePath
              "conway-template"
              "JSON file with genesis defaults for conway."
        <*> pNodeConfigTemplate

    pGenesisCreate :: Parser GenesisCmd
    pGenesisCreate =
      GenesisCreate
        <$> pKeyOutputFormat
        <*> pGenesisDir
        <*> pGenesisNumGenesisKeys
        <*> pGenesisNumUTxOKeys
        <*> pMaybeSystemStart
        <*> pInitialSupplyNonDelegated
        <*> pNetworkId envCli

    pGenesisCreateStaked :: Parser GenesisCmd
    pGenesisCreateStaked =
      GenesisCreateStaked
        <$> pKeyOutputFormat
        <*> pGenesisDir
        <*> pGenesisNumGenesisKeys
        <*> pGenesisNumUTxOKeys
        <*> pGenesisNumPools
        <*> pGenesisNumStDelegs
        <*> pMaybeSystemStart
        <*> pInitialSupplyNonDelegated
        <*> pInitialSupplyDelegated
        <*> pNetworkId envCli
        <*> pBulkPoolCredFiles
        <*> pBulkPoolsPerFile
        <*> pStuffedUtxoCount
        <*> Opt.optional pRelayJsonFp

    pGenesisHash :: Parser GenesisCmd
    pGenesisHash =
      GenesisHashFile <$> pGenesisFile "The genesis file."

    pGenesisDir :: Parser GenesisDir
    pGenesisDir =
      fmap GenesisDir $ Opt.strOption $ mconcat
        [ Opt.long "genesis-dir"
        , Opt.metavar "DIR"
        , Opt.help "The genesis directory containing the genesis template and required genesis/delegation/spending keys."
        ]

    pMaybeSystemStart :: Parser (Maybe SystemStart)
    pMaybeSystemStart =
      Opt.optional $ fmap (SystemStart . convertTime) $ Opt.strOption $ mconcat
        [ Opt.long "start-time"
        , Opt.metavar "UTC-TIME"
        , Opt.help "The genesis start time in YYYY-MM-DDThh:mm:ssZ format. If unspecified, will be the current time +30 seconds."
        ]

    pGenesisNumGenesisKeys :: Parser Word
    pGenesisNumGenesisKeys =
      Opt.option Opt.auto $ mconcat
        [ Opt.long "gen-genesis-keys"
        , Opt.metavar "INT"
        , Opt.help "The number of genesis keys to make [default is 3]."
        , Opt.value 3
        ]

    pNodeConfigTemplate :: Parser (Maybe FilePath)
    pNodeConfigTemplate = optional $ parseFilePath "node-config-template" "the node config template"

    pGenesisNumUTxOKeys :: Parser Word
    pGenesisNumUTxOKeys =
      Opt.option Opt.auto $ mconcat
        [ Opt.long "gen-utxo-keys"
        , Opt.metavar "INT"
        , Opt.help "The number of UTxO keys to make [default is 0]."
        , Opt.value 0
        ]

    pGenesisNumPools :: Parser Word
    pGenesisNumPools =
      Opt.option Opt.auto $ mconcat
        [ Opt.long "gen-pools"
        , Opt.metavar "INT"
        , Opt.help "The number of stake pool credential sets to make [default is 0]."
        , Opt.value 0
        ]

    pGenesisNumStDelegs :: Parser Word
    pGenesisNumStDelegs =
      Opt.option Opt.auto $ mconcat
        [ Opt.long "gen-stake-delegs"
        , Opt.metavar "INT"
        , Opt.help "The number of stake delegator credential sets to make [default is 0]."
        , Opt.value 0
        ]

    pStuffedUtxoCount :: Parser Word
    pStuffedUtxoCount =
      Opt.option Opt.auto $ mconcat
        [ Opt.long "num-stuffed-utxo"
        , Opt.metavar "INT"
        , Opt.help "The number of fake UTxO entries to generate [default is 0]."
        , Opt.value 0
        ]

    pRelayJsonFp :: Parser FilePath
    pRelayJsonFp =
      Opt.strOption $ mconcat
        [ Opt.long "relay-specification-file"
        , Opt.metavar "FILE"
        , Opt.help "JSON file specified the relays of each stake pool."
        , Opt.completer (Opt.bashCompleter "file")
        ]

    pInitialSupplyNonDelegated :: Parser (Maybe Lovelace)
    pInitialSupplyNonDelegated =
      Opt.optional $ fmap Lovelace $ Opt.option Opt.auto $ mconcat
        [ Opt.long "supply"
        , Opt.metavar "LOVELACE"
        , Opt.help "The initial coin supply in Lovelace which will be evenly distributed across initial, non-delegating stake holders."
        ]

    pInitialSupplyDelegated :: Parser Lovelace
    pInitialSupplyDelegated =
      fmap (Lovelace . fromMaybe 0) $ Opt.optional $ Opt.option Opt.auto $ mconcat
        [ Opt.long "supply-delegated"
        , Opt.metavar "LOVELACE"
        , Opt.help "The initial coin supply in Lovelace which will be evenly distributed across initial, delegating stake holders."
        , Opt.value 0
        ]

    pSecurityParam :: Parser Word64
    pSecurityParam =
      Opt.option Opt.auto $ mconcat
        [ Opt.long "security-param"
        , Opt.metavar "INT"
        , Opt.help "Security parameter for genesis file [default is 108]."
        , Opt.value 108
        ]

    pSlotLength :: Parser Word
    pSlotLength =
      Opt.option Opt.auto $ mconcat
        [ Opt.long "slot-length"
        , Opt.metavar "INT"
        , Opt.help "slot length (ms) parameter for genesis file [default is 1000]."
        , Opt.value 1000
        ]


    pSlotCoefficient :: Parser Rational
    pSlotCoefficient =
      Opt.option readRationalUnitInterval $ mconcat
        [ Opt.long "slot-coefficient"
        , Opt.metavar "RATIONAL"
        , Opt.help "Slot Coefficient for genesis file [default is .05]."
        , Opt.value 0.05
        ]

    pBulkPoolCredFiles :: Parser Word
    pBulkPoolCredFiles =
      Opt.option Opt.auto $ mconcat
        [ Opt.long "bulk-pool-cred-files"
        , Opt.metavar "INT"
        , Opt.help "Generate bulk pool credential files [default is 0]."
        , Opt.value 0
        ]

    pBulkPoolsPerFile :: Parser Word
    pBulkPoolsPerFile =
      Opt.option Opt.auto $ mconcat
        [ Opt.long "bulk-pools-per-file"
        , Opt.metavar "INT"
        , Opt.help "Each bulk pool to contain this many pool credential sets [default is 0]."
        , Opt.value 0
        ]


--
-- Shelley CLI flag parsers
--

data ParserFileDirection
  = Input
  | Output
  deriving (Eq, Show)

pAddressKeyType :: Parser AddressKeyType
pAddressKeyType =
  asum
    [ Opt.flag' AddressKeyShelley $ mconcat
        [ Opt.long "normal-key"
        , Opt.help "Use a normal Shelley-era key (default)."
        ]
    , Opt.flag' AddressKeyShelleyExtended $ mconcat
        [ Opt.long "extended-key"
        , Opt.help "Use an extended ed25519 Shelley-era key."
        ]
    , Opt.flag' AddressKeyByron $ mconcat
        [ Opt.long "byron-key"
        , Opt.help "Use a Byron-era key."
        ]
    , pure AddressKeyShelley
    ]

pProtocolParamsFile :: Parser ProtocolParamsFile
pProtocolParamsFile =
  fmap ProtocolParamsFile $ Opt.strOption $ mconcat
    [ Opt.long "protocol-params-file"
    , Opt.metavar "FILE"
    , Opt.help "Filepath of the JSON-encoded protocol parameters file"
    , Opt.completer (Opt.bashCompleter "file")
    ]

pCalculatePlutusScriptCost :: Parser TxBuildOutputOptions
pCalculatePlutusScriptCost =
  OutputScriptCostOnly <$> Opt.strOption
   ( Opt.long "calculate-plutus-script-cost" <>
     Opt.metavar "FILE" <>
     Opt.help "(File () Out) filepath of the script cost information." <>
     Opt.completer (Opt.bashCompleter "file")
   )

pCertificateFile
  :: BalanceTxExecUnits
  -> Parser (CertificateFile, Maybe (ScriptWitnessFiles WitCtxStake))
pCertificateFile balanceExecUnits =
  (,)
    <$> ( fmap CertificateFile $ asum
            [ Opt.strOption $ mconcat
                [ Opt.long "certificate-file"
                , Opt.metavar "CERTIFICATEFILE"
                , Opt.help helpText
                , Opt.completer (Opt.bashCompleter "file")
                ]
            , Opt.strOption (Opt.long "certificate" <> Opt.internal)
            ]
        )
    <*> optional (pCertifyingScriptOrReferenceScriptWit balanceExecUnits)
 where
  pCertifyingScriptOrReferenceScriptWit
    :: BalanceTxExecUnits -> Parser (ScriptWitnessFiles WitCtxStake)
  pCertifyingScriptOrReferenceScriptWit bExecUnits =
    pScriptWitnessFiles
     WitCtxStake
     balanceExecUnits
     "certificate" Nothing
     "the use of the certificate." <|>
    pPlutusStakeReferenceScriptWitnessFiles "certificate-" bExecUnits

  helpText = mconcat
    [ "Filepath of the certificate. This encompasses all "
    , "types of certificates (stake pool certificates, "
    , "stake key certificates etc). Optionally specify a script witness."
    ]

pPoolMetadataFile :: Parser (StakePoolMetadataFile In)
pPoolMetadataFile =
  fmap File $ Opt.strOption $ mconcat
    [ Opt.long "pool-metadata-file"
    , Opt.metavar "FILE"
    , Opt.help "Filepath of the pool metadata."
    , Opt.completer (Opt.bashCompleter "file")
    ]

pTxMetadataJsonSchema :: Parser TxMetadataJsonSchema
pTxMetadataJsonSchema =
  asum
    [ Opt.flag' ()
        (  Opt.long "json-metadata-no-schema"
        <> Opt.help "Use the \"no schema\" conversion from JSON to tx metadata."
        )
        $> TxMetadataJsonNoSchema
    , Opt.flag' ()
        (  Opt.long "json-metadata-detailed-schema"
        <> Opt.help "Use the \"detailed schema\" conversion from JSON to tx metadata."
        )
        $> TxMetadataJsonDetailedSchema
    , -- Default to the no-schema conversion.
      pure TxMetadataJsonNoSchema
    ]

convertTime :: String -> UTCTime
convertTime =
  parseTimeOrError False defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"

pMetadataFile :: Parser MetadataFile
pMetadataFile =
  asum
    [ fmap MetadataFileJSON
        $ asum
            [ Opt.strOption $ mconcat
                [ Opt.long "metadata-json-file"
                , Opt.metavar "FILE"
                , Opt.help "Filepath of the metadata file, in JSON format."
                , Opt.completer (Opt.bashCompleter "file")
                ]
            , Opt.strOption $ mconcat
                [ Opt.long "metadata-file" -- backward compat name
                , Opt.internal
                ]
            ]
    , fmap MetadataFileCBOR $ Opt.strOption $ mconcat
        [ Opt.long "metadata-cbor-file"
        , Opt.metavar "FILE"
        , Opt.help "Filepath of the metadata, in raw CBOR format."
        , Opt.completer (Opt.bashCompleter "file")
        ]
    ]

pWithdrawal
  :: BalanceTxExecUnits
  -> Parser (StakeAddress,
            Lovelace,
            Maybe (ScriptWitnessFiles WitCtxStake))
pWithdrawal balance =
    (\(stakeAddr,lovelace) maybeScriptFp -> (stakeAddr, lovelace, maybeScriptFp))
      <$> Opt.option (readerFromParsecParser parseWithdrawal)
            (  Opt.long "withdrawal"
            <> Opt.metavar "WITHDRAWAL"
            <> Opt.help helpText
            )
      <*> optional pWithdrawalScriptOrReferenceScriptWit
 where
  pWithdrawalScriptOrReferenceScriptWit :: Parser (ScriptWitnessFiles WitCtxStake)
  pWithdrawalScriptOrReferenceScriptWit =
   pScriptWitnessFiles
     WitCtxStake
     balance
     "withdrawal" Nothing
     "the withdrawal of rewards." <|>
   pPlutusStakeReferenceScriptWitnessFiles "withdrawal-" balance

  helpText = mconcat
    [ "The reward withdrawal as StakeAddress+Lovelace where "
    , "StakeAddress is the Bech32-encoded stake address "
    , "followed by the amount in Lovelace. Optionally specify "
    , "a script witness."
    ]

  parseWithdrawal :: Parsec.Parser (StakeAddress, Lovelace)
  parseWithdrawal =
    (,) <$> parseStakeAddress <* Parsec.char '+' <*> parseLovelace

pPlutusStakeReferenceScriptWitnessFiles
  :: String
  -> BalanceTxExecUnits -- ^ Use the @execution-units@ flag.
  -> Parser (ScriptWitnessFiles WitCtxStake)
pPlutusStakeReferenceScriptWitnessFiles prefix autoBalanceExecUnits =
  PlutusReferenceScriptWitnessFiles
    <$> pReferenceTxIn prefix "plutus"
    <*> pPlutusScriptLanguage prefix
    <*> pure NoScriptDatumOrFileForStake
    <*> pScriptRedeemerOrFile (prefix ++ "reference-tx-in")
    <*> (case autoBalanceExecUnits of
          AutoBalance -> pure (ExecutionUnits 0 0)
          ManualBalance -> pExecutionUnits $ prefix ++ "reference-tx-in")
    <*> pure Nothing

pPlutusScriptLanguage :: String -> Parser AnyScriptLanguage
pPlutusScriptLanguage prefix =
  Opt.flag' (AnyScriptLanguage $ PlutusScriptLanguage PlutusScriptV2)
    (  Opt.long (prefix ++ "plutus-script-v2")
    <> Opt.help "Specify a plutus script v2 reference script."
    )

pUpdateProposalFile :: Parser UpdateProposalFile
pUpdateProposalFile =
  fmap UpdateProposalFile
    $ asum
        [ Opt.strOption $ mconcat
          [ Opt.long "update-proposal-file"
          , Opt.metavar "FILE"
          , Opt.help "Filepath of the update proposal."
          , Opt.completer (Opt.bashCompleter "file")
          ]
        , Opt.strOption $ mconcat
          [ Opt.long "update-proposal"
          , Opt.internal
          ]
        ]

pRequiredSigner :: Parser RequiredSigner
pRequiredSigner =
      RequiredSignerSkeyFile <$> sKeyFile
  <|> RequiredSignerHash <$> sPayKeyHash
 where
  sKeyFile :: Parser (SigningKeyFile In)
  sKeyFile = fmap File $ Opt.strOption $ mconcat
    [ Opt.long "required-signer"
    , Opt.metavar "FILE"
    , Opt.help $ mconcat
      [ "Input filepath of the signing key (zero or more) whose "
      , "signature is required."
      ]
    , Opt.completer (Opt.bashCompleter "file")
    ]
  sPayKeyHash :: Parser (Hash PaymentKey)
  sPayKeyHash =
    Opt.option (readerFromParsecParser $ parseHash (AsHash AsPaymentKey)) $ mconcat
      [ Opt.long "required-signer-hash"
      , Opt.metavar "HASH"
      , Opt.help $ mconcat
        [ "Hash of the verification key (zero or more) whose "
        , "signature is required."
        ]
      ]

pVrfSigningKeyFile :: Parser (SigningKeyFile In)
pVrfSigningKeyFile =
  fmap File $ Opt.strOption $ mconcat
    [ Opt.long "vrf-signing-key-file"
    , Opt.metavar "FILE"
    , Opt.help "Input filepath of the VRF signing key."
    , Opt.completer (Opt.bashCompleter "file")
    ]

pWhichLeadershipSchedule :: Parser EpochLeadershipSchedule
pWhichLeadershipSchedule = pCurrent <|> pNext
  where
    pCurrent :: Parser EpochLeadershipSchedule
    pCurrent =
      Opt.flag' CurrentEpoch $ mconcat
        [ Opt.long "current"
        , Opt.help "Get the leadership schedule for the current epoch."
        ]

    pNext :: Parser EpochLeadershipSchedule
    pNext =
      Opt.flag' NextEpoch $ mconcat
        [ Opt.long "next"
        , Opt.help "Get the leadership schedule for the following epoch."
        ]

pWitnessSigningData :: Parser WitnessSigningData
pWitnessSigningData =
  KeyWitnessSigningData
    <$> ( fmap File $ Opt.strOption $ mconcat
          [ Opt.long "signing-key-file"
          , Opt.metavar "FILE"
          , Opt.help "Input filepath of the signing key (one or more)."
          , Opt.completer (Opt.bashCompleter "file")
          ]
        )
    <*> optional pByronAddress

pSigningKeyFileIn :: Parser (SigningKeyFile In)
pSigningKeyFileIn =
  fmap File $ Opt.strOption $ mconcat
    [ Opt.long "signing-key-file"
    , Opt.metavar "FILE"
    , Opt.help "Input filepath of the signing key."
    , Opt.completer (Opt.bashCompleter "file")
    ]

pKesPeriod :: Parser KESPeriod
pKesPeriod =
  fmap KESPeriod $ Opt.option (bounded "KES_PERIOD") $ mconcat
    [ Opt.long "kes-period"
    , Opt.metavar "NATURAL"
    , Opt.help "The start of the KES key validity period."
    ]

pEpochNo :: Parser EpochNo
pEpochNo =
  fmap EpochNo $ Opt.option (bounded "EPOCH") $ mconcat
    [ Opt.long "epoch"
    , Opt.metavar "NATURAL"
    , Opt.help "The epoch number."
    ]


pEpochNoUpdateProp :: Parser EpochNo
pEpochNoUpdateProp =
  fmap EpochNo $ Opt.option (bounded "EPOCH") $ mconcat
    [ Opt.long "epoch"
    , Opt.metavar "EPOCH"
    , Opt.help "The epoch number in which the update proposal is valid."
    ]

pGenesisFile :: String -> Parser GenesisFile
pGenesisFile desc =
  fmap GenesisFile $ Opt.strOption $ mconcat
    [ Opt.long "genesis"
    , Opt.metavar "FILE"
    , Opt.help desc
    , Opt.completer (Opt.bashCompleter "file")
    ]

pOperationalCertificateFile :: Parser (File () direction)
pOperationalCertificateFile =
  Opt.strOption $ mconcat
    [ Opt.long "op-cert-file"
    , Opt.metavar "FILE"
    , Opt.help "Filepath of the node's operational certificate."
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

pPoolIdOutputFormat :: Parser PoolIdOutputFormat
pPoolIdOutputFormat =
  Opt.option readPoolIdOutputFormat $ mconcat
    [ Opt.long "output-format"
    , Opt.metavar "STRING"
    , Opt.help $ mconcat
      [ "Optional pool id output format. Accepted output formats are \"hex\" "
      , "and \"bech32\" (default is \"bech32\")."
      ]
    , Opt.value PoolIdOutputFormatBech32
    ]

pMaybeOutputFile :: Parser (Maybe (File content Out))
pMaybeOutputFile =
  optional $ fmap File $ Opt.strOption $ mconcat
    [ Opt.long "out-file"
    , Opt.metavar "FILE"
    , Opt.help "Optional output file. Default is to write to stdout."
    , Opt.completer (Opt.bashCompleter "file")
    ]

pVerificationKey
  :: forall keyrole. SerialiseAsBech32 (VerificationKey keyrole)
  => AsType keyrole
  -> Parser (VerificationKey keyrole)
pVerificationKey asType =
  Opt.option (readVerificationKey asType) $ mconcat
    [ Opt.long "verification-key"
    , Opt.metavar "STRING"
    , Opt.help "Verification key (Bech32 or hex-encoded)."
    ]

pVerificationKeyOrFile
  :: SerialiseAsBech32 (VerificationKey keyrole)
  => AsType keyrole
  -> Parser (VerificationKeyOrFile keyrole)
pVerificationKeyOrFile asType =
  asum
    [ VerificationKeyValue <$> pVerificationKey asType
    , VerificationKeyFilePath <$> pVerificationKeyFileIn
    ]

pExtendedVerificationKeyFileIn :: Parser (VerificationKeyFile In)
pExtendedVerificationKeyFileIn =
  fmap File $ Opt.strOption $ mconcat
    [ Opt.long "extended-verification-key-file"
    , Opt.metavar "FILE"
    , Opt.help "Input filepath of the ed25519-bip32 verification key."
    , Opt.completer (Opt.bashCompleter "file")
    ]

pGenesisVerificationKeyFile :: Parser (VerificationKeyFile In)
pGenesisVerificationKeyFile =
  fmap File $ Opt.strOption $ mconcat
    [ Opt.long "genesis-verification-key-file"
    , Opt.metavar "FILE"
    , Opt.help "Filepath of the genesis verification key."
    , Opt.completer (Opt.bashCompleter "file")
    ]

pGenesisVerificationKeyHash :: Parser (Hash GenesisKey)
pGenesisVerificationKeyHash =
  Opt.option (Opt.eitherReader deserialiseFromHex) $ mconcat
    [ Opt.long "genesis-verification-key-hash"
    , Opt.metavar "STRING"
    , Opt.help "Genesis verification key hash (hex-encoded)."
    ]
  where
    deserialiseFromHex :: String -> Either String (Hash GenesisKey)
    deserialiseFromHex =
      first (\e -> "Invalid genesis verification key hash: " ++ displayError e)
        . deserialiseFromRawBytesHex (AsHash AsGenesisKey)
        . BSC.pack

pGenesisVerificationKey :: Parser (VerificationKey GenesisKey)
pGenesisVerificationKey =
  Opt.option (Opt.eitherReader deserialiseFromHex) $ mconcat
    [ Opt.long "genesis-verification-key"
    , Opt.metavar "STRING"
    , Opt.help "Genesis verification key (hex-encoded)."
    ]
  where
    deserialiseFromHex :: String -> Either String (VerificationKey GenesisKey)
    deserialiseFromHex =
      first (\e -> "Invalid genesis verification key: " ++ displayError e)
        . deserialiseFromRawBytesHex (AsVerificationKey AsGenesisKey)
        . BSC.pack

pGenesisVerificationKeyOrFile :: Parser (VerificationKeyOrFile GenesisKey)
pGenesisVerificationKeyOrFile =
  asum
    [ VerificationKeyValue <$> pGenesisVerificationKey
    , VerificationKeyFilePath <$> pGenesisVerificationKeyFile
    ]

pGenesisVerificationKeyOrHashOrFile :: Parser (VerificationKeyOrHashOrFile GenesisKey)
pGenesisVerificationKeyOrHashOrFile =
  asum
    [ VerificationKeyOrFile <$> pGenesisVerificationKeyOrFile
    , VerificationKeyHash <$> pGenesisVerificationKeyHash
    ]

pGenesisDelegateVerificationKeyFile :: Parser (VerificationKeyFile In)
pGenesisDelegateVerificationKeyFile =
  fmap File $ Opt.strOption $ mconcat
    [ Opt.long "genesis-delegate-verification-key-file"
    , Opt.metavar "FILE"
    , Opt.help "Filepath of the genesis delegate verification key."
    , Opt.completer (Opt.bashCompleter "file")
    ]

pGenesisDelegateVerificationKeyHash :: Parser (Hash GenesisDelegateKey)
pGenesisDelegateVerificationKeyHash =
  Opt.option (Opt.eitherReader deserialiseFromHex) $ mconcat
    [ Opt.long "genesis-delegate-verification-key-hash"
    , Opt.metavar "STRING"
    , Opt.help "Genesis delegate verification key hash (hex-encoded)."
    ]
  where
    deserialiseFromHex :: String -> Either String (Hash GenesisDelegateKey)
    deserialiseFromHex =
      first
        (\e ->
          "Invalid genesis delegate verification key hash: " ++ displayError e)
        . deserialiseFromRawBytesHex (AsHash AsGenesisDelegateKey)
        . BSC.pack

pGenesisDelegateVerificationKeyOrFile
  :: Parser (VerificationKeyOrFile GenesisDelegateKey)
pGenesisDelegateVerificationKeyOrFile =
  asum
    [ VerificationKeyValue <$> pGenesisDelegateVerificationKey
    , VerificationKeyFilePath <$> pGenesisDelegateVerificationKeyFile
    ]

pGenesisDelegateVerificationKeyOrHashOrFile
  :: Parser (VerificationKeyOrHashOrFile GenesisDelegateKey)
pGenesisDelegateVerificationKeyOrHashOrFile =
  asum
    [ VerificationKeyOrFile <$> pGenesisDelegateVerificationKeyOrFile
    , VerificationKeyHash <$> pGenesisDelegateVerificationKeyHash
    ]

pKesVerificationKeyOrFile :: Parser (VerificationKeyOrFile KesKey)
pKesVerificationKeyOrFile =
  asum
    [ VerificationKeyValue <$> pKesVerificationKey
    , VerificationKeyFilePath <$> pKesVerificationKeyFile
    ]

pKesVerificationKey :: Parser (VerificationKey KesKey)
pKesVerificationKey =
  Opt.option (Opt.eitherReader deserialiseVerKey) $ mconcat
    [ Opt.long "kes-verification-key"
    , Opt.metavar "STRING"
    , Opt.help "A Bech32 or hex-encoded hot KES verification key."
    ]
  where
    asType :: AsType (VerificationKey KesKey)
    asType = AsVerificationKey AsKesKey

    deserialiseVerKey :: String -> Either String (VerificationKey KesKey)
    deserialiseVerKey str =
      case deserialiseFromBech32 asType (Text.pack str) of
        Right res -> Right res

        -- The input was valid Bech32, but some other error occurred.
        Left err@(Bech32UnexpectedPrefix _ _) -> Left (displayError err)
        Left err@(Bech32DataPartToBytesError _) -> Left (displayError err)
        Left err@(Bech32DeserialiseFromBytesError _) -> Left (displayError err)
        Left err@(Bech32WrongPrefix _ _) -> Left (displayError err)

        -- The input was not valid Bech32. Attempt to deserialise it as hex.
        Left (Bech32DecodingError _) ->
          first
            (\e -> "Invalid stake pool verification key: " ++ displayError e) $
          deserialiseFromRawBytesHex asType (BSC.pack str)

pKesVerificationKeyFile :: Parser (VerificationKeyFile In)
pKesVerificationKeyFile =
  fmap File $ asum
    [ Opt.strOption $ mconcat
      [ Opt.long "kes-verification-key-file"
      , Opt.metavar "FILE"
      , Opt.help "Filepath of the hot KES verification key."
      , Opt.completer (Opt.bashCompleter "file")
      ]
    , Opt.strOption $ mconcat
      [ Opt.long "hot-kes-verification-key-file"
      , Opt.internal
      ]
    ]

pTxSubmitFile :: Parser FilePath
pTxSubmitFile =
  Opt.strOption $ mconcat
    [ Opt.long "tx-file"
    , Opt.metavar "FILE"
    , Opt.help "Filepath of the transaction you intend to submit."
    , Opt.completer (Opt.bashCompleter "file")
    ]

pTxIn :: BalanceTxExecUnits
      -> Parser (TxIn, Maybe (ScriptWitnessFiles WitCtxTxIn))
pTxIn balance =
     (,) <$> Opt.option (readerFromParsecParser parseTxIn)
               (  Opt.long "tx-in"
                <> Opt.metavar "TX-IN"
               <> Opt.help "TxId#TxIx"
               )
         <*> optional (pPlutusReferenceScriptWitness balance <|>
                       pSimpleReferenceSpendingScriptWitess <|>
                       pEmbeddedPlutusScriptWitness
                       )
 where
  pSimpleReferenceSpendingScriptWitess :: Parser (ScriptWitnessFiles WitCtxTxIn)
  pSimpleReferenceSpendingScriptWitess =
    createSimpleReferenceScriptWitnessFiles
      <$> pReferenceTxIn "simple-script-" "simple"
   where
    createSimpleReferenceScriptWitnessFiles
      :: TxIn
      -> ScriptWitnessFiles WitCtxTxIn
    createSimpleReferenceScriptWitnessFiles refTxIn  =
      let simpleLang = AnyScriptLanguage SimpleScriptLanguage
      in SimpleReferenceScriptWitnessFiles refTxIn simpleLang Nothing

  pPlutusReferenceScriptWitness :: BalanceTxExecUnits -> Parser (ScriptWitnessFiles WitCtxTxIn)
  pPlutusReferenceScriptWitness autoBalanceExecUnits =
    createPlutusReferenceScriptWitnessFiles
      <$> pReferenceTxIn "spending-" "plutus"
      <*> pPlutusScriptLanguage "spending-"
      <*> pScriptDatumOrFile "spending-reference-tx-in" WitCtxTxIn
      <*> pScriptRedeemerOrFile "spending-reference-tx-in"
      <*> (case autoBalanceExecUnits of
              AutoBalance -> pure (ExecutionUnits 0 0)
              ManualBalance -> pExecutionUnits "spending-reference-tx-in")
   where
    createPlutusReferenceScriptWitnessFiles
      :: TxIn
      -> AnyScriptLanguage
      -> ScriptDatumOrFile WitCtxTxIn
      -> ScriptRedeemerOrFile
      -> ExecutionUnits
      -> ScriptWitnessFiles WitCtxTxIn
    createPlutusReferenceScriptWitnessFiles refIn sLang sDatum sRedeemer execUnits =
      PlutusReferenceScriptWitnessFiles refIn sLang sDatum sRedeemer execUnits Nothing

  pEmbeddedPlutusScriptWitness :: Parser (ScriptWitnessFiles WitCtxTxIn)
  pEmbeddedPlutusScriptWitness =
    pScriptWitnessFiles
      WitCtxTxIn
      balance
      "tx-in" (Just "txin")
      "the spending of the transaction input."

pTxInCollateral :: Parser TxIn
pTxInCollateral =
    Opt.option (readerFromParsecParser parseTxIn)
      (  Opt.long "tx-in-collateral"
      <> Opt.metavar "TX-IN"
      <> Opt.help "TxId#TxIx"
      )

pReturnCollateral :: Parser TxOutAnyEra
pReturnCollateral =
  Opt.option (readerFromParsecParser parseTxOutAnyEra)
          ( mconcat
            [ Opt.long "tx-out-return-collateral"
            , Opt.metavar "ADDRESS VALUE"
            -- TODO alonzo: Update the help text to describe the new syntax as well.
            , Opt.help ( "The transaction output as ADDRESS VALUE where ADDRESS is " <>
                        "the Bech32-encoded address followed by the value in " <>
                        "Lovelace. In the situation where your collateral txin " <>
                        "over collateralizes the transaction, you can optionally " <>
                        "specify a tx out of your choosing to return the excess Lovelace."
            )
            ]
          )
    <*> pure TxOutDatumByNone -- TODO: Babbage era - we should be able to return these
    <*> pure ReferenceScriptAnyEraNone -- TODO: Babbage era - we should be able to return these

pTotalCollateral :: Parser Lovelace
pTotalCollateral =
  Opt.option (Lovelace <$> readerFromParsecParser decimal) $ mconcat
  [ Opt.long "tx-total-collateral"
  , Opt.metavar "INTEGER"
  , Opt.help $ mconcat
    [ "The total amount of collateral that will be collected "
    , "as fees in the event of a Plutus script failure. Must be used "
    , "in conjuction with \"--tx-out-return-collateral\"."
    ]
  ]

pWitnessOverride :: Parser Word
pWitnessOverride = Opt.option Opt.auto $ mconcat
  [ Opt.long "witness-override"
  , Opt.metavar "WORD"
  , Opt.help "Specify and override the number of witnesses the transaction requires."
  ]


pTxOut :: Parser TxOutAnyEra
pTxOut =
        Opt.option (readerFromParsecParser parseTxOutAnyEra)
          (  Opt.long "tx-out"
          <> Opt.metavar "ADDRESS VALUE"
          -- TODO alonzo: Update the help text to describe the new syntax as well.
          <> Opt.help "The transaction output as ADDRESS VALUE where ADDRESS is \
                      \the Bech32-encoded address followed by the value in \
                      \the multi-asset syntax (including simply Lovelace)."
          )
    <*> pTxOutDatum
    <*> pRefScriptFp


pTxOutDatum :: Parser TxOutDatumAnyEra
pTxOutDatum =
      pTxOutDatumByHashOnly
  <|> pTxOutDatumByHashOf
  <|> pTxOutDatumByValue
  <|> pTxOutInlineDatumByValue
  <|> pure TxOutDatumByNone
  where
    pTxOutDatumByHashOnly =
      fmap TxOutDatumByHashOnly
      $ Opt.option (readerFromParsecParser $ parseHash (AsHash AsScriptData))
      $ mconcat
        [ Opt.long "tx-out-datum-hash"
        , Opt.metavar "HASH"
        , Opt.help $ mconcat
          [ "The script datum hash for this tx output, as "
          , "the raw datum hash (in hex)."
          ]
        ]

    pTxOutDatumByHashOf = TxOutDatumByHashOf <$>
        pScriptDataOrFile
          "tx-out-datum-hash"
          ( mconcat
            [ "The script datum hash for this tx output, by hashing the "
            , "script datum given here in JSON syntax."
            ]
          )
          ( mconcat
            [ "The script datum hash for this tx output, by hashing the "
            , "script datum in the given JSON file."
            ]
          )

    pTxOutDatumByValue =
      TxOutDatumByValue <$>
        pScriptDataOrFile
          "tx-out-datum-embed"
          ( mconcat
            [ "The script datum to embed in the tx for this output, "
            , "given here in JSON syntax."
            ]
          )
          ( mconcat
            [ "The script datum to embed in the tx for this output, "
            , "in the given JSON file."
            ]
          )

    pTxOutInlineDatumByValue =
      TxOutInlineDatumByValue <$>
        pScriptDataOrFile
          "tx-out-inline-datum"
          ( mconcat
            [ "The script datum to embed in the tx output as an inline datum, "
            , "given here in JSON syntax."
            ]
          )
          ( mconcat
            [ "The script datum to embed in the tx output as an inline datum, "
            , "in the given JSON file."
            ]
          )

pRefScriptFp :: Parser ReferenceScriptAnyEra
pRefScriptFp =
  ReferenceScriptAnyEra <$> Opt.strOption
    (  Opt.long "tx-out-reference-script-file"
    <> Opt.metavar "FILE"
    <> Opt.help "Reference script input file."
    <> Opt.completer (Opt.bashCompleter "file")
    ) <|> pure ReferenceScriptAnyEraNone

pMintMultiAsset
  :: BalanceTxExecUnits
  -> Parser (Value, [ScriptWitnessFiles WitCtxMint])
pMintMultiAsset balanceExecUnits =
  (,) <$> Opt.option
            (readerFromParsecParser parseValue)
              (  Opt.long "mint"
              <> Opt.metavar "VALUE"
              <> Opt.help helpText
              )
      <*> some (pMintingScriptOrReferenceScriptWit balanceExecUnits <|>
                pSimpleReferenceMintingScriptWitness <|>
                pPlutusMintReferenceScriptWitnessFiles balanceExecUnits
               )
 where
  pMintingScriptOrReferenceScriptWit
    :: BalanceTxExecUnits -> Parser (ScriptWitnessFiles WitCtxMint)
  pMintingScriptOrReferenceScriptWit bExecUnits =
   pScriptWitnessFiles
     WitCtxMint
     bExecUnits
     "mint" (Just "minting")
     "the minting of assets for a particular policy Id."

  pSimpleReferenceMintingScriptWitness :: Parser (ScriptWitnessFiles WitCtxMint)
  pSimpleReferenceMintingScriptWitness =
    createSimpleMintingReferenceScriptWitnessFiles
      <$> pReferenceTxIn "simple-minting-script-" "simple"
      <*> pPolicyId
   where
    createSimpleMintingReferenceScriptWitnessFiles
      :: TxIn
      -> PolicyId
      -> ScriptWitnessFiles WitCtxMint
    createSimpleMintingReferenceScriptWitnessFiles refTxIn pid =
      let simpleLang = AnyScriptLanguage SimpleScriptLanguage
      in SimpleReferenceScriptWitnessFiles refTxIn simpleLang (Just pid)

  pPlutusMintReferenceScriptWitnessFiles
    :: BalanceTxExecUnits ->  Parser (ScriptWitnessFiles WitCtxMint)
  pPlutusMintReferenceScriptWitnessFiles autoBalanceExecUnits =
   PlutusReferenceScriptWitnessFiles
     <$> pReferenceTxIn "mint-" "plutus"
     <*> pPlutusScriptLanguage "mint-"
     <*> pure NoScriptDatumOrFileForMint
     <*> pScriptRedeemerOrFile "mint-reference-tx-in"
     <*> (case autoBalanceExecUnits of
           AutoBalance -> pure (ExecutionUnits 0 0)
           ManualBalance -> pExecutionUnits "mint-reference-tx-in")
     <*> (Just <$> pPolicyId)

  helpText = mconcat
    [ "Mint multi-asset value(s) with the multi-asset cli syntax. "
    , "You must specify a script witness."
    ]

pPolicyId :: Parser PolicyId
pPolicyId =
  Opt.option (readerFromParsecParser policyId) $ mconcat
    [ Opt.long "policy-id"
    , Opt.metavar "HASH"
    , Opt.help "Policy id of minting script."
    ]


pInvalidBefore :: Parser SlotNo
pInvalidBefore = fmap SlotNo $ asum
  [ Opt.option (bounded "SLOT") $ mconcat
    [ Opt.long "invalid-before"
    , Opt.metavar "SLOT"
    , Opt.help "Time that transaction is valid from (in slots)."
    ]
  , Opt.option (bounded "SLOT") $ mconcat
    [ Opt.long "lower-bound"
    , Opt.metavar "SLOT"
    , Opt.help $ mconcat
      [ "Time that transaction is valid from (in slots) "
      , "(deprecated; use --invalid-before instead)."
      ]
    , Opt.internal
    ]
  ]

pInvalidHereafter :: Parser SlotNo
pInvalidHereafter =
  fmap SlotNo $ asum
  [ Opt.option (bounded "SLOT") $ mconcat
    [ Opt.long "invalid-hereafter"
    , Opt.metavar "SLOT"
    , Opt.help "Time that transaction is valid until (in slots)."
    ]
  , Opt.option (bounded "SLOT") $ mconcat
    [ Opt.long "upper-bound"
    , Opt.metavar "SLOT"
    , Opt.help $ mconcat
      [ "Time that transaction is valid until (in slots) "
      , "(deprecated; use --invalid-hereafter instead)."
      ]
    , Opt.internal
    ]
  , Opt.option (bounded "SLOT") $ mconcat
    [ Opt.long "ttl"
    , Opt.metavar "SLOT"
    , Opt.help "Time to live (in slots) (deprecated; use --invalid-hereafter instead)."
    , Opt.internal
    ]
  ]

pTxFee :: Parser Lovelace
pTxFee =
  fmap (Lovelace . (fromIntegral :: Natural -> Integer)) $ Opt.option Opt.auto $ mconcat
    [ Opt.long "fee"
    , Opt.metavar "LOVELACE"
    , Opt.help "The fee amount in Lovelace."
    ]

pWitnessFile :: Parser WitnessFile
pWitnessFile =
  fmap WitnessFile $ Opt.strOption $ mconcat
    [ Opt.long "witness-file"
    , Opt.metavar "FILE"
    , Opt.help "Filepath of the witness"
    , Opt.completer (Opt.bashCompleter "file")
    ]

pTxBodyFileIn :: Parser (TxBodyFile In)
pTxBodyFileIn =
  fmap File $ Opt.strOption $ mconcat
    [ Opt.long "tx-body-file"
    , Opt.metavar "FILE"
    , Opt.help "Input filepath of the JSON TxBody."
    , Opt.completer (Opt.bashCompleter "file")
    ]

pTxBodyFileOut :: Parser (TxBodyFile Out)
pTxBodyFileOut =
  fmap File $ asum
    [ Opt.strOption $ mconcat
      [ Opt.long "out-file"
      , Opt.metavar "FILE"
      , Opt.help "Output filepath of the JSON TxBody."
      , Opt.completer (Opt.bashCompleter "file")
      ]
    , Opt.strOption $ mconcat
      [ Opt.long "tx-body-file"
      , Opt.internal
      ]
    ]

pTxFileIn :: Parser (TxFile In)
pTxFileIn =
  fmap File $ Opt.strOption $ mconcat
    [ Opt.long "tx-file"
    , Opt.metavar "FILE"
    , Opt.help "Input filepath of the JSON Tx."
    , Opt.completer (Opt.bashCompleter "file")
    ]

pTxFileOut :: Parser (TxFile Out)
pTxFileOut =
  fmap File $ asum
    [ Opt.strOption $ mconcat
      [ Opt.long "out-file"
      , Opt.metavar "FILE"
      , Opt.help "Output filepath of the JSON Tx."
      , Opt.completer (Opt.bashCompleter "file")
      ]
    , Opt.strOption $ mconcat
      [ Opt.long "tx-file"
      , Opt.internal
      ]
    ]

pInputTxOrTxBodyFile :: Parser InputTxBodyOrTxFile
pInputTxOrTxBodyFile =
  asum
    [ InputTxBodyFile <$> pTxBodyFileIn
    , InputTxFile <$> pTxFileIn
    ]

pTxInCount :: Parser TxInCount
pTxInCount =
  fmap TxInCount $ Opt.option Opt.auto $ mconcat
    [ Opt.long "tx-in-count"
    , Opt.metavar "NATURAL"
    , Opt.help "The number of transaction inputs."
    ]

pTxOutCount :: Parser TxOutCount
pTxOutCount =
  fmap TxOutCount $ Opt.option Opt.auto $ mconcat
    [ Opt.long "tx-out-count"
    , Opt.metavar "NATURAL"
    , Opt.help "The number of transaction outputs."
    ]

pTxShelleyWitnessCount :: Parser TxShelleyWitnessCount
pTxShelleyWitnessCount =
  fmap TxShelleyWitnessCount $ Opt.option Opt.auto $ mconcat
    [ Opt.long "witness-count"
    , Opt.metavar "NATURAL"
    , Opt.help "The number of Shelley key witnesses."
    ]

pTxByronWitnessCount :: Parser TxByronWitnessCount
pTxByronWitnessCount =
  fmap TxByronWitnessCount $ Opt.option Opt.auto $ mconcat
    [ Opt.long "byron-witness-count"
    , Opt.metavar "NATURAL"
    , Opt.help "The number of Byron key witnesses (default is 0)."
    , Opt.value 0
    ]

pQueryUTxOFilter :: Parser QueryUTxOFilter
pQueryUTxOFilter =
  asum
    [ pQueryUTxOWhole
    , pQueryUTxOByAddress
    , pQueryUTxOByTxIn
    ]
  where
    pQueryUTxOWhole =
      Opt.flag' QueryUTxOWhole $ mconcat
        [ Opt.long "whole-utxo"
        , Opt.help "Return the whole UTxO (only appropriate on small testnets)."
        ]

    pQueryUTxOByAddress :: Parser QueryUTxOFilter
    pQueryUTxOByAddress = QueryUTxOByAddress . Set.fromList <$> some pByAddress

    pByAddress :: Parser AddressAny
    pByAddress =
      Opt.option (readerFromParsecParser parseAddressAny) $ mconcat
        [ Opt.long "address"
        , Opt.metavar "ADDRESS"
        , Opt.help "Filter by Cardano address(es) (Bech32-encoded)."
        ]

    pQueryUTxOByTxIn :: Parser QueryUTxOFilter
    pQueryUTxOByTxIn = QueryUTxOByTxIn . Set.fromList <$> some pByTxIn

    pByTxIn :: Parser TxIn
    pByTxIn =
      Opt.option (readerFromParsecParser parseTxIn) $ mconcat
        [ Opt.long "tx-in"
        , Opt.metavar "TX-IN"
        , Opt.help "Filter by transaction input (TxId#TxIx)."
        ]

pFilterByStakeAddress :: Parser StakeAddress
pFilterByStakeAddress =
    Opt.option (readerFromParsecParser parseStakeAddress) $ mconcat
      [ Opt.long "address"
      , Opt.metavar "ADDRESS"
      , Opt.help "Filter by Cardano stake address (Bech32-encoded)."
      ]

pByronAddress :: Parser (Address ByronAddr)
pByronAddress =
  Opt.option (Opt.eitherReader deserialise) $ mconcat
    [ Opt.long "address"
    , Opt.metavar "STRING"
    , Opt.help "Byron address (Base58-encoded)."
    ]
  where
    deserialise :: String -> Either String (Address ByronAddr)
    deserialise =
      maybe (Left "Invalid Byron address.") Right
        . deserialiseAddress AsByronAddress
        . Text.pack

pAddress :: Parser Text
pAddress =
  fmap Text.pack $ Opt.strOption $ mconcat
    [ Opt.long "address"
    , Opt.metavar "ADDRESS"
    , Opt.help "A Cardano address"
    ]


pStakePoolVerificationKeyHash :: Parser (Hash StakePoolKey)
pStakePoolVerificationKeyHash =
    Opt.option (pBech32KeyHash AsStakePoolKey <|> pHexHash AsStakePoolKey) $ mconcat
      [ Opt.long "stake-pool-id"
      , Opt.metavar "STAKE_POOL_ID"
      , Opt.help $ mconcat
          [ "Stake pool ID/verification key hash (either Bech32-encoded or hex-encoded).  "
          , "Zero or more occurences of this option is allowed."
          ]
      ]

pDelegationTarget
  :: Parser DelegationTarget
pDelegationTarget = StakePoolDelegationTarget <$> pStakePoolVerificationKeyOrHashOrFile

pStakePoolVerificationKeyOrHashOrFile
  :: Parser (VerificationKeyOrHashOrFile StakePoolKey)
pStakePoolVerificationKeyOrHashOrFile =
  asum
    [ VerificationKeyOrFile <$> pStakePoolVerificationKeyOrFile
    , VerificationKeyHash <$> pStakePoolVerificationKeyHash
    ]

pVrfVerificationKeyFile :: Parser (VerificationKeyFile In)
pVrfVerificationKeyFile =
  fmap File $ Opt.strOption $ mconcat
    [ Opt.long "vrf-verification-key-file"
    , Opt.metavar "FILE"
    , Opt.help "Filepath of the VRF verification key."
    , Opt.completer (Opt.bashCompleter "file")
    ]

pVrfVerificationKeyHash :: Parser (Hash VrfKey)
pVrfVerificationKeyHash =
  Opt.option (Opt.eitherReader deserialiseFromHex) $ mconcat
    [ Opt.long "vrf-verification-key-hash"
    , Opt.metavar "STRING"
    , Opt.help "VRF verification key hash (hex-encoded)."
    ]
  where
    deserialiseFromHex :: String -> Either String (Hash VrfKey)
    deserialiseFromHex =
      first (\e -> "Invalid VRF verification key hash: " ++ displayError e)
        . deserialiseFromRawBytesHex (AsHash AsVrfKey)
        . BSC.pack

pVrfVerificationKey :: Parser (VerificationKey VrfKey)
pVrfVerificationKey =
  Opt.option (readVerificationKey AsVrfKey) $ mconcat
    [ Opt.long "vrf-verification-key"
    , Opt.metavar "STRING"
    , Opt.help "VRF verification key (Bech32 or hex-encoded)."
    ]

pVrfVerificationKeyOrFile :: Parser (VerificationKeyOrFile VrfKey)
pVrfVerificationKeyOrFile =
  asum
    [ VerificationKeyValue <$> pVrfVerificationKey
    , VerificationKeyFilePath <$> pVrfVerificationKeyFile
    ]

pVrfVerificationKeyOrHashOrFile :: Parser (VerificationKeyOrHashOrFile VrfKey)
pVrfVerificationKeyOrHashOrFile =
  asum
    [ VerificationKeyOrFile <$> pVrfVerificationKeyOrFile
    , VerificationKeyHash <$> pVrfVerificationKeyHash
    ]

pRewardAcctVerificationKeyFile :: Parser (VerificationKeyFile In)
pRewardAcctVerificationKeyFile =
  fmap File $ asum
    [ Opt.strOption $ mconcat
      [ Opt.long "pool-reward-account-verification-key-file"
      , Opt.metavar "FILE"
      , Opt.help "Filepath of the reward account stake verification key."
      , Opt.completer (Opt.bashCompleter "file")
      ]
    , Opt.strOption $ mconcat
      [ Opt.long "reward-account-verification-key-file"
      , Opt.internal
      ]
    ]

pRewardAcctVerificationKey :: Parser (VerificationKey StakeKey)
pRewardAcctVerificationKey =
  Opt.option (readVerificationKey AsStakeKey) $ mconcat
    [ Opt.long "pool-reward-account-verification-key"
    , Opt.metavar "STRING"
    , Opt.help "Reward account stake verification key (Bech32 or hex-encoded)."
    ]

pRewardAcctVerificationKeyOrFile :: Parser (VerificationKeyOrFile StakeKey)
pRewardAcctVerificationKeyOrFile =
  asum
    [ VerificationKeyValue <$> pRewardAcctVerificationKey
    , VerificationKeyFilePath <$> pRewardAcctVerificationKeyFile
    ]

pPoolOwnerVerificationKeyFile :: Parser (VerificationKeyFile In)
pPoolOwnerVerificationKeyFile =
  fmap File $ asum
    [ Opt.strOption $ mconcat
      [ Opt.long "pool-owner-stake-verification-key-file"
      , Opt.metavar "FILE"
      , Opt.help "Filepath of the pool owner stake verification key."
      , Opt.completer (Opt.bashCompleter "file")
      ]
    , Opt.strOption $ mconcat
      [ Opt.long "pool-owner-staking-verification-key"
      , Opt.internal
      ]
    ]

pPoolOwnerVerificationKey :: Parser (VerificationKey StakeKey)
pPoolOwnerVerificationKey =
  Opt.option (readVerificationKey AsStakeKey) $ mconcat
    [ Opt.long "pool-owner-verification-key"
    , Opt.metavar "STRING"
    , Opt.help "Pool owner stake verification key (Bech32 or hex-encoded)."
    ]

pPoolOwnerVerificationKeyOrFile :: Parser (VerificationKeyOrFile StakeKey)
pPoolOwnerVerificationKeyOrFile =
  asum
    [ VerificationKeyValue <$> pPoolOwnerVerificationKey
    , VerificationKeyFilePath <$> pPoolOwnerVerificationKeyFile
    ]

pPoolPledge :: Parser Lovelace
pPoolPledge =
  Opt.option (readerFromParsecParser parseLovelace) $ mconcat
    [ Opt.long "pool-pledge"
    , Opt.metavar "LOVELACE"
    , Opt.help "The stake pool's pledge."
    ]


pPoolCost :: Parser Lovelace
pPoolCost =
  Opt.option (readerFromParsecParser parseLovelace) $ mconcat
    [ Opt.long "pool-cost"
    , Opt.metavar "LOVELACE"
    , Opt.help "The stake pool's cost."
    ]

pPoolMargin :: Parser Rational
pPoolMargin =
  Opt.option readRationalUnitInterval $ mconcat
    [ Opt.long "pool-margin"
    , Opt.metavar "RATIONAL"
    , Opt.help "The stake pool's margin."
    ]

pPoolRelay :: Parser StakePoolRelay
pPoolRelay =
  asum
    [ pSingleHostAddress
    , pSingleHostName
    , pMultiHostName
    ]

pMultiHostName :: Parser StakePoolRelay
pMultiHostName =
  StakePoolRelayDnsSrvRecord <$> pDNSName
  where
    pDNSName :: Parser ByteString
    pDNSName =
      Opt.option (Opt.eitherReader eDNSName) $ mconcat
        [ Opt.long "multi-host-pool-relay"
        , Opt.metavar "STRING"
        , Opt.help "The stake pool relay's DNS name that corresponds to an SRV DNS record"
        ]

pSingleHostName :: Parser StakePoolRelay
pSingleHostName =
  StakePoolRelayDnsARecord <$> pDNSName <*> optional pPort
  where
    pDNSName :: Parser ByteString
    pDNSName = Opt.option (Opt.eitherReader eDNSName) $ mconcat
      [ Opt.long "single-host-pool-relay"
      , Opt.metavar "STRING"
      , Opt.help $ mconcat
        [ "The stake pool relay's DNS name that corresponds to an"
        , " A or AAAA DNS record"
        ]
      ]

eDNSName :: String -> Either String ByteString
eDNSName str =
  -- We're using 'Shelley.textToDns' to validate the string.
  case Shelley.textToDns (toS str) of
    Nothing -> Left $ "DNS name is more than 64 bytes: " <> str
    Just dnsName -> Right . Text.encodeUtf8 . Shelley.dnsToText $ dnsName

pSingleHostAddress :: Parser StakePoolRelay
pSingleHostAddress =
  singleHostAddress
    <$> optional pIpV4
    <*> optional pIpV6
    <*> pPort
  where
    singleHostAddress :: Maybe IP.IPv4 -> Maybe IP.IPv6 -> PortNumber -> StakePoolRelay
    singleHostAddress ipv4 ipv6 port =
      case (ipv4, ipv6) of
        (Nothing, Nothing) ->
          error "Please enter either an IPv4 or IPv6 address for the pool relay"
        (Just i4, Nothing) ->
          StakePoolRelayIp (Just i4) Nothing (Just port)
        (Nothing, Just i6) ->
          StakePoolRelayIp Nothing (Just i6) (Just port)
        (Just i4, Just i6) ->
          StakePoolRelayIp (Just i4) (Just i6) (Just port)


pIpV4 :: Parser IP.IPv4
pIpV4 =
  Opt.option (Opt.maybeReader readMaybe :: Opt.ReadM IP.IPv4) $ mconcat
    [ Opt.long "pool-relay-ipv4"
    , Opt.metavar "STRING"
    , Opt.help "The stake pool relay's IPv4 address"
    ]

pIpV6 :: Parser IP.IPv6
pIpV6 =
  Opt.option (Opt.maybeReader readMaybe :: Opt.ReadM IP.IPv6) $ mconcat
    [ Opt.long "pool-relay-ipv6"
    , Opt.metavar "STRING"
    , Opt.help "The stake pool relay's IPv6 address"
    ]

pPort :: Parser PortNumber
pPort =
  Opt.option (fromInteger <$> Opt.eitherReader readEither) $ mconcat
    [ Opt.long "pool-relay-port"
    , Opt.metavar "INT"
    , Opt.help "The stake pool relay's port"
    ]

pStakePoolMetadataReference :: Parser (Maybe StakePoolMetadataReference)
pStakePoolMetadataReference =
  optional $
    StakePoolMetadataReference
      <$> pStakePoolMetadataUrl
      <*> pStakePoolMetadataHash

pStakePoolMetadataUrl :: Parser Text
pStakePoolMetadataUrl =
  Opt.option (readURIOfMaxLength 64) $ mconcat
    [ Opt.long "metadata-url"
    , Opt.metavar "URL"
    , Opt.help "Pool metadata URL (maximum length of 64 characters)."
    ]

pStakePoolMetadataHash :: Parser (Hash StakePoolMetadata)
pStakePoolMetadataHash =
  Opt.option (Opt.eitherReader metadataHash) $ mconcat
    [ Opt.long "metadata-hash"
    , Opt.metavar "HASH"
    , Opt.help "Pool metadata hash."
    ]
  where
    metadataHash :: String -> Either String (Hash StakePoolMetadata)
    metadataHash =
      first displayError
        . deserialiseFromRawBytesHex (AsHash AsStakePoolMetadata)
        . BSC.pack

pStakePoolRegistrationCert :: EnvCli -> Parser PoolCmd
pStakePoolRegistrationCert envCli =
  PoolRegistrationCert
    <$> pAnyShelleyBasedEra envCli
    <*> pStakePoolVerificationKeyOrFile
    <*> pVrfVerificationKeyOrFile
    <*> pPoolPledge
    <*> pPoolCost
    <*> pPoolMargin
    <*> pRewardAcctVerificationKeyOrFile
    <*> some pPoolOwnerVerificationKeyOrFile
    <*> many pPoolRelay
    <*> pStakePoolMetadataReference
    <*> pNetworkId envCli
    <*> pOutputFile


pStakePoolRegistrationParserRequirements
  :: EnvCli -> Parser StakePoolRegistrationParserRequirements
pStakePoolRegistrationParserRequirements envCli =
  StakePoolRegistrationParserRequirements
    <$> pStakePoolVerificationKeyOrFile
    <*> pVrfVerificationKeyOrFile
    <*> pPoolPledge
    <*> pPoolCost
    <*> pPoolMargin
    <*> pRewardAcctVerificationKeyOrFile
    <*> some pPoolOwnerVerificationKeyOrFile
    <*> many pPoolRelay
    <*> pStakePoolMetadataReference
    <*> pNetworkId envCli


pStakePoolRetirementCert :: EnvCli -> Parser PoolCmd
pStakePoolRetirementCert envCli =
  PoolRetirementCert
    <$> pAnyShelleyBasedEra envCli
    <*> pStakePoolVerificationKeyOrFile
    <*> pEpochNo
    <*> pOutputFile


pProtocolParametersUpdate :: Parser ProtocolParametersUpdate
pProtocolParametersUpdate =
  ProtocolParametersUpdate
    <$> optional pProtocolVersion
    <*> optional pDecentralParam
    <*> optional pExtraEntropy
    <*> optional pMaxBlockHeaderSize
    <*> optional pMaxBodySize
    <*> optional pMaxTransactionSize
    <*> optional pMinFeeConstantFactor
    <*> optional pMinFeePerByteFactor
    <*> optional pMinUTxOValue
    <*> optional pKeyRegistDeposit
    <*> optional pPoolDeposit
    <*> optional pMinPoolCost
    <*> optional pEpochBoundRetirement
    <*> optional pNumberOfPools
    <*> optional pPoolInfluence
    <*> optional pMonetaryExpansion
    <*> optional pTreasuryExpansion
    <*> optional pUTxOCostPerWord
    <*> pure mempty
    <*> optional pExecutionUnitPrices
    <*> optional pMaxTxExecutionUnits
    <*> optional pMaxBlockExecutionUnits
    <*> optional pMaxValueSize
    <*> optional pCollateralPercent
    <*> optional pMaxCollateralInputs
    <*> optional pUTxOCostPerByte

pCostModels :: Parser FilePath
pCostModels =
  Opt.strOption $ mconcat
    [ Opt.long "cost-model-file"
    , Opt.metavar "FILE"
    , Opt.help "Filepath of the JSON formatted cost model"
    , Opt.completer (Opt.bashCompleter "file")
    ]

pMinFeePerByteFactor :: Parser Lovelace
pMinFeePerByteFactor =
  Opt.option (readerFromParsecParser parseLovelace) $ mconcat
    [ Opt.long "min-fee-linear"
    , Opt.metavar "LOVELACE"
    , Opt.help "The linear factor per byte for the minimum fee calculation."
    ]

pMinFeeConstantFactor :: Parser Lovelace
pMinFeeConstantFactor =
  Opt.option (readerFromParsecParser parseLovelace) $ mconcat
    [ Opt.long "min-fee-constant"
    , Opt.metavar "LOVELACE"
    , Opt.help "The constant factor for the minimum fee calculation."
    ]

pMinUTxOValue :: Parser Lovelace
pMinUTxOValue =
  Opt.option (readerFromParsecParser parseLovelace) $ mconcat
    [ Opt.long "min-utxo-value"
    , Opt.metavar "NATURAL"
    , Opt.help "The minimum allowed UTxO value (Shelley to Mary eras)."
    ]

pMinPoolCost :: Parser Lovelace
pMinPoolCost =
  Opt.option (readerFromParsecParser parseLovelace) $ mconcat
    [ Opt.long "min-pool-cost"
    , Opt.metavar "NATURAL"
    , Opt.help "The minimum allowed cost parameter for stake pools."
    ]

pMaxBodySize :: Parser Natural
pMaxBodySize =
  Opt.option Opt.auto $ mconcat
    [ Opt.long "max-block-body-size"
    , Opt.metavar "NATURAL"
    , Opt.help "Maximal block body size."
    ]

pMaxTransactionSize :: Parser Natural
pMaxTransactionSize =
  Opt.option Opt.auto $ mconcat
    [ Opt.long "max-tx-size"
    , Opt.metavar "NATURAL"
    , Opt.help "Maximum transaction size."
    ]

pMaxBlockHeaderSize :: Parser Natural
pMaxBlockHeaderSize =
  Opt.option Opt.auto $ mconcat
   [ Opt.long "max-block-header-size"
   , Opt.metavar "NATURAL"
   , Opt.help "Maximum block header size."
   ]

pKeyRegistDeposit :: Parser Lovelace
pKeyRegistDeposit =
  Opt.option (readerFromParsecParser parseLovelace) $ mconcat
   [ Opt.long "key-reg-deposit-amt"
   , Opt.metavar "NATURAL"
   , Opt.help "Key registration deposit amount."
   ]

pPoolDeposit :: Parser Lovelace
pPoolDeposit =
  Opt.option (readerFromParsecParser parseLovelace) $ mconcat
   [ Opt.long "pool-reg-deposit"
   , Opt.metavar "NATURAL"
   , Opt.help "The amount of a pool registration deposit."
   ]

pEpochBoundRetirement :: Parser EpochNo
pEpochBoundRetirement =
  fmap EpochNo $ Opt.option (bounded "EPOCH_BOUNDARY") $ mconcat
    [ Opt.long "pool-retirement-epoch-boundary"
    , Opt.metavar "EPOCH_BOUNDARY"
    , Opt.help "Epoch bound on pool retirement."
    ]

pNumberOfPools :: Parser Natural
pNumberOfPools =
  Opt.option Opt.auto $ mconcat
   [ Opt.long "number-of-pools"
   , Opt.metavar "NATURAL"
   , Opt.help "Desired number of pools."
   ]

pPoolInfluence :: Parser Rational
pPoolInfluence =
  Opt.option readRational $ mconcat
    [ Opt.long "pool-influence"
    , Opt.metavar "RATIONAL"
    , Opt.help "Pool influence."
    ]

pTreasuryExpansion :: Parser Rational
pTreasuryExpansion =
  Opt.option readRationalUnitInterval $ mconcat
    [ Opt.long "treasury-expansion"
    , Opt.metavar "RATIONAL"
    , Opt.help "Treasury expansion."
    ]

pMonetaryExpansion :: Parser Rational
pMonetaryExpansion =
  Opt.option readRationalUnitInterval $ mconcat
    [ Opt.long "monetary-expansion"
    , Opt.metavar "RATIONAL"
    , Opt.help "Monetary expansion."
    ]

pDecentralParam :: Parser Rational
pDecentralParam =
  Opt.option readRationalUnitInterval $ mconcat
    [ Opt.long "decentralization-parameter"
    , Opt.metavar "RATIONAL"
    , Opt.help "Decentralization parameter."
    ]

pExtraEntropy :: Parser (Maybe PraosNonce)
pExtraEntropy =
  asum
    [ Opt.option (Just <$> readerFromParsecParser parsePraosNonce) $ mconcat
        [ Opt.long "extra-entropy"
        , Opt.metavar "HEX"
        , Opt.help "Praos extra entropy seed, as a hex byte string."
        ]
    , Opt.flag' Nothing $ mconcat
        [  Opt.long "reset-extra-entropy"
        , Opt.help "Reset the Praos extra entropy to none."
        ]
    ]
  where
    parsePraosNonce :: Parsec.Parser PraosNonce
    parsePraosNonce = makePraosNonce <$> parseEntropyBytes

    parseEntropyBytes :: Parsec.Parser ByteString
    parseEntropyBytes = either fail return
                      . B16.decode . BSC.pack
                    =<< some Parsec.hexDigit

pUTxOCostPerWord :: Parser Lovelace
pUTxOCostPerWord =
  Opt.option (readerFromParsecParser parseLovelace) $ mconcat
    [ Opt.long "utxo-cost-per-word"
    , Opt.metavar "LOVELACE"
    , Opt.help "Cost in lovelace per unit of UTxO storage (from Alonzo era)."
    ]

pUTxOCostPerByte :: Parser Lovelace
pUTxOCostPerByte =
  Opt.option (readerFromParsecParser parseLovelace) $ mconcat
    [ Opt.long "utxo-cost-per-byte"
    , Opt.metavar "LOVELACE"
    , Opt.help "Cost in lovelace per unit of UTxO storage (from Babbage era)."
    ]

pExecutionUnitPrices :: Parser ExecutionUnitPrices
pExecutionUnitPrices = ExecutionUnitPrices
  <$> Opt.option readRational
      ( mconcat
        [ Opt.long "price-execution-steps"
        , Opt.metavar "RATIONAL"
        , Opt.help $ mconcat
          [ "Step price of execution units for script languages that use "
          , "them (from Alonzo era).  (Examples: '1.1', '11/10')"
          ]
        ]
      )
  <*> Opt.option readRational
      ( mconcat
        [ Opt.long "price-execution-memory"
        , Opt.metavar "RATIONAL"
        , Opt.help $ mconcat
          [ "Memory price of execution units for script languages that "
          , "use them (from Alonzo era).  (Examples: '1.1', '11/10')"
          ]
        ]
      )

pMaxTxExecutionUnits :: Parser ExecutionUnits
pMaxTxExecutionUnits =
  uncurry ExecutionUnits <$>
  Opt.option Opt.auto
      ( mconcat
        [ Opt.long "max-tx-execution-units"
        , Opt.metavar "(INT, INT)"
        , Opt.help $ mconcat
          [ "Max total script execution resources units allowed per tx "
          , "(from Alonzo era). They are denominated as follows (steps, memory)."
          ]
        ]
      )

pMaxBlockExecutionUnits :: Parser ExecutionUnits
pMaxBlockExecutionUnits =
  uncurry ExecutionUnits <$>
  Opt.option Opt.auto
      ( mconcat
        [ Opt.long "max-block-execution-units"
        , Opt.metavar "(INT, INT)"
        , Opt.help $ mconcat
          [ "Max total script execution resources units allowed per block "
          , "(from Alonzo era). They are denominated as follows (steps, memory)."
          ]
        ]
      )

pMaxValueSize :: Parser Natural
pMaxValueSize =
  Opt.option Opt.auto $ mconcat
  [ Opt.long "max-value-size"
  , Opt.metavar "INT"
  , Opt.help $ mconcat
    [ "Max size of a multi-asset value in a tx output (from Alonzo era)."
    ]
  ]

pCollateralPercent :: Parser Natural
pCollateralPercent =
  Opt.option Opt.auto $ mconcat
  [ Opt.long "collateral-percent"
  , Opt.metavar "INT"
  , Opt.help $ mconcat
    [ "The percentage of the script contribution to the txfee that "
    , "must be provided as collateral inputs when including Plutus "
    , "scripts (from Alonzo era)."
    ]
  ]

pMaxCollateralInputs :: Parser Natural
pMaxCollateralInputs =
  Opt.option Opt.auto $ mconcat
  [ Opt.long "max-collateral-inputs"
  , Opt.metavar "INT"
  , Opt.help $ mconcat
    [ "The maximum number of collateral inputs allowed in a "
    , "transaction (from Alonzo era)."
    ]
  ]

pProtocolVersion :: Parser (Natural, Natural)
pProtocolVersion =
    (,) <$> pProtocolMajorVersion <*> pProtocolMinorVersion
  where
    pProtocolMajorVersion =
      Opt.option Opt.auto $ mconcat
        [ Opt.long "protocol-major-version"
        , Opt.metavar "NATURAL"
        , Opt.help "Major protocol version. An increase indicates a hard fork."
        ]
    pProtocolMinorVersion =
      Opt.option Opt.auto $ mconcat
        [ Opt.long "protocol-minor-version"
        , Opt.metavar "NATURAL"
        , Opt.help $ mconcat
          [ "Minor protocol version. An increase indicates a soft fork"
          , " (old software canvalidate but not produce new blocks)."
          ]
        ]

--
-- Shelley CLI flag field parsers
--

parseTxOutAnyEra
  :: Parsec.Parser (TxOutDatumAnyEra -> ReferenceScriptAnyEra -> TxOutAnyEra)
parseTxOutAnyEra = do
    addr <- parseAddressAny
    Parsec.spaces
    -- Accept the old style of separating the address and value in a
    -- transaction output:
    Parsec.option () (Parsec.char '+' >> Parsec.spaces)
    val <- parseValue
    return (TxOutAnyEra addr val)

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------



readPoolIdOutputFormat :: Opt.ReadM PoolIdOutputFormat
readPoolIdOutputFormat = do
  s <- Opt.str @String
  case s of
    "hex" -> pure PoolIdOutputFormatHex
    "bech32" -> pure PoolIdOutputFormatBech32
    _ ->
      fail $ mconcat
        [ "Invalid output format: " <> show s
        , ". Accepted output formats are \"hex\" and \"bech32\"."
        ]

readKeyOutputFormat :: Opt.ReadM KeyOutputFormat
readKeyOutputFormat = do
  s <- Opt.str @String
  case s of
    "text-envelope" -> pure KeyOutputFormatTextEnvelope
    "bech32" -> pure KeyOutputFormatBech32
    _ ->
      fail $ mconcat
        [ "Invalid key output format: " <> show s
        , ". Accepted output formats are \"text-envelope\" and \"bech32\"."
        ]

readURIOfMaxLength :: Int -> Opt.ReadM Text
readURIOfMaxLength maxLen =
  Text.pack <$> readStringOfMaxLength maxLen

readStringOfMaxLength :: Int -> Opt.ReadM String
readStringOfMaxLength maxLen = do
  s <- Opt.str
  let strLen = length s
  if strLen <= maxLen
    then pure s
    else
      fail $ mconcat
        [ "The provided string must have at most 64 characters, but it has "
        , show strLen
        , " characters."
        ]

readRationalUnitInterval :: Opt.ReadM Rational
readRationalUnitInterval = readRational >>= checkUnitInterval
  where
   checkUnitInterval :: Rational -> Opt.ReadM Rational
   checkUnitInterval q
     | q >= 0 && q <= 1 = return q
     | otherwise        = fail "Please enter a value in the range [0,1]"

readFractionAsRational :: Opt.ReadM Rational
readFractionAsRational = readerFromAttoParser fractionalAsRational
  where fractionalAsRational :: Atto.Parser Rational
        fractionalAsRational = (%) <$> (Atto.decimal @Integer <* Atto.char '/') <*> Atto.decimal @Integer

readRational :: Opt.ReadM Rational
readRational =
  asum
    [ toRational <$> readerFromAttoParser Atto.scientific
    , readFractionAsRational
    ]

readerFromAttoParser :: Atto.Parser a -> Opt.ReadM a
readerFromAttoParser p =
  Opt.eitherReader (Atto.parseOnly (p <* Atto.endOfInput) . BSC.pack)

hiddenSubParser :: String -> ParserInfo a -> Parser a
hiddenSubParser availableCommand pInfo =
  Opt.hsubparser $ Opt.command availableCommand pInfo <> Opt.metavar availableCommand <> Opt.hidden
