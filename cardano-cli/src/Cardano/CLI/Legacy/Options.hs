{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.CLI.Legacy.Options
  ( -- * CLI command parser
    parseLegacyCmds

    -- * CLI command and flag types
  , module Cardano.CLI.Legacy.Commands

    -- * Field parser and renderers
  , parseTxIn

  , pKeyRegistDeposit
  , pStakePoolRegistrationParserRequirements
  , pStakePoolVerificationKeyOrHashOrFile
  ) where

import           Cardano.Api hiding (QueryInShelleyBasedEra (..))
import           Cardano.Api.Shelley hiding (QueryInShelleyBasedEra (..))

import           Cardano.Chain.Common (BlockCount (BlockCount))
import           Cardano.CLI.Environment (EnvCli (..))
import           Cardano.CLI.EraBased.Options.Common
import           Cardano.CLI.Legacy.Commands
import           Cardano.CLI.Legacy.Commands.Address
import           Cardano.CLI.Legacy.Commands.Genesis
import           Cardano.CLI.Legacy.Commands.Governance
import           Cardano.CLI.Legacy.Commands.Key
import           Cardano.CLI.Legacy.Commands.Node
import           Cardano.CLI.Legacy.Commands.Pool
import           Cardano.CLI.Legacy.Commands.Query
import           Cardano.CLI.Legacy.Commands.StakeAddress
import           Cardano.CLI.Legacy.Commands.TextView
import           Cardano.CLI.Legacy.Commands.Transaction
import           Cardano.CLI.Parser
import           Cardano.CLI.Types.Common

import           Data.Foldable
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import           Data.Word (Word64)
import           Options.Applicative hiding (help, str)
import qualified Options.Applicative as Opt
import qualified Options.Applicative.Help as H
import           Prettyprinter (line, pretty)

{- HLINT ignore "Use <$>" -}
{- HLINT ignore "Move brackets to avoid $" -}

--
-- Shelley CLI command parsers
--

parseLegacyCmds :: EnvCli -> Parser LegacyCmds
parseLegacyCmds envCli =
  Opt.hsubparser $ mconcat
    [ Opt.metavar "Legacy commands"
    , Opt.commandGroup "Legacy commands"
    , Opt.command "address"
        $ Opt.info (LegacyAddressCmds <$> pAddressCmds envCli)
        $ Opt.progDesc "Payment address commands"
    , Opt.command "stake-address"
        $ Opt.info (LegacyStakeAddressCmds <$> pStakeAddressCmds envCli)
        $ Opt.progDesc "Stake address commands"
    , Opt.command "key"
        $ Opt.info (LegacyKeyCmds <$> pKeyCmds)
        $ Opt.progDesc "Key utility commands"
    , Opt.command "transaction"
        $ Opt.info (LegacyTransactionCmds <$> pTransaction envCli)
        $ Opt.progDesc "Transaction commands"
    , Opt.command "node"
        $ Opt.info (LegacyNodeCmds <$> pNodeCmds)
        $ Opt.progDesc "Node operation commands"
    , Opt.command "stake-pool"
        $ Opt.info (LegacyPoolCmds <$> pPoolCmds envCli)
        $ Opt.progDesc "Stake pool commands"
    , Opt.command "query"
        $ Opt.info (LegacyQueryCmds <$> pQueryCmds envCli) . Opt.progDesc
        $ mconcat
            [ "Node query commands. Will query the local node whose Unix domain socket "
            , "is obtained from the CARDANO_NODE_SOCKET_PATH environment variable."
            ]
    , Opt.command "genesis"
        $ Opt.info (LegacyGenesisCmds <$> pGenesisCmds envCli)
        $ Opt.progDesc "Genesis block commands"
    , Opt.command "governance"
        $ Opt.info (LegacyGovernanceCmds <$> pGovernanceCmds envCli)
        $ Opt.progDesc "Governance commands"
    , Opt.command "text-view"
        $ Opt.info (LegacyTextViewCmds <$> pTextViewCmds) . Opt.progDesc
        $ mconcat
            [ "Commands for dealing with Shelley TextView files. "
            , "Transactions, addresses etc are stored on disk as TextView files."
            ]
    ]

pTextViewCmds :: Parser LegacyTextViewCmds
pTextViewCmds =
  asum
    [ subParser "decode-cbor"
        (Opt.info (TextViewInfo <$> pCBORInFile <*> pMaybeOutputFile)
          $ Opt.progDesc "Print a TextView file as decoded CBOR."
          )
    ]

pAddressCmds :: EnvCli -> Parser LegacyAddressCmds
pAddressCmds envCli =
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
    pAddressKeyGen :: Parser LegacyAddressCmds
    pAddressKeyGen =
      AddressKeyGen
        <$> pKeyOutputFormat
        <*> pAddressKeyType
        <*> pVerificationKeyFileOut
        <*> pSigningKeyFileOut

    pAddressKeyHash :: Parser LegacyAddressCmds
    pAddressKeyHash =
      AddressKeyHash
        <$> pPaymentVerificationKeyTextOrFile
        <*> pMaybeOutputFile

    pAddressBuild :: Parser LegacyAddressCmds
    pAddressBuild = AddressBuild
      <$> pPaymentVerifier
      <*> Opt.optional pStakeIdentifier
      <*> pNetworkId envCli
      <*> pMaybeOutputFile

    pAddressInfo :: Parser LegacyAddressCmds
    pAddressInfo = AddressInfo <$> pAddress <*> pMaybeOutputFile

pStakeAddressCmds :: EnvCli -> Parser LegacyStakeAddressCmds
pStakeAddressCmds envCli =
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
    pStakeAddressKeyGen :: Parser LegacyStakeAddressCmds
    pStakeAddressKeyGen =
      StakeAddressKeyGen
        <$> pKeyOutputFormat
        <*> pVerificationKeyFileOut
        <*> pSigningKeyFileOut

    pStakeAddressKeyHash :: Parser LegacyStakeAddressCmds
    pStakeAddressKeyHash = StakeAddressKeyHash <$> pStakeVerificationKeyOrFile <*> pMaybeOutputFile

    pStakeAddressBuild :: Parser LegacyStakeAddressCmds
    pStakeAddressBuild =
      StakeAddressBuild
        <$> pStakeVerifier
        <*> pNetworkId envCli
        <*> pMaybeOutputFile

    pStakeAddressRegistrationCert :: Parser LegacyStakeAddressCmds
    pStakeAddressRegistrationCert =
      StakeRegistrationCert
        <$> pAnyShelleyBasedEra envCli
        <*> pStakeIdentifier
        <*> optional pKeyRegistDeposit
        <*> pOutputFile

    pStakeAddressDeregistrationCert :: Parser LegacyStakeAddressCmds
    pStakeAddressDeregistrationCert =
      StakeCredentialDeRegistrationCert
        <$> pAnyShelleyBasedEra envCli
        <*> pStakeIdentifier
        <*> optional pKeyRegistDeposit
        <*> pOutputFile

    pStakeAddressPoolDelegationCert :: Parser LegacyStakeAddressCmds
    pStakeAddressPoolDelegationCert =
      StakeCredentialDelegationCert
        <$> pAnyShelleyBasedEra envCli
        <*> pStakeIdentifier
        <*> pDelegationTarget
        <*> pOutputFile

pKeyCmds :: Parser LegacyKeyCmds
pKeyCmds =
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
    pKeyGetVerificationKey :: Parser LegacyKeyCmds
    pKeyGetVerificationKey =
      KeyGetVerificationKey
        <$> pSigningKeyFileIn
        <*> pVerificationKeyFileOut

    pKeyNonExtendedKey :: Parser LegacyKeyCmds
    pKeyNonExtendedKey =
      KeyNonExtendedKey
        <$> pExtendedVerificationKeyFileIn
        <*> pVerificationKeyFileOut

    pKeyConvertByronKey :: Parser LegacyKeyCmds
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

    pKeyConvertByronGenesisVKey :: Parser LegacyKeyCmds
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

    pKeyConvertITNKey :: Parser LegacyKeyCmds
    pKeyConvertITNKey =
      KeyConvertITNStakeKey
        <$> pITNKeyFIle
        <*> pOutputFile

    pKeyConvertITNExtendedKey :: Parser LegacyKeyCmds
    pKeyConvertITNExtendedKey =
      KeyConvertITNExtendedToStakeKey
        <$> pITNSigningKeyFile
        <*> pOutputFile

    pKeyConvertITNBip32Key :: Parser LegacyKeyCmds
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

    pKeyConvertCardanoAddressSigningKey :: Parser LegacyKeyCmds
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

pTransaction :: EnvCli -> Parser LegacyTransactionCmds
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
  calcMinValueInfo :: ParserInfo LegacyTransactionCmds
  calcMinValueInfo =
    Opt.info pTransactionCalculateMinReqUTxO
      $ Opt.progDesc "DEPRECATED: Use 'calculate-min-required-utxo' instead."

  pCalculateMinRequiredUtxoBackwardCompatible :: Parser LegacyTransactionCmds
  pCalculateMinRequiredUtxoBackwardCompatible =
    Opt.subparser
      $ Opt.command "calculate-min-value" calcMinValueInfo <> Opt.internal

  assembleInfo :: ParserInfo LegacyTransactionCmds
  assembleInfo =
    Opt.info pTransactionAssembleTxBodyWit
      $ Opt.progDesc "Assemble a tx body and witness(es) to form a transaction"

  pSignWitnessBackwardCompatible :: Parser LegacyTransactionCmds
  pSignWitnessBackwardCompatible =
    Opt.subparser
      $ Opt.command "sign-witness" assembleInfo <> Opt.internal

  pTransactionBuild :: Parser LegacyTransactionCmds
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

  pTransactionBuildRaw :: Parser LegacyTransactionCmds
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

  pTransactionSign  :: Parser LegacyTransactionCmds
  pTransactionSign =
    TxSign
      <$> pInputTxOrTxBodyFile
      <*> many pWitnessSigningData
      <*> optional (pNetworkId envCli)
      <*> pTxFileOut

  pTransactionCreateWitness :: Parser LegacyTransactionCmds
  pTransactionCreateWitness =
    TxCreateWitness
      <$> pTxBodyFileIn
      <*> pWitnessSigningData
      <*> optional (pNetworkId envCli)
      <*> pOutputFile

  pTransactionAssembleTxBodyWit :: Parser LegacyTransactionCmds
  pTransactionAssembleTxBodyWit =
    TxAssembleTxBodyWitness
      <$> pTxBodyFileIn
      <*> many pWitnessFile
      <*> pOutputFile

  pTransactionSubmit :: Parser LegacyTransactionCmds
  pTransactionSubmit =
    TxSubmit
      <$> pSocketPath envCli
      <*> pConsensusModeParams
      <*> pNetworkId envCli
      <*> pTxSubmitFile

  pTransactionPolicyId :: Parser LegacyTransactionCmds
  pTransactionPolicyId = TxMintedPolicyId <$> pScript

  pTransactionCalculateMinFee :: Parser LegacyTransactionCmds
  pTransactionCalculateMinFee =
    TxCalculateMinFee
      <$> pTxBodyFileIn
      <*> pNetworkId envCli
      <*> pProtocolParamsFile
      <*> pTxInCount
      <*> pTxOutCount
      <*> pTxShelleyWitnessCount
      <*> pTxByronWitnessCount

  pTransactionCalculateMinReqUTxO :: Parser LegacyTransactionCmds
  pTransactionCalculateMinReqUTxO = TxCalculateMinRequiredUTxO
    <$> pCardanoEra envCli
    <*> pProtocolParamsFile
    <*> pTxOut

  pTxHashScriptData :: Parser LegacyTransactionCmds
  pTxHashScriptData =
    fmap TxHashScriptData
      $ pScriptDataOrFile
          "script-data"
          "The script data, in JSON syntax."
          "The script data, in the given JSON file."

  pTransactionId  :: Parser LegacyTransactionCmds
  pTransactionId = TxGetTxId <$> pInputTxOrTxBodyFile

  pTransactionView :: Parser LegacyTransactionCmds
  pTransactionView = TxView <$> pInputTxOrTxBodyFile

pNodeCmds :: Parser LegacyNodeCmds
pNodeCmds =
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
    pKeyGenOperator :: Parser LegacyNodeCmds
    pKeyGenOperator =
      NodeKeyGenCold
        <$> pKeyOutputFormat
        <*> pColdVerificationKeyFile
        <*> pColdSigningKeyFile
        <*> pOperatorCertIssueCounterFile

    pKeyGenKES :: Parser LegacyNodeCmds
    pKeyGenKES =
      NodeKeyGenKES
        <$> pKeyOutputFormat
        <*> pVerificationKeyFileOut
        <*> pSigningKeyFileOut

    pKeyGenVRF :: Parser LegacyNodeCmds
    pKeyGenVRF =
      NodeKeyGenVRF
        <$> pKeyOutputFormat
        <*> pVerificationKeyFileOut
        <*> pSigningKeyFileOut

    pKeyHashVRF :: Parser LegacyNodeCmds
    pKeyHashVRF =
      NodeKeyHashVRF
        <$> pVerificationKeyOrFileIn AsVrfKey
        <*> pMaybeOutputFile

    pNewCounter :: Parser LegacyNodeCmds
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

    pIssueOpCert :: Parser LegacyNodeCmds
    pIssueOpCert =
      NodeIssueOpCert
        <$> pKesVerificationKeyOrFile
        <*> pColdSigningKeyFile
        <*> pOperatorCertIssueCounterFile
        <*> pKesPeriod
        <*> pOutputFile

pPoolCmds :: EnvCli -> Parser LegacyPoolCmds
pPoolCmds  envCli =
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
    pId :: Parser LegacyPoolCmds
    pId =
      PoolGetId
        <$> pStakePoolVerificationKeyOrFile
        <*> pPoolIdOutputFormat
        <*> pMaybeOutputFile

    pPoolMetadataHashSubCmd :: Parser LegacyPoolCmds
    pPoolMetadataHashSubCmd = PoolMetadataHash <$> pPoolMetadataFile <*> pMaybeOutputFile

pQueryCmds :: EnvCli -> Parser LegacyQueryCmds
pQueryCmds envCli =
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
    pQueryProtocolParameters :: Parser LegacyQueryCmds
    pQueryProtocolParameters =
      QueryProtocolParameters'
        <$> pSocketPath envCli
        <*> pConsensusModeParams
        <*> pNetworkId envCli
        <*> pMaybeOutputFile

    pQueryConstitutionHash :: Parser LegacyQueryCmds
    pQueryConstitutionHash =
      QueryConstitutionHash
        <$> pSocketPath envCli
        <*> pConsensusModeParams
        <*> pNetworkId envCli
        <*> pMaybeOutputFile

    pQueryTip :: Parser LegacyQueryCmds
    pQueryTip =
      QueryTip
        <$> pSocketPath envCli
        <*> pConsensusModeParams
        <*> pNetworkId envCli
        <*> pMaybeOutputFile

    pQueryUTxO :: Parser LegacyQueryCmds
    pQueryUTxO =
      QueryUTxO'
        <$> pSocketPath envCli
        <*> pConsensusModeParams
        <*> pQueryUTxOFilter
        <*> pNetworkId envCli
        <*> pMaybeOutputFile

    pQueryStakePools :: Parser LegacyQueryCmds
    pQueryStakePools =
      QueryStakePools'
        <$> pSocketPath envCli
        <*> pConsensusModeParams
        <*> pNetworkId envCli
        <*> pMaybeOutputFile

    pQueryStakeDistribution :: Parser LegacyQueryCmds
    pQueryStakeDistribution =
      QueryStakeDistribution'
        <$> pSocketPath envCli
        <*> pConsensusModeParams
        <*> pNetworkId envCli
        <*> pMaybeOutputFile

    pQueryStakeAddressInfo :: Parser LegacyQueryCmds
    pQueryStakeAddressInfo =
      QueryStakeAddressInfo
        <$> pSocketPath envCli
        <*> pConsensusModeParams
        <*> pFilterByStakeAddress
        <*> pNetworkId envCli
        <*> pMaybeOutputFile

    pQueryLedgerState :: Parser LegacyQueryCmds
    pQueryLedgerState =
      QueryDebugLedgerState'
        <$> pSocketPath envCli
        <*> pConsensusModeParams
        <*> pNetworkId envCli
        <*> pMaybeOutputFile

    pQueryProtocolState :: Parser LegacyQueryCmds
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

    pQueryStakeSnapshot :: Parser LegacyQueryCmds
    pQueryStakeSnapshot =
      QueryStakeSnapshot'
        <$> pSocketPath envCli
        <*> pConsensusModeParams
        <*> pNetworkId envCli
        <*> pAllStakePoolsOrOnly
        <*> pMaybeOutputFile

    pQueryPoolState :: Parser LegacyQueryCmds
    pQueryPoolState =
      QueryPoolState'
        <$> pSocketPath envCli
        <*> pConsensusModeParams
        <*> pNetworkId envCli
        <*> many pStakePoolVerificationKeyHash

    pQueryTxMempool :: Parser LegacyQueryCmds
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
    pLeadershipSchedule :: Parser LegacyQueryCmds
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

    pKesPeriodInfo :: Parser LegacyQueryCmds
    pKesPeriodInfo =
      QueryKesPeriodInfo
        <$> pSocketPath envCli
        <*> pConsensusModeParams
        <*> pNetworkId envCli
        <*> pOperationalCertificateFile
        <*> pMaybeOutputFile

    pQuerySlotNumber :: Parser LegacyQueryCmds
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
pGovernanceCmds :: EnvCli -> Parser LegacyGovernanceCmds
pGovernanceCmds envCli =
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
    mirCertParsers :: Parser LegacyGovernanceCmds
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

    pLegacyMIRPayStakeAddresses :: Parser LegacyGovernanceCmds
    pLegacyMIRPayStakeAddresses =
      GovernanceMIRPayStakeAddressesCertificate
        <$> pAnyShelleyToBabbageEra envCli
        <*> pMIRPot
        <*> some pStakeAddress
        <*> some pRewardAmt
        <*> pOutputFile

    pLegacyMIRTransferToTreasury :: Parser LegacyGovernanceCmds
    pLegacyMIRTransferToTreasury =
      GovernanceMIRTransfer
        <$> pAnyShelleyToBabbageEra envCli
        <*> pTransferAmt
        <*> pOutputFile
        <*> pure TransferToTreasury

    pLegacyMIRTransferToReserves :: Parser LegacyGovernanceCmds
    pLegacyMIRTransferToReserves =
      GovernanceMIRTransfer
        <$> pAnyShelleyToBabbageEra envCli
        <*> pTransferAmt
        <*> pOutputFile
        <*> pure TransferToReserves

    pGovernanceGenesisKeyDelegationCertificate :: Parser LegacyGovernanceCmds
    pGovernanceGenesisKeyDelegationCertificate =
      GovernanceGenesisKeyDelegationCertificate
        <$> pAnyShelleyBasedEra envCli
        <*> pGenesisVerificationKeyOrHashOrFile
        <*> pGenesisDelegateVerificationKeyOrHashOrFile
        <*> pVrfVerificationKeyOrHashOrFile
        <*> pOutputFile

    pUpdateProposal :: Parser LegacyGovernanceCmds
    pUpdateProposal =
      GovernanceUpdateProposal
        <$> pOutputFile
        <*> pEpochNoUpdateProp
        <*> some pGenesisVerificationKeyFile
        <*> pProtocolParametersUpdate
        <*> optional pCostModels

    pGovernanceCreatePoll :: Parser LegacyGovernanceCmds
    pGovernanceCreatePoll =
      GovernanceCreatePoll
        <$> pPollQuestion
        <*> some pPollAnswer
        <*> optional pPollNonce
        <*> pOutputFile

    pGovernanceAnswerPoll :: Parser LegacyGovernanceCmds
    pGovernanceAnswerPoll =
      GovernanceAnswerPoll
        <$> pPollFile
        <*> optional pPollAnswerIndex
        <*> optional pOutputFile

    pGovernanceVerifyPoll :: Parser LegacyGovernanceCmds
    pGovernanceVerifyPoll =
      GovernanceVerifyPoll
        <$> pPollFile
        <*> pPollTxFile
        <*> optional pOutputFile

pGenesisCmds :: EnvCli -> Parser LegacyGenesisCmds
pGenesisCmds envCli =
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
    pGenesisKeyGen :: Parser LegacyGenesisCmds
    pGenesisKeyGen =
      GenesisKeyGenGenesis
        <$> pVerificationKeyFileOut
        <*> pSigningKeyFileOut

    pGenesisDelegateKeyGen :: Parser LegacyGenesisCmds
    pGenesisDelegateKeyGen =
      GenesisKeyGenDelegate
        <$> pVerificationKeyFileOut
        <*> pSigningKeyFileOut
        <*> pOperatorCertIssueCounterFile

    pGenesisUTxOKeyGen :: Parser LegacyGenesisCmds
    pGenesisUTxOKeyGen =
      GenesisKeyGenUTxO
        <$> pVerificationKeyFileOut
        <*> pSigningKeyFileOut

    pGenesisKeyHash :: Parser LegacyGenesisCmds
    pGenesisKeyHash =
      GenesisCmdKeyHash
        <$> pVerificationKeyFileIn

    pGenesisVerKey :: Parser LegacyGenesisCmds
    pGenesisVerKey =
      GenesisVerKey
        <$> pVerificationKeyFileOut
        <*> pSigningKeyFileIn

    pGenesisAddr :: Parser LegacyGenesisCmds
    pGenesisAddr =
      GenesisAddr
        <$> pVerificationKeyFileIn
        <*> pNetworkId envCli
        <*> pMaybeOutputFile

    pGenesisTxIn :: Parser LegacyGenesisCmds
    pGenesisTxIn =
      GenesisTxIn
        <$> pVerificationKeyFileIn
        <*> pNetworkId envCli
        <*> pMaybeOutputFile

    pGenesisCreateCardano :: Parser LegacyGenesisCmds
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

    pGenesisCreate :: Parser LegacyGenesisCmds
    pGenesisCreate =
      GenesisCreate
        <$> pKeyOutputFormat
        <*> pGenesisDir
        <*> pGenesisNumGenesisKeys
        <*> pGenesisNumUTxOKeys
        <*> pMaybeSystemStart
        <*> pInitialSupplyNonDelegated
        <*> pNetworkId envCli

    pGenesisCreateStaked :: Parser LegacyGenesisCmds
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

    pGenesisHash :: Parser LegacyGenesisCmds
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

pStakePoolRegistrationCert :: EnvCli -> Parser LegacyPoolCmds
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

pStakePoolRetirementCert :: EnvCli -> Parser LegacyPoolCmds
pStakePoolRetirementCert envCli =
  PoolRetirementCert
    <$> pAnyShelleyBasedEra envCli
    <*> pStakePoolVerificationKeyOrFile
    <*> pEpochNo "The epoch number."
    <*> pOutputFile
