{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.EraBased.Legacy
  ( -- * CLI command parser
    parseLegacyCommands

    -- * CLI command and flag types
  , module Cardano.CLI.Commands.Legacy

    -- * Field parser and renderers
  , parseTxIn

  , pKeyRegistDeposit
  , pStakePoolVerificationKeyOrHashOrFile
  ) where

import           Cardano.Api hiding (QueryInShelleyBasedEra (..))
import           Cardano.Api.Shelley hiding (QueryInShelleyBasedEra (..))

import           Cardano.Chain.Common (BlockCount (BlockCount))
import           Cardano.CLI.Commands.Legacy
import           Cardano.CLI.Environment (EnvCli (..))
import           Cardano.CLI.EraBased.Options.Common
import           Cardano.CLI.Types.Key (PaymentVerifier (..), VerificationKeyOrFile (..),
                   VerificationKeyTextOrFile (..))
import           Cardano.CLI.Types.Legacy

import           Data.Bifunctor
import qualified Data.ByteString.Char8 as BSC
import           Data.Foldable
import           Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Time.Clock (UTCTime)
import           Data.Time.Format (defaultTimeLocale, parseTimeOrError)
import           Data.Word (Word64)
import           Options.Applicative hiding (help, str)
import qualified Options.Applicative as Opt

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
    , Opt.command "key"
        $ Opt.info (KeyCmd <$> pKeyCmd)
        $ Opt.progDesc "Key utility commands"
    , Opt.command "node"
        $ Opt.info (NodeCmd <$> pNodeCmd)
        $ Opt.progDesc "Node operation commands"
    , Opt.command "query"
        $ Opt.info (QueryCmd <$> pQueryCmd envCli) . Opt.progDesc
        $ mconcat
            [ "Node query commands. Will query the local node whose Unix domain socket "
            , "is obtained from the CARDANO_NODE_SOCKET_PATH environment variable."
            ]
    , Opt.command "genesis"
        $ Opt.info (GenesisCmd <$> pGenesisCmd envCli)
        $ Opt.progDesc "Genesis block commands"
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

convertTime :: String -> UTCTime
convertTime =
  parseTimeOrError False defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"

pColdSigningKeyFile :: Parser (SigningKeyFile direction)
pColdSigningKeyFile =
  fmap File $ asum
    [ Opt.strOption $ mconcat
      [ Opt.long "cold-signing-key-file"
      , Opt.metavar "FILE"
      , Opt.help "Filepath of the cold signing key."
      , Opt.completer (Opt.bashCompleter "file")
      ]
    , Opt.strOption $ mconcat
      [ Opt.long "signing-key-file"
      , Opt.internal
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

pSigningKeyFileIn :: Parser (SigningKeyFile In)
pSigningKeyFileIn =
  fmap File $ Opt.strOption $ mconcat
    [ Opt.long "signing-key-file"
    , Opt.metavar "FILE"
    , Opt.help "Input filepath of the signing key."
    , Opt.completer (Opt.bashCompleter "file")
    ]

pSigningKeyFileOut :: Parser (SigningKeyFile Out)
pSigningKeyFileOut =
  fmap File $ Opt.strOption $ mconcat
    [ Opt.long "signing-key-file"
    , Opt.metavar "FILE"
    , Opt.help "Output filepath of the signing key."
    , Opt.completer (Opt.bashCompleter "file")
    ]

pKesPeriod :: Parser KESPeriod
pKesPeriod =
  fmap KESPeriod $ Opt.option (bounded "KES_PERIOD") $ mconcat
    [ Opt.long "kes-period"
    , Opt.metavar "NATURAL"
    , Opt.help "The start of the KES key validity period."
    ]

pGenesisFile :: String -> Parser GenesisFile
pGenesisFile desc =
  fmap GenesisFile $ Opt.strOption $ mconcat
    [ Opt.long "genesis"
    , Opt.metavar "FILE"
    , Opt.help desc
    , Opt.completer (Opt.bashCompleter "file")
    ]

pOperatorCertIssueCounterFile :: Parser (OpCertCounterFile direction)
pOperatorCertIssueCounterFile =
  fmap File $ asum
    [ Opt.strOption $ mconcat
      [ Opt.long "operational-certificate-issue-counter-file"
      , Opt.metavar "FILE"
      , Opt.help "The file with the issue counter for the operational certificate."
      , Opt.completer (Opt.bashCompleter "file")
      ]
    , Opt.strOption $ mconcat
      [ Opt.long "operational-certificate-issue-counter"
      , Opt.internal
      ]
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

pMaybeOutputFile :: Parser (Maybe (File content Out))
pMaybeOutputFile =
  optional $ fmap File $ Opt.strOption $ mconcat
    [ Opt.long "out-file"
    , Opt.metavar "FILE"
    , Opt.help "Optional output file. Default is to write to stdout."
    , Opt.completer (Opt.bashCompleter "file")
    ]

pColdVerificationKeyOrFile :: Parser ColdVerificationKeyOrFile
pColdVerificationKeyOrFile =
  asum
    [ ColdStakePoolVerificationKey <$> pStakePoolVerificationKey
    , ColdGenesisDelegateVerificationKey <$> pGenesisDelegateVerificationKey
    , ColdVerificationKeyFile <$> pColdVerificationKeyFile
    ]

pColdVerificationKeyFile :: Parser (VerificationKeyFile direction)
pColdVerificationKeyFile =
  fmap File $ asum
    [ Opt.strOption $ mconcat
      [ Opt.long "cold-verification-key-file"
      , Opt.metavar "FILE"
      , Opt.help "Filepath of the cold verification key."
      , Opt.completer (Opt.bashCompleter "file")
      ]
    , Opt.strOption $ mconcat
      [ Opt.long "verification-key-file"
      , Opt.internal
      ]
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

pVerificationKeyFileIn :: Parser (VerificationKeyFile In)
pVerificationKeyFileIn =
  fmap File $ Opt.strOption $ mconcat
    [ Opt.long "verification-key-file"
    , Opt.metavar "FILE"
    , Opt.help "Input filepath of the verification key."
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

pExtendedVerificationKeyFileIn :: Parser (VerificationKeyFile In)
pExtendedVerificationKeyFileIn =
  fmap File $ Opt.strOption $ mconcat
    [ Opt.long "extended-verification-key-file"
    , Opt.metavar "FILE"
    , Opt.help "Input filepath of the ed25519-bip32 verification key."
    , Opt.completer (Opt.bashCompleter "file")
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

pAddress :: Parser Text
pAddress =
  fmap Text.pack $ Opt.strOption $ mconcat
    [ Opt.long "address"
    , Opt.metavar "ADDRESS"
    , Opt.help "A Cardano address"
    ]

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

hiddenSubParser :: String -> ParserInfo a -> Parser a
hiddenSubParser availableCommand pInfo =
  Opt.hsubparser $ Opt.command availableCommand pInfo <> Opt.metavar availableCommand <> Opt.hidden
