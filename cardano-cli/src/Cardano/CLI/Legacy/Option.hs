{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.CLI.Legacy.Option
  ( -- * CLI command parser
    parseLegacyCmds

    -- * CLI command and flag types
  , module Cardano.CLI.Legacy.Command

    -- * Field parser and renderers
  , parseTxIn
  , pKeyRegistDeposit
  , pStakePoolRegistrationParserRequirements
  , pStakePoolVerificationKeyOrHashOrFile
  )
where

import Cardano.Api hiding (QueryInShelleyBasedEra (..))

import Cardano.CLI.Environment
import Cardano.CLI.EraBased.Common.Option
import Cardano.CLI.Legacy.Command
import Cardano.CLI.Legacy.Genesis.Command
import Cardano.CLI.Parser
import Cardano.CLI.Type.Common
import Cardano.Ledger.BaseTypes (NonZero, knownNonZeroBounded)

import Data.Foldable
import Data.Maybe (fromMaybe)
import Data.Word (Word64)
import Options.Applicative hiding (help, str)
import Options.Applicative qualified as Opt

{- HLINT ignore "Use <$>" -}
{- HLINT ignore "Move brackets to avoid $" -}

--
-- Shelley CLI command parsers
--
-- This is necessary for QA to create update proposals pre-Conway
parseLegacyCmds :: EnvCli -> Parser LegacyCmds
parseLegacyCmds envCli =
  Opt.hsubparser $
    mconcat
      [ Opt.metavar "COMMAND"
      , Opt.command "genesis" $
          Opt.info (LegacyGenesisCmds <$> pGenesisCmds envCli) $
            Opt.progDesc "Genesis block commands"
      ]

pGenesisCmds :: EnvCli -> Parser LegacyGenesisCmds
pGenesisCmds envCli =
  asum
    [ Opt.hsubparser $
        commandWithMetavar "key-gen-genesis" $
          Opt.info pGenesisKeyGen $
            Opt.progDesc "Create a Shelley genesis key pair"
    , Opt.hsubparser $
        commandWithMetavar "key-gen-delegate" $
          Opt.info pGenesisDelegateKeyGen $
            Opt.progDesc "Create a Shelley genesis delegate key pair"
    , Opt.hsubparser $
        commandWithMetavar "key-gen-utxo" $
          Opt.info pGenesisUTxOKeyGen $
            Opt.progDesc "Create a Shelley genesis UTxO key pair"
    , Opt.hsubparser $
        commandWithMetavar "key-hash" $
          Opt.info pGenesisKeyHash $
            Opt.progDesc "Print the identifier (hash) of a public key"
    , Opt.hsubparser $
        commandWithMetavar "get-ver-key" $
          Opt.info pGenesisVerKey $
            Opt.progDesc "Derive the verification key from a signing key"
    , Opt.hsubparser $
        commandWithMetavar "initial-addr" $
          Opt.info pGenesisAddr $
            Opt.progDesc "Get the address for an initial UTxO based on the verification key"
    , Opt.hsubparser $
        commandWithMetavar "initial-txin" $
          Opt.info pGenesisTxIn $
            Opt.progDesc "Get the TxIn for an initial UTxO based on the verification key"
    , Opt.hsubparser $
        commandWithMetavar "create-cardano" $
          Opt.info pGenesisCreateCardano $
            Opt.progDesc $
              mconcat
                [ "Create a Byron and Shelley genesis file from a genesis "
                , "template and genesis/delegation/spending keys."
                ]
    , Opt.hsubparser $
        commandWithMetavar "create" $
          Opt.info pGenesisCreate $
            Opt.progDesc $
              mconcat
                [ "Create a Shelley genesis file from a genesis "
                , "template and genesis/delegation/spending keys."
                ]
    , Opt.hsubparser $
        commandWithMetavar "create-staked" $
          Opt.info pGenesisCreateStaked $
            Opt.progDesc $
              mconcat
                [ "Create a staked Shelley genesis file from a genesis "
                , "template and genesis/delegation/spending keys."
                ]
    , Opt.hsubparser $
        commandWithMetavar "hash" $
          Opt.info pGenesisHash $
            Opt.progDesc $
              unlines
                [ "DEPRECATION WARNING! This command is deprecated and will be "
                , "removed in a future release. Please use hash genesis-file "
                , "instead. "
                , "Compute the hash of a genesis file."
                ]
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
      <$> pAnyShelleyBasedEra envCli
      <*> pGenesisDir
      <*> pGenesisNumGenesisKeys
      <*> pGenesisNumUTxOKeys
      <*> pMaybeSystemStart
      <*> pInitialSupplyNonDelegated
      <*> pSecurityParam
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
      <$> pAnyShelleyBasedEra envCli
      <*> pKeyOutputFormat
      <*> pGenesisDir
      <*> pGenesisNumGenesisKeys
      <*> pGenesisNumUTxOKeys
      <*> pMaybeSystemStart
      <*> pInitialSupplyNonDelegated
      <*> pNetworkId envCli

  pGenesisCreateStaked :: Parser LegacyGenesisCmds
  pGenesisCreateStaked =
    GenesisCreateStaked
      <$> pConwayEra envCli
      <*> pKeyOutputFormat
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
    fmap GenesisDir $
      Opt.strOption $
        mconcat
          [ Opt.long "genesis-dir"
          , Opt.metavar "DIR"
          , Opt.help
              "The genesis directory containing the genesis template and required genesis/delegation/spending keys."
          ]

  pMaybeSystemStart :: Parser (Maybe SystemStart)
  pMaybeSystemStart =
    Opt.optional $
      fmap (SystemStart . convertTime) $
        Opt.strOption $
          mconcat
            [ Opt.long "start-time"
            , Opt.metavar "UTC_TIME"
            , Opt.help
                "The genesis start time in YYYY-MM-DDThh:mm:ssZ format. If unspecified, will be the current time +30 seconds."
            ]

  pGenesisNumGenesisKeys :: Parser Word
  pGenesisNumGenesisKeys =
    Opt.option integralReader $
      mconcat
        [ Opt.long "gen-genesis-keys"
        , Opt.metavar "INT"
        , Opt.help "The number of genesis keys to make [default is 3]."
        , Opt.value 3
        ]

  pNodeConfigTemplate :: Parser (Maybe FilePath)
  pNodeConfigTemplate = optional $ parseFilePath "node-config-template" "the node config template"

  pGenesisNumUTxOKeys :: Parser Word
  pGenesisNumUTxOKeys =
    Opt.option integralReader $
      mconcat
        [ Opt.long "gen-utxo-keys"
        , Opt.metavar "INT"
        , Opt.help "The number of UTxO keys to make [default is 0]."
        , Opt.value 0
        ]

  pGenesisNumPools :: Parser Word
  pGenesisNumPools =
    Opt.option integralReader $
      mconcat
        [ Opt.long "gen-pools"
        , Opt.metavar "INT"
        , Opt.help "The number of stake pool credential sets to make [default is 0]."
        , Opt.value 0
        ]

  pGenesisNumStDelegs :: Parser Word
  pGenesisNumStDelegs =
    Opt.option integralReader $
      mconcat
        [ Opt.long "gen-stake-delegs"
        , Opt.metavar "INT"
        , Opt.help "The number of stake delegator credential sets to make [default is 0]."
        , Opt.value 0
        ]

  pStuffedUtxoCount :: Parser Word
  pStuffedUtxoCount =
    Opt.option integralReader $
      mconcat
        [ Opt.long "num-stuffed-utxo"
        , Opt.metavar "INT"
        , Opt.help "The number of fake UTxO entries to generate [default is 0]."
        , Opt.value 0
        ]

  pRelayJsonFp :: Parser FilePath
  pRelayJsonFp =
    parseFilePath "relay-specification-file" "JSON file specified the relays of each stake pool."

  pInitialSupplyNonDelegated :: Parser (Maybe Coin)
  pInitialSupplyNonDelegated =
    Opt.optional $
      fmap Coin $
        Opt.option integralReader $
          mconcat
            [ Opt.long "supply"
            , Opt.metavar "LOVELACE"
            , Opt.help
                "The initial coin supply in Lovelace which will be evenly distributed across initial, non-delegating stake holders."
            ]

  pInitialSupplyDelegated :: Parser Coin
  pInitialSupplyDelegated =
    fmap (Coin . fromMaybe 0) $
      Opt.optional $
        Opt.option integralReader $
          mconcat
            [ Opt.long "supply-delegated"
            , Opt.metavar "LOVELACE"
            , Opt.help
                "The initial coin supply in Lovelace which will be evenly distributed across initial, delegating stake holders."
            , Opt.value 0
            ]

  pSecurityParam :: Parser (NonZero Word64)
  pSecurityParam =
    Opt.option nonZeroReader $
      mconcat
        [ Opt.long "security-param"
        , Opt.metavar "INT"
        , Opt.help "Security parameter for genesis file [default is 108]."
        , Opt.value $ knownNonZeroBounded @108
        ]

  pSlotLength :: Parser Word
  pSlotLength =
    Opt.option integralReader $
      mconcat
        [ Opt.long "slot-length"
        , Opt.metavar "INT"
        , Opt.help "slot length (ms) parameter for genesis file [default is 1000]."
        , Opt.value 1000
        ]

  pSlotCoefficient :: Parser Rational
  pSlotCoefficient =
    Opt.option readRationalUnitInterval $
      mconcat
        [ Opt.long "slot-coefficient"
        , Opt.metavar "RATIONAL"
        , Opt.help "Slot Coefficient for genesis file [default is .05]."
        , Opt.value 0.05
        ]

  pBulkPoolCredFiles :: Parser Word
  pBulkPoolCredFiles =
    Opt.option integralReader $
      mconcat
        [ Opt.long "bulk-pool-cred-files"
        , Opt.metavar "INT"
        , Opt.help "Generate bulk pool credential files [default is 0]."
        , Opt.value 0
        ]

  pBulkPoolsPerFile :: Parser Word
  pBulkPoolsPerFile =
    Opt.option integralReader $
      mconcat
        [ Opt.long "bulk-pools-per-file"
        , Opt.metavar "INT"
        , Opt.help "Each bulk pool to contain this many pool credential sets [default is 0]."
        , Opt.value 0
        ]
