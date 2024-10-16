{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
  )
where

import           Cardano.Api hiding (QueryInShelleyBasedEra (..))
import qualified Cardano.Api.Byron as Byron
import           Cardano.Api.Ledger (Coin (..))

import           Cardano.CLI.Environment
import           Cardano.CLI.EraBased.Options.Common
import           Cardano.CLI.Legacy.Commands
import           Cardano.CLI.Legacy.Commands.Genesis
import           Cardano.CLI.Legacy.Commands.Governance
import           Cardano.CLI.Parser
import           Cardano.CLI.Types.Common

import           Data.Foldable
import           Data.Maybe (fromMaybe)
import           Data.Word (Word64)
import           Options.Applicative hiding (help, str)
import qualified Options.Applicative as Opt

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
      [ Opt.metavar "Legacy commands"
      , Opt.commandGroup "Legacy commands"
      , Opt.command "genesis" $
          Opt.info (LegacyGenesisCmds <$> pGenesisCmds envCli) $
            Opt.progDesc "Genesis block commands"
      , Opt.command "governance" $
          Opt.info (LegacyGovernanceCmds <$> pGovernanceCmds envCli) $
            Opt.progDesc "Governance commands"
      ]

pGovernanceCmds :: EnvCli -> Parser LegacyGovernanceCmds
pGovernanceCmds envCli =
  asum
    [ subParser "create-mir-certificate" $
        Opt.info (pLegacyMIRPayStakeAddresses <|> mirCertParsers) $
          Opt.progDesc "Create an MIR (Move Instantaneous Rewards) certificate"
    , subParser "create-genesis-key-delegation-certificate" $
        Opt.info pGovernanceGenesisKeyDelegationCertificate $
          Opt.progDesc "Create a genesis key delegation certificate"
    , subParser "create-update-proposal" $
        Opt.info pUpdateProposal $
          Opt.progDesc "Create an update proposal"
    ]
 where
  mirCertParsers :: Parser LegacyGovernanceCmds
  mirCertParsers =
    asum
      [ subParser "stake-addresses" $
          Opt.info pLegacyMIRPayStakeAddresses $
            Opt.progDesc "Create an MIR certificate to pay stake addresses"
      , subParser "transfer-to-treasury" $
          Opt.info pLegacyMIRTransferToTreasury $
            Opt.progDesc "Create an MIR certificate to transfer from the reserves pot to the treasury pot"
      , subParser "transfer-to-rewards" $
          Opt.info pLegacyMIRTransferToReserves $
            Opt.progDesc "Create an MIR certificate to transfer from the treasury pot to the reserves pot"
      ]

  pLegacyMIRPayStakeAddresses :: Parser LegacyGovernanceCmds
  pLegacyMIRPayStakeAddresses =
    GovernanceCreateMirCertificateStakeAddressesCmd
      <$> pAnyShelleyToBabbageEra envCli
      <*> pMIRPot
      <*> some (pStakeAddress Nothing)
      <*> some pRewardAmt
      <*> pOutputFile

  pLegacyMIRTransferToTreasury :: Parser LegacyGovernanceCmds
  pLegacyMIRTransferToTreasury =
    GovernanceCreateMirCertificateTransferToTreasuryCmd
      <$> pAnyShelleyToBabbageEra envCli
      <*> pTransferAmt
      <*> pOutputFile

  pLegacyMIRTransferToReserves :: Parser LegacyGovernanceCmds
  pLegacyMIRTransferToReserves =
    GovernanceCreateMirCertificateTransferToReservesCmd
      <$> pAnyShelleyToBabbageEra envCli
      <*> pTransferAmt
      <*> pOutputFile

  pGovernanceGenesisKeyDelegationCertificate :: Parser LegacyGovernanceCmds
  pGovernanceGenesisKeyDelegationCertificate =
    GovernanceGenesisKeyDelegationCertificate
      <$> pAnyShelleyToBabbageEra envCli
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

pGenesisCmds :: EnvCli -> Parser LegacyGenesisCmds
pGenesisCmds envCli =
  asum
    [ subParser "key-gen-genesis" $
        Opt.info pGenesisKeyGen $
          Opt.progDesc "Create a Shelley genesis key pair"
    , subParser "key-gen-delegate" $
        Opt.info pGenesisDelegateKeyGen $
          Opt.progDesc "Create a Shelley genesis delegate key pair"
    , subParser "key-gen-utxo" $
        Opt.info pGenesisUTxOKeyGen $
          Opt.progDesc "Create a Shelley genesis UTxO key pair"
    , subParser "key-hash" $
        Opt.info pGenesisKeyHash $
          Opt.progDesc "Print the identifier (hash) of a public key"
    , subParser "get-ver-key" $
        Opt.info pGenesisVerKey $
          Opt.progDesc "Derive the verification key from a signing key"
    , subParser "initial-addr" $
        Opt.info pGenesisAddr $
          Opt.progDesc "Get the address for an initial UTxO based on the verification key"
    , subParser "initial-txin" $
        Opt.info pGenesisTxIn $
          Opt.progDesc "Get the TxIn for an initial UTxO based on the verification key"
    , subParser "create-cardano" $
        Opt.info pGenesisCreateCardano $
          Opt.progDesc $
            mconcat
              [ "Create a Byron and Shelley genesis file from a genesis "
              , "template and genesis/delegation/spending keys."
              ]
    , subParser "create" $
        Opt.info pGenesisCreate $
          Opt.progDesc $
            mconcat
              [ "Create a Shelley genesis file from a genesis "
              , "template and genesis/delegation/spending keys."
              ]
    , subParser "create-staked" $
        Opt.info pGenesisCreateStaked $
          Opt.progDesc $
            mconcat
              [ "Create a staked Shelley genesis file from a genesis "
              , "template and genesis/delegation/spending keys."
              ]
    , subParser "hash" $
        Opt.info pGenesisHash $
          Opt.progDesc "Compute the hash of a genesis file"
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
      <*> (Byron.BlockCount <$> pSecurityParam)
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
      <$> pAnyShelleyBasedEra envCli
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
            , Opt.metavar "UTC-TIME"
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

  pSecurityParam :: Parser Word64
  pSecurityParam =
    Opt.option integralReader $
      mconcat
        [ Opt.long "security-param"
        , Opt.metavar "INT"
        , Opt.help "Security parameter for genesis file [default is 108]."
        , Opt.value 108
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
