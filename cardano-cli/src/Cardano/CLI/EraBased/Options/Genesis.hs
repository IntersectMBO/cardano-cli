{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.EraBased.Options.Genesis
  ( pGenesisCmds
  )
where

import           Cardano.Api hiding (QueryInShelleyBasedEra (..))
import qualified Cardano.Api.Byron as Byron
import           Cardano.Api.Ledger (Coin (..))

import           Cardano.CLI.Environment (EnvCli (..))
import           Cardano.CLI.EraBased.Commands.Genesis
import           Cardano.CLI.EraBased.Options.Common
import           Cardano.CLI.Parser
import           Cardano.CLI.Types.Common

import           Data.Maybe
import           Data.Word (Word64)
import           Options.Applicative hiding (help, str)
import qualified Options.Applicative as Opt

pGenesisCmds
  :: ()
  => ShelleyBasedEra era
  -> EnvCli
  -> Maybe (Parser (GenesisCmds era))
pGenesisCmds era envCli =
  subInfoParser
    "genesis"
    ( Opt.progDesc $
        mconcat
          [ "Genesis block commands."
          ]
    )
    [ Just $
        subParser "key-gen-genesis" $
          Opt.info pGenesisKeyGen $
            Opt.progDesc "Create a Shelley genesis key pair"
    , Just $
        subParser "key-gen-delegate" $
          Opt.info pGenesisDelegateKeyGen $
            Opt.progDesc "Create a Shelley genesis delegate key pair"
    , Just $
        subParser "key-gen-utxo" $
          Opt.info pGenesisUTxOKeyGen $
            Opt.progDesc "Create a Shelley genesis UTxO key pair"
    , Just $
        subParser "key-hash" $
          Opt.info pGenesisKeyHash $
            Opt.progDesc "Print the identifier (hash) of a public key"
    , Just $
        subParser "get-ver-key" $
          Opt.info pGenesisVerKey $
            Opt.progDesc "Derive the verification key from a signing key"
    , Just $
        subParser "initial-addr" $
          Opt.info (pGenesisAddr envCli) $
            Opt.progDesc "Get the address for an initial UTxO based on the verification key"
    , Just $
        subParser "initial-txin" $
          Opt.info (pGenesisTxIn envCli) $
            Opt.progDesc "Get the TxIn for an initial UTxO based on the verification key"
    , forShelleyBasedEraInEonMaybe
        era
        ( \sbe ->
            subParser "create-cardano" $
              Opt.info (pGenesisCreateCardano sbe envCli) $
                Opt.progDesc $
                  mconcat
                    [ "Create a Byron and Shelley genesis file from a genesis "
                    , "template and genesis/delegation/spending keys."
                    ]
        )
    , forShelleyBasedEraInEonMaybe era $ \sbe ->
        subParser "create" $
          Opt.info (pGenesisCreate sbe envCli) $
            Opt.progDesc $
              mconcat
                [ "Create a Shelley genesis file from a genesis "
                , "template and genesis/delegation/spending keys."
                ]
    , forShelleyBasedEraInEonMaybe era $ \sbe ->
        subParser "create-staked" $
          Opt.info (pGenesisCreateStaked sbe envCli) $
            Opt.progDesc $
              mconcat
                [ "Create a staked Shelley genesis file from a genesis "
                , "template and genesis/delegation/spending keys."
                ]
    , forShelleyBasedEraInEonMaybe era $ \sbe ->
        subParser "create-testnet-data" $
          Opt.info (pGenesisCreateTestNetData sbe envCli) $
            Opt.progDesc $
              mconcat
                [ "Create data to use for starting a testnet."
                ]
    , Just $
        subParser "hash" $
          Opt.info pGenesisHash $
            Opt.progDesc "Compute the hash of a genesis file"
    ]

pGenesisKeyGen :: Parser (GenesisCmds era)
pGenesisKeyGen =
  fmap GenesisKeyGenGenesis $
    GenesisKeyGenGenesisCmdArgs
      <$> pVerificationKeyFileOut
      <*> pSigningKeyFileOut

pGenesisDelegateKeyGen :: Parser (GenesisCmds era)
pGenesisDelegateKeyGen =
  fmap GenesisKeyGenDelegate $
    GenesisKeyGenDelegateCmdArgs
      <$> pVerificationKeyFileOut
      <*> pSigningKeyFileOut
      <*> pOperatorCertIssueCounterFile

pGenesisUTxOKeyGen :: Parser (GenesisCmds era)
pGenesisUTxOKeyGen =
  fmap GenesisKeyGenUTxO $
    GenesisKeyGenUTxOCmdArgs
      <$> pVerificationKeyFileOut
      <*> pSigningKeyFileOut

pGenesisKeyHash :: Parser (GenesisCmds era)
pGenesisKeyHash =
  GenesisCmdKeyHash
    <$> pVerificationKeyFileIn

pGenesisVerKey :: Parser (GenesisCmds era)
pGenesisVerKey =
  fmap GenesisVerKey $
    GenesisVerKeyCmdArgs
      <$> pVerificationKeyFileOut
      <*> pSigningKeyFileIn

pGenesisAddr :: EnvCli -> Parser (GenesisCmds era)
pGenesisAddr envCli =
  fmap GenesisAddr $
    GenesisAddrCmdArgs
      <$> pVerificationKeyFileIn
      <*> pNetworkId envCli
      <*> pMaybeOutputFile

pGenesisTxIn :: EnvCli -> Parser (GenesisCmds era)
pGenesisTxIn envCli =
  fmap GenesisTxIn $
    GenesisTxInCmdArgs
      <$> pVerificationKeyFileIn
      <*> pNetworkId envCli
      <*> pMaybeOutputFile

pGenesisCreateCardano :: ShelleyBasedEra era -> EnvCli -> Parser (GenesisCmds era)
pGenesisCreateCardano sbe envCli =
  fmap GenesisCreateCardano $
    GenesisCreateCardanoCmdArgs sbe
      <$> pGenesisDir
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

pGenesisCreate :: ShelleyBasedEra era -> EnvCli -> Parser (GenesisCmds era)
pGenesisCreate sbe envCli =
  fmap GenesisCreate $
    GenesisCreateCmdArgs sbe
      <$> pKeyOutputFormat
      <*> pGenesisDir
      <*> pGenesisNumGenesisKeys
      <*> pGenesisNumUTxOKeys
      <*> pMaybeSystemStart
      <*> pInitialSupplyNonDelegated
      <*> pNetworkId envCli

pGenesisCreateStaked :: ShelleyBasedEra era -> EnvCli -> Parser (GenesisCmds era)
pGenesisCreateStaked sbe envCli =
  fmap GenesisCreateStaked $
    GenesisCreateStakedCmdArgs sbe
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
 where
  pRelayJsonFp :: Parser FilePath
  pRelayJsonFp =
    parseFilePath "relay-specification-file" "JSON file that specifies the relays of each stake pool."

pGenesisCreateTestNetData :: ShelleyBasedEra era -> EnvCli -> Parser (GenesisCmds era)
pGenesisCreateTestNetData sbe envCli =
  fmap GenesisCreateTestNetData $
    GenesisCreateTestNetDataCmdArgs sbe
      <$> optional (pSpecFile "shelley")
      <*> optional (pSpecFile "alonzo")
      <*> optional (pSpecFile "conway")
      <*> pNumGenesisKeys
      <*> pNumPools
      <*> pNumStakeDelegs
      <*> pNumDReps
      <*> pNumStuffedUtxoCount
      <*> pNumUtxoKeys
      <*> pSupply
      <*> pSupplyDelegated
      <*> optional (pNetworkIdForTestnetData envCli)
      <*> Opt.optional pRelays
      <*> pMaybeSystemStart
      <*> pOutputDir
 where
  pSpecFile eraStr =
    parseFilePath
      ("spec-" <> eraStr)
      ("The " <> eraStr <> " specification file to use as input. A default one is generated if omitted.")
  pNumGenesisKeys =
    Opt.option integralReader $
      mconcat
        [ Opt.long "genesis-keys"
        , Opt.metavar "INT"
        , Opt.help "The number of genesis keys to make (default is 3)."
        , Opt.value 3
        ]
  pNumPools :: Parser Word
  pNumPools =
    Opt.option integralReader $
      mconcat
        [ Opt.long "pools"
        , Opt.metavar "INT"
        , Opt.help "The number of stake pool credential sets to make (default is 0)."
        , Opt.value 0
        ]
  pNumDReps :: Parser DRepCredentials
  pNumDReps =
    pDReps OnDisk "drep-keys" "Credentials are written to disk."
      <|> pDReps Transient "transient-drep-keys" "The credentials are NOT written to disk."
   where
    pDReps :: CredentialGenerationMode -> String -> String -> Parser DRepCredentials
    pDReps mode modeOptionName modeExplanation =
      DRepCredentials mode
        <$> Opt.option
          integralReader
          ( mconcat
              [ Opt.long modeOptionName
              , Opt.help $ "The number of DRep credentials to make (default is 0). " <> modeExplanation
              , Opt.metavar "INT"
              , Opt.value 0
              ]
          )
  pNumStakeDelegs :: Parser StakeDelegators
  pNumStakeDelegs =
    pStakeDelegators OnDisk "stake-delegators" "Credentials are written to disk."
      <|> pStakeDelegators Transient "transient-stake-delegators" "The credentials are NOT written to disk."
   where
    pStakeDelegators :: CredentialGenerationMode -> String -> String -> Parser StakeDelegators
    pStakeDelegators mode modeOptionName modeExplanation =
      StakeDelegators mode
        <$> Opt.option
          integralReader
          ( mconcat
              [ Opt.long modeOptionName
              , Opt.help $
                  "The number of stake delegator credential sets to make (default is 0). " <> modeExplanation
              , Opt.metavar "INT"
              , Opt.value 0
              ]
          )
  pNumStuffedUtxoCount :: Parser Word
  pNumStuffedUtxoCount =
    Opt.option integralReader $
      mconcat
        [ Opt.long "stuffed-utxo"
        , Opt.metavar "INT"
        , Opt.help "The number of fake UTxO entries to generate (default is 0)."
        , Opt.value 0
        ]
  pNumUtxoKeys :: Parser Word
  pNumUtxoKeys =
    Opt.option integralReader $
      mconcat
        [ Opt.long "utxo-keys"
        , Opt.metavar "INT"
        , Opt.help "The number of UTxO keys to make (default is 0)."
        , Opt.value 0
        ]
  pSupply :: Parser (Maybe Coin)
  pSupply =
    Opt.optional $
      fmap Coin $
        Opt.option integralReader $
          mconcat
            [ Opt.long "total-supply"
            , Opt.metavar "LOVELACE"
            , Opt.help $
                mconcat
                  [ "The maximum possible amount of Lovelace, which is evenly distributed across stake holders. Overrides the value from the shelley genesis."
                  , " If --delegated-supply is specified, a part of this amount will be delegated."
                  ]
            ]
  pSupplyDelegated :: Parser (Maybe Coin)
  pSupplyDelegated =
    Opt.optional $
      fmap Coin $
        Opt.option integralReader $
          mconcat
            [ Opt.long "delegated-supply"
            , Opt.metavar "LOVELACE"
            , Opt.help $
                mconcat
                  [ "The amount of the total supply which is evenly delegated. Defaulted to half of the total supply."
                  , " Cannot be more than the amount specified with --total-supply."
                  ]
            ]
  pRelays :: Parser FilePath
  pRelays =
    parseFilePath "relays" "JSON file specifying the relays of each stake pool."
  pOutputDir =
    Opt.strOption $
      mconcat
        [ Opt.long "out-dir"
        , Opt.metavar "DIR"
        , Opt.help "The directory where to generate the data. Created if not existing."
        ]

pGenesisHash :: Parser (GenesisCmds era)
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
      , Opt.value 1_000
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
