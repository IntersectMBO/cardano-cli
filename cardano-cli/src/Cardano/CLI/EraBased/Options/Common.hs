{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.CLI.EraBased.Options.Common where

import           Cardano.Api hiding (QueryInShelleyBasedEra (..))
import           Cardano.Api.Shelley hiding (QueryInShelleyBasedEra (..))

import           Cardano.CLI.Environment (EnvCli (..), envCliAnyShelleyBasedEra)
import           Cardano.CLI.Types.Key
import           Cardano.CLI.Types.Legacy
import           Cardano.Prelude (purer)

import           Control.Monad (mfilter)
import qualified Data.Attoparsec.ByteString.Char8 as Atto
import           Data.Bifunctor
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BSC
import           Data.Foldable
import           Data.Function
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import           Data.Maybe (maybeToList)
import           Data.Ratio ((%))
import qualified Data.Text as Text
import           Data.Word (Word64)
import           GHC.Natural (Natural)
import           Options.Applicative hiding (help, str)
import qualified Options.Applicative as Opt
import qualified Text.Parsec as Parsec
import           Text.Parsec ((<?>))
import qualified Text.Parsec.Error as Parsec
import qualified Text.Parsec.Language as Parsec
import qualified Text.Parsec.String as Parsec
import qualified Text.Parsec.Token as Parsec

defaultCurrentEra :: AnyShelleyBasedEra
defaultCurrentEra = AnyShelleyBasedEra ShelleyBasedEraBabbage

pCardanoEra :: EnvCli -> Parser AnyCardanoEra
pCardanoEra envCli =
  asum $ mconcat
    [ [ Opt.flag' (AnyCardanoEra ByronEra) $ mconcat
        [ Opt.long "byron-era"
        , Opt.help "Specify the Byron era"
        ]
      , Opt.flag' (AnyCardanoEra ShelleyEra) $ mconcat
        [ Opt.long "shelley-era"
        , Opt.help "Specify the Shelley era"
        ]
      , Opt.flag' (AnyCardanoEra AllegraEra) $ mconcat
        [ Opt.long "allegra-era"
        , Opt.help "Specify the Allegra era"
        ]
      , Opt.flag' (AnyCardanoEra MaryEra) $ mconcat
        [ Opt.long "mary-era"
        , Opt.help "Specify the Mary era"
        ]
      , Opt.flag' (AnyCardanoEra AlonzoEra) $ mconcat
        [ Opt.long "alonzo-era"
        , Opt.help "Specify the Alonzo era"
        ]
      , Opt.flag' (AnyCardanoEra BabbageEra) $ mconcat
        [ Opt.long "babbage-era"
        , Opt.help "Specify the Babbage era (default)"
        ]
      , Opt.flag' (AnyCardanoEra ConwayEra) $ mconcat
        [ Opt.long "conway-era"
        , Opt.help "Specify the Conway era"
        ]

        -- NEW-ERA-ADD-NEW: When a new era is added, add a new flag here.
        -- NEW-ERA-SET-DEFAULT: When a new era is working, select a new default above and below.
      ]
    , maybeToList $ pure <$> envCliAnyCardanoEra envCli
    -- TODO is this default needed anymore?
    , purer defaultCardanoEra
  ]
    where
      defaultCardanoEra = defaultCurrentEra & \(AnyShelleyBasedEra era) ->
        AnyCardanoEra (shelleyBasedToCardanoEra era)

command' :: String -> String -> Parser a -> Mod CommandFields a
command' c descr p =
  mconcat
    [ command c (info (p <**> helper) $ mconcat [ progDesc descr ])
    , metavar c
    ]

pNetworkId :: EnvCli -> Parser NetworkId
pNetworkId envCli = asum $ mconcat
  [ [ Opt.flag' Mainnet $ mconcat
      [ Opt.long "mainnet"
      , Opt.help $ mconcat
        [ "Use the mainnet magic id. This overrides the CARDANO_NODE_NETWORK_ID "
        , "environment variable"
        ]
      ]
    , fmap (Testnet . NetworkMagic) $ Opt.option (bounded "TESTNET_MAGIC") $ mconcat
      [ Opt.long "testnet-magic"
      , Opt.metavar "NATURAL"
      , Opt.help $ mconcat
        [ "Specify a testnet magic id. This overrides the CARDANO_NODE_NETWORK_ID "
        , "environment variable"
        ]
      ]
    ]
  , -- Default to the network id specified by the environment variable if it is available.
    pure <$> maybeToList (envCliNetworkId envCli)
  ]

pConsensusModeParams :: Parser AnyConsensusModeParams
pConsensusModeParams = asum
  [ pShelleyMode *> pShelleyConsensusMode
  , pByronMode *> pByronConsensusMode
  , pCardanoMode *> pCardanoConsensusMode
  , pDefaultConsensusMode
  ]
  where
    pShelleyMode :: Parser ()
    pShelleyMode =
      Opt.flag' () $ mconcat
        [ Opt.long "shelley-mode"
        , Opt.help "For talking to a node running in Shelley-only mode."
        ]

    pByronMode :: Parser ()
    pByronMode =
      Opt.flag' () $ mconcat
        [ Opt.long "byron-mode"
        , Opt.help "For talking to a node running in Byron-only mode."
        ]

    pCardanoMode :: Parser ()
    pCardanoMode =
      Opt.flag' () $ mconcat
        [ Opt.long "cardano-mode"
        , Opt.help "For talking to a node running in full Cardano mode (default)."
        ]

    pCardanoConsensusMode :: Parser AnyConsensusModeParams
    pCardanoConsensusMode = AnyConsensusModeParams . CardanoModeParams <$> pEpochSlots

    pByronConsensusMode :: Parser AnyConsensusModeParams
    pByronConsensusMode = AnyConsensusModeParams . ByronModeParams <$> pEpochSlots

    pShelleyConsensusMode :: Parser AnyConsensusModeParams
    pShelleyConsensusMode = pure (AnyConsensusModeParams ShelleyModeParams)

    pDefaultConsensusMode :: Parser AnyConsensusModeParams
    pDefaultConsensusMode =
      pure . AnyConsensusModeParams . CardanoModeParams $ EpochSlots defaultByronEpochSlots

defaultByronEpochSlots :: Word64
defaultByronEpochSlots = 21600

pEpochSlots :: Parser EpochSlots
pEpochSlots =
  fmap EpochSlots $ Opt.option (bounded "SLOTS") $ mconcat
    [ Opt.long "epoch-slots"
    , Opt.metavar "SLOTS"
    , Opt.help "The number of slots per epoch for the Byron era."
    , Opt.value defaultByronEpochSlots -- Default to the mainnet value.
    , Opt.showDefault
    ]

pSocketPath :: EnvCli -> Parser SocketPath
pSocketPath envCli =
  asum $ mconcat
    [ [ fmap File $ Opt.strOption $ mconcat
        [ Opt.long "socket-path"
        , Opt.metavar "SOCKET_PATH"
        , Opt.help $ mconcat
          [ "Path to the node socket.  This overrides the CARDANO_NODE_SOCKET_PATH "
          , "environment variable.  The argument is optional if CARDANO_NODE_SOCKET_PATH "
          , "is defined and mandatory otherwise."
          ]
        , Opt.completer (Opt.bashCompleter "file")
        ]
      ]
    , -- Default to the socket path specified by the environment variable if it is available.
      pure . File <$> maybeToList (envCliSocketPath envCli)
    ]

readerFromParsecParser :: Parsec.Parser a -> Opt.ReadM a
readerFromParsecParser p =
    Opt.eitherReader (first formatError . Parsec.parse (p <* Parsec.eof) "")
  where
    formatError err =
      Parsec.showErrorMessages "or" "unknown parse error"
                               "expecting" "unexpected" "end of input"
                               (Parsec.errorMessages err)

parseTxIn :: Parsec.Parser TxIn
parseTxIn = TxIn <$> parseTxId <*> (Parsec.char '#' *> parseTxIx)

parseTxId :: Parsec.Parser TxId
parseTxId = do
  str' <- some Parsec.hexDigit <?> "transaction id (hexadecimal)"
  case deserialiseFromRawBytesHex AsTxId (BSC.pack str') of
    Right addr -> return addr
    Left e -> fail $ "Incorrect transaction id format: " ++ displayError e

parseTxIx :: Parsec.Parser TxIx
parseTxIx = TxIx . fromIntegral <$> decimal

decimal :: Parsec.Parser Integer
Parsec.TokenParser { Parsec.decimal = decimal } = Parsec.haskell


pStakeIdentifier :: Parser StakeIdentifier
pStakeIdentifier = asum
  [ StakeIdentifierVerifier <$> pStakeVerifier
  , StakeIdentifierAddress <$> pStakeAddress
  ]

pStakeVerifier :: Parser StakeVerifier
pStakeVerifier = asum
  [ StakeVerifierKey <$> pStakeVerificationKeyOrFile
  , StakeVerifierScriptFile <$> pScriptFor "stake-script-file" Nothing "Filepath of the staking script."
  ]

pStakeAddress :: Parser StakeAddress
pStakeAddress =
  Opt.option (readerFromParsecParser parseStakeAddress) $ mconcat
    [ Opt.long "stake-address"
    , Opt.metavar "ADDRESS"
    , Opt.help "Target stake address (bech32 format)."
    ]

parseStakeAddress :: Parsec.Parser StakeAddress
parseStakeAddress = do
  str' <- lexPlausibleAddressString
  case deserialiseAddress AsStakeAddress str' of
    Nothing   -> fail $ "invalid address: " <> Text.unpack str'
    Just addr -> pure addr

pStakeVerificationKeyOrFile :: Parser (VerificationKeyOrFile StakeKey)
pStakeVerificationKeyOrFile =
  VerificationKeyValue <$> pStakeVerificationKey
    <|> VerificationKeyFilePath <$> pStakeVerificationKeyFile

pScriptFor :: String -> Maybe String -> String -> Parser ScriptFile
pScriptFor name Nothing help' =
  fmap ScriptFile $ Opt.strOption $ mconcat
    [ Opt.long name
    , Opt.metavar "FILE"
    , Opt.help help'
    , Opt.completer (Opt.bashCompleter "file")
    ]

pScriptFor name (Just deprecated) help' =
      pScriptFor name Nothing help'
  <|> ScriptFile <$> Opt.strOption
        (  Opt.long deprecated
        <> Opt.internal
        )

pStakeVerificationKey :: Parser (VerificationKey StakeKey)
pStakeVerificationKey =
  Opt.option (readVerificationKey AsStakeKey) $ mconcat
    [ Opt.long "stake-verification-key"
    , Opt.metavar "STRING"
    , Opt.help "Stake verification key (Bech32 or hex-encoded)."
    ]

-- | Read a Bech32 or hex-encoded verification key.
readVerificationKey
  :: forall keyrole. SerialiseAsBech32 (VerificationKey keyrole)
  => AsType keyrole
  -> Opt.ReadM (VerificationKey keyrole)
readVerificationKey asType =
    Opt.eitherReader deserialiseFromBech32OrHex
  where
    keyFormats :: NonEmpty (InputFormat (VerificationKey keyrole))
    keyFormats = NE.fromList [InputFormatBech32, InputFormatHex]

    deserialiseFromBech32OrHex
      :: String
      -> Either String (VerificationKey keyrole)
    deserialiseFromBech32OrHex str' =
      first (Text.unpack . renderInputDecodeError) $
        deserialiseInput (AsVerificationKey asType) keyFormats (BSC.pack str')

pStakeVerificationKeyFile :: Parser (VerificationKeyFile In)
pStakeVerificationKeyFile =
  File <$> asum
    [ Opt.strOption $ mconcat
      [ Opt.long "stake-verification-key-file"
      , Opt.metavar "FILE"
      , Opt.help "Filepath of the staking verification key."
      , Opt.completer (Opt.bashCompleter "file")
      ]
    , Opt.strOption $ mconcat
      [ Opt.long "staking-verification-key-file"
      , Opt.internal
      ]
    ]

subParser :: String -> ParserInfo a -> Parser a
subParser availableCommand pInfo =
  Opt.hsubparser $ Opt.command availableCommand pInfo <> Opt.metavar availableCommand

pAnyShelleyBasedEra :: EnvCli -> Parser AnyShelleyBasedEra
pAnyShelleyBasedEra envCli =
  asum $ mconcat
    [ [ Opt.flag' (AnyShelleyBasedEra ShelleyBasedEraShelley)
        $ mconcat [Opt.long "shelley-era", Opt.help "Specify the Shelley era"]
      , Opt.flag' (AnyShelleyBasedEra ShelleyBasedEraAllegra)
        $ mconcat [Opt.long "allegra-era", Opt.help "Specify the Allegra era"]
      , Opt.flag' (AnyShelleyBasedEra ShelleyBasedEraMary)
        $ mconcat [Opt.long "mary-era", Opt.help "Specify the Mary era"]
      , Opt.flag' (AnyShelleyBasedEra ShelleyBasedEraAlonzo)
        $ mconcat [Opt.long "alonzo-era", Opt.help "Specify the Alonzo era"]
      , Opt.flag' (AnyShelleyBasedEra ShelleyBasedEraBabbage)
        $ mconcat [Opt.long "babbage-era", Opt.help "Specify the Babbage era (default)"]
      , Opt.flag' (AnyShelleyBasedEra ShelleyBasedEraConway)
        $ mconcat [Opt.long "conway-era", Opt.help "Specify the Conway era"]
      ]
    , maybeToList $ pure <$> envCliAnyShelleyBasedEra envCli
    , purer defaultCurrentEra
  ]

pShelleyBasedShelley :: EnvCli -> Parser AnyShelleyBasedEra
pShelleyBasedShelley envCli =
  asum $ mconcat
    [ [ Opt.flag' (AnyShelleyBasedEra ShelleyBasedEraShelley)
          $ mconcat [Opt.long "shelley-era", Opt.help "Specify the Shelley era"]
      ]
    , maybeToList
        $ fmap pure
        $ mfilter (== AnyShelleyBasedEra ShelleyBasedEraShelley)
        $ envCliAnyShelleyBasedEra envCli
    ]

pShelleyBasedAllegra :: EnvCli -> Parser AnyShelleyBasedEra
pShelleyBasedAllegra envCli =
  asum $ mconcat
    [ [ Opt.flag' (AnyShelleyBasedEra ShelleyBasedEraAllegra)
          $ mconcat [Opt.long "allegra-era", Opt.help "Specify the Allegra era"]
      ]
    , maybeToList
        $ fmap pure
        $ mfilter (== AnyShelleyBasedEra ShelleyBasedEraAllegra)
        $ envCliAnyShelleyBasedEra envCli
    ]

pShelleyBasedMary :: EnvCli -> Parser AnyShelleyBasedEra
pShelleyBasedMary envCli =
  asum $ mconcat
    [ [ Opt.flag' (AnyShelleyBasedEra ShelleyBasedEraMary)
          $ mconcat [Opt.long "mary-era", Opt.help "Specify the Mary era"]
      ]
    , maybeToList
        $ fmap pure
        $ mfilter (== AnyShelleyBasedEra ShelleyBasedEraMary)
        $ envCliAnyShelleyBasedEra envCli
    ]

pShelleyBasedAlonzo :: EnvCli -> Parser AnyShelleyBasedEra
pShelleyBasedAlonzo envCli =
  asum $ mconcat
    [ [ Opt.flag' (AnyShelleyBasedEra ShelleyBasedEraAlonzo)
          $ mconcat [Opt.long "alonzo-era", Opt.help "Specify the Alonzo era"]
      ]
    , maybeToList
        $ fmap pure
        $ mfilter (== AnyShelleyBasedEra ShelleyBasedEraAlonzo)
        $ envCliAnyShelleyBasedEra envCli
    ]

pShelleyBasedBabbage :: EnvCli -> Parser AnyShelleyBasedEra
pShelleyBasedBabbage envCli =
  asum $ mconcat
    [ [ Opt.flag' (AnyShelleyBasedEra ShelleyBasedEraBabbage)
          $ mconcat [Opt.long "babbage-era", Opt.help "Specify the Babbage era (default)"]
      ]
    , maybeToList
        $ fmap pure
        $ mfilter (== AnyShelleyBasedEra ShelleyBasedEraBabbage)
        $ envCliAnyShelleyBasedEra envCli
    ]

pShelleyBasedConway :: EnvCli -> Parser AnyShelleyBasedEra
pShelleyBasedConway envCli =
  asum $ mconcat
    [ [ Opt.flag' (AnyShelleyBasedEra ShelleyBasedEraConway)
          $ mconcat [Opt.long "conway-era", Opt.help "Specify the Conway era"]
      ]
    , maybeToList
        $ fmap pure
        $ mfilter (== AnyShelleyBasedEra ShelleyBasedEraConway)
        $ envCliAnyShelleyBasedEra envCli
    ]

pFileOutDirection :: String -> String -> Parser (File a Out)
pFileOutDirection l h =
  Opt.strOption $ mconcat
    [ Opt.long l
    , Opt.metavar "FILE"
    , Opt.help h
    , Opt.completer (Opt.bashCompleter "file")
    ]

pFileInDirection :: String -> String -> Parser (File a In)
pFileInDirection l h =
  Opt.strOption $ mconcat
    [ Opt.long l
    , Opt.metavar "FILE"
    , Opt.help h
    , Opt.completer (Opt.bashCompleter "file")
    ]

parseLovelace :: Parsec.Parser Lovelace
parseLovelace = do
  i <- decimal
  if i > toInteger (maxBound :: Word64)
    then fail $ show i <> " lovelace exceeds the Word64 upper bound"
    else return $ Lovelace i

pStakePoolVerificationKeyOrFile :: Parser (VerificationKeyOrFile StakePoolKey)
pStakePoolVerificationKeyOrFile =
  asum
    [ VerificationKeyValue <$> pStakePoolVerificationKey
    , VerificationKeyFilePath <$> pStakePoolVerificationKeyFile
    ]

pStakePoolVerificationKey :: Parser (VerificationKey StakePoolKey)
pStakePoolVerificationKey =
  Opt.option (readVerificationKey AsStakePoolKey) $ mconcat
    [ Opt.long "stake-pool-verification-key"
    , Opt.metavar "STRING"
    , Opt.help "Stake pool verification key (Bech32 or hex-encoded)."
    ]

pStakePoolVerificationKeyFile :: Parser (VerificationKeyFile In)
pStakePoolVerificationKeyFile =
  File <$> asum
    [ Opt.strOption $ mconcat
      [ Opt.long "cold-verification-key-file"
      , Opt.metavar "FILE"
      , Opt.help "Filepath of the stake pool verification key."
      , Opt.completer (Opt.bashCompleter "file")
      ]
    , Opt.strOption $ mconcat
      [ Opt.long "stake-pool-verification-key-file"
      , Opt.internal
      ]
    ]

pOutputFile :: Parser (File content Out)
pOutputFile =
  fmap File $ Opt.strOption $ mconcat
    [ Opt.long "out-file"
    , Opt.metavar "FILE"
    , Opt.help "The output file."
    , Opt.completer (Opt.bashCompleter "file")
    ]

pVrfVerificationKeyOrFile :: Parser (VerificationKeyOrFile VrfKey)
pVrfVerificationKeyOrFile =
  asum
    [ VerificationKeyValue <$> pVrfVerificationKey
    , VerificationKeyFilePath <$> pVrfVerificationKeyFile
    ]

pVrfVerificationKey :: Parser (VerificationKey VrfKey)
pVrfVerificationKey =
  Opt.option (readVerificationKey AsVrfKey) $ mconcat
    [ Opt.long "vrf-verification-key"
    , Opt.metavar "STRING"
    , Opt.help "VRF verification key (Bech32 or hex-encoded)."
    ]

pVrfVerificationKeyFile :: Parser (VerificationKeyFile In)
pVrfVerificationKeyFile =
  fmap File $ Opt.strOption $ mconcat
    [ Opt.long "vrf-verification-key-file"
    , Opt.metavar "FILE"
    , Opt.help "Filepath of the VRF verification key."
    , Opt.completer (Opt.bashCompleter "file")
    ]

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

pGenesisDelegateVerificationKeyFile :: Parser (VerificationKeyFile In)
pGenesisDelegateVerificationKeyFile =
  fmap File $ Opt.strOption $ mconcat
    [ Opt.long "genesis-delegate-verification-key-file"
    , Opt.metavar "FILE"
    , Opt.help "Filepath of the genesis delegate verification key."
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

pGenesisVerificationKeyFile :: Parser (VerificationKeyFile In)
pGenesisVerificationKeyFile =
  fmap File $ Opt.strOption $ mconcat
    [ Opt.long "genesis-verification-key-file"
    , Opt.metavar "FILE"
    , Opt.help "Filepath of the genesis verification key."
    , Opt.completer (Opt.bashCompleter "file")
    ]

pGenesisDelegateVerificationKey :: Parser (VerificationKey GenesisDelegateKey)
pGenesisDelegateVerificationKey =
  Opt.option (Opt.eitherReader deserialiseFromHex) $ mconcat
    [ Opt.long "genesis-delegate-verification-key"
    , Opt.metavar "STRING"
    , Opt.help "Genesis delegate verification key (hex-encoded)."
    ]
  where
    deserialiseFromHex
      :: String
      -> Either String (VerificationKey GenesisDelegateKey)
    deserialiseFromHex =
      first
        (\e -> "Invalid genesis delegate verification key: " ++ displayError e)
        . deserialiseFromRawBytesHex (AsVerificationKey AsGenesisDelegateKey)
        . BSC.pack

pVrfVerificationKeyOrHashOrFile :: Parser (VerificationKeyOrHashOrFile VrfKey)
pVrfVerificationKeyOrHashOrFile =
  asum
    [ VerificationKeyOrFile <$> pVrfVerificationKeyOrFile
    , VerificationKeyHash <$> pVrfVerificationKeyHash
    ]

pGenesisVerificationKeyOrFile :: Parser (VerificationKeyOrFile GenesisKey)
pGenesisVerificationKeyOrFile =
  asum
    [ VerificationKeyValue <$> pGenesisVerificationKey
    , VerificationKeyFilePath <$> pGenesisVerificationKeyFile
    ]

pGenesisDelegateVerificationKeyOrFile :: Parser (VerificationKeyOrFile GenesisDelegateKey)
pGenesisDelegateVerificationKeyOrFile =
  asum
    [ VerificationKeyValue <$> pGenesisDelegateVerificationKey
    , VerificationKeyFilePath <$> pGenesisDelegateVerificationKeyFile
    ]

pGenesisVerificationKeyOrHashOrFile :: Parser (VerificationKeyOrHashOrFile GenesisKey)
pGenesisVerificationKeyOrHashOrFile =
  asum
    [ VerificationKeyOrFile <$> pGenesisVerificationKeyOrFile
    , VerificationKeyHash <$> pGenesisVerificationKeyHash
    ]

pGenesisDelegateVerificationKeyOrHashOrFile :: Parser (VerificationKeyOrHashOrFile GenesisDelegateKey)
pGenesisDelegateVerificationKeyOrHashOrFile =
  asum
    [ VerificationKeyOrFile <$> pGenesisDelegateVerificationKeyOrFile
    , VerificationKeyHash <$> pGenesisDelegateVerificationKeyHash
    ]

pStakePoolVerificationKeyOrHashOrFile :: Parser (VerificationKeyOrHashOrFile StakePoolKey)
pStakePoolVerificationKeyOrHashOrFile =
  asum
    [ VerificationKeyOrFile <$> pStakePoolVerificationKeyOrFile
    , VerificationKeyHash <$> pStakePoolVerificationKeyHash
    ]

pStakePoolVerificationKeyHash :: Parser (Hash StakePoolKey)
pStakePoolVerificationKeyHash =
  Opt.option (pBech32StakePoolId <|> pHexStakePoolId) $ mconcat
    [ Opt.long "stake-pool-id"
    , Opt.metavar "STAKE_POOL_ID"
    , Opt.help $ mconcat
        [ "Stake pool ID/verification key hash (either Bech32-encoded or hex-encoded).  "
        , "Zero or more occurences of this option is allowed."
        ]
    ]
  where
    pHexStakePoolId :: ReadM (Hash StakePoolKey)
    pHexStakePoolId =
      Opt.eitherReader $
        first displayError
          . deserialiseFromRawBytesHex (AsHash AsStakePoolKey)
          . BSC.pack

    pBech32StakePoolId :: ReadM (Hash StakePoolKey)
    pBech32StakePoolId =
      Opt.eitherReader $
        first displayError
        . deserialiseFromBech32 (AsHash AsStakePoolKey)
        . Text.pack

pEpochNoUpdateProp :: Parser EpochNo
pEpochNoUpdateProp =
  fmap EpochNo $ Opt.option (bounded "EPOCH") $ mconcat
    [ Opt.long "epoch"
    , Opt.metavar "EPOCH"
    , Opt.help "The epoch number in which the update proposal is valid."
    ]

-- Protocol paramters

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

readRational :: Opt.ReadM Rational
readRational =
  asum
    [ toRational <$> readerFromAttoParser Atto.scientific
    , readFractionAsRational
    ]

readRationalUnitInterval :: Opt.ReadM Rational
readRationalUnitInterval = readRational >>= checkUnitInterval
  where
   checkUnitInterval :: Rational -> Opt.ReadM Rational
   checkUnitInterval q
     | q >= 0 && q <= 1 = return q
     | otherwise        = fail "Please enter a value in the range [0,1]"

readerFromAttoParser :: Atto.Parser a -> Opt.ReadM a
readerFromAttoParser p =
  Opt.eitherReader (Atto.parseOnly (p <* Atto.endOfInput) . BSC.pack)

readFractionAsRational :: Opt.ReadM Rational
readFractionAsRational = readerFromAttoParser fractionalAsRational
  where fractionalAsRational :: Atto.Parser Rational
        fractionalAsRational = (%) <$> (Atto.decimal @Integer <* Atto.char '/') <*> Atto.decimal @Integer
