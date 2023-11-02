{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- HLINT ignore "Move brackets to avoid $" -}
{- HLINT ignore "Use <$>" -}
{- HLINT ignore "Move brackets to avoid $" -}

module Cardano.CLI.EraBased.Options.Common where

import           Cardano.Api
import qualified Cardano.Api.Ledger as Ledger
import           Cardano.Api.Shelley

import           Cardano.CLI.Environment (EnvCli (..), envCliAnyShelleyBasedEra,
                   envCliAnyShelleyToBabbageEra)
import           Cardano.CLI.Parser
import           Cardano.CLI.Read
import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Governance
import           Cardano.CLI.Types.Key
import           Cardano.CLI.Types.Key.VerificationKey
import qualified Cardano.Ledger.BaseTypes as L
import qualified Cardano.Ledger.Crypto as Crypto
import qualified Cardano.Ledger.SafeHash as L
import qualified Cardano.Ledger.Shelley.TxBody as Shelley

import           Control.Monad (mfilter)
import qualified Data.Aeson as Aeson
import           Data.Bifunctor
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BSC
import           Data.Foldable
import           Data.Functor (($>))
import qualified Data.IP as IP
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import           Data.Maybe
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Time.Clock (UTCTime)
import           Data.Time.Format (defaultTimeLocale, parseTimeOrError)
import           Data.Word
import           GHC.Natural (Natural)
import           Network.Socket (PortNumber)
import           Options.Applicative hiding (help, str)
import qualified Options.Applicative as Opt
import qualified Text.Parsec as Parsec
import           Text.Parsec ((<?>))
import qualified Text.Parsec.Error as Parsec
import qualified Text.Parsec.Language as Parsec
import qualified Text.Parsec.String as Parsec
import qualified Text.Parsec.Token as Parsec
import           Text.Read (readEither, readMaybe)

defaultShelleyBasedEra :: EraInEon ShelleyBasedEra
defaultShelleyBasedEra = EraInEon ShelleyBasedEraBabbage

defaultShelleyToBabbageEra :: EraInEon ShelleyToBabbageEra
defaultShelleyToBabbageEra = EraInEon ShelleyToBabbageEraBabbage

command' :: String -> String -> Parser a -> Mod CommandFields a
command' c descr p =
  mconcat
    [ command c (info (p <**> helper) $ mconcat [ progDesc descr ])
    , metavar c
    ]

-- | @prefixFlag Nothing bar@ is @bar@, while @prefixFlag (Just "foo") bar@ is @foo-bar@.
-- This function is used to optionally prefix some long flags
prefixFlag :: Maybe String -> String -> String
prefixFlag prefix longFlag =
  case prefix of
    Nothing -> longFlag
    Just prefix' -> prefix' <> "-" <> longFlag

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

toUnitIntervalOrErr :: Rational -> L.UnitInterval
toUnitIntervalOrErr r = case Ledger.boundRational r of
                         Nothing ->
                           error $ mconcat [ "toUnitIntervalOrErr: "
                                           , "rational out of bounds " <> show r
                                           ]
                         Just n -> n

pConsensusModeParams :: Parser ConsensusModeParams
pConsensusModeParams = asum
  [ pCardanoMode *> pCardanoConsensusMode
  , pDefaultConsensusMode
  ]
  where
    pCardanoMode :: Parser ()
    pCardanoMode =
      Opt.flag' () $ mconcat
        [ Opt.long "cardano-mode"
        , Opt.help "For talking to a node running in full Cardano mode (default)."
        ]

    pCardanoConsensusMode :: Parser ConsensusModeParams
    pCardanoConsensusMode = CardanoModeParams <$> pEpochSlots

    pDefaultConsensusMode :: Parser ConsensusModeParams
    pDefaultConsensusMode =
      pure . CardanoModeParams $ EpochSlots defaultByronEpochSlots

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
  [ StakeVerifierKey <$> pStakeVerificationKeyOrFile Nothing
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

-- | First argument is the optional prefix
pStakeVerificationKeyOrFile :: Maybe String -> Parser (VerificationKeyOrFile StakeKey)
pStakeVerificationKeyOrFile prefix =
  VerificationKeyValue <$> pStakeVerificationKey prefix
    <|> VerificationKeyFilePath <$> pStakeVerificationKeyFile prefix

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

-- | The first argument is the optional prefix.
pStakeVerificationKey :: Maybe String -> Parser (VerificationKey StakeKey)
pStakeVerificationKey prefix =
  Opt.option (readVerificationKey AsStakeKey) $ mconcat
    [ Opt.long $ prefixFlag prefix "stake-verification-key"
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

-- | The first argument is the optional prefix.
pStakeVerificationKeyFile :: Maybe String -> Parser (VerificationKeyFile In)
pStakeVerificationKeyFile prefix =
  File <$> asum
    [ Opt.strOption $ mconcat
      [ Opt.long $ prefixFlag prefix "stake-verification-key-file"
      , Opt.metavar "FILE"
      , Opt.help "Filepath of the staking verification key."
      , Opt.completer (Opt.bashCompleter "file")
      ]
    , Opt.strOption $ mconcat
      [ Opt.long $ prefixFlag prefix "staking-verification-key-file"
      , Opt.internal
      ]
    ]

subParser :: String -> ParserInfo a -> Parser a
subParser availableCommand pInfo =
  Opt.hsubparser $ Opt.command availableCommand pInfo <> Opt.metavar availableCommand

subInfoParser :: String -> InfoMod a -> [Maybe (Parser a)] -> Maybe (Parser a)
subInfoParser name i mps = case catMaybes mps of
  [] -> Nothing
  parsers -> Just $ subParser name $ Opt.info (asum parsers) i

pAnyShelleyBasedEra :: EnvCli -> Parser (EraInEon ShelleyBasedEra)
pAnyShelleyBasedEra envCli =
  asum $ mconcat
    [ [ Opt.flag' (EraInEon ShelleyBasedEraShelley)
        $ mconcat [Opt.long "shelley-era", Opt.help "Specify the Shelley era"]
      , Opt.flag' (EraInEon ShelleyBasedEraAllegra)
        $ mconcat [Opt.long "allegra-era", Opt.help "Specify the Allegra era"]
      , Opt.flag' (EraInEon ShelleyBasedEraMary)
        $ mconcat [Opt.long "mary-era", Opt.help "Specify the Mary era"]
      , Opt.flag' (EraInEon ShelleyBasedEraAlonzo)
        $ mconcat [Opt.long "alonzo-era", Opt.help "Specify the Alonzo era"]
      , Opt.flag' (EraInEon ShelleyBasedEraBabbage)
        $ mconcat [Opt.long "babbage-era", Opt.help "Specify the Babbage era (default)"]
      , Opt.flag' (EraInEon ShelleyBasedEraConway)
        $ mconcat [Opt.long "conway-era", Opt.help "Specify the Conway era"]
      ]
    , maybeToList $ pure <$> envCliAnyShelleyBasedEra envCli
    , pure $ pure defaultShelleyBasedEra
  ]

pAnyShelleyToBabbageEra :: EnvCli -> Parser (EraInEon ShelleyToBabbageEra)
pAnyShelleyToBabbageEra envCli =
  asum $ mconcat
    [ [ Opt.flag' (EraInEon ShelleyToBabbageEraShelley)
        $ mconcat [Opt.long "shelley-era", Opt.help "Specify the Shelley era"]
      , Opt.flag' (EraInEon ShelleyToBabbageEraAllegra)
        $ mconcat [Opt.long "allegra-era", Opt.help "Specify the Allegra era"]
      , Opt.flag' (EraInEon ShelleyToBabbageEraMary)
        $ mconcat [Opt.long "mary-era", Opt.help "Specify the Mary era"]
      , Opt.flag' (EraInEon ShelleyToBabbageEraAlonzo)
        $ mconcat [Opt.long "alonzo-era", Opt.help "Specify the Alonzo era"]
      , Opt.flag' (EraInEon ShelleyToBabbageEraBabbage)
        $ mconcat [Opt.long "babbage-era", Opt.help "Specify the Babbage era (default)"]
      ]
    , maybeToList $ pure <$> envCliAnyShelleyToBabbageEra envCli
    , pure $ pure defaultShelleyToBabbageEra
  ]

pShelleyBasedShelley :: EnvCli -> Parser (EraInEon ShelleyBasedEra)
pShelleyBasedShelley envCli =
  asum $ mconcat
    [ [ Opt.flag' (EraInEon ShelleyBasedEraShelley)
          $ mconcat [Opt.long "shelley-era", Opt.help "Specify the Shelley era"]
      ]
    , maybeToList
        $ fmap pure
        $ mfilter (== EraInEon ShelleyBasedEraShelley)
        $ envCliAnyShelleyBasedEra envCli
    ]

pShelleyBasedAllegra :: EnvCli -> Parser (EraInEon ShelleyBasedEra)
pShelleyBasedAllegra envCli =
  asum $ mconcat
    [ [ Opt.flag' (EraInEon ShelleyBasedEraAllegra)
          $ mconcat [Opt.long "allegra-era", Opt.help "Specify the Allegra era"]
      ]
    , maybeToList
        $ fmap pure
        $ mfilter (== EraInEon ShelleyBasedEraAllegra)
        $ envCliAnyShelleyBasedEra envCli
    ]

pShelleyBasedMary :: EnvCli -> Parser (EraInEon ShelleyBasedEra)
pShelleyBasedMary envCli =
  asum $ mconcat
    [ [ Opt.flag' (EraInEon ShelleyBasedEraMary)
          $ mconcat [Opt.long "mary-era", Opt.help "Specify the Mary era"]
      ]
    , maybeToList
        $ fmap pure
        $ mfilter (== EraInEon ShelleyBasedEraMary)
        $ envCliAnyShelleyBasedEra envCli
    ]

pShelleyBasedAlonzo :: EnvCli -> Parser (EraInEon ShelleyBasedEra)
pShelleyBasedAlonzo envCli =
  asum $ mconcat
    [ [ Opt.flag' (EraInEon ShelleyBasedEraAlonzo)
          $ mconcat [Opt.long "alonzo-era", Opt.help "Specify the Alonzo era"]
      ]
    , maybeToList
        $ fmap pure
        $ mfilter (== EraInEon ShelleyBasedEraAlonzo)
        $ envCliAnyShelleyBasedEra envCli
    ]

pShelleyBasedBabbage :: EnvCli -> Parser (EraInEon ShelleyBasedEra)
pShelleyBasedBabbage envCli =
  asum $ mconcat
    [ [ Opt.flag' (EraInEon ShelleyBasedEraBabbage)
          $ mconcat [Opt.long "babbage-era", Opt.help "Specify the Babbage era (default)"]
      ]
    , maybeToList
        $ fmap pure
        $ mfilter (== EraInEon ShelleyBasedEraBabbage)
        $ envCliAnyShelleyBasedEra envCli
    ]

pShelleyBasedConway :: EnvCli -> Parser (EraInEon ShelleyBasedEra)
pShelleyBasedConway envCli =
  asum $ mconcat
    [ [ Opt.flag' (EraInEon ShelleyBasedEraConway)
          $ mconcat [Opt.long "conway-era", Opt.help "Specify the Conway era"]
      ]
    , maybeToList
        $ fmap pure
        $ mfilter (== EraInEon ShelleyBasedEraConway)
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

-- | The first argument is the optional prefix.
pStakePoolVerificationKeyOrFile :: Maybe String -> Parser (VerificationKeyOrFile StakePoolKey)
pStakePoolVerificationKeyOrFile prefix =
  asum
    [ VerificationKeyValue <$> pStakePoolVerificationKey prefix
    , VerificationKeyFilePath <$> pStakePoolVerificationKeyFile prefix
    ]

-- | The first argument is the optional prefix.
pStakePoolVerificationKey :: Maybe String -> Parser (VerificationKey StakePoolKey)
pStakePoolVerificationKey prefix =
  Opt.option (readVerificationKey AsStakePoolKey) $ mconcat
    [ Opt.long $ prefixFlag prefix "stake-pool-verification-key"
    , Opt.metavar "STRING"
    , Opt.help "Stake pool verification key (Bech32 or hex-encoded)."
    ]

-- | The first argument is the optional prefix.
pStakePoolVerificationKeyFile :: Maybe String -> Parser (VerificationKeyFile In)
pStakePoolVerificationKeyFile prefix =
  File <$> asum
    [ Opt.strOption $ mconcat
      [ Opt.long $ prefixFlag prefix "cold-verification-key-file"
      , Opt.metavar "FILE"
      , Opt.help "Filepath of the stake pool verification key."
      , Opt.completer (Opt.bashCompleter "file")
      ]
    , Opt.strOption $ mconcat
      [ Opt.long $ prefixFlag prefix "stake-pool-verification-key-file"
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

pMIRPot :: Parser Shelley.MIRPot
pMIRPot =
  asum
    [ Opt.flag' Shelley.ReservesMIR $ mconcat
        [ Opt.long "reserves"
        , Opt.help "Use the reserves pot."
        ]
    , Opt.flag' Shelley.TreasuryMIR $ mconcat
        [ Opt.long "treasury"
        , Opt.help "Use the treasury pot."
        ]
    ]

pRewardAmt :: Parser Lovelace
pRewardAmt =
  Opt.option (readerFromParsecParser parseLovelace) $ mconcat
    [ Opt.long "reward"
    , Opt.metavar "LOVELACE"
    , Opt.help "The reward for the relevant reward account."
    ]

pTransferAmt :: Parser Lovelace
pTransferAmt =
  Opt.option (readerFromParsecParser parseLovelace) $ mconcat
    [ Opt.long "transfer"
    , Opt.metavar "LOVELACE"
    , Opt.help "The amount to transfer."
    ]

pHexHash
  :: SerialiseAsRawBytes (Hash a) => AsType a -> ReadM (Hash a)
pHexHash a =
  Opt.eitherReader $
    first displayError
      . deserialiseFromRawBytesHex (AsHash a)
      . BSC.pack

pBech32KeyHash :: SerialiseAsBech32 (Hash a) => AsType a -> ReadM (Hash a)
pBech32KeyHash a =
  Opt.eitherReader $
    first displayError
    . deserialiseFromBech32 (AsHash a)
    . Text.pack

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

-- | The first argument is the optional prefix.
pColdVerificationKeyOrFile :: Maybe String -> Parser ColdVerificationKeyOrFile
pColdVerificationKeyOrFile prefix =
  asum
    [ ColdStakePoolVerificationKey <$> pStakePoolVerificationKey prefix
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

-- TODO CIP-1694 parameterise this by signing key role
pColdSigningKeyFile :: Parser (File (SigningKey keyrole) direction)
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

-- TODO CIP-1694 parameterise this by verification key role
pVerificationKeyFileOut :: Parser (File (VerificationKey keyrole) Out)
pVerificationKeyFileOut =
  fmap File $ Opt.strOption $ mconcat
    [ Opt.long "verification-key-file"
    , Opt.metavar "FILE"
    , Opt.help "Output filepath of the verification key."
    , Opt.completer (Opt.bashCompleter "file")
    ]

pSigningKeyFileOut :: Parser (File (SigningKey keyrole) Out)
pSigningKeyFileOut =
  fmap File $ Opt.strOption $ mconcat
    [ Opt.long "signing-key-file"
    , Opt.metavar "FILE"
    , Opt.help "Output filepath of the signing key."
    , Opt.completer (Opt.bashCompleter "file")
    ]

pOperatorCertIssueCounterFile :: Parser (File OpCertCounter direction)
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

---

pAddCommitteeColdVerificationKeyOrHashOrFile :: Parser (VerificationKeyOrHashOrFile CommitteeColdKey)
pAddCommitteeColdVerificationKeyOrHashOrFile =
  asum
    [ VerificationKeyOrFile <$> pAddCommitteeColdVerificationKeyOrFile
    , VerificationKeyHash <$> pAddCommitteeColdVerificationKeyHash
    ]

pAddCommitteeColdVerificationKeyHash :: Parser (Hash CommitteeColdKey)
pAddCommitteeColdVerificationKeyHash =
  Opt.option (Opt.eitherReader deserialiseFromHex) $ mconcat
    [ Opt.long "add-cc-cold-verification-key-hash"
    , Opt.metavar "STRING"
    , Opt.help "Constitutional Committee key hash (hex-encoded)."
    ]
  where
    deserialiseFromHex :: String -> Either String (Hash CommitteeColdKey)
    deserialiseFromHex =
      first (\e -> "Invalid Consitutional Committee cold key hash: " ++ displayError e)
        . deserialiseFromRawBytesHex (AsHash AsCommitteeColdKey)
        . BSC.pack

pAddCommitteeColdVerificationKeyOrFile :: Parser (VerificationKeyOrFile CommitteeColdKey)
pAddCommitteeColdVerificationKeyOrFile =
  asum
    [ VerificationKeyValue <$> pAddCommitteeColdVerificationKey
    , VerificationKeyFilePath <$> pAddCommitteeColdVerificationKeyFile
    ]

pAddCommitteeColdVerificationKey :: Parser (VerificationKey CommitteeColdKey)
pAddCommitteeColdVerificationKey =
  Opt.option (Opt.eitherReader deserialiseFromHex) $ mconcat
    [ Opt.long "add-cc-cold-verification-key"
    , Opt.metavar "STRING"
    , Opt.help "Constitutional Committee cold key (hex-encoded)."
    ]
  where
    deserialiseFromHex :: String -> Either String (VerificationKey CommitteeColdKey)
    deserialiseFromHex =
      first (\e -> "Invalid Constitutional Committee cold key: " ++ displayError e)
        . deserialiseFromRawBytesHex (AsVerificationKey AsCommitteeColdKey)
        . BSC.pack

pAddCommitteeColdVerificationKeyFile :: Parser (File (VerificationKey keyrole) In)
pAddCommitteeColdVerificationKeyFile =
  fmap File $ Opt.strOption $ mconcat
    [ Opt.long "add-cc-cold-verification-key-file"
    , Opt.metavar "FILE"
    , Opt.help "Filepath of the Consitutional Committee cold key."
    , Opt.completer (Opt.bashCompleter "file")
    ]

---
pRemoveCommitteeColdVerificationKeyOrHashOrFile :: Parser (VerificationKeyOrHashOrFile CommitteeColdKey)
pRemoveCommitteeColdVerificationKeyOrHashOrFile =
  asum
    [ VerificationKeyOrFile <$> pRemoveCommitteeColdVerificationKeyOrFile
    , VerificationKeyHash <$> pRemoveCommitteeColdVerificationKeyHash
    ]

pRemoveCommitteeColdVerificationKeyHash :: Parser (Hash CommitteeColdKey)
pRemoveCommitteeColdVerificationKeyHash =
  Opt.option (Opt.eitherReader deserialiseFromHex) $ mconcat
    [ Opt.long "remove-cc-cold-verification-key-hash"
    , Opt.metavar "STRING"
    , Opt.help "Constitutional Committee key hash (hex-encoded)."
    ]
  where
    deserialiseFromHex :: String -> Either String (Hash CommitteeColdKey)
    deserialiseFromHex =
      first (\e -> "Invalid Consitutional Committee cold key hash: " ++ displayError e)
        . deserialiseFromRawBytesHex (AsHash AsCommitteeColdKey)
        . BSC.pack

pRemoveCommitteeColdVerificationKeyOrFile :: Parser (VerificationKeyOrFile CommitteeColdKey)
pRemoveCommitteeColdVerificationKeyOrFile =
  asum
    [ VerificationKeyValue <$> pRemoveCommitteeColdVerificationKey
    , VerificationKeyFilePath <$> pRemoveCommitteeColdVerificationKeyFile
    ]

pRemoveCommitteeColdVerificationKey :: Parser (VerificationKey CommitteeColdKey)
pRemoveCommitteeColdVerificationKey =
  Opt.option (Opt.eitherReader deserialiseFromHex) $ mconcat
    [ Opt.long "remove-cc-cold-verification-key"
    , Opt.metavar "STRING"
    , Opt.help "Constitutional Committee cold key (hex-encoded)."
    ]
  where
    deserialiseFromHex :: String -> Either String (VerificationKey CommitteeColdKey)
    deserialiseFromHex =
      first (\e -> "Invalid Constitutional Committee cold key: " ++ displayError e)
        . deserialiseFromRawBytesHex (AsVerificationKey AsCommitteeColdKey)
        . BSC.pack

pRemoveCommitteeColdVerificationKeyFile :: Parser (File (VerificationKey keyrole) In)
pRemoveCommitteeColdVerificationKeyFile =
  fmap File $ Opt.strOption $ mconcat
    [ Opt.long "remove-cc-cold-verification-key-file"
    , Opt.metavar "FILE"
    , Opt.help "Filepath of the Consitutional Committee cold key."
    , Opt.completer (Opt.bashCompleter "file")
    ]

---

pCommitteeColdVerificationKeyOrHashOrFile :: Parser (VerificationKeyOrHashOrFile CommitteeColdKey)
pCommitteeColdVerificationKeyOrHashOrFile =
  asum
    [ VerificationKeyOrFile <$> pCommitteeColdVerificationKeyOrFile
    , VerificationKeyHash <$> pCommitteeColdVerificationKeyHash
    ]

pCommitteeColdVerificationKeyOrFile :: Parser (VerificationKeyOrFile CommitteeColdKey)
pCommitteeColdVerificationKeyOrFile =
  asum
    [ VerificationKeyValue <$> pCommitteeColdVerificationKey
    , VerificationKeyFilePath <$> pCommitteeColdVerificationKeyFile
    ]

pCommitteeColdVerificationKey :: Parser (VerificationKey CommitteeColdKey)
pCommitteeColdVerificationKey =
  Opt.option (Opt.eitherReader deserialiseFromHex) $ mconcat
    [ Opt.long "cold-verification-key"
    , Opt.metavar "STRING"
    , Opt.help "Constitutional Committee cold key (hex-encoded)."
    ]
  where
    deserialiseFromHex :: String -> Either String (VerificationKey CommitteeColdKey)
    deserialiseFromHex =
      first (\e -> "Invalid Constitutional Committee cold key: " ++ displayError e)
        . deserialiseFromRawBytesHex (AsVerificationKey AsCommitteeColdKey)
        . BSC.pack

pCommitteeColdVerificationKeyHash :: Parser (Hash CommitteeColdKey)
pCommitteeColdVerificationKeyHash =
  Opt.option (Opt.eitherReader deserialiseFromHex) $ mconcat
    [ Opt.long "cold-verification-key-hash"
    , Opt.metavar "STRING"
    , Opt.help "Constitutional Committee key hash (hex-encoded)."
    ]
  where
    deserialiseFromHex :: String -> Either String (Hash CommitteeColdKey)
    deserialiseFromHex =
      first (\e -> "Invalid Consitutional Committee cold key hash: " ++ displayError e)
        . deserialiseFromRawBytesHex (AsHash AsCommitteeColdKey)
        . BSC.pack

pCommitteeColdVerificationKeyFile :: Parser (File (VerificationKey keyrole) In)
pCommitteeColdVerificationKeyFile =
  fmap File $ Opt.strOption $ mconcat
    [ Opt.long "cold-verification-key-file"
    , Opt.metavar "FILE"
    , Opt.help "Filepath of the Consitutional Committee cold key."
    , Opt.completer (Opt.bashCompleter "file")
    ]

pVerificationKeyFileIn :: Parser (VerificationKeyFile In)
pVerificationKeyFileIn =
  fmap File $ Opt.strOption $ mconcat
    [ Opt.long "verification-key-file"
    , Opt.metavar "FILE"
    , Opt.help "Input filepath of the verification key."
    , Opt.completer (Opt.bashCompleter "file")
    ]

pAnyVerificationKeyFileIn :: String -> Parser (VerificationKeyFile In)
pAnyVerificationKeyFileIn helpText =
  fmap File $ Opt.strOption $ mconcat
    [ Opt.long "verification-key-file"
    , Opt.metavar "FILE"
    , Opt.help $ "Input filepath of the " <> helpText <> "."
    , Opt.completer (Opt.bashCompleter "file")
    ]

pAnyVerificationKeyText :: String -> Parser AnyVerificationKeyText
pAnyVerificationKeyText helpText =
  fmap (AnyVerificationKeyText . Text.pack) $ Opt.strOption $ mconcat
    [ Opt.long "verification-key"
    , Opt.metavar "STRING"
    , Opt.help $ helpText <> " (Bech32-encoded)"
    ]

pAnyVerificationKeySource :: String -> Parser AnyVerificationKeySource
pAnyVerificationKeySource helpText =
  asum
    [ AnyVerificationKeySourceOfText <$> pAnyVerificationKeyText helpText
    , AnyVerificationKeySourceOfFile <$> pAnyVerificationKeyFileIn helpText
    ]

pCommitteeHotKey :: Parser (VerificationKey CommitteeHotKey)
pCommitteeHotKey =
  Opt.option (Opt.eitherReader deserialiseHotCCKeyFromHex) $ mconcat
    [ Opt.long "hot-key"
    , Opt.metavar "STRING"
    , Opt.help "Constitutional Committee hot key (hex-encoded)."
    ]

pCommitteeHotVerificationKey :: Parser (VerificationKey CommitteeHotKey)
pCommitteeHotVerificationKey =
  Opt.option (Opt.eitherReader deserialiseHotCCKeyFromHex) $ mconcat
    [ Opt.long "cc-hot-verification-key"
    , Opt.metavar "STRING"
    , Opt.help "Constitutional Committee hot key (hex-encoded)."
    ]

deserialiseHotCCKeyFromHex :: String -> Either String (VerificationKey CommitteeHotKey)
deserialiseHotCCKeyFromHex =
  first (\e -> "Invalid Constitutional Committee hot key: " ++ displayError e)
    . deserialiseFromRawBytesHex (AsVerificationKey AsCommitteeHotKey)
    . BSC.pack

pCommitteeHotKeyFile :: Parser (VerificationKeyFile In)
pCommitteeHotKeyFile =
  fmap File $ Opt.strOption $ mconcat
    [ Opt.long "hot-key-file"
    , Opt.metavar "FILE"
    , Opt.help "Filepath of the Consitutional Committee hot key."
    , Opt.completer (Opt.bashCompleter "file")
    ]

pCommitteeHotVerificationKeyFile :: Parser (VerificationKeyFile In)
pCommitteeHotVerificationKeyFile =
  fmap File $ Opt.strOption $ mconcat
    [ Opt.long "cc-hot-verification-key-file"
    , Opt.metavar "FILE"
    , Opt.help "Filepath of the Consitutional Committee hot key."
    , Opt.completer (Opt.bashCompleter "file")
    ]

-- | The first argument is the optional prefix.
pCommitteeHotKeyHash :: Maybe String -> Parser (Hash CommitteeHotKey)
pCommitteeHotKeyHash prefix =
  Opt.option (Opt.eitherReader deserialiseFromHex) $ mconcat
    [ Opt.long $ prefixFlag prefix "hot-key-hash"
    , Opt.metavar "STRING"
    , Opt.help "Constitutional Committee key hash (hex-encoded)."
    ]
  where
    deserialiseFromHex :: String -> Either String (Hash CommitteeHotKey)
    deserialiseFromHex =
      first (\e -> "Invalid Consitutional Committee hot key hash: " ++ displayError e)
        . deserialiseFromRawBytesHex (AsHash AsCommitteeHotKey)
        . BSC.pack

pCommitteeHotKeyOrFile :: Parser (VerificationKeyOrFile CommitteeHotKey)
pCommitteeHotKeyOrFile =
  asum
    [ VerificationKeyValue <$> pCommitteeHotKey
    , VerificationKeyFilePath <$> pCommitteeHotKeyFile
    ]

pCommitteeHotKeyOrHashOrFile :: Parser (VerificationKeyOrHashOrFile CommitteeHotKey)
pCommitteeHotKeyOrHashOrFile =
  asum
    [ VerificationKeyOrFile <$> pCommitteeHotKeyOrFile
    , VerificationKeyHash <$> pCommitteeHotKeyHash Nothing
    ]

pCommitteeHotVerificationKeyOrHashOrVerificationFile :: Parser (VerificationKeyOrHashOrFile CommitteeHotKey)
pCommitteeHotVerificationKeyOrHashOrVerificationFile =
  asum
    [ VerificationKeyOrFile . VerificationKeyValue <$> pCommitteeHotVerificationKey,
      VerificationKeyOrFile . VerificationKeyFilePath <$> pCommitteeHotVerificationKeyFile,
      VerificationKeyHash <$> pCommitteeHotKeyHash (Just "cc")
    ]

catCommands :: [Parser a] -> Maybe (Parser a)
catCommands = \case
  [] -> Nothing
  ps -> Just $ asum ps

pConstitutionUrl :: Parser ConstitutionUrl
pConstitutionUrl =
  ConstitutionUrl
    <$> pUrl "constitution-anchor-url" "Constitution anchor URL."

pConstitutionHashSource :: Parser ConstitutionHashSource
pConstitutionHashSource =
  asum
    [ ConstitutionHashSourceText
        <$> Opt.strOption
            ( mconcat
                [ Opt.long "constitution-anchor-metadata"
                , Opt.metavar "TEXT"
                , Opt.help "Constitution anchor contents as UTF-8 encoded text."
                ]
            )
    , ConstitutionHashSourceFile
        <$> pFileInDirection "constitution-anchor-metadata-file" "Constitution anchor contents as a text file."
    , ConstitutionHashSourceHash
        <$> pConstitutionHash
    ]

pConstitutionHash :: Parser (L.SafeHash Crypto.StandardCrypto L.AnchorData)
pConstitutionHash =
  Opt.option readSafeHash $ mconcat
    [ Opt.long "constitution-anchor-metadata-hash"
    , Opt.metavar "HASH"
    , Opt.help "Hash of the constitution anchor data."
    ]

pUrl :: String -> String -> Parser Ledger.Url
pUrl l h = fromMaybe (error "Url longer than 64 bytes")
         . Ledger.textToUrl <$>
             Opt.strOption (mconcat
                            [ Opt.long l
                            , Opt.metavar "TEXT"
                            , Opt.help h
                            ])

pGovActionDeposit :: Parser Lovelace
pGovActionDeposit =
  Opt.option (readerFromParsecParser parseLovelace) $ mconcat
    [ Opt.long "governance-action-deposit"
    , Opt.metavar "NATURAL"
    , Opt.help "Deposit required to submit a governance action."
    ]


-- | First argument is the optional prefix
pStakeVerificationKeyOrHashOrFile :: Maybe String -> Parser (VerificationKeyOrHashOrFile StakeKey)
pStakeVerificationKeyOrHashOrFile prefix = asum
  [ VerificationKeyOrFile <$> pStakeVerificationKeyOrFile prefix
  , VerificationKeyHash <$> pStakeVerificationKeyHash prefix
  ]

-- | First argument is the optional prefix
pStakeVerificationKeyHash :: Maybe String -> Parser (Hash StakeKey)
pStakeVerificationKeyHash prefix =
   Opt.option (pHexHash AsStakeKey) $ mconcat
      [ Opt.long $ prefixFlag prefix "stake-key-hash"
      , Opt.metavar "HASH"
      , Opt.help "Stake verification key hash (hex-encoded)."
      ]


-- | The first argument is the optional prefix.
pStakePoolVerificationKeyOrHashOrFile
  :: Maybe String -> Parser (VerificationKeyOrHashOrFile StakePoolKey)
pStakePoolVerificationKeyOrHashOrFile prefix =
  asum
    [ VerificationKeyOrFile <$> pStakePoolVerificationKeyOrFile prefix
    , VerificationKeyHash <$> pStakePoolVerificationKeyHash prefix
    ]

pCombinedStakePoolVerificationKeyOrHashOrFile
  :: Parser (VerificationKeyOrHashOrFile StakePoolKey)
pCombinedStakePoolVerificationKeyOrHashOrFile =
  asum
    [ VerificationKeyOrFile <$> pCombinedStakePoolVerificationKeyOrFile
    , VerificationKeyHash <$> pCombinedStakePoolVerificationKeyHash
    ]

pCombinedStakePoolVerificationKeyOrFile :: Parser (VerificationKeyOrFile StakePoolKey)
pCombinedStakePoolVerificationKeyOrFile =
  asum
    [ VerificationKeyValue <$> pCombinedStakePoolVerificationKey
    , VerificationKeyFilePath <$> pCombinedStakePoolVerificationKeyFile
    ]

pCombinedStakePoolVerificationKeyHash :: Parser (Hash StakePoolKey)
pCombinedStakePoolVerificationKeyHash =
    Opt.option (pBech32KeyHash AsStakePoolKey <|> pHexHash AsStakePoolKey) $ mconcat
      [ Opt.long "combined-stake-pool-id"
      , Opt.metavar "STAKE_POOL_ID"
      , Opt.help $ mconcat
          [ "Stake pool ID/verification key hash (either Bech32-encoded or hex-encoded).  "
          , "Zero or more occurences of this option is allowed."
          ]
      ]

pCombinedStakePoolVerificationKey :: Parser (VerificationKey StakePoolKey)
pCombinedStakePoolVerificationKey =
  Opt.option (readVerificationKey AsStakePoolKey) $ mconcat
    [ Opt.long "combined-stake-pool-verification-key"
    , Opt.metavar "STRING"
    , Opt.help "Stake pool verification key (Bech32 or hex-encoded)."
    ]

pCombinedStakePoolVerificationKeyFile :: Parser (VerificationKeyFile In)
pCombinedStakePoolVerificationKeyFile =
  File <$> asum
    [ Opt.strOption $ mconcat
      [ Opt.long "combined-cold-verification-key-file"
      , Opt.metavar "FILE"
      , Opt.help "Filepath of the stake pool verification key."
      , Opt.completer (Opt.bashCompleter "file")
      ]
    , Opt.strOption $ mconcat
      [ Opt.long "stake-pool-verification-key-file"
      , Opt.internal
      ]
    ]

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------

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

pEpochNo :: String -> Parser EpochNo
pEpochNo h =
  fmap EpochNo $ Opt.option (bounded "EPOCH") $ mconcat
    [ Opt.long "epoch"
    , Opt.metavar "NATURAL"
    , Opt.help h
    ]


pEpochNoUpdateProp :: Parser EpochNo
pEpochNoUpdateProp = pEpochNo "The epoch number in which the update proposal is valid."

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

pPoolIdOutputFormat :: Parser IdOutputFormat
pPoolIdOutputFormat =
  Opt.option readIdOutputFormat $ mconcat
    [ Opt.long "output-format"
    , Opt.metavar "STRING"
    , Opt.help $ mconcat
      [ "Optional pool id output format. Accepted output formats are \"hex\" "
      , "and \"bech32\" (default is \"bech32\")."
      ]
    , Opt.value IdOutputFormatBech32
    ]

pTxViewOutputFormat :: Parser TxViewOutputFormat
pTxViewOutputFormat =
  Opt.option readTxViewOutputFormat $ mconcat
    [ Opt.long "output-format"
    , Opt.metavar "STRING"
    , Opt.help $ mconcat
      [ "Optional transaction view output format. Accepted output formats are \"json\" "
      , "and \"yaml\" (default is \"json\")."
      ]
    , Opt.value TxViewOutputFormatJson
    ]

pGovernanceActionViewOutputFormat :: Parser GovernanceActionViewOutputFormat
pGovernanceActionViewOutputFormat =
  Opt.option readGovernanceActionViewOutputFormat $ mconcat
    [ Opt.long "output-format"
    , Opt.metavar "STRING"
    , Opt.help $ mconcat
      [ "Optional governance action view output format. Accepted output formats are \"json\" "
      , "and \"yaml\" (default is \"json\")."
      ]
    , Opt.value GovernanceActionViewOutputFormatJson
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

pVerificationKeyOrFileIn
  :: SerialiseAsBech32 (VerificationKey keyrole)
  => AsType keyrole
  -> Parser (VerificationKeyOrFile keyrole)
pVerificationKeyOrFileIn asType =
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

pLegacyInvalidHereafter :: Parser SlotNo
pLegacyInvalidHereafter =
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

pInvalidHereafter :: ()
  => CardanoEra era
  -> Parser (TxValidityUpperBound era)
pInvalidHereafter =
  caseByronOrShelleyBasedEra
    (pure . TxValidityNoUpperBound)
    (\eon ->
      fmap (TxValidityUpperBound eon) $ asum
        [ fmap (Just . SlotNo) $ Opt.option (bounded "SLOT") $ mconcat
          [ Opt.long "invalid-hereafter"
          , Opt.metavar "SLOT"
          , Opt.help "Time that transaction is valid until (in slots)."
          ]
        , fmap (Just . SlotNo) $ Opt.option (bounded "SLOT") $ mconcat
          [ Opt.long "upper-bound"
          , Opt.metavar "SLOT"
          , Opt.help $ mconcat
            [ "Time that transaction is valid until (in slots) "
            , "(deprecated; use --invalid-hereafter instead)."
            ]
          , Opt.internal
          ]
        , fmap (Just . SlotNo) $ Opt.option (bounded "SLOT") $ mconcat
          [ Opt.long "ttl"
          , Opt.metavar "SLOT"
          , Opt.help "Time to live (in slots) (deprecated; use --invalid-hereafter instead)."
          , Opt.internal
          ]
        , pure Nothing
        ]
    )

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

-- | First argument is the prefix to use
pStakePoolVerificationKeyHash :: Maybe String -> Parser (Hash StakePoolKey)
pStakePoolVerificationKeyHash prefix =
    Opt.option (pBech32KeyHash AsStakePoolKey <|> pHexHash AsStakePoolKey) $ mconcat
      [ Opt.long $ prefixFlag prefix "stake-pool-id"
      , Opt.metavar "STAKE_POOL_ID"
      , Opt.help $ mconcat
          [ "Stake pool ID/verification key hash (either Bech32-encoded or hex-encoded).  "
          , "Zero or more occurences of this option is allowed."
          ]
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

pRational :: String -> String -> Parser Rational
pRational opt h =
  Opt.option readRationalUnitInterval $ mconcat
    [ Opt.long opt
    , Opt.metavar "RATIONAL"
    , Opt.help h
    ]

pPoolMargin :: Parser Rational
pPoolMargin = pRational "pool-margin" "The stake pool's margin."


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

pStakePoolRegistrationParserRequirements
  :: EnvCli -> Parser StakePoolRegistrationParserRequirements
pStakePoolRegistrationParserRequirements envCli =
  StakePoolRegistrationParserRequirements
    <$> pStakePoolVerificationKeyOrFile Nothing
    <*> pVrfVerificationKeyOrFile
    <*> pPoolPledge
    <*> pPoolCost
    <*> pPoolMargin
    <*> pRewardAcctVerificationKeyOrFile
    <*> some pPoolOwnerVerificationKeyOrFile
    <*> many pPoolRelay
    <*> pStakePoolMetadataReference
    <*> pNetworkId envCli

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

pDrepDeposit :: Parser Lovelace
pDrepDeposit =
  Opt.option (readerFromParsecParser parseLovelace) $ mconcat
   [ Opt.long "deposit-amt"
   , Opt.metavar "LOVELACE"
   , Opt.help "DRep deposit amount (same at registration and retirement)."
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

pPoolVotingThresholds :: Parser Ledger.PoolVotingThresholds
pPoolVotingThresholds =
    Ledger.PoolVotingThresholds
      <$> pMotionNoConfidence
      <*> pCommitteeNormal
      <*> pCommitteeNoConfidence
      <*> pHardForkInitiation
  where
    pMotionNoConfidence =
      Opt.option (toUnitIntervalOrErr <$> readRationalUnitInterval) $ mconcat
        [ Opt.long "pool-voting-threshold-motion-no-confidence"
        , Opt.metavar "RATIONAL"
        , Opt.help "TODO"
        ]
    pCommitteeNormal =
      Opt.option (toUnitIntervalOrErr <$> readRationalUnitInterval) $ mconcat
        [ Opt.long "pool-voting-threshold-committee-normal"
        , Opt.metavar "RATIONAL"
        , Opt.help "TODO"
        ]
    pCommitteeNoConfidence =
      Opt.option (toUnitIntervalOrErr <$> readRationalUnitInterval) $ mconcat
        [ Opt.long "pool-voting-threshold-committee-no-confidence"
        , Opt.metavar "RATIONAL"
        , Opt.help "TODO"
        ]
    pHardForkInitiation =
      Opt.option (toUnitIntervalOrErr <$> readRationalUnitInterval) $ mconcat
        [ Opt.long "pool-voting-threshold-hard-fork-initiation"
        , Opt.metavar "RATIONAL"
        , Opt.help "TODO"
        ]

pDRepVotingThresholds :: Parser Ledger.DRepVotingThresholds
pDRepVotingThresholds =
    Ledger.DRepVotingThresholds
      <$> pMotionNoConfidence
      <*> pCommitteeNormal
      <*> pCommitteeNoConfidence
      <*> pUpdateToConstitution
      <*> pHardForkInitiation
      <*> pPPNetworkGroup
      <*> pPPEconomicGroup
      <*> pPPTechnicalGroup
      <*> pPPGovGroup
      <*> pTreasuryWithdrawal
  where
    pMotionNoConfidence =
      Opt.option (toUnitIntervalOrErr <$> readRationalUnitInterval) $ mconcat
        [ Opt.long "drep-voting-threshold-motion-no-confidence"
        , Opt.metavar "RATIONAL"
        , Opt.help "TODO"
        ]
    pCommitteeNormal =
      Opt.option (toUnitIntervalOrErr <$> readRationalUnitInterval) $ mconcat
        [ Opt.long "drep-voting-threshold-committee-normal"
        , Opt.metavar "RATIONAL"
        , Opt.help "TODO"
        ]
    pCommitteeNoConfidence =
      Opt.option (toUnitIntervalOrErr <$> readRationalUnitInterval) $ mconcat
        [ Opt.long "drep-voting-threshold-committee-no-confidence"
        , Opt.metavar "RATIONAL"
        , Opt.help "TODO"
        ]
    pUpdateToConstitution =
      Opt.option (toUnitIntervalOrErr <$> readRationalUnitInterval) $ mconcat
        [ Opt.long "drep-voting-threshold-update-to-constitution"
        , Opt.metavar "RATIONAL"
        , Opt.help "TODO"
        ]
    pHardForkInitiation =
      Opt.option (toUnitIntervalOrErr <$> readRationalUnitInterval) $ mconcat
        [ Opt.long "drep-voting-threshold-hard-fork-initiation"
        , Opt.metavar "RATIONAL"
        , Opt.help "TODO"
        ]
    pPPNetworkGroup =
      Opt.option (toUnitIntervalOrErr <$> readRationalUnitInterval) $ mconcat
        [ Opt.long "drep-voting-threshold-pp-network-group"
        , Opt.metavar "RATIONAL"
        , Opt.help "TODO"
        ]
    pPPEconomicGroup =
      Opt.option (toUnitIntervalOrErr <$> readRationalUnitInterval) $ mconcat
        [ Opt.long "drep-voting-threshold-pp-economic-group"
        , Opt.metavar "RATIONAL"
        , Opt.help "TODO"
        ]
    pPPTechnicalGroup =
      Opt.option (toUnitIntervalOrErr <$> readRationalUnitInterval) $ mconcat
        [ Opt.long "drep-voting-threshold-pp-technical-group"
        , Opt.metavar "RATIONAL"
        , Opt.help "TODO"
        ]
    pPPGovGroup =
      Opt.option (toUnitIntervalOrErr <$> readRationalUnitInterval) $ mconcat
        [ Opt.long "drep-voting-threshold-pp-governance-group"
        , Opt.metavar "RATIONAL"
        , Opt.help "TODO"
        ]
    pTreasuryWithdrawal =
      Opt.option (toUnitIntervalOrErr <$> readRationalUnitInterval) $ mconcat
        [ Opt.long "drep-voting-threshold-treasury-withdrawal"
        , Opt.metavar "RATIONAL"
        , Opt.help "TODO"
        ]

pMinCommitteeSize :: Parser Natural
pMinCommitteeSize =
  Opt.option Opt.auto $ mconcat
    [ Opt.long "min-committee-size"
    , Opt.metavar "INT"
    , Opt.help "TODO"
    ]

pCommitteeTermLength :: Parser Natural
pCommitteeTermLength =
  Opt.option Opt.auto $ mconcat
    [ Opt.long "committee-term-length"
    , Opt.metavar "INT"
    , Opt.help "TODO"
    ]

pGovActionLifetime :: Parser EpochNo
pGovActionLifetime =
  fmap EpochNo $ Opt.option (bounded "EPOCH") $ mconcat
    [ Opt.long "governance-action-lifetime"
    , Opt.metavar "NATURAL"
    , Opt.help "TODO"
    ]

pDRepDeposit :: Parser Lovelace
pDRepDeposit =
  Opt.option (readerFromParsecParser parseLovelace) $ mconcat
    [ Opt.long "drep-deposit"
    , Opt.metavar "LOVELACE"
    , Opt.help "TODO"
    ]

pDRepActivity :: Parser EpochNo
pDRepActivity =
  fmap EpochNo $ Opt.option (bounded "EPOCH") $ mconcat
    [ Opt.long "drep-activity"
    , Opt.metavar "NATURAL"
    , Opt.help "TODO"
    ]

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

pVoteChoice :: Parser Vote
pVoteChoice =
  asum
   [  flag' Yes $ long "yes"
   ,  flag' No $ long "no"
   ,  flag' Abstain $ long "abstain"
   ]

pVoterType :: Parser VType
pVoterType =
  asum
   [  flag' VCC $ mconcat [long "constitutional-committee-member", Opt.help "Member of the constiutional committee"]
   ,  flag' VDR $ mconcat [long "drep", Opt.help "Delegate representative"]
   ,  flag' VSP $ mconcat [long "spo", Opt.help "Stake pool operator"]
   ]

-- TODO: Conway era include "normal" stake keys
pVotingCredential :: Parser (VerificationKeyOrFile StakePoolKey)
pVotingCredential = pStakePoolVerificationKeyOrFile Nothing

pVoteDelegationTarget :: Parser VoteDelegationTarget
pVoteDelegationTarget =
  asum
    [ VoteDelegationTargetOfDRep <$> pDRepHashSource
    , VoteDelegationTargetOfAbstain <$ pAlwaysAbstain
    , VoteDelegationTargetOfNoConfidence <$ pAlwaysNoConfidence
    ]

pAlwaysAbstain :: Parser ()
pAlwaysAbstain =
  Opt.flag' () $ mconcat
    [ Opt.long "always-abstain"
    , Opt.help "Abstain from voting on all proposals."
    ]

pVoteAnchor :: Parser (VoteUrl, VoteHashSource)
pVoteAnchor = (,)
  <$> (VoteUrl <$> pUrl "vote-anchor-url" "Vote anchor URL")
  <*> pVoteHashSource

pVoteHashSource :: Parser VoteHashSource
pVoteHashSource =
  asum
    [ VoteHashSourceText
        <$> Opt.strOption
            ( mconcat
                [ Opt.long "vote-anchor-metadata"
                , Opt.metavar "TEXT"
                , Opt.help "Vote anchor contents as UTF-8 encoded text."
                ]
            )
    , VoteHashSourceFile
        <$> pFileInDirection "vote-anchor-metadata-file" "Vote anchor contents as a text file."
    , VoteHashSourceHash
        <$> pVoteHash
    ]

pVoteHash :: Parser (L.SafeHash Crypto.StandardCrypto L.AnchorData)
pVoteHash =
  Opt.option readSafeHash $ mconcat
    [ Opt.long "vote-anchor-metadata-hash"
    , Opt.metavar "HASH"
    , Opt.help "Hash of the vote anchor data."
    ]

pAlwaysNoConfidence :: Parser ()
pAlwaysNoConfidence =
  Opt.flag' () $ mconcat
    [ Opt.long "always-no-confidence"
    , Opt.help "Always vote no confidence"
    ]

pDRepHashSource :: Parser DRepHashSource
pDRepHashSource =
  asum
    [ DRepHashSourceScript <$> pDRepScriptHash
    , DRepHashSourceVerificationKey <$> pDRepVerificationKeyOrHashOrFile
    ]

pDRepScriptHash :: Parser ScriptHash
pDRepScriptHash =
  Opt.option scriptHashReader $ mconcat
    [ Opt.long "drep-script-hash"
    , Opt.metavar "HASH"
    , Opt.help $ mconcat
        [ "DRep script hash (hex-encoded)."
        ]
    ]

pDRepVerificationKeyOrHashOrFile
  :: Parser (VerificationKeyOrHashOrFile DRepKey)
pDRepVerificationKeyOrHashOrFile =
  asum
    [ VerificationKeyOrFile <$> pDRepVerificationKeyOrFile
    , VerificationKeyHash <$> pDRepVerificationKeyHash
    ]

pCombinedDRepVerificationKeyOrHashOrFile
  :: Parser (VerificationKeyOrHashOrFile DRepKey)
pCombinedDRepVerificationKeyOrHashOrFile =
  asum
    [ VerificationKeyOrFile <$> pCombinedDRepVerificationKeyOrFile
    , VerificationKeyHash <$> pCombinedDRepVerificationKeyHash
    ]

pCombinedDRepVerificationKeyHash :: Parser (Hash DRepKey)
pCombinedDRepVerificationKeyHash =
    Opt.option (pBech32KeyHash AsDRepKey <|> pHexHash AsDRepKey) $ mconcat
      [ Opt.long "combined-drep-key-hash"
      , Opt.metavar "HASH"
      , Opt.help $ mconcat
          [ "DRep verification key hash (either Bech32-encoded or hex-encoded).  "
          , "Zero or more occurences of this option is allowed."
          ]
      ]

pCombinedDRepVerificationKey :: Parser (VerificationKey DRepKey)
pCombinedDRepVerificationKey =
  Opt.option (readVerificationKey AsDRepKey) $ mconcat
    [ Opt.long "combined-drep-verification-key"
    , Opt.metavar "STRING"
    , Opt.help "DRep verification key (Bech32 or hex-encoded)."
    ]

pCombinedDRepVerificationKeyOrFile :: Parser (VerificationKeyOrFile DRepKey)
pCombinedDRepVerificationKeyOrFile =
  asum
    [ VerificationKeyValue <$> pCombinedDRepVerificationKey
    , VerificationKeyFilePath <$> pCombinedDRepVerificationKeyFile
    ]

pCombinedDRepVerificationKeyFile :: Parser (VerificationKeyFile In)
pCombinedDRepVerificationKeyFile =
  fmap File . Opt.strOption $ mconcat
    [ Opt.long "combined-drep-verification-key-file"
    , Opt.metavar "FILE"
    , Opt.help "Filepath of the DRep verification key."
    , Opt.completer (Opt.bashCompleter "file")
    ]

pDRepVerificationKeyHash :: Parser (Hash DRepKey)
pDRepVerificationKeyHash =
    Opt.option (pBech32KeyHash AsDRepKey <|> pHexHash AsDRepKey) $ mconcat
      [ Opt.long "drep-key-hash"
      , Opt.metavar "HASH"
      , Opt.help $ mconcat
          [ "DRep verification key hash (either Bech32-encoded or hex-encoded).  "
          , "Zero or more occurences of this option is allowed."
          ]
      ]

pDRepVerificationKey :: Parser (VerificationKey DRepKey)
pDRepVerificationKey =
  Opt.option (readVerificationKey AsDRepKey) $ mconcat
    [ Opt.long "drep-verification-key"
    , Opt.metavar "STRING"
    , Opt.help "DRep verification key (Bech32 or hex-encoded)."
    ]

pDRepVerificationKeyOrFile :: Parser (VerificationKeyOrFile DRepKey)
pDRepVerificationKeyOrFile =
  asum
    [ VerificationKeyValue <$> pDRepVerificationKey
    , VerificationKeyFilePath <$> pDRepVerificationKeyFile
    ]

pDRepVerificationKeyFile :: Parser (VerificationKeyFile In)
pDRepVerificationKeyFile =
  fmap File . Opt.strOption $ mconcat
    [ Opt.long "drep-verification-key-file"
    , Opt.metavar "FILE"
    , Opt.help "Filepath of the DRep verification key."
    , Opt.completer (Opt.bashCompleter "file")
    ]

pProposalUrl :: Parser ProposalUrl
pProposalUrl =
  ProposalUrl
    <$> pUrl "proposal-anchor-url" "Proposal anchor URL"

pProposalHashSource :: Parser ProposalHashSource
pProposalHashSource =
  asum
    [ ProposalHashSourceText
        <$> Opt.strOption
            ( mconcat
                [ Opt.long "proposal-anchor-metadata"
                , Opt.metavar "TEXT"
                , Opt.help "Proposal anchor contents as UTF-8 encoded text."
                ]
            )
    , ProposalHashSourceFile
        <$> pFileInDirection "proposal-anchor-metadata-file" "Proposal anchor contents as a text file."
    , ProposalHashSourceHash
        <$> pProposalHash
    ]

pProposalHash :: Parser (L.SafeHash Crypto.StandardCrypto L.AnchorData)
pProposalHash =
  Opt.option readSafeHash $ mconcat
    [ Opt.long "proposal-anchor-metadata-hash"
    , Opt.metavar "HASH"
    , Opt.help "Proposal anchor data hash."
    ]

pPreviousGovernanceAction :: Parser (Maybe (TxId, Word32))
pPreviousGovernanceAction = optional $
  (,) <$> pTxId "governance-action-tx-id" "Previous txid of the governance action."
      <*> pWord32 "governance-action-index" "Previous tx's governance action index."

pGovernanceActionId :: Parser (TxId, Word32)
pGovernanceActionId =
  (,) <$> pTxId "governance-action-tx-id" "Txid of the governance action."
      <*> pWord32 "governance-action-index" "Tx's governance action index."

pWord32 :: String -> String -> Parser Word32
pWord32 l h =
  Opt.option auto $ mconcat
    [ Opt.long l
    , Opt.metavar "WORD32"
    , Opt.help h
    ]

pTxId :: String -> String -> Parser TxId
pTxId l h =
  Opt.option (readerFromParsecParser parseTxId) $ mconcat
    [ Opt.long l
    , Opt.metavar "TXID"
    , Opt.help h
    ]


--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

pFeatured :: ()
  => Eon eon
  => ToCardanoEra peon
  => peon era
  -> Parser a
  -> Parser (Maybe (Featured eon era a))
pFeatured peon p = do
  let mw = forEraMaybeEon (toCardanoEra peon)
  case mw of
    Nothing -> pure Nothing
    Just eon' -> Just . Featured eon' <$> p

hiddenSubParser :: String -> ParserInfo a -> Parser a
hiddenSubParser availableCommand pInfo =
  Opt.hsubparser $ Opt.command availableCommand pInfo <> Opt.metavar availableCommand <> Opt.hidden
