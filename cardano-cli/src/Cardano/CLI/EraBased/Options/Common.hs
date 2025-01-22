{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- HLINT ignore "Move brackets to avoid $" -}
{- HLINT ignore "Use <$>" -}
{- HLINT ignore "Move brackets to avoid $" -}

module Cardano.CLI.EraBased.Options.Common where

import           Cardano.Api
import qualified Cardano.Api.Ledger as L
import qualified Cardano.Api.Network as Consensus
import           Cardano.Api.Shelley

import           Cardano.CLI.Environment (EnvCli (..), envCliAnyEon)
import           Cardano.CLI.EraBased.Script.Mint.Types
import           Cardano.CLI.EraBased.Script.Spend.Types (CliSpendScriptRequirements)
import qualified Cardano.CLI.EraBased.Script.Spend.Types as PlutusSpend
import           Cardano.CLI.Parser
import           Cardano.CLI.Read
import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Governance
import           Cardano.CLI.Types.Key
import           Cardano.CLI.Types.Key.VerificationKey

import           Control.Monad (void, when)
import qualified Data.Aeson as Aeson
import           Data.Bifunctor
import           Data.Bits (Bits, toIntegralSized)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BSC
import           Data.Data (Proxy (..), Typeable, typeRep)
import           Data.Foldable
import           Data.Functor (($>))
import qualified Data.IP as IP
import           Data.List.NonEmpty (NonEmpty)
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Time.Clock (UTCTime)
import           Data.Time.Format (defaultTimeLocale, parseTimeOrError)
import           Data.Word
import           GHC.Exts (IsList (..))
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
import qualified Text.Read as Read
import           Text.Read (readEither, readMaybe)

command' :: String -> String -> Parser a -> Mod CommandFields a
command' c descr p =
  mconcat
    [ command c (info (p <**> helper) $ mconcat [progDesc descr])
    , metavar c
    ]

-- | @prefixFlag Nothing bar@ is @bar@, while @prefixFlag (Just "foo") bar@ is @foo-bar@.
-- This function is used to optionally prefix some long flags
prefixFlag :: Maybe String -> String -> String
prefixFlag prefix longFlag =
  case prefix of
    Nothing -> longFlag
    Just prefix' -> prefix' <> "-" <> longFlag

bounded :: forall a. (Bounded a, Integral a, Show a) => String -> Opt.ReadM a
bounded t = Opt.eitherReader $ \s -> do
  i <- Read.readEither @Integer s
  when (i < fromIntegral (minBound @a)) $ Left $ t <> " must not be less than " <> show (minBound @a)
  when (i > fromIntegral (maxBound @a)) $ Left $ t <> " must not greater than " <> show (maxBound @a)
  pure (fromIntegral i)

parseFilePath :: String -> String -> Opt.Parser FilePath
parseFilePath optname desc =
  Opt.strOption
    ( Opt.long optname
        <> Opt.metavar "FILEPATH"
        <> Opt.help desc
        <> Opt.completer (Opt.bashCompleter "file")
    )

pNetworkIdDeprecated :: Parser NetworkId
pNetworkIdDeprecated =
  asum
    [ Opt.flag' Mainnet $
        mconcat
          [ Opt.long "mainnet"
          , Opt.help "DEPRECATED. This argument has no effect."
          ]
    , fmap (Testnet . NetworkMagic) $
        Opt.option (bounded "TESTNET_MAGIC") $
          mconcat
            [ Opt.long "testnet-magic"
            , Opt.metavar "NATURAL"
            , Opt.help "DEPRECATED. This argument has no effect."
            ]
    ]

pNetworkId :: EnvCli -> Parser NetworkId
pNetworkId envCli =
  asum $
    mconcat
      [
        [ Opt.flag' Mainnet $
            mconcat
              [ Opt.long "mainnet"
              , Opt.help $
                  mconcat
                    [ "Use the mainnet magic id. This overrides the CARDANO_NODE_NETWORK_ID "
                    , "environment variable"
                    ]
              ]
        , fmap (Testnet . NetworkMagic) $
            Opt.option (bounded "TESTNET_MAGIC") $
              mconcat
                [ Opt.long "testnet-magic"
                , Opt.metavar "NATURAL"
                , Opt.help $
                    mconcat
                      [ "Specify a testnet magic id. This overrides the CARDANO_NODE_NETWORK_ID "
                      , "environment variable"
                      ]
                ]
        ]
      , -- Default to the network id specified by the environment variable if it is available.
        pure <$> maybeToList (envCliNetworkId envCli)
      ]

pTarget :: ShelleyBasedEra era -> Parser (Consensus.Target ChainPoint)
pTarget sbe =
  maybe (pure Consensus.VolatileTip) pTargetFromConway (forShelleyBasedEraMaybeEon sbe)
 where
  pTargetFromConway :: ConwayEraOnwards era -> Parser (Consensus.Target ChainPoint)
  pTargetFromConway _ =
    asum $
      mconcat
        [
          [ Opt.flag' Consensus.VolatileTip $
              mconcat
                [ Opt.long "volatile-tip"
                , Opt.help $
                    mconcat
                      [ "Use the volatile tip as a target. (This is the default)"
                      ]
                ]
          , Opt.flag' Consensus.ImmutableTip $
              mconcat
                [ Opt.long "immutable-tip"
                , Opt.help $
                    mconcat
                      [ "Use the immutable tip as a target."
                      ]
                ]
          ]
        , -- Default to volatile tip if not specified
          [pure Consensus.VolatileTip]
        ]

toUnitIntervalOrErr :: Rational -> L.UnitInterval
toUnitIntervalOrErr r = case L.boundRational r of
  Nothing ->
    error $
      mconcat
        [ "toUnitIntervalOrErr: "
        , "rational out of bounds " <> show r
        ]
  Just n -> n

pConsensusModeParams :: Parser ConsensusModeParams
pConsensusModeParams =
  asum
    [ pCardanoMode *> pCardanoConsensusMode
    , pDefaultConsensusMode
    ]
 where
  pCardanoMode :: Parser ()
  pCardanoMode =
    Opt.flag' () $
      mconcat
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
  fmap EpochSlots $
    Opt.option (bounded "SLOTS") $
      mconcat
        [ Opt.long "epoch-slots"
        , Opt.metavar "SLOTS"
        , Opt.help "The number of slots per epoch for the Byron era."
        , Opt.value defaultByronEpochSlots -- Default to the mainnet value.
        , Opt.showDefault
        ]

pSocketPath :: EnvCli -> Parser SocketPath
pSocketPath envCli =
  asum $
    mconcat
      [
        [ fmap File $
            Opt.strOption $
              mconcat
                [ Opt.long "socket-path"
                , Opt.metavar "SOCKET_PATH"
                , Opt.help $
                    mconcat
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
    Parsec.showErrorMessages
      "or"
      "unknown parse error"
      "expecting"
      "unexpected"
      "end of input"
      (Parsec.errorMessages err)

parseTxIn :: Parsec.Parser TxIn
parseTxIn = TxIn <$> parseTxId <*> (Parsec.char '#' *> parseTxIx)

parseTxId :: Parsec.Parser TxId
parseTxId = do
  str' <- some Parsec.hexDigit <?> "transaction id (hexadecimal)"
  case deserialiseFromRawBytesHex AsTxId (BSC.pack str') of
    Right addr -> return addr
    Left e -> fail $ docToString $ "Incorrect transaction id format: " <> prettyError e

parseTxIx :: Parsec.Parser TxIx
parseTxIx = TxIx . fromIntegral <$> decimal

decimal :: Parsec.Parser Integer
Parsec.TokenParser{Parsec.decimal = decimal} = Parsec.haskell

pStakeIdentifier :: Maybe String -> Parser StakeIdentifier
pStakeIdentifier prefix =
  asum
    [ StakeIdentifierVerifier <$> pStakeVerifier prefix
    , StakeIdentifierAddress <$> pStakeAddress prefix
    ]

pStakeVerifier :: Maybe String -> Parser StakeVerifier
pStakeVerifier prefix =
  asum
    [ StakeVerifierKey <$> pStakeVerificationKeyOrHashOrFile prefix
    , StakeVerifierScriptFile
        <$> pScriptFor (prefixFlag prefix "stake-script-file") Nothing "Filepath of the staking script."
    ]

pStakeAddress :: Maybe String -> Parser StakeAddress
pStakeAddress prefix =
  Opt.option (readerFromParsecParser parseStakeAddress) $
    mconcat
      [ Opt.long $ prefixFlag prefix "stake-address"
      , Opt.metavar "ADDRESS"
      , Opt.help "Target stake address (bech32 format)."
      ]

parseStakeAddress :: Parsec.Parser StakeAddress
parseStakeAddress = do
  str' <- lexPlausibleAddressString
  case deserialiseAddress AsStakeAddress str' of
    Nothing -> fail $ "invalid address: " <> Text.unpack str'
    Just addr -> pure addr

-- | First argument is the optional prefix
pStakeVerificationKeyOrFile :: Maybe String -> Parser (VerificationKeyOrFile StakeKey)
pStakeVerificationKeyOrFile prefix =
  VerificationKeyValue
    <$> pStakeVerificationKey prefix
      <|> VerificationKeyFilePath
    <$> pStakeVerificationKeyFile prefix

pScriptFor :: String -> Maybe String -> String -> Parser ScriptFile
pScriptFor name Nothing help' =
  fmap File $ parseFilePath name help'
pScriptFor name (Just deprecated) help' =
  pScriptFor name Nothing help'
    <|> File
    <$> Opt.strOption
      ( Opt.long deprecated
          <> Opt.internal
      )

-- | The first argument is the optional prefix.
pStakeVerificationKey :: Maybe String -> Parser (VerificationKey StakeKey)
pStakeVerificationKey prefix =
  Opt.option (readVerificationKey AsStakeKey) $
    mconcat
      [ Opt.long $ prefixFlag prefix "stake-verification-key"
      , Opt.metavar "STRING"
      , Opt.help "Stake verification key (Bech32 or hex-encoded)."
      ]

-- | Read a Bech32 or hex-encoded verification key.
readVerificationKey
  :: forall keyrole
   . SerialiseAsBech32 (VerificationKey keyrole)
  => AsType keyrole
  -> Opt.ReadM (VerificationKey keyrole)
readVerificationKey asType =
  Opt.eitherReader deserialiseFromBech32OrHex
 where
  keyFormats :: NonEmpty (InputFormat (VerificationKey keyrole))
  keyFormats = fromList [InputFormatBech32, InputFormatHex]

  deserialiseFromBech32OrHex
    :: String
    -> Either String (VerificationKey keyrole)
  deserialiseFromBech32OrHex str' =
    first (docToString . renderInputDecodeError) $
      deserialiseInput (AsVerificationKey asType) keyFormats (BSC.pack str')

-- | The first argument is the optional prefix.
pStakeVerificationKeyFile :: Maybe String -> Parser (VerificationKeyFile In)
pStakeVerificationKeyFile prefix =
  File
    <$> asum
      [ parseFilePath
          (prefixFlag prefix "stake-verification-key-file")
          "Filepath of the staking verification key."
      , Opt.strOption $
          mconcat
            [ Opt.long $ prefixFlag prefix "staking-verification-key-file"
            , Opt.internal
            ]
      ]

subInfoParser :: String -> InfoMod a -> [Maybe (Parser a)] -> Maybe (Parser a)
subInfoParser name i mps = case catMaybes mps of
  [] -> Nothing
  parsers -> Just $ subParser name $ Opt.info (asum parsers) i

pAnyShelleyBasedEra :: EnvCli -> Parser (EraInEon ShelleyBasedEra)
pAnyShelleyBasedEra envCli =
  asum $
    mconcat
      [
        [ Opt.flag' (EraInEon ShelleyBasedEraShelley) $
            mconcat [Opt.long "shelley-era", Opt.help $ "Specify the Shelley era" <> deprecationText]
        , Opt.flag' (EraInEon ShelleyBasedEraAllegra) $
            mconcat [Opt.long "allegra-era", Opt.help $ "Specify the Allegra era" <> deprecationText]
        , Opt.flag' (EraInEon ShelleyBasedEraMary) $
            mconcat [Opt.long "mary-era", Opt.help $ "Specify the Mary era" <> deprecationText]
        , Opt.flag' (EraInEon ShelleyBasedEraAlonzo) $
            mconcat [Opt.long "alonzo-era", Opt.help $ "Specify the Alonzo era" <> deprecationText]
        , Opt.flag' (EraInEon ShelleyBasedEraBabbage) $
            mconcat [Opt.long "babbage-era", Opt.help $ "Specify the Babbage era (default)" <> deprecationText]
        , Opt.flag' (EraInEon ShelleyBasedEraConway) $
            mconcat [Opt.long "conway-era", Opt.help "Specify the Conway era"]
        ]
      , maybeToList $ pure <$> envCliAnyEon envCli
      , pure $ pure $ EraInEon ShelleyBasedEraBabbage
      ]

deprecationText :: String
deprecationText = " - DEPRECATED - will be removed in the future"

pAnyShelleyToBabbageEra :: EnvCli -> Parser (EraInEon ShelleyToBabbageEra)
pAnyShelleyToBabbageEra envCli =
  asum $
    mconcat
      [
        [ Opt.flag' (EraInEon ShelleyToBabbageEraShelley) $
            mconcat [Opt.long "shelley-era", Opt.help $ "Specify the Shelley era" <> deprecationText]
        , Opt.flag' (EraInEon ShelleyToBabbageEraAllegra) $
            mconcat [Opt.long "allegra-era", Opt.help $ "Specify the Allegra era" <> deprecationText]
        , Opt.flag' (EraInEon ShelleyToBabbageEraMary) $
            mconcat [Opt.long "mary-era", Opt.help $ "Specify the Mary era" <> deprecationText]
        , Opt.flag' (EraInEon ShelleyToBabbageEraAlonzo) $
            mconcat [Opt.long "alonzo-era", Opt.help $ "Specify the Alonzo era" <> deprecationText]
        , Opt.flag' (EraInEon ShelleyToBabbageEraBabbage) $
            mconcat [Opt.long "babbage-era", Opt.help $ "Specify the Babbage era (default)" <> deprecationText]
        ]
      , maybeToList $ pure <$> envCliAnyEon envCli
      , pure . pure $ EraInEon ShelleyToBabbageEraBabbage
      ]

pFileOutDirection :: String -> String -> Parser (File a Out)
pFileOutDirection l h = File <$> parseFilePath l h

pFileInDirection :: String -> String -> Parser (File a In)
pFileInDirection l h = File <$> parseFilePath l h

parseLovelace :: Parsec.Parser Lovelace
parseLovelace = do
  i <- decimal
  if i > toInteger (maxBound :: Word64)
    then fail $ show i <> " lovelace exceeds the Word64 upper bound"
    else return $ L.Coin i

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
  Opt.option (readVerificationKey AsStakePoolKey) $
    mconcat
      [ Opt.long $ prefixFlag prefix "stake-pool-verification-key"
      , Opt.metavar "STRING"
      , Opt.help "Stake pool verification key (Bech32 or hex-encoded)."
      ]

-- | The first argument is the optional prefix.
pStakePoolVerificationKeyFile :: Maybe String -> Parser (VerificationKeyFile In)
pStakePoolVerificationKeyFile prefix =
  File
    <$> asum
      [ parseFilePath "cold-verification-key-file" "Filepath of the stake pool verification key."
      , Opt.strOption $
          mconcat
            [ Opt.long $ prefixFlag prefix "stake-pool-verification-key-file"
            , Opt.internal
            ]
      ]

pOutputFile :: Parser (File content Out)
pOutputFile = File <$> parseFilePath "out-file" "The output file."

pMIRPot :: Parser L.MIRPot
pMIRPot =
  asum
    [ Opt.flag' L.ReservesMIR $
        mconcat
          [ Opt.long "reserves"
          , Opt.help "Use the reserves pot."
          ]
    , Opt.flag' L.TreasuryMIR $
        mconcat
          [ Opt.long "treasury"
          , Opt.help "Use the treasury pot."
          ]
    ]

pRewardAmt :: Parser Lovelace
pRewardAmt =
  Opt.option (readerFromParsecParser parseLovelace) $
    mconcat
      [ Opt.long "reward"
      , Opt.metavar "LOVELACE"
      , Opt.help "The reward for the relevant reward account."
      ]

pTransferAmt :: Parser Lovelace
pTransferAmt =
  Opt.option (readerFromParsecParser parseLovelace) $
    mconcat
      [ Opt.long "transfer"
      , Opt.metavar "LOVELACE"
      , Opt.help "The amount to transfer."
      ]

pTreasuryWithdrawalAmt :: Parser Lovelace
pTreasuryWithdrawalAmt =
  Opt.option (readerFromParsecParser parseLovelace) $
    mconcat
      [ Opt.long "transfer"
      , Opt.metavar "LOVELACE"
      , Opt.help $
          mconcat
            [ "The amount of lovelace the proposal intends to withdraw from the Treasury. "
            , "Multiple withdrawals can be proposed in a single governance action "
            , "by repeating the --funds-receiving-stake and --transfer options as many times as needed."
            ]
      ]

rHexHash
  :: ()
  => SerialiseAsRawBytes (Hash a)
  => AsType a
  -> Maybe String
  -- ^ Optional prefix to the error message
  -> ReadM (Hash a)
rHexHash a mErrPrefix =
  Opt.eitherReader $
    first (\e -> errPrefix <> (docToString $ prettyError e))
      . deserialiseFromRawBytesHex (AsHash a)
      . BSC.pack
 where
  errPrefix = maybe "" (": " <>) mErrPrefix

rBech32KeyHash :: SerialiseAsBech32 (Hash a) => AsType a -> ReadM (Hash a)
rBech32KeyHash a =
  Opt.eitherReader $
    first (docToString . prettyError)
      . deserialiseFromBech32 (AsHash a)
      . Text.pack

pGenesisDelegateVerificationKey :: Parser (VerificationKey GenesisDelegateKey)
pGenesisDelegateVerificationKey =
  Opt.option deserialiseFromHex $
    mconcat
      [ Opt.long "genesis-delegate-verification-key"
      , Opt.metavar "STRING"
      , Opt.help "Genesis delegate verification key (hex-encoded)."
      ]
 where
  deserialiseFromHex =
    rVerificationKey AsGenesisDelegateKey (Just "Invalid genesis delegate verification key")

-- | Reader for verification keys
rVerificationKey
  :: ()
  => SerialiseAsRawBytes (VerificationKey a)
  => AsType a
  -- ^ Singleton value identifying the kind of verification keys
  -> Maybe String
  -- ^ Optional prefix to the error message
  -> ReadM (VerificationKey a)
rVerificationKey a mErrPrefix =
  Opt.eitherReader $
    first
      (\e -> errPrefix <> (docToString $ prettyError e))
      . deserialiseFromRawBytesHex (AsVerificationKey a)
      . BSC.pack
 where
  errPrefix = maybe "" (": " <>) mErrPrefix

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
  fmap File $
    asum
      [ parseFilePath "cold-verification-key-file" "Filepath of the cold verification key."
      , Opt.strOption $
          mconcat
            [ Opt.long "verification-key-file"
            , Opt.internal
            ]
      ]

pColdSigningKeyFile :: Parser (File (SigningKey keyrole) direction)
pColdSigningKeyFile =
  fmap File $
    asum
      [ parseFilePath "cold-signing-key-file" "Filepath of the cold signing key."
      , Opt.strOption $
          mconcat
            [ Opt.long "signing-key-file"
            , Opt.internal
            ]
      ]

pVerificationKeyFileOut :: Parser (File (VerificationKey keyrole) Out)
pVerificationKeyFileOut =
  File <$> parseFilePath "verification-key-file" "Output filepath of the verification key."

pSigningKeyFileOut :: Parser (File (SigningKey keyrole) Out)
pSigningKeyFileOut =
  File <$> parseFilePath "signing-key-file" "Output filepath of the signing key."

pOperatorCertIssueCounterFile :: Parser (File OpCertCounter direction)
pOperatorCertIssueCounterFile =
  fmap File $
    asum
      [ parseFilePath
          "operational-certificate-issue-counter-file"
          "The file with the issue counter for the operational certificate."
      , Opt.strOption $
          mconcat
            [ Opt.long "operational-certificate-issue-counter"
            , Opt.internal
            ]
      ]

---

pAddCommitteeColdVerificationKeySource
  :: Parser (VerificationKeyOrHashOrFileOrScriptHash CommitteeColdKey)
pAddCommitteeColdVerificationKeySource =
  asum
    [ VkhfshKeyHashFile . VerificationKeyOrFile <$> pAddCommitteeColdVerificationKeyOrFile
    , VkhfshKeyHashFile . VerificationKeyHash <$> pAddCommitteeColdVerificationKeyHash
    , VkhfshScriptHash
        <$> pScriptHash
          "add-cc-cold-script-hash"
          "Cold Native or Plutus script file hash (hex-encoded). Obtain it with \"cardano-cli hash script ...\"."
    ]

pAddCommitteeColdVerificationKeyHash :: Parser (Hash CommitteeColdKey)
pAddCommitteeColdVerificationKeyHash =
  Opt.option deserialiseColdCCKeyHashFromHex $
    mconcat
      [ Opt.long "add-cc-cold-verification-key-hash"
      , Opt.metavar "STRING"
      , Opt.help "Constitutional Committee key hash (hex-encoded)."
      ]

pAddCommitteeColdVerificationKeyOrFile :: Parser (VerificationKeyOrFile CommitteeColdKey)
pAddCommitteeColdVerificationKeyOrFile =
  asum
    [ VerificationKeyValue <$> pAddCommitteeColdVerificationKey
    , VerificationKeyFilePath <$> pAddCommitteeColdVerificationKeyFile
    ]

pAddCommitteeColdVerificationKey :: Parser (VerificationKey CommitteeColdKey)
pAddCommitteeColdVerificationKey =
  Opt.option deserialiseFromHex $
    mconcat
      [ Opt.long "add-cc-cold-verification-key"
      , Opt.metavar "STRING"
      , Opt.help "Constitutional Committee cold key (hex-encoded)."
      ]
 where
  deserialiseFromHex =
    rVerificationKey AsCommitteeColdKey (Just "Invalid Constitutional Committee cold key")

pAddCommitteeColdVerificationKeyFile :: Parser (File (VerificationKey keyrole) In)
pAddCommitteeColdVerificationKeyFile =
  File
    <$> parseFilePath
      "add-cc-cold-verification-key-file"
      "Filepath of the Constitutional Committee cold key."

---
pRemoveCommitteeColdVerificationKeySource
  :: Parser (VerificationKeyOrHashOrFileOrScriptHash CommitteeColdKey)
pRemoveCommitteeColdVerificationKeySource =
  asum
    [ VkhfshKeyHashFile . VerificationKeyOrFile <$> pRemoveCommitteeColdVerificationKeyOrFile
    , VkhfshKeyHashFile . VerificationKeyHash <$> pRemoveCommitteeColdVerificationKeyHash
    , VkhfshScriptHash
        <$> pScriptHash
          "remove-cc-cold-script-hash"
          "Cold Native or Plutus script file hash (hex-encoded). Obtain it with \"cardano-cli hash script ...\"."
    ]

pScriptHash
  :: String
  -- ^ long option name
  -> String
  -- ^ help text
  -> Parser ScriptHash
pScriptHash longOptionName helpText =
  Opt.option scriptHashReader $
    mconcat
      [ Opt.long longOptionName
      , Opt.metavar "HASH"
      , Opt.help helpText
      ]

pRemoveCommitteeColdVerificationKeyHash :: Parser (Hash CommitteeColdKey)
pRemoveCommitteeColdVerificationKeyHash =
  Opt.option deserialiseColdCCKeyHashFromHex $
    mconcat
      [ Opt.long "remove-cc-cold-verification-key-hash"
      , Opt.metavar "STRING"
      , Opt.help "Constitutional Committee key hash (hex-encoded)."
      ]

pRemoveCommitteeColdVerificationKeyOrFile :: Parser (VerificationKeyOrFile CommitteeColdKey)
pRemoveCommitteeColdVerificationKeyOrFile =
  asum
    [ VerificationKeyValue <$> pRemoveCommitteeColdVerificationKey
    , VerificationKeyFilePath <$> pRemoveCommitteeColdVerificationKeyFile
    ]

pRemoveCommitteeColdVerificationKey :: Parser (VerificationKey CommitteeColdKey)
pRemoveCommitteeColdVerificationKey =
  Opt.option deserialiseColdCCKeyFromHex $
    mconcat
      [ Opt.long "remove-cc-cold-verification-key"
      , Opt.metavar "STRING"
      , Opt.help "Constitutional Committee cold key (hex-encoded)."
      ]

deserialiseColdCCKeyFromHex :: ReadM (VerificationKey CommitteeColdKey)
deserialiseColdCCKeyFromHex =
  rVerificationKey AsCommitteeColdKey (Just "Invalid Constitutional Committee cold key")

deserialiseColdCCKeyHashFromHex :: ReadM (Hash CommitteeColdKey)
deserialiseColdCCKeyHashFromHex =
  rHexHash AsCommitteeColdKey (Just "Invalid Constitutional Committee cold key hash")

pRemoveCommitteeColdVerificationKeyFile :: Parser (File (VerificationKey keyrole) In)
pRemoveCommitteeColdVerificationKeyFile =
  File
    <$> parseFilePath
      "remove-cc-cold-verification-key-file"
      "Filepath of the Constitutional Committee cold key."

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
  Opt.option deserialiseColdCCKeyFromHex $
    mconcat
      [ Opt.long "cold-verification-key"
      , Opt.metavar "STRING"
      , Opt.help "Constitutional Committee cold key (hex-encoded)."
      ]

pCommitteeColdVerificationKeyHash :: Parser (Hash CommitteeColdKey)
pCommitteeColdVerificationKeyHash =
  Opt.option deserialiseColdCCKeyHashFromHex $
    mconcat
      [ Opt.long "cold-verification-key-hash"
      , Opt.metavar "STRING"
      , Opt.help "Constitutional Committee key hash (hex-encoded)."
      ]

pCommitteeColdVerificationKeyFile :: Parser (File (VerificationKey keyrole) In)
pCommitteeColdVerificationKeyFile =
  File
    <$> parseFilePath "cold-verification-key-file" "Filepath of the Constitutional Committee cold key."

pVerificationKeyFileIn :: Parser (VerificationKeyFile In)
pVerificationKeyFileIn =
  File <$> parseFilePath "verification-key-file" "Input filepath of the verification key."

pAnyVerificationKeyFileIn :: String -> Parser (VerificationKeyFile In)
pAnyVerificationKeyFileIn helpText =
  File <$> parseFilePath "verification-key-file" ("Input filepath of the " <> helpText <> ".")

pAnyVerificationKeyText :: String -> Parser AnyVerificationKeyText
pAnyVerificationKeyText helpText =
  fmap (AnyVerificationKeyText . Text.pack) $
    Opt.strOption $
      mconcat
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
pCommitteeHotKey = pCommitteeHotVerificationKey "hot-key"

pCommitteeHotVerificationKeyOrFile :: Parser (VerificationKeyOrFile CommitteeHotKey)
pCommitteeHotVerificationKeyOrFile =
  asum
    [ VerificationKeyValue <$> pCommitteeHotVerificationKey "hot-verification-key"
    , VerificationKeyFilePath <$> pCommitteeHotVerificationKeyFile "hot-verification-key-file"
    ]

pCommitteeHotVerificationKeyHash :: Parser (Hash CommitteeHotKey)
pCommitteeHotVerificationKeyHash =
  Opt.option deserialiseHotCCKeyHashFromHex $
    mconcat
      [ Opt.long "hot-verification-key-hash"
      , Opt.metavar "STRING"
      , Opt.help "Constitutional Committee key hash (hex-encoded)."
      ]

pCommitteeHotVerificationKey :: String -> Parser (VerificationKey CommitteeHotKey)
pCommitteeHotVerificationKey longFlag =
  Opt.option deserialiseHotCCKeyFromHex $
    mconcat
      [ Opt.long longFlag
      , Opt.metavar "STRING"
      , Opt.help "Constitutional Committee hot key (hex-encoded)."
      ]

deserialiseHotCCKeyFromHex :: ReadM (VerificationKey CommitteeHotKey)
deserialiseHotCCKeyFromHex =
  rVerificationKey AsCommitteeHotKey (Just "Invalid Constitutional Committee hot key")

deserialiseHotCCKeyHashFromHex :: ReadM (Hash CommitteeHotKey)
deserialiseHotCCKeyHashFromHex =
  rHexHash AsCommitteeHotKey (Just "Invalid Constitutional Committee hot key hash")

pCommitteeHotVerificationKeyFile :: String -> Parser (VerificationKeyFile In)
pCommitteeHotVerificationKeyFile longFlag =
  File <$> parseFilePath longFlag "Filepath of the Constitutional Committee hot key."

-- | The first argument is the optional prefix.
pCommitteeHotKeyHash :: Maybe String -> Parser (Hash CommitteeHotKey)
pCommitteeHotKeyHash prefix =
  Opt.option deserialiseHotCCKeyHashFromHex $
    mconcat
      [ Opt.long $ prefixFlag prefix "hot-key-hash"
      , Opt.metavar "STRING"
      , Opt.help "Constitutional Committee key hash (hex-encoded)."
      ]

pCommitteeHotKeyOrHashOrFile :: Parser (VerificationKeyOrHashOrFile CommitteeHotKey)
pCommitteeHotKeyOrHashOrFile =
  asum
    [ VerificationKeyOrFile . VerificationKeyValue <$> pCommitteeHotKey
    , VerificationKeyOrFile . VerificationKeyFilePath <$> pCommitteeHotVerificationKeyFile "hot-key-file"
    , VerificationKeyHash <$> pCommitteeHotKeyHash Nothing
    ]

pCommitteeHotVerificationKeyOrHashOrVerificationFile
  :: Parser (VerificationKeyOrHashOrFile CommitteeHotKey)
pCommitteeHotVerificationKeyOrHashOrVerificationFile =
  asum
    [ VerificationKeyOrFile . VerificationKeyValue
        <$> pCommitteeHotVerificationKey "cc-hot-verification-key"
    , VerificationKeyOrFile . VerificationKeyFilePath
        <$> pCommitteeHotVerificationKeyFile "cc-hot-verification-key-file"
    , VerificationKeyHash <$> pCommitteeHotKeyHash (Just "cc")
    ]

pCommitteeHotVerificationKeyOrHashOrVerificationFileOrScriptHash
  :: Parser (VerificationKeyOrHashOrFileOrScriptHash CommitteeHotKey)
pCommitteeHotVerificationKeyOrHashOrVerificationFileOrScriptHash =
  asum
    [ VkhfshKeyHashFile <$> pCommitteeHotVerificationKeyOrHashOrVerificationFile
    , VkhfshScriptHash
        <$> pScriptHash
          "cc-hot-script-hash"
          "Cold Native or Plutus script file hash (hex-encoded). Obtain it with \"cardano-cli hash script ...\"."
    ]

catCommands :: [Parser a] -> Maybe (Parser a)
catCommands = \case
  [] -> Nothing
  ps -> Just $ asum ps

pConstitutionUrl :: Parser ConstitutionUrl
pConstitutionUrl =
  ConstitutionUrl
    <$> pUrl "constitution-url" "Constitution URL."

pConstitutionHash :: Parser (L.SafeHash L.StandardCrypto L.AnchorData)
pConstitutionHash =
  Opt.option readSafeHash $
    mconcat
      [ Opt.long "constitution-hash"
      , Opt.metavar "HASH"
      , Opt.help "Hash of the constitution data (obtain it with \"cardano-cli hash anchor-data ...\")."
      ]

pUrl :: String -> String -> Parser L.Url
pUrl l h =
  let toUrl urlText =
        fromMaybe (error "Url longer than 64 bytes") $
          L.textToUrl (Text.length urlText) urlText
   in fmap toUrl . Opt.strOption $
        mconcat
          [ Opt.long l
          , Opt.metavar "TEXT"
          , Opt.help h
          ]

pGovActionDeposit :: Parser Lovelace
pGovActionDeposit =
  Opt.option (readerFromParsecParser parseLovelace) $
    mconcat
      [ Opt.long "governance-action-deposit"
      , Opt.metavar "NATURAL"
      , Opt.help "Deposit required to submit a governance action."
      ]

pNewGovActionDeposit :: Parser Lovelace
pNewGovActionDeposit =
  Opt.option (readerFromParsecParser parseLovelace) $
    mconcat
      [ Opt.long "new-governance-action-deposit"
      , Opt.metavar "NATURAL"
      , Opt.help "Proposed new value of the deposit required to submit a governance action."
      ]

-- | First argument is the optional prefix
pStakeVerificationKeyOrHashOrFile :: Maybe String -> Parser (VerificationKeyOrHashOrFile StakeKey)
pStakeVerificationKeyOrHashOrFile prefix =
  asum
    [ VerificationKeyOrFile <$> pStakeVerificationKeyOrFile prefix
    , VerificationKeyHash <$> pStakeVerificationKeyHash prefix
    ]

-- | First argument is the optional prefix
pStakeVerificationKeyHash :: Maybe String -> Parser (Hash StakeKey)
pStakeVerificationKeyHash prefix =
  Opt.option (rHexHash AsStakeKey Nothing) $
    mconcat
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

--------------------------------------------------------------------------------

pCBORInFile :: Parser FilePath
pCBORInFile =
  asum
    [ parseFilePath "in-file" "CBOR input file."
    , Opt.strOption $
        mconcat
          [ Opt.long "file"
          , Opt.internal
          ]
    ]

--------------------------------------------------------------------------------

pPollQuestion :: Parser Text
pPollQuestion =
  Opt.strOption $
    mconcat
      [ Opt.long "question"
      , Opt.metavar "STRING"
      , Opt.help "The question for the poll."
      ]

pPollAnswer :: Parser Text
pPollAnswer =
  Opt.strOption $
    mconcat
      [ Opt.long "answer"
      , Opt.metavar "STRING"
      , Opt.help "A possible choice for the poll. The option is repeatable."
      ]

pPollAnswerIndex :: Parser Word
pPollAnswerIndex =
  Opt.option integralReader $
    mconcat
      [ Opt.long "answer"
      , Opt.metavar "INT"
      , Opt.help "The index of the chosen answer in the poll. Optional. Asked interactively if omitted."
      ]

pPollFile :: Parser (File GovernancePoll In)
pPollFile = File <$> parseFilePath "poll-file" "Filepath to the ongoing poll."

pPollTxFile :: Parser (TxFile In)
pPollTxFile =
  File
    <$> parseFilePath "tx-file" "Filepath to the JSON TxBody or JSON Tx carrying a valid poll answer."

pPollNonce :: Parser Word
pPollNonce =
  Opt.option integralReader $
    mconcat
      [ Opt.long "nonce"
      , Opt.metavar "UINT"
      , Opt.help "An (optional) nonce for non-replayability."
      ]

--------------------------------------------------------------------------------

pMintScriptFile :: Parser (File ScriptInAnyLang In)
pMintScriptFile =
  pScriptFor
    "mint-script-file"
    (Just "minting-script-file")
    "The file containing the script to witness the minting of assets for a particular policy Id."

pPlutusMintScriptWitnessData
  :: ShelleyBasedEra era
  -> WitCtx witctx
  -> BalanceTxExecUnits
  -> Parser (ScriptDataOrFile, ExecutionUnits)
pPlutusMintScriptWitnessData _sbe _witctx autoBalanceExecUnits =
  let scriptFlagPrefix = "mint"
   in ( (,)
          <$> pScriptRedeemerOrFile scriptFlagPrefix
          <*> ( case autoBalanceExecUnits of
                  AutoBalance -> pure (ExecutionUnits 0 0)
                  ManualBalance -> pExecutionUnits scriptFlagPrefix
              )
      )

pSimpleScriptOrPlutusSpendingScriptWitness
  :: ShelleyBasedEra era
  -> BalanceTxExecUnits
  -> String
  -- ^ Script flag prefix
  -> Maybe String
  -- ^ Potential deprecated script flag prefix
  -> String
  -- ^ Help text
  -> Parser CliSpendScriptRequirements
pSimpleScriptOrPlutusSpendingScriptWitness sbe autoBalanceExecUnits scriptFlagPrefix scriptFlagPrefixDeprecated help =
  PlutusSpend.createSimpleOrPlutusScriptFromCliArgs
    <$> pScriptFor
      (scriptFlagPrefix ++ "-script-file")
      ((++ "-script-file") <$> scriptFlagPrefixDeprecated)
      ("The file containing the script to witness " ++ help)
    <*> optional
      ( (,,)
          <$> pScriptDatumOrFileSpendingCip69 sbe scriptFlagPrefix
          <*> pScriptRedeemerOrFile scriptFlagPrefix
          <*> ( case autoBalanceExecUnits of
                  AutoBalance -> pure (ExecutionUnits 0 0)
                  ManualBalance -> pExecutionUnits scriptFlagPrefix
              )
      )

pScriptWitnessFiles
  :: forall witctx
   . WitCtx witctx
  -> BalanceTxExecUnits
  -> String
  -- ^ Script flag prefix
  -> Maybe String
  -> String
  -> Parser (ScriptWitnessFiles witctx)
pScriptWitnessFiles witctx autoBalanceExecUnits scriptFlagPrefix scriptFlagPrefixDeprecated help =
  toScriptWitnessFiles
    <$> pScriptFor
      (scriptFlagPrefix ++ "-script-file")
      ((++ "-script-file") <$> scriptFlagPrefixDeprecated)
      ("The file containing the script to witness " ++ help)
    <*> optional
      ( (,,)
          <$> pure (excludeTxInScriptWitnesses witctx)
          <*> pScriptRedeemerOrFile scriptFlagPrefix
          <*> ( case autoBalanceExecUnits of
                  AutoBalance -> pure (ExecutionUnits 0 0)
                  ManualBalance -> pExecutionUnits scriptFlagPrefix
              )
      )
 where
  excludeTxInScriptWitnesses :: WitCtx witctx -> ScriptDatumOrFile witctx
  excludeTxInScriptWitnesses WitCtxStake = NoScriptDatumOrFileForStake
  excludeTxInScriptWitnesses WitCtxMint =
    error $
      mconcat
        [ "pScriptWitnessFiles.excludeTxInScriptWitnesses: Should be impossible as "
        , "mint script witnesses are handled by the pMintMultiAsset parser."
        ]
  excludeTxInScriptWitnesses WitCtxTxIn =
    error $
      mconcat
        [ "pScriptWitnessFiles.excludeTxInScriptWitnesses: Should be impossible as "
        , "tx in script witnesses are handled by the pSimpleScriptOrPlutusSpendingScriptWitness parser."
        ]

  toScriptWitnessFiles
    :: ScriptFile
    -> Maybe
        ( ScriptDatumOrFile witctx
        , ScriptRedeemerOrFile
        , ExecutionUnits
        )
    -> ScriptWitnessFiles witctx
  toScriptWitnessFiles sf Nothing = SimpleScriptWitnessFile sf
  toScriptWitnessFiles sf (Just (d, r, e)) = PlutusScriptWitnessFiles sf d r e

pExecutionUnits :: String -> Parser ExecutionUnits
pExecutionUnits scriptFlagPrefix =
  fmap (uncurry ExecutionUnits) $
    Opt.option pairIntegralReader $
      mconcat
        [ Opt.long (scriptFlagPrefix ++ "-execution-units")
        , Opt.metavar "(INT, INT)"
        , Opt.help "The time and space units needed by the script."
        ]

pScriptRedeemerOrFile :: String -> Parser ScriptDataOrFile
pScriptRedeemerOrFile scriptFlagPrefix =
  pScriptDataOrFile
    (scriptFlagPrefix ++ "-redeemer")
    "The script redeemer value."
    "The script redeemer file."

pScriptDatumOrFileCip69 :: String -> WitCtx witctx -> Parser (ScriptDatumOrFile witctx)
pScriptDatumOrFileCip69 scriptFlagPrefix witctx =
  case witctx of
    WitCtxTxIn ->
      asum
        [ ScriptDatumOrFileForTxIn
            <$> optional
              ( pScriptDataOrFile
                  (scriptFlagPrefix ++ "-datum")
                  "The script datum."
                  "The script datum file."
              )
        , pInlineDatumPresent
        ]
    WitCtxMint -> pure NoScriptDatumOrFileForMint
    WitCtxStake -> pure NoScriptDatumOrFileForStake
 where
  pInlineDatumPresent :: Parser (ScriptDatumOrFile WitCtxTxIn)
  pInlineDatumPresent =
    flag' InlineDatumPresentAtTxIn $
      mconcat
        [ long (scriptFlagPrefix ++ "-inline-datum-present")
        , Opt.help "Inline datum present at transaction input."
        ]

pScriptDatumOrFile :: String -> WitCtx witctx -> Parser (ScriptDatumOrFile witctx)
pScriptDatumOrFile scriptFlagPrefix witctx =
  case witctx of
    WitCtxTxIn ->
      asum
        [ ScriptDatumOrFileForTxIn . Just
            <$> pScriptDataOrFile
              (scriptFlagPrefix ++ "-datum")
              "The script datum."
              "The script datum file."
        , pInlineDatumPresent
        ]
    WitCtxMint -> pure NoScriptDatumOrFileForMint
    WitCtxStake -> pure NoScriptDatumOrFileForStake
 where
  pInlineDatumPresent :: Parser (ScriptDatumOrFile WitCtxTxIn)
  pInlineDatumPresent =
    flag' InlineDatumPresentAtTxIn $
      mconcat
        [ long (scriptFlagPrefix ++ "-inline-datum-present")
        , Opt.help "Inline datum present at transaction input."
        ]

pScriptDatumOrFileSpendingCip69
  :: ShelleyBasedEra era -> String -> Parser PlutusSpend.ScriptDatumOrFileSpending
pScriptDatumOrFileSpendingCip69 sbe scriptFlagPrefix =
  caseShelleyToBabbageOrConwayEraOnwards
    (const datumMandatory)
    (const datumOptional)
    sbe
 where
  datumMandatory =
    asum
      [ PlutusSpend.PotentialDatum . Just
          <$> datumParser
      , pInlineDatumPresent
      ]

  datumOptional =
    asum
      [ PlutusSpend.PotentialDatum
          <$> optional datumParser
      , pInlineDatumPresent
      ]

  datumParser =
    pScriptDataOrFile
      (scriptFlagPrefix ++ "-datum")
      "The script datum."
      "The script datum file."

  pInlineDatumPresent :: Parser PlutusSpend.ScriptDatumOrFileSpending
  pInlineDatumPresent =
    flag' PlutusSpend.InlineDatum $
      mconcat
        [ long (scriptFlagPrefix ++ "-inline-datum-present")
        , Opt.help "Inline datum present at transaction input."
        ]

pScriptDataOrFile
  :: String
  -- ^ data flag prefix
  -> String
  -- ^ value help text
  -> String
  -- ^ file help text
  -> Parser ScriptDataOrFile
pScriptDataOrFile dataFlagPrefix helpTextForValue helpTextForFile =
  asum
    [ pScriptDataCborFile
    , pScriptDataFile
    , pScriptDataValue
    ]
 where
  pScriptDataCborFile =
    fmap ScriptDataCborFile . Opt.strOption $
      mconcat
        [ Opt.long (dataFlagPrefix ++ "-cbor-file")
        , Opt.metavar "CBOR_FILE"
        , Opt.help $
            mconcat
              [ helpTextForFile
              , " The file has to be in CBOR format."
              ]
        ]

  pScriptDataFile =
    fmap ScriptDataJsonFile . Opt.strOption $
      mconcat
        [ Opt.long (dataFlagPrefix ++ "-file")
        , Opt.metavar "JSON_FILE"
        , Opt.help $
            mconcat
              [ helpTextForFile
              , " The file must follow the detailed JSON schema for script data."
              ]
        ]

  pScriptDataValue =
    fmap ScriptDataValue . Opt.option readerScriptData $
      mconcat
        [ Opt.long (dataFlagPrefix ++ "-value")
        , Opt.metavar "JSON_VALUE"
        , Opt.help $
            mconcat
              [ helpTextForValue
              , " There is no schema: (almost) any JSON value is supported, including "
              , "top-level strings and numbers."
              ]
        ]

  readerScriptData :: ReadM HashableScriptData
  readerScriptData = do
    v <- Opt.str
    sDataValue <-
      liftWith ("readerScriptData: " <>) $
        Aeson.eitherDecode v
    liftWith (docToString . prettyError) $
      scriptDataJsonToHashable ScriptDataJsonNoSchema sDataValue
   where
    liftWith f = either (fail . f) pure

pVoteFiles
  :: ShelleyBasedEra era
  -> BalanceTxExecUnits
  -> Parser [(VoteFile In, Maybe (ScriptWitnessFiles WitCtxStake))]
pVoteFiles sbe bExUnits =
  caseShelleyToBabbageOrConwayEraOnwards
    (const $ pure [])
    (const . many $ pVoteFile bExUnits)
    sbe

pVoteFile
  :: BalanceTxExecUnits
  -> Parser (VoteFile In, Maybe (ScriptWitnessFiles WitCtxStake))
pVoteFile balExUnits =
  (,)
    <$> pFileInDirection "vote-file" "Filepath of the vote."
    <*> optional (pVoteScriptOrReferenceScriptWitness balExUnits)
 where
  pVoteScriptOrReferenceScriptWitness
    :: BalanceTxExecUnits -> Parser (ScriptWitnessFiles WitCtxStake)
  pVoteScriptOrReferenceScriptWitness bExUnits =
    pScriptWitnessFiles
      WitCtxStake
      bExUnits
      "vote"
      Nothing
      "a vote"
      <|> pPlutusStakeReferenceScriptWitnessFilesVotingProposing "vote-" balExUnits

pProposalFiles
  :: ShelleyBasedEra era
  -> BalanceTxExecUnits
  -> Parser [(ProposalFile In, Maybe (ScriptWitnessFiles WitCtxStake))]
pProposalFiles sbe balExUnits =
  caseShelleyToBabbageOrConwayEraOnwards
    (const $ pure [])
    (const $ many (pProposalFile balExUnits))
    sbe

pProposalFile
  :: BalanceTxExecUnits
  -> Parser (ProposalFile In, Maybe (ScriptWitnessFiles WitCtxStake))
pProposalFile balExUnits =
  (,)
    <$> pFileInDirection "proposal-file" "Filepath of the proposal."
    <*> optional (pProposingScriptOrReferenceScriptWitness balExUnits)
 where
  pProposingScriptOrReferenceScriptWitness
    :: BalanceTxExecUnits -> Parser (ScriptWitnessFiles WitCtxStake)
  pProposingScriptOrReferenceScriptWitness bExUnits =
    pScriptWitnessFiles
      WitCtxStake
      bExUnits
      "proposal"
      Nothing
      "a proposal"
      <|> pPlutusStakeReferenceScriptWitnessFilesVotingProposing "proposal-" balExUnits

pCurrentTreasuryValueAndDonation
  :: ShelleyBasedEra era -> Parser (Maybe (TxCurrentTreasuryValue, TxTreasuryDonation))
pCurrentTreasuryValueAndDonation =
  caseShelleyToBabbageOrConwayEraOnwards
    (const $ pure Nothing)
    ( const $
        optional ((,) <$> pCurrentTreasuryValue' <*> pTreasuryDonation')
    )

pCurrentTreasuryValue' :: Parser TxCurrentTreasuryValue
pCurrentTreasuryValue' =
  TxCurrentTreasuryValue
    <$> ( Opt.option (readerFromParsecParser parseLovelace) $
            mconcat
              [ Opt.long "current-treasury-value"
              , Opt.metavar "LOVELACE"
              , Opt.help "The current treasury value."
              ]
        )

pTreasuryDonation :: ShelleyBasedEra era -> Parser (Maybe TxTreasuryDonation)
pTreasuryDonation =
  caseShelleyToBabbageOrConwayEraOnwards
    (const $ pure Nothing)
    (const $ optional pTreasuryDonation')

pTreasuryDonation' :: Parser TxTreasuryDonation
pTreasuryDonation' =
  TxTreasuryDonation
    <$> ( Opt.option (readerFromParsecParser parseLovelace) $
            mconcat
              [ Opt.long "treasury-donation"
              , Opt.metavar "LOVELACE"
              , Opt.help "The donation to the treasury to perform."
              ]
        )

--------------------------------------------------------------------------------

pPaymentVerifier :: Parser PaymentVerifier
pPaymentVerifier =
  asum
    [ PaymentVerifierKey <$> pPaymentVerificationKeyTextOrFile
    , PaymentVerifierScriptFile
        <$> pScriptFor "payment-script-file" Nothing "Filepath of the payment script."
    ]

pPaymentVerificationKeyTextOrFile :: Parser VerificationKeyTextOrFile
pPaymentVerificationKeyTextOrFile =
  asum
    [ VktofVerificationKeyText <$> pPaymentVerificationKeyText
    , VktofVerificationKeyFile <$> pPaymentVerificationKeyFile
    ]

pPaymentVerificationKeyText :: Parser Text
pPaymentVerificationKeyText =
  fmap Text.pack $
    Opt.strOption $
      mconcat
        [ Opt.long "payment-verification-key"
        , Opt.metavar "STRING"
        , Opt.help "Payment verification key (Bech32-encoded)"
        ]

pPaymentVerificationKeyFile :: Parser (VerificationKeyFile In)
pPaymentVerificationKeyFile =
  fmap File $
    asum
      [ parseFilePath "payment-verification-key-file" "Filepath of the payment verification key."
      , Opt.strOption $
          mconcat
            [ Opt.long "verification-key-file"
            , Opt.internal
            ]
      ]

pScript :: Parser ScriptFile
pScript = pScriptFor "script-file" Nothing "Filepath of the script."

pReferenceTxIn :: String -> String -> Parser TxIn
pReferenceTxIn prefix scriptType =
  Opt.option (readerFromParsecParser parseTxIn) $
    mconcat
      [ Opt.long (prefix ++ "tx-in-reference")
      , Opt.metavar "TX-IN"
      , Opt.help $
          mconcat
            [ "TxId#TxIx - Specify a reference input. The reference input must have"
            , " a " <> scriptType <> " reference script attached."
            ]
      ]

pReadOnlyReferenceTxIn :: Parser TxIn
pReadOnlyReferenceTxIn =
  Opt.option (readerFromParsecParser parseTxIn) $
    mconcat
      [ Opt.long "read-only-tx-in-reference"
      , Opt.metavar "TX-IN"
      , Opt.help $
          mconcat
            [ "Specify a read only reference input. This reference input is not witnessing anything "
            , "it is simply provided in the plutus script context."
            ]
      ]

--------------------------------------------------------------------------------

pAddressKeyType :: Parser AddressKeyType
pAddressKeyType =
  asum
    [ Opt.flag' AddressKeyShelley $
        mconcat
          [ Opt.long "normal-key"
          , Opt.help "Use a normal Shelley-era key (default)."
          ]
    , Opt.flag' AddressKeyShelleyExtended $
        mconcat
          [ Opt.long "extended-key"
          , Opt.help "Use an extended ed25519 Shelley-era key."
          ]
    , Opt.flag' AddressKeyByron $
        mconcat
          [ Opt.long "byron-key"
          , Opt.help "Use a Byron-era key."
          ]
    , pure AddressKeyShelley
    ]

pProtocolParamsFile :: Parser ProtocolParamsFile
pProtocolParamsFile =
  ProtocolParamsFile
    <$> parseFilePath "protocol-params-file" "Filepath of the JSON-encoded protocol parameters file"

pTxBuildOutputOptions :: Parser TxBuildOutputOptions
pTxBuildOutputOptions =
  (OutputTxBodyOnly <$> pTxBodyFileOut) <|> pCalculatePlutusScriptCost
 where
  pCalculatePlutusScriptCost :: Parser TxBuildOutputOptions
  pCalculatePlutusScriptCost =
    OutputScriptCostOnly . File
      <$> parseFilePath
        "calculate-plutus-script-cost"
        "Where to write the script cost information."

pCertificateFile
  :: BalanceTxExecUnits
  -> Parser (CertificateFile, Maybe (ScriptWitnessFiles WitCtxStake))
pCertificateFile balanceExecUnits =
  (,)
    <$> ( fmap CertificateFile $
            asum
              [ parseFilePath "certificate-file" helpText
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
      "certificate"
      Nothing
      "the use of the certificate."
      <|> pPlutusStakeReferenceScriptWitnessFiles "certificate-" bExecUnits

  helpText =
    mconcat
      [ "Filepath of the certificate. This encompasses all "
      , "types of certificates (stake pool certificates, "
      , "stake key certificates etc). Optionally specify a script witness."
      ]

pPoolMetadataFile :: Parser (StakePoolMetadataFile In)
pPoolMetadataFile =
  File <$> parseFilePath "pool-metadata-file" "Filepath of the pool metadata."

pTxMetadataJsonSchema :: Parser TxMetadataJsonSchema
pTxMetadataJsonSchema =
  asum
    [ Opt.flag'
        ()
        ( Opt.long "json-metadata-no-schema"
            <> Opt.help "Use the \"no schema\" conversion from JSON to tx metadata (default)."
        )
        $> TxMetadataJsonNoSchema
    , Opt.flag'
        ()
        ( Opt.long "json-metadata-detailed-schema"
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
    [ fmap MetadataFileJSON $
        asum
          [ File <$> parseFilePath "metadata-json-file" "Filepath of the metadata file, in JSON format."
          , Opt.strOption $
              mconcat
                [ Opt.long "metadata-file" -- backward compat name
                , Opt.internal
                ]
          ]
    , fmap MetadataFileCBOR $
        File <$> parseFilePath "metadata-cbor-file" "Filepath of the metadata, in raw CBOR format."
    ]

pWithdrawal
  :: BalanceTxExecUnits
  -> Parser
      ( StakeAddress
      , Lovelace
      , Maybe (ScriptWitnessFiles WitCtxStake)
      )
pWithdrawal balance =
  (\(stakeAddr, lovelace) maybeScriptFp -> (stakeAddr, lovelace, maybeScriptFp))
    <$> Opt.option
      (readerFromParsecParser parseWithdrawal)
      ( Opt.long "withdrawal"
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
      "withdrawal"
      Nothing
      "the withdrawal of rewards."
      <|> pPlutusStakeReferenceScriptWitnessFiles "withdrawal-" balance

  helpText =
    mconcat
      [ "The reward withdrawal as StakeAddress+Lovelace where "
      , "StakeAddress is the Bech32-encoded stake address "
      , "followed by the amount in Lovelace. Optionally specify "
      , "a script witness."
      ]

  parseWithdrawal :: Parsec.Parser (StakeAddress, Lovelace)
  parseWithdrawal =
    (,) <$> parseStakeAddress <* Parsec.char '+' <*> parseLovelace

pPlutusStakeReferenceScriptWitnessFilesVotingProposing
  :: String
  -> BalanceTxExecUnits
  -> Parser (ScriptWitnessFiles WitCtxStake)
pPlutusStakeReferenceScriptWitnessFilesVotingProposing prefix autoBalanceExecUnits =
  PlutusReferenceScriptWitnessFiles
    <$> pReferenceTxIn prefix "plutus"
    <*> plutusP prefix PlutusScriptV3 "v3"
    <*> pure NoScriptDatumOrFileForStake
    <*> pScriptRedeemerOrFile (prefix ++ "reference-tx-in")
    <*> ( case autoBalanceExecUnits of
            AutoBalance -> pure (ExecutionUnits 0 0)
            ManualBalance -> pExecutionUnits $ prefix ++ "reference-tx-in"
        )

pPlutusStakeReferenceScriptWitnessFiles
  :: String
  -> BalanceTxExecUnits
  -> Parser (ScriptWitnessFiles WitCtxStake)
pPlutusStakeReferenceScriptWitnessFiles prefix autoBalanceExecUnits =
  PlutusReferenceScriptWitnessFiles
    <$> pReferenceTxIn prefix "plutus"
    <*> pPlutusScriptLanguage prefix
    <*> pure NoScriptDatumOrFileForStake
    <*> pScriptRedeemerOrFile (prefix ++ "reference-tx-in")
    <*> ( case autoBalanceExecUnits of
            AutoBalance -> pure (ExecutionUnits 0 0)
            ManualBalance -> pExecutionUnits $ prefix ++ "reference-tx-in"
        )

pPlutusScriptLanguage :: String -> Parser AnyPlutusScriptVersion
pPlutusScriptLanguage prefix = plutusP prefix PlutusScriptV2 "v2" <|> plutusP prefix PlutusScriptV3 "v3"

plutusP
  :: IsPlutusScriptLanguage lang
  => String -> PlutusScriptVersion lang -> String -> Parser AnyPlutusScriptVersion
plutusP prefix plutusVersion versionString =
  Opt.flag'
    (AnyPlutusScriptVersion plutusVersion)
    ( Opt.long (prefix <> "plutus-script-" <> versionString)
        <> Opt.help ("Specify a plutus script " <> versionString <> " reference script.")
    )

pUpdateProposalFile :: Parser UpdateProposalFile
pUpdateProposalFile =
  fmap UpdateProposalFile $
    asum
      [ parseFilePath "update-proposal-file" "Filepath of the update proposal."
      , Opt.strOption $
          mconcat
            [ Opt.long "update-proposal"
            , Opt.internal
            ]
      ]

pRequiredSigner :: Parser RequiredSigner
pRequiredSigner =
  RequiredSignerSkeyFile
    <$> sKeyFile
      <|> RequiredSignerHash
    <$> sPayKeyHash
 where
  sKeyFile :: Parser (SigningKeyFile In)
  sKeyFile =
    File
      <$> parseFilePath
        "required-signer"
        "Input filepath of the signing key (zero or more) whose signature is required."
  sPayKeyHash :: Parser (Hash PaymentKey)
  sPayKeyHash =
    Opt.option (readerFromParsecParser $ parseHash (AsHash AsPaymentKey)) $
      mconcat
        [ Opt.long "required-signer-hash"
        , Opt.metavar "HASH"
        , Opt.help $
            mconcat
              [ "Hash of the verification key (zero or more) whose "
              , "signature is required."
              ]
        ]

pVrfSigningKeyFile :: Parser (SigningKeyFile In)
pVrfSigningKeyFile =
  File <$> parseFilePath "vrf-signing-key-file" "Input filepath of the VRF signing key."

pWhichLeadershipSchedule :: Parser EpochLeadershipSchedule
pWhichLeadershipSchedule = pCurrent <|> pNext
 where
  pCurrent :: Parser EpochLeadershipSchedule
  pCurrent =
    Opt.flag' CurrentEpoch $
      mconcat
        [ Opt.long "current"
        , Opt.help "Get the leadership schedule for the current epoch."
        ]

  pNext :: Parser EpochLeadershipSchedule
  pNext =
    Opt.flag' NextEpoch $
      mconcat
        [ Opt.long "next"
        , Opt.help "Get the leadership schedule for the following epoch."
        ]

pWitnessSigningData :: Parser WitnessSigningData
pWitnessSigningData =
  KeyWitnessSigningData . File
    <$> parseFilePath "signing-key-file" "Input filepath of the signing key (one or more)."
    <*> optional pByronAddress

pSigningKeyFileIn :: Parser (SigningKeyFile In)
pSigningKeyFileIn =
  File <$> parseFilePath "signing-key-file" "Input filepath of the signing key."

pKesPeriod :: Parser KESPeriod
pKesPeriod =
  fmap KESPeriod $
    Opt.option (bounded "KES_PERIOD") $
      mconcat
        [ Opt.long "kes-period"
        , Opt.metavar "NATURAL"
        , Opt.help "The start of the KES key validity period."
        ]

pEpochNo :: String -> Parser EpochNo
pEpochNo h =
  fmap EpochNo $
    Opt.option (bounded "EPOCH") $
      mconcat
        [ Opt.long "epoch"
        , Opt.metavar "NATURAL"
        , Opt.help h
        ]

pEpochNoUpdateProp :: Parser EpochNo
pEpochNoUpdateProp = pEpochNo "The epoch number in which the update proposal is valid."

pGenesisFile :: String -> Parser GenesisFile
pGenesisFile desc = GenesisFile <$> parseFilePath "genesis" desc

pOperationalCertificateFile :: Parser (File () direction)
pOperationalCertificateFile =
  File <$> parseFilePath "op-cert-file" "Filepath of the node's operational certificate."

pKeyOutputFormat :: Parser KeyOutputFormat
pKeyOutputFormat =
  Opt.option readKeyOutputFormat $
    mconcat
      [ Opt.long "key-output-format"
      , Opt.metavar "STRING"
      , Opt.help $
          mconcat
            [ "Optional key output format. Accepted output formats are \"text-envelope\" "
            , "and \"bech32\" (default is \"text-envelope\")."
            ]
      , Opt.value KeyOutputFormatTextEnvelope
      ]

pPoolIdOutputFormat :: Parser IdOutputFormat
pPoolIdOutputFormat =
  Opt.option readIdOutputFormat $
    mconcat
      [ Opt.long "output-format"
      , Opt.metavar "STRING"
      , Opt.help $
          mconcat
            [ "Optional pool id output format. Accepted output formats are \"hex\" "
            , "and \"bech32\" (default is \"bech32\")."
            ]
      , Opt.value IdOutputFormatBech32
      ]

-- | @pOutputFormatJsonOrText kind@ is a parser to specify in which format
-- to view some data (json or text). @kind@ is the kind of data considered.
pOutputFormatJsonOrText :: String -> Parser OutputFormatJsonOrText
pOutputFormatJsonOrText kind =
  asum
    [ make OutputFormatJson "JSON" "json" (Just " Default format when writing to a file")
    , make OutputFormatText "TEXT" "text" (Just " Default format when writing to stdout")
    ]
 where
  make format desc flag_ extraHelp =
    -- Not using Opt.flag, because there is no default. We can't have
    -- a default and preserve the historical behavior (that differed whether
    -- an output file was specified or not).
    Opt.flag' format $
      mconcat
        [ Opt.help $
            "Format "
              <> kind
              <> " query output to "
              <> desc
              <> "."
              <> fromMaybe "" extraHelp
        , Opt.long ("output-" <> flag_)
        ]

-- | @pTxIdOutputFormatJsonOrText kind@ is a parser to specify in which format
-- to write @transaction txid@'s output on standard output.
pTxIdOutputFormatJsonOrText :: Parser OutputFormatJsonOrText
pTxIdOutputFormatJsonOrText =
  asum [make OutputFormatJson "JSON" "json", make OutputFormatText "TEXT" "text"]
    <|> pure default_
 where
  default_ = OutputFormatText
  make format desc flag_ =
    Opt.flag' format $
      mconcat
        [ Opt.help $ "Format output as " <> desc <> (if format == default_ then " (the default)." else ".")
        , Opt.long ("output-" <> flag_)
        ]

pTxViewOutputFormat :: Parser ViewOutputFormat
pTxViewOutputFormat = pViewOutputFormat "transaction"

pGovernanceActionViewOutputFormat :: Parser ViewOutputFormat
pGovernanceActionViewOutputFormat = pViewOutputFormat "governance action"

pGovernanceVoteViewOutputFormat :: Parser ViewOutputFormat
pGovernanceVoteViewOutputFormat = pViewOutputFormat "governance vote"

-- | @pViewOutputFormat kind@ is a parser to specify in which format
-- to view some data (json or yaml). @what@ is the kind of data considered.
pViewOutputFormat :: String -> Parser ViewOutputFormat
pViewOutputFormat kind =
  asum
    [ make ViewOutputFormatJson "JSON" "json" Nothing
    , make ViewOutputFormatYaml "YAML" "yaml" (Just " Defaults to JSON if unspecified.")
    ]
 where
  make format desc flag_ extraHelp =
    Opt.flag ViewOutputFormatJson format $
      mconcat
        [ Opt.help $
            "Format "
              <> kind
              <> " view output to "
              <> desc
              <> "."
              <> fromMaybe "" extraHelp
        , Opt.long ("output-" <> flag_)
        ]

pMaybeOutputFile :: Parser (Maybe (File content Out))
pMaybeOutputFile =
  optional $
    File <$> parseFilePath "out-file" "Optional output file. Default is to write to stdout."

pVerificationKey
  :: forall keyrole
   . SerialiseAsBech32 (VerificationKey keyrole)
  => AsType keyrole
  -> Parser (VerificationKey keyrole)
pVerificationKey asType =
  Opt.option (readVerificationKey asType) $
    mconcat
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
  File
    <$> parseFilePath
      "extended-verification-key-file"
      "Input filepath of the ed25519-bip32 verification key."

pGenesisVerificationKeyFile :: Parser (VerificationKeyFile In)
pGenesisVerificationKeyFile =
  File <$> parseFilePath "genesis-verification-key-file" "Filepath of the genesis verification key."

pGenesisVerificationKeyHash :: Parser (Hash GenesisKey)
pGenesisVerificationKeyHash =
  Opt.option deserialiseFromHex $
    mconcat
      [ Opt.long "genesis-verification-key-hash"
      , Opt.metavar "STRING"
      , Opt.help "Genesis verification key hash (hex-encoded)."
      ]
 where
  deserialiseFromHex :: ReadM (Hash GenesisKey)
  deserialiseFromHex =
    rHexHash AsGenesisKey (Just "Invalid genesis verification key hash")

pGenesisVerificationKey :: Parser (VerificationKey GenesisKey)
pGenesisVerificationKey =
  Opt.option deserialiseFromHex $
    mconcat
      [ Opt.long "genesis-verification-key"
      , Opt.metavar "STRING"
      , Opt.help "Genesis verification key (hex-encoded)."
      ]
 where
  deserialiseFromHex =
    rVerificationKey AsGenesisKey (Just "Invalid genesis verification key")

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
  File
    <$> parseFilePath
      "genesis-delegate-verification-key-file"
      "Filepath of the genesis delegate verification key."

pGenesisDelegateVerificationKeyHash :: Parser (Hash GenesisDelegateKey)
pGenesisDelegateVerificationKeyHash =
  Opt.option deserialiseFromHex $
    mconcat
      [ Opt.long "genesis-delegate-verification-key-hash"
      , Opt.metavar "STRING"
      , Opt.help "Genesis delegate verification key hash (hex-encoded)."
      ]
 where
  deserialiseFromHex :: ReadM (Hash GenesisDelegateKey)
  deserialiseFromHex =
    rHexHash AsGenesisDelegateKey (Just "Invalid genesis delegate verification key hash")

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
  Opt.option (Opt.eitherReader deserialiseVerKey) $
    mconcat
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
      Left err@(Bech32UnexpectedPrefix _ _) -> Left (docToString $ prettyError err)
      Left err@(Bech32DataPartToBytesError _) -> Left (docToString $ prettyError err)
      Left err@(Bech32DeserialiseFromBytesError _) -> Left (docToString $ prettyError err)
      Left err@(Bech32WrongPrefix _ _) -> Left (docToString $ prettyError err)
      -- The input was not valid Bech32. Attempt to deserialise it as hex.
      Left (Bech32DecodingError _) ->
        first
          (\e -> docToString $ "Invalid stake pool verification key: " <> prettyError e)
          $ deserialiseFromRawBytesHex asType (BSC.pack str)

pKesVerificationKeyFile :: Parser (VerificationKeyFile In)
pKesVerificationKeyFile =
  fmap File $
    asum
      [ parseFilePath "kes-verification-key-file" "Filepath of the hot KES verification key."
      , Opt.strOption $
          mconcat
            [ Opt.long "hot-kes-verification-key-file"
            , Opt.internal
            ]
      ]

pTxSubmitFile :: Parser FilePath
pTxSubmitFile = parseFilePath "tx-file" "Filepath of the transaction you intend to submit."

pTxIn
  :: ShelleyBasedEra era
  -> BalanceTxExecUnits
  -> Parser (TxIn, Maybe PlutusSpend.CliSpendScriptRequirements)
pTxIn sbe balance =
  (,)
    <$> Opt.option
      (readerFromParsecParser parseTxIn)
      ( Opt.long "tx-in"
          <> Opt.metavar "TX-IN"
          <> Opt.help "TxId#TxIx"
      )
    <*> ( optional
            ( pPlutusReferenceSpendScriptWitness balance
                <|> pSimpleReferenceSpendingScriptWitess
                <|> pOnDiskSimpleOrPlutusScriptWitness
            )
        )
 where
  pSimpleReferenceSpendingScriptWitess :: Parser CliSpendScriptRequirements
  pSimpleReferenceSpendingScriptWitess =
    PlutusSpend.createSimpleReferenceScriptFromCliArgs
      <$> pReferenceTxIn "simple-script-" "simple"

  pPlutusReferenceSpendScriptWitness
    :: BalanceTxExecUnits -> Parser CliSpendScriptRequirements
  pPlutusReferenceSpendScriptWitness autoBalanceExecUnits =
    PlutusSpend.createPlutusReferenceScriptFromCliArgs
      <$> pReferenceTxIn "spending-" "plutus"
      <*> pPlutusScriptLanguage "spending-"
      <*> pScriptDatumOrFileSpendingCip69 sbe "spending-reference-tx-in"
      <*> pScriptRedeemerOrFile "spending-reference-tx-in"
      <*> ( case autoBalanceExecUnits of
              AutoBalance -> pure (ExecutionUnits 0 0)
              ManualBalance -> pExecutionUnits "spending-reference-tx-in"
          )

  pOnDiskSimpleOrPlutusScriptWitness :: Parser CliSpendScriptRequirements
  pOnDiskSimpleOrPlutusScriptWitness =
    pSimpleScriptOrPlutusSpendingScriptWitness
      sbe
      balance
      "tx-in"
      (Just "txin")
      "the spending of the transaction input."

pTxInCollateral :: Parser TxIn
pTxInCollateral =
  Opt.option
    (readerFromParsecParser parseTxIn)
    ( Opt.long "tx-in-collateral"
        <> Opt.metavar "TX-IN"
        <> Opt.help "TxId#TxIx"
    )

pReturnCollateral :: Parser TxOutShelleyBasedEra
pReturnCollateral =
  Opt.option
    (readerFromParsecParser parseTxOutShelleyBasedEra)
    ( mconcat
        [ Opt.long "tx-out-return-collateral"
        , Opt.metavar "ADDRESS VALUE"
        , -- TODO alonzo: Update the help text to describe the new syntax as well.
          Opt.help
            ( "The transaction output as ADDRESS VALUE where ADDRESS is "
                <> "the Bech32-encoded address followed by the value in "
                <> "Lovelace. In the situation where your collateral txin "
                <> "over collateralizes the transaction, you can optionally "
                <> "specify a tx out of your choosing to return the excess Lovelace."
            )
        ]
    )
    <*> pure TxOutDatumByNone -- TODO: Babbage era - we should be able to return these
    <*> pure ReferenceScriptAnyEraNone -- TODO: Babbage era - we should be able to return these

pTotalCollateral :: Parser Lovelace
pTotalCollateral =
  Opt.option (L.Coin <$> readerFromParsecParser decimal) $
    mconcat
      [ Opt.long "tx-total-collateral"
      , Opt.metavar "INTEGER"
      , Opt.help $
          mconcat
            [ "The total amount of collateral that will be collected "
            , "as fees in the event of a Plutus script failure. Must be used "
            , "in conjuction with \"--tx-out-return-collateral\"."
            ]
      ]

pWitnessOverride :: Parser Word
pWitnessOverride =
  Opt.option integralReader $
    mconcat
      [ Opt.long "witness-override"
      , Opt.metavar "WORD"
      , Opt.help "Specify and override the number of witnesses the transaction requires."
      ]

pNumberOfShelleyKeyWitnesses :: Parser Int
pNumberOfShelleyKeyWitnesses =
  Opt.option integralReader $
    mconcat
      [ Opt.long "shelley-key-witnesses"
      , Opt.metavar "INT"
      , Opt.help "Specify the number of Shelley key witnesses the transaction requires."
      ]

pNumberOfByronKeyWitnesses :: Parser Int
pNumberOfByronKeyWitnesses =
  Opt.option integralReader $
    mconcat
      [ Opt.long "byron-key-witnesses"
      , Opt.metavar "Int"
      , Opt.help "Specify the number of Byron key witnesses the transaction requires."
      ]

pTotalUTxOValue :: Parser Value
pTotalUTxOValue =
  Opt.option (readerFromParsecParser $ parseValue RoleUTxO) $
    mconcat
      [ Opt.long "total-utxo-value"
      , Opt.metavar "VALUE"
      , Opt.help "The total value of the UTxO that exists at the tx inputs being spent."
      ]

pTxOut :: Parser TxOutAnyEra
pTxOut =
  Opt.option
    (readerFromParsecParser parseTxOutAnyEra)
    ( Opt.long "tx-out"
        <> Opt.metavar "ADDRESS VALUE"
        -- TODO alonzo: Update the help text to describe the new syntax as well.
        <> Opt.help
          "The transaction output as ADDRESS VALUE where ADDRESS is \
          \the Bech32-encoded address followed by the value in \
          \the multi-asset syntax (including simply Lovelace)."
    )
    <*> pTxOutDatum
    <*> pRefScriptFp

pTxOutShelleyBased :: Parser TxOutShelleyBasedEra
pTxOutShelleyBased =
  Opt.option
    (readerFromParsecParser parseTxOutShelleyBasedEra)
    ( Opt.long "tx-out"
        <> Opt.metavar "ADDRESS VALUE"
        -- TODO alonzo: Update the help text to describe the new syntax as well.
        <> Opt.help
          "The transaction output as ADDRESS VALUE where ADDRESS is \
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
    fmap TxOutDatumByHashOnly $
      Opt.option (readerFromParsecParser $ parseHash (AsHash AsScriptData)) $
        mconcat
          [ Opt.long "tx-out-datum-hash"
          , Opt.metavar "HASH"
          , Opt.help $
              mconcat
                [ "The script datum hash for this tx output, as "
                , "the raw datum hash (in hex)."
                ]
          ]

  pTxOutDatumByHashOf =
    TxOutDatumByHashOf
      <$> pScriptDataOrFile
        "tx-out-datum-hash"
        "The script datum hash for this tx output, by hashing the script datum given here."
        "The script datum hash for this tx output, by hashing the script datum in the file."

  pTxOutDatumByValue =
    TxOutDatumByValue
      <$> pScriptDataOrFile
        "tx-out-datum-embed"
        "The script datum to embed in the tx for this output, given here."
        "The script datum to embed in the tx for this output, in the given file."

  pTxOutInlineDatumByValue =
    TxOutInlineDatumByValue
      <$> pScriptDataOrFile
        "tx-out-inline-datum"
        "The script datum to embed in the tx output as an inline datum, given here."
        "The script datum to embed in the tx output as an inline datum, in the given file."

pRefScriptFp :: Parser ReferenceScriptAnyEra
pRefScriptFp =
  ReferenceScriptAnyEra
    <$> parseFilePath "tx-out-reference-script-file" "Reference script input file."
      <|> pure ReferenceScriptAnyEraNone

pMintMultiAsset
  :: ShelleyBasedEra era
  -> BalanceTxExecUnits
  -> Parser (Value, [CliMintScriptRequirements])
pMintMultiAsset sbe balanceExecUnits =
  (,)
    <$> Opt.option
      (readerFromParsecParser $ parseValue RoleMint)
      ( Opt.long "mint"
          <> Opt.metavar "VALUE"
          <> Opt.help helpText
      )
    <*> some
      ( pMintingScript
          <|> pSimpleReferenceMintingScriptWitness
          <|> pPlutusMintReferenceScriptWitnessFiles balanceExecUnits
      )
 where
  pMintingScript :: Parser CliMintScriptRequirements
  pMintingScript =
    createSimpleOrPlutusScriptFromCliArgs
      <$> pMintScriptFile
      <*> optional (pPlutusMintScriptWitnessData sbe WitCtxMint balanceExecUnits)

  pSimpleReferenceMintingScriptWitness :: Parser CliMintScriptRequirements
  pSimpleReferenceMintingScriptWitness =
    createSimpleReferenceScriptFromCliArgs
      <$> pReferenceTxIn "simple-minting-script-" "simple"
      <*> pPolicyId

  pPlutusMintReferenceScriptWitnessFiles
    :: BalanceTxExecUnits -> Parser CliMintScriptRequirements
  pPlutusMintReferenceScriptWitnessFiles autoBalanceExecUnits =
    createPlutusReferenceScriptFromCliArgs
      <$> pReferenceTxIn "mint-" "plutus"
      <*> pPlutusScriptLanguage "mint-"
      <*> pScriptRedeemerOrFile "mint-reference-tx-in"
      <*> ( case autoBalanceExecUnits of
              AutoBalance -> pure (ExecutionUnits 0 0)
              ManualBalance -> pExecutionUnits "mint-reference-tx-in"
          )
      <*> pPolicyId

  helpText =
    mconcat
      [ "Mint multi-asset value(s) with the multi-asset cli syntax. "
      , "You must specify a script witness."
      ]

pPolicyId :: Parser PolicyId
pPolicyId =
  Opt.option (readerFromParsecParser parsePolicyId) $
    mconcat
      [ Opt.long "policy-id"
      , Opt.metavar "HASH"
      , Opt.help "Policy id of minting script."
      ]

pInvalidBefore :: Parser SlotNo
pInvalidBefore =
  fmap SlotNo $
    asum
      [ Opt.option (bounded "SLOT") $
          mconcat
            [ Opt.long "invalid-before"
            , Opt.metavar "SLOT"
            , Opt.help "Time that transaction is valid from (in slots)."
            ]
      , Opt.option (bounded "SLOT") $
          mconcat
            [ Opt.long "lower-bound"
            , Opt.metavar "SLOT"
            , Opt.help $
                mconcat
                  [ "Time that transaction is valid from (in slots) "
                  , "(deprecated; use --invalid-before instead)."
                  ]
            , Opt.internal
            ]
      ]

pLegacyInvalidHereafter :: Parser SlotNo
pLegacyInvalidHereafter =
  fmap SlotNo $
    asum
      [ Opt.option (bounded "SLOT") $
          mconcat
            [ Opt.long "invalid-hereafter"
            , Opt.metavar "SLOT"
            , Opt.help "Time that transaction is valid until (in slots)."
            ]
      , Opt.option (bounded "SLOT") $
          mconcat
            [ Opt.long "upper-bound"
            , Opt.metavar "SLOT"
            , Opt.help $
                mconcat
                  [ "Time that transaction is valid until (in slots) "
                  , "(deprecated; use --invalid-hereafter instead)."
                  ]
            , Opt.internal
            ]
      , Opt.option (bounded "SLOT") $
          mconcat
            [ Opt.long "ttl"
            , Opt.metavar "SLOT"
            , Opt.help "Time to live (in slots) (deprecated; use --invalid-hereafter instead)."
            , Opt.internal
            ]
      ]

pInvalidHereafter
  :: ()
  => ShelleyBasedEra era
  -> Parser (TxValidityUpperBound era)
pInvalidHereafter eon =
  fmap (TxValidityUpperBound eon) $
    asum
      [ fmap (Just . SlotNo) $
          Opt.option (bounded "SLOT") $
            mconcat
              [ Opt.long "invalid-hereafter"
              , Opt.metavar "SLOT"
              , Opt.help "Time that transaction is valid until (in slots)."
              ]
      , fmap (Just . SlotNo) $
          Opt.option (bounded "SLOT") $
            mconcat
              [ Opt.long "upper-bound"
              , Opt.metavar "SLOT"
              , Opt.help $
                  mconcat
                    [ "Time that transaction is valid until (in slots) "
                    , "(deprecated; use --invalid-hereafter instead)."
                    ]
              , Opt.internal
              ]
      , fmap (Just . SlotNo) $
          Opt.option (bounded "SLOT") $
            mconcat
              [ Opt.long "ttl"
              , Opt.metavar "SLOT"
              , Opt.help "Time to live (in slots) (deprecated; use --invalid-hereafter instead)."
              , Opt.internal
              ]
      , pure Nothing
      ]

pTxFee :: Parser Lovelace
pTxFee =
  fmap (L.Coin . (fromIntegral :: Natural -> Integer)) $
    Opt.option integralReader $
      mconcat
        [ Opt.long "fee"
        , Opt.metavar "LOVELACE"
        , Opt.help "The fee amount in Lovelace."
        ]

pWitnessFile :: Parser WitnessFile
pWitnessFile = WitnessFile <$> parseFilePath "witness-file" "Filepath of the witness"

pTxBodyFileIn :: Parser (TxBodyFile In)
pTxBodyFileIn = File <$> parseFilePath "tx-body-file" "Input filepath of the JSON TxBody."

pTxBodyFileOut :: Parser (TxBodyFile Out)
pTxBodyFileOut =
  fmap File $
    asum
      [ parseFilePath "out-file" "Output filepath of the JSON TxBody."
      , Opt.strOption $
          mconcat
            [ Opt.long "tx-body-file"
            , Opt.internal
            ]
      ]

pTxFileIn :: Parser (TxFile In)
pTxFileIn = File <$> parseFilePath "tx-file" "Input filepath of the JSON Tx."

pTxFileOut :: Parser (TxFile Out)
pTxFileOut =
  File
    <$> asum
      [ parseFilePath "out-file" "Output filepath of the JSON Tx."
      , Opt.strOption $
          mconcat
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

pTxInCountDeprecated :: Parser TxInCount
pTxInCountDeprecated =
  fmap TxInCount $
    Opt.option integralReader $
      mconcat
        [ Opt.long "tx-in-count"
        , Opt.metavar "NATURAL"
        , Opt.help "DEPRECATED. This argument has no effect."
        ]

pTxOutCountDeprecated :: Parser TxOutCount
pTxOutCountDeprecated =
  fmap TxOutCount $
    Opt.option integralReader $
      mconcat
        [ Opt.long "tx-out-count"
        , Opt.metavar "NATURAL"
        , Opt.help "DEPRECATED. This argument has no effect."
        ]

pTxShelleyWitnessCount :: Parser TxShelleyWitnessCount
pTxShelleyWitnessCount =
  fmap TxShelleyWitnessCount $
    Opt.option integralReader $
      mconcat
        [ Opt.long "witness-count"
        , Opt.metavar "NATURAL"
        , Opt.help "The number of Shelley key witnesses."
        ]

pTxByronWitnessCount :: Parser TxByronWitnessCount
pTxByronWitnessCount =
  fmap TxByronWitnessCount $
    Opt.option integralReader $
      mconcat
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
    Opt.flag' QueryUTxOWhole $
      mconcat
        [ Opt.long "whole-utxo"
        , Opt.help "Return the whole UTxO (only appropriate on small testnets)."
        ]

  pQueryUTxOByAddress :: Parser QueryUTxOFilter
  pQueryUTxOByAddress = QueryUTxOByAddress . fromList <$> some pByAddress

  pByAddress :: Parser AddressAny
  pByAddress =
    Opt.option (readerFromParsecParser parseAddressAny) $
      mconcat
        [ Opt.long "address"
        , Opt.metavar "ADDRESS"
        , Opt.help "Filter by Cardano address(es) (Bech32-encoded)."
        ]

  pQueryUTxOByTxIn :: Parser QueryUTxOFilter
  pQueryUTxOByTxIn = QueryUTxOByTxIn . fromList <$> some pByTxIn

  pByTxIn :: Parser TxIn
  pByTxIn =
    Opt.option (readerFromParsecParser parseTxIn) $
      mconcat
        [ Opt.long "tx-in"
        , Opt.metavar "TX-IN"
        , Opt.help "Filter by transaction input (TxId#TxIx)."
        ]

pFilterByStakeAddress :: Parser StakeAddress
pFilterByStakeAddress =
  Opt.option (readerFromParsecParser parseStakeAddress) $
    mconcat
      [ Opt.long "address"
      , Opt.metavar "ADDRESS"
      , Opt.help "Filter by Cardano stake address (Bech32-encoded)."
      ]

pByronAddress :: Parser (Address ByronAddr)
pByronAddress =
  Opt.option (Opt.eitherReader deserialise) $
    mconcat
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
  fmap Text.pack $
    Opt.strOption $
      mconcat
        [ Opt.long "address"
        , Opt.metavar "ADDRESS"
        , Opt.help "A Cardano address"
        ]

-- | First argument is the prefix for the option's flag to use
pStakePoolVerificationKeyHash :: Maybe String -> Parser (Hash StakePoolKey)
pStakePoolVerificationKeyHash prefix =
  Opt.option (rBech32KeyHash AsStakePoolKey <|> rHexHash AsStakePoolKey Nothing) $
    mconcat
      [ Opt.long $ prefixFlag prefix "stake-pool-id"
      , Opt.metavar "STAKE_POOL_ID"
      , Opt.help
          "Stake pool ID/verification key hash (either Bech32-encoded or hex-encoded)."
      ]

pVrfVerificationKeyFile :: Parser (VerificationKeyFile In)
pVrfVerificationKeyFile =
  File <$> parseFilePath "vrf-verification-key-file" "Filepath of the VRF verification key."

pVrfVerificationKeyHash :: Parser (Hash VrfKey)
pVrfVerificationKeyHash =
  Opt.option deserialiseFromHex $
    mconcat
      [ Opt.long "vrf-verification-key-hash"
      , Opt.metavar "STRING"
      , Opt.help "VRF verification key hash (hex-encoded)."
      ]
 where
  deserialiseFromHex :: ReadM (Hash VrfKey)
  deserialiseFromHex =
    rHexHash AsVrfKey (Just "Invalid VRF verification key hash")

pVrfVerificationKey :: Parser (VerificationKey VrfKey)
pVrfVerificationKey =
  Opt.option (readVerificationKey AsVrfKey) $
    mconcat
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
  File
    <$> asum
      [ parseFilePath
          "pool-reward-account-verification-key-file"
          "Filepath of the reward account stake verification key."
      , Opt.strOption $
          mconcat
            [ Opt.long "reward-account-verification-key-file"
            , Opt.internal
            ]
      ]

pRewardAcctVerificationKey :: Parser (VerificationKey StakeKey)
pRewardAcctVerificationKey =
  Opt.option (readVerificationKey AsStakeKey) $
    mconcat
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
  File
    <$> asum
      [ parseFilePath
          "pool-owner-stake-verification-key-file"
          "Filepath of the pool owner stake verification key."
      , Opt.strOption $
          mconcat
            [ Opt.long "pool-owner-staking-verification-key"
            , Opt.internal
            ]
      ]

pPoolOwnerVerificationKey :: Parser (VerificationKey StakeKey)
pPoolOwnerVerificationKey =
  Opt.option (readVerificationKey AsStakeKey) $
    mconcat
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
  Opt.option (readerFromParsecParser parseLovelace) $
    mconcat
      [ Opt.long "pool-pledge"
      , Opt.metavar "LOVELACE"
      , Opt.help "The stake pool's pledge."
      ]

pPoolCost :: Parser Lovelace
pPoolCost =
  Opt.option (readerFromParsecParser parseLovelace) $
    mconcat
      [ Opt.long "pool-cost"
      , Opt.metavar "LOVELACE"
      , Opt.help "The stake pool's cost."
      ]

pRational :: String -> String -> Parser Rational
pRational opt h =
  Opt.option readRationalUnitInterval $
    mconcat
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
    Opt.option (Opt.eitherReader eDNSName) $
      mconcat
        [ Opt.long "multi-host-pool-relay"
        , Opt.metavar "STRING"
        , Opt.help "The stake pool relay's DNS name that corresponds to an SRV DNS record"
        ]

pSingleHostName :: Parser StakePoolRelay
pSingleHostName =
  StakePoolRelayDnsARecord <$> pDNSName <*> optional pPort
 where
  pDNSName :: Parser ByteString
  pDNSName =
    Opt.option (Opt.eitherReader eDNSName) $
      mconcat
        [ Opt.long "single-host-pool-relay"
        , Opt.metavar "STRING"
        , Opt.help $
            mconcat
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
  Opt.option (Opt.maybeReader readMaybe :: Opt.ReadM IP.IPv4) $
    mconcat
      [ Opt.long "pool-relay-ipv4"
      , Opt.metavar "STRING"
      , Opt.help "The stake pool relay's IPv4 address"
      ]

pIpV6 :: Parser IP.IPv6
pIpV6 =
  Opt.option (Opt.maybeReader readMaybe :: Opt.ReadM IP.IPv6) $
    mconcat
      [ Opt.long "pool-relay-ipv6"
      , Opt.metavar "STRING"
      , Opt.help "The stake pool relay's IPv6 address"
      ]

pPort :: Parser PortNumber
pPort =
  Opt.option (fromInteger <$> Opt.eitherReader readEither) $
    mconcat
      [ Opt.long "pool-relay-port"
      , Opt.metavar "INT"
      , Opt.help "The stake pool relay's port"
      ]

pStakePoolMetadataReference :: Parser StakePoolMetadataReference
pStakePoolMetadataReference =
  StakePoolMetadataReference
    <$> pStakePoolMetadataUrl
    <*> pStakePoolMetadataHash

pStakePoolMetadataUrl :: Parser Text
pStakePoolMetadataUrl =
  Opt.option (readURIOfMaxLength 64) $
    mconcat
      [ Opt.long "metadata-url"
      , Opt.metavar "URL"
      , Opt.help "Pool metadata URL (maximum length of 64 characters)."
      ]

pStakePoolMetadataHash :: Parser (Hash StakePoolMetadata)
pStakePoolMetadataHash =
  Opt.option deserializeFromHex $
    mconcat
      [ Opt.long "metadata-hash"
      , Opt.metavar "HASH"
      , Opt.help "Pool metadata hash."
      ]
 where
  deserializeFromHex :: ReadM (Hash StakePoolMetadata)
  deserializeFromHex =
    rHexHash AsStakePoolMetadata Nothing

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
    <*> optional
      ( pPotentiallyCheckedAnchorData
          pMustCheckStakeMetadataHash
          pStakePoolMetadataReference
      )
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
  Opt.strOption $
    mconcat
      [ Opt.long "cost-model-file"
      , Opt.metavar "FILE"
      , Opt.help "Filepath of the JSON formatted cost model"
      , Opt.completer (Opt.bashCompleter "file")
      ]

pMinFeePerByteFactor :: Parser Lovelace
pMinFeePerByteFactor =
  Opt.option (readerFromParsecParser parseLovelace) $
    mconcat
      [ Opt.long "min-fee-linear"
      , Opt.metavar "LOVELACE"
      , Opt.help "The linear factor per byte for the minimum fee calculation."
      ]

pMinFeeConstantFactor :: Parser Lovelace
pMinFeeConstantFactor =
  Opt.option (readerFromParsecParser parseLovelace) $
    mconcat
      [ Opt.long "min-fee-constant"
      , Opt.metavar "LOVELACE"
      , Opt.help "The constant factor for the minimum fee calculation."
      ]

pMinUTxOValue :: Parser Lovelace
pMinUTxOValue =
  Opt.option (readerFromParsecParser parseLovelace) $
    mconcat
      [ Opt.long "min-utxo-value"
      , Opt.metavar "NATURAL"
      , Opt.help "The minimum allowed UTxO value (Shelley to Mary eras)."
      ]

pMinPoolCost :: Parser Lovelace
pMinPoolCost =
  Opt.option (readerFromParsecParser parseLovelace) $
    mconcat
      [ Opt.long "min-pool-cost"
      , Opt.metavar "NATURAL"
      , Opt.help "The minimum allowed cost parameter for stake pools."
      ]

pMaxBodySize :: Parser Word32
pMaxBodySize =
  Opt.option integralReader $
    mconcat
      [ Opt.long "max-block-body-size"
      , Opt.metavar "WORD32"
      , Opt.help "Maximal block body size."
      ]

pMaxTransactionSize :: Parser Word32
pMaxTransactionSize =
  Opt.option integralReader $
    mconcat
      [ Opt.long "max-tx-size"
      , Opt.metavar "WORD32"
      , Opt.help "Maximum transaction size."
      ]

-- | A parser for @(Int, Int)@-like expressions. In other words, 'integralReader'-lifted
-- to a pairs with a Haskell-like syntax.
pairIntegralReader :: (Typeable a, Integral a, Bits a) => ReadM (a, a)
pairIntegralReader = readerFromParsecParser pairIntegralParsecParser

pairIntegralParsecParser :: (Typeable a, Integral a, Bits a) => Parsec.Parser (a, a)
pairIntegralParsecParser = do
  Parsec.spaces -- Skip initial spaces
  void $ Parsec.char '('
  Parsec.spaces -- Skip spaces between opening paren and lhs
  lhs :: a <- integralParsecParser
  Parsec.spaces -- Skip spaces between lhs and comma
  void $ Parsec.char ','
  Parsec.spaces -- Skip spaces between comma and rhs
  rhs :: a <- integralParsecParser
  Parsec.spaces -- Skip spaces between comma and closing paren
  void $ Parsec.char ')'
  Parsec.spaces -- Skip trailing spaces
  return (lhs, rhs)

-- | @integralReader@ is a reader for a word of type @a@. When it fails
-- parsing, it provides a nice error message. This custom reader is needed
-- to avoid the overflow issues of 'Opt.auto' described in https://github.com/IntersectMBO/cardano-cli/issues/860.
integralReader :: (Typeable a, Integral a, Bits a) => ReadM a
integralReader = readerFromParsecParser integralParsecParser

integralParsecParser :: forall a. (Typeable a, Integral a, Bits a) => Parsec.Parser a
integralParsecParser = do
  i <- decimal
  case toIntegralSized i of
    Nothing -> fail $ "Cannot parse " <> show i <> " as a " <> typeName
    Just n -> return n
 where
  typeName = show $ typeRep (Proxy @a)

pMaxBlockHeaderSize :: Parser Word16
pMaxBlockHeaderSize =
  Opt.option integralReader $
    mconcat
      [ Opt.long "max-block-header-size"
      , Opt.metavar "WORD16"
      , Opt.help "Maximum block header size."
      ]

pKeyRegistDeposit :: Parser Lovelace
pKeyRegistDeposit =
  Opt.option (readerFromParsecParser parseLovelace) $
    mconcat
      [ Opt.long "key-reg-deposit-amt"
      , Opt.metavar "NATURAL"
      , Opt.help "Key registration deposit amount."
      ]

pDrepDeposit :: Parser Lovelace
pDrepDeposit =
  Opt.option (readerFromParsecParser parseLovelace) $
    mconcat
      [ Opt.long "deposit-amt"
      , Opt.metavar "LOVELACE"
      , Opt.help "DRep deposit amount (same at registration and retirement)."
      ]

pPoolDeposit :: Parser Lovelace
pPoolDeposit =
  Opt.option (readerFromParsecParser parseLovelace) $
    mconcat
      [ Opt.long "pool-reg-deposit"
      , Opt.metavar "NATURAL"
      , Opt.help "The amount of a pool registration deposit."
      ]

pEpochBoundRetirement :: Parser L.EpochInterval
pEpochBoundRetirement =
  fmap L.EpochInterval $
    asum
      [ Opt.option (bounded "EPOCH_INTERVAL") $
          mconcat
            [ Opt.long "pool-retirement-epoch-interval"
            , Opt.metavar "WORD32"
            , Opt.help "Epoch interval of pool retirement."
            ]
      , Opt.option (bounded "EPOCH_BOUNDARY") $
          mconcat
            [ Opt.long "pool-retirement-epoch-boundary"
            , Opt.internal
            ]
      ]

pNumberOfPools :: Parser Word16
pNumberOfPools =
  Opt.option integralReader $
    mconcat
      [ Opt.long "number-of-pools"
      , Opt.metavar "WORD16"
      , Opt.help "Desired number of pools."
      ]

pPoolInfluence :: Parser Rational
pPoolInfluence =
  Opt.option readRational $
    mconcat
      [ Opt.long "pool-influence"
      , Opt.metavar "RATIONAL"
      , Opt.help "Pool influence."
      ]

pTreasuryExpansion :: Parser Rational
pTreasuryExpansion =
  Opt.option readRationalUnitInterval $
    mconcat
      [ Opt.long "treasury-expansion"
      , Opt.metavar "RATIONAL"
      , Opt.help "Treasury expansion."
      ]

pMonetaryExpansion :: Parser Rational
pMonetaryExpansion =
  Opt.option readRationalUnitInterval $
    mconcat
      [ Opt.long "monetary-expansion"
      , Opt.metavar "RATIONAL"
      , Opt.help "Monetary expansion."
      ]

pDecentralParam :: Parser Rational
pDecentralParam =
  Opt.option readRationalUnitInterval $
    mconcat
      [ Opt.long "decentralization-parameter"
      , Opt.metavar "RATIONAL"
      , Opt.help "Decentralization parameter."
      ]

pExtraEntropy :: Parser (Maybe PraosNonce)
pExtraEntropy =
  asum
    [ Opt.option (Just <$> readerFromParsecParser parsePraosNonce) $
        mconcat
          [ Opt.long "extra-entropy"
          , Opt.metavar "HEX"
          , Opt.help "Praos extra entropy seed, as a hex byte string."
          ]
    , Opt.flag' Nothing $
        mconcat
          [ Opt.long "reset-extra-entropy"
          , Opt.help "Reset the Praos extra entropy to none."
          ]
    ]
 where
  parsePraosNonce :: Parsec.Parser PraosNonce
  parsePraosNonce = makePraosNonce <$> parseEntropyBytes

  parseEntropyBytes :: Parsec.Parser ByteString
  parseEntropyBytes =
    either fail return
      . B16.decode
      . BSC.pack
      =<< some Parsec.hexDigit

pUTxOCostPerByte :: Parser Lovelace
pUTxOCostPerByte =
  Opt.option (readerFromParsecParser parseLovelace) $
    mconcat
      [ Opt.long "utxo-cost-per-byte"
      , Opt.metavar "LOVELACE"
      , Opt.help "Cost in lovelace per unit of UTxO storage (from Babbage era)."
      ]

pExecutionUnitPrices :: Parser ExecutionUnitPrices
pExecutionUnitPrices =
  ExecutionUnitPrices
    <$> Opt.option
      readRational
      ( mconcat
          [ Opt.long "price-execution-steps"
          , Opt.metavar "RATIONAL"
          , Opt.help $
              mconcat
                [ "Step price of execution units for script languages that use "
                , "them (from Alonzo era).  (Examples: '1.1', '11/10')"
                ]
          ]
      )
    <*> Opt.option
      readRational
      ( mconcat
          [ Opt.long "price-execution-memory"
          , Opt.metavar "RATIONAL"
          , Opt.help $
              mconcat
                [ "Memory price of execution units for script languages that "
                , "use them (from Alonzo era).  (Examples: '1.1', '11/10')"
                ]
          ]
      )

pMaxTxExecutionUnits :: Parser ExecutionUnits
pMaxTxExecutionUnits =
  uncurry ExecutionUnits
    <$> Opt.option
      pairIntegralReader
      ( mconcat
          [ Opt.long "max-tx-execution-units"
          , Opt.metavar "(INT, INT)"
          , Opt.help $
              mconcat
                [ "Max total script execution resources units allowed per tx "
                , "(from Alonzo era). They are denominated as follows (steps, memory)."
                ]
          ]
      )

pMaxBlockExecutionUnits :: Parser ExecutionUnits
pMaxBlockExecutionUnits =
  uncurry ExecutionUnits
    <$> Opt.option
      pairIntegralReader
      ( mconcat
          [ Opt.long "max-block-execution-units"
          , Opt.metavar "(INT, INT)"
          , Opt.help $
              mconcat
                [ "Max total script execution resources units allowed per block "
                , "(from Alonzo era). They are denominated as follows (steps, memory)."
                ]
          ]
      )

pMaxValueSize :: Parser Natural
pMaxValueSize =
  Opt.option integralReader $
    mconcat
      [ Opt.long "max-value-size"
      , Opt.metavar "INT"
      , Opt.help $
          mconcat
            [ "Max size of a multi-asset value in a tx output (from Alonzo era)."
            ]
      ]

pCollateralPercent :: Parser Natural
pCollateralPercent =
  Opt.option integralReader $
    mconcat
      [ Opt.long "collateral-percent"
      , Opt.metavar "INT"
      , Opt.help $
          mconcat
            [ "The percentage of the script contribution to the txfee that "
            , "must be provided as collateral inputs when including Plutus "
            , "scripts (from Alonzo era)."
            ]
      ]

pMaxCollateralInputs :: Parser Natural
pMaxCollateralInputs =
  Opt.option integralReader $
    mconcat
      [ Opt.long "max-collateral-inputs"
      , Opt.metavar "INT"
      , Opt.help $
          mconcat
            [ "The maximum number of collateral inputs allowed in a "
            , "transaction (from Alonzo era)."
            ]
      ]

pProtocolVersion :: Parser (Natural, Natural)
pProtocolVersion =
  (,) <$> pProtocolMajorVersion <*> pProtocolMinorVersion
 where
  pProtocolMajorVersion =
    Opt.option integralReader $
      mconcat
        [ Opt.long "protocol-major-version"
        , Opt.metavar "MAJOR"
        , Opt.help $
            mconcat
              [ "Specify the major protocol version to fork into. An increase indicates a hard fork. "
              , "It must be the next natural number after the current version and must be supported by the node."
              ]
        ]
  pProtocolMinorVersion =
    Opt.option integralReader $
      mconcat
        [ Opt.long "protocol-minor-version"
        , Opt.metavar "MINOR"
        , Opt.help $
            mconcat
              [ "Minor protocol version. An increase indicates a soft fork "
              , "(old software can validate but not produce new blocks). "
              , "Must be zero when the major protocol version is increased."
              ]
        ]

pPoolVotingThresholds :: Parser L.PoolVotingThresholds
pPoolVotingThresholds =
  L.PoolVotingThresholds
    <$> pMotionNoConfidence
    <*> pCommitteeNormal
    <*> pCommitteeNoConfidence
    <*> pHardForkInitiation
    <*> pPPSecurityGroup
 where
  pMotionNoConfidence =
    Opt.option (toUnitIntervalOrErr <$> readRationalUnitInterval) $
      mconcat
        [ Opt.long "pool-voting-threshold-motion-no-confidence"
        , Opt.metavar "RATIONAL"
        , Opt.help "Acceptance threshold for stake pool votes on motions no confidence."
        ]
  pCommitteeNormal =
    Opt.option (toUnitIntervalOrErr <$> readRationalUnitInterval) $
      mconcat
        [ Opt.long "pool-voting-threshold-committee-normal"
        , Opt.metavar "RATIONAL"
        , Opt.help "Acceptance threshold for stake pool votes on normal committee updates."
        ]
  pCommitteeNoConfidence =
    Opt.option (toUnitIntervalOrErr <$> readRationalUnitInterval) $
      mconcat
        [ Opt.long "pool-voting-threshold-committee-no-confidence"
        , Opt.metavar "RATIONAL"
        , Opt.help
            "Acceptance threshold for stake pool votes on committee updates when the committee is in a state of no confidence."
        ]
  pHardForkInitiation =
    Opt.option (toUnitIntervalOrErr <$> readRationalUnitInterval) $
      mconcat
        [ Opt.long "pool-voting-threshold-hard-fork-initiation"
        , Opt.metavar "RATIONAL"
        , Opt.help "Acceptance threshold for stake pool votes on hard fork initiations."
        ]
  pPPSecurityGroup =
    Opt.option (toUnitIntervalOrErr <$> readRationalUnitInterval) $
      mconcat
        [ Opt.long "pool-voting-threshold-pp-security-group"
        , Opt.metavar "RATIONAL"
        , Opt.help
            "Acceptance threshold for stake pool votes on protocol parameters for parameters in the 'security' group."
        ]

pDRepVotingThresholds :: Parser L.DRepVotingThresholds
pDRepVotingThresholds =
  L.DRepVotingThresholds
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
    Opt.option (toUnitIntervalOrErr <$> readRationalUnitInterval) $
      mconcat
        [ Opt.long "drep-voting-threshold-motion-no-confidence"
        , Opt.metavar "RATIONAL"
        , Opt.help "Acceptance threshold for DRep votes on motions of no confidence."
        ]
  pCommitteeNormal =
    Opt.option (toUnitIntervalOrErr <$> readRationalUnitInterval) $
      mconcat
        [ Opt.long "drep-voting-threshold-committee-normal"
        , Opt.metavar "RATIONAL"
        , Opt.help "Acceptance threshold for DRep votes on normal committee updates."
        ]
  pCommitteeNoConfidence =
    Opt.option (toUnitIntervalOrErr <$> readRationalUnitInterval) $
      mconcat
        [ Opt.long "drep-voting-threshold-committee-no-confidence"
        , Opt.metavar "RATIONAL"
        , Opt.help
            "Acceptance threshold for DRep votes on committee updates when the committee is in a state of no confidence."
        ]
  pUpdateToConstitution =
    Opt.option (toUnitIntervalOrErr <$> readRationalUnitInterval) $
      mconcat
        [ Opt.long "drep-voting-threshold-update-to-constitution"
        , Opt.metavar "RATIONAL"
        , Opt.help "Acceptance threshold for DRep votes on constitution updates."
        ]
  pHardForkInitiation =
    Opt.option (toUnitIntervalOrErr <$> readRationalUnitInterval) $
      mconcat
        [ Opt.long "drep-voting-threshold-hard-fork-initiation"
        , Opt.metavar "RATIONAL"
        , Opt.help "Acceptance threshold for DRep votes on hard fork initiations."
        ]
  pPPNetworkGroup =
    Opt.option (toUnitIntervalOrErr <$> readRationalUnitInterval) $
      mconcat
        [ Opt.long "drep-voting-threshold-pp-network-group"
        , Opt.metavar "RATIONAL"
        , Opt.help
            "Acceptance threshold for DRep votes on protocol parameters for parameters in the 'network' group."
        ]
  pPPEconomicGroup =
    Opt.option (toUnitIntervalOrErr <$> readRationalUnitInterval) $
      mconcat
        [ Opt.long "drep-voting-threshold-pp-economic-group"
        , Opt.metavar "RATIONAL"
        , Opt.help
            "Acceptance threshold for DRep votes on protocol parameters for parameters in the 'economic' group."
        ]
  pPPTechnicalGroup =
    Opt.option (toUnitIntervalOrErr <$> readRationalUnitInterval) $
      mconcat
        [ Opt.long "drep-voting-threshold-pp-technical-group"
        , Opt.metavar "RATIONAL"
        , Opt.help
            "Acceptance threshold for DRep votes on protocol parameters for parameters in the 'technical' group."
        ]
  pPPGovGroup =
    Opt.option (toUnitIntervalOrErr <$> readRationalUnitInterval) $
      mconcat
        [ Opt.long "drep-voting-threshold-pp-governance-group"
        , Opt.metavar "RATIONAL"
        , Opt.help
            "Acceptance threshold for DRep votes on protocol parameters for parameters in the 'governance' group."
        ]
  pTreasuryWithdrawal =
    Opt.option (toUnitIntervalOrErr <$> readRationalUnitInterval) $
      mconcat
        [ Opt.long "drep-voting-threshold-treasury-withdrawal"
        , Opt.metavar "RATIONAL"
        , Opt.help "Acceptance threshold for DRep votes on treasury withdrawals."
        ]

pMinCommitteeSize :: Parser Natural
pMinCommitteeSize =
  Opt.option integralReader $
    mconcat
      [ Opt.long "min-committee-size"
      , Opt.metavar "INT"
      , Opt.help "Minimal size of the constitutional committee."
      ]

pCommitteeTermLength :: Parser L.EpochInterval
pCommitteeTermLength =
  fmap L.EpochInterval $
    Opt.option (bounded "EPOCH_INTERVAL") $
      mconcat
        [ Opt.long "committee-term-length"
        , Opt.metavar "WORD32"
        , Opt.help "Maximal term length for members of the constitutional committee, in epochs."
        ]

pGovActionLifetime :: Parser L.EpochInterval
pGovActionLifetime =
  fmap L.EpochInterval $
    Opt.option (bounded "EPOCH_INTERVAL") $
      mconcat
        [ Opt.long "governance-action-lifetime"
        , Opt.metavar "WORD32"
        , Opt.help "Maximal lifetime of governance actions, in epochs."
        ]

pDRepDeposit :: Parser Lovelace
pDRepDeposit =
  Opt.option (readerFromParsecParser parseLovelace) $
    mconcat
      [ Opt.long "drep-deposit"
      , Opt.metavar "LOVELACE"
      , Opt.help "DRep deposit amount."
      ]

pDRepActivity :: Parser L.EpochInterval
pDRepActivity =
  fmap L.EpochInterval $
    Opt.option (bounded "EPOCH_INTERVAL") $
      mconcat
        [ Opt.long "drep-activity"
        , Opt.metavar "WORD32"
        , Opt.help "DRep activity period, in epochs."
        ]

parseTxOutShelleyBasedEra
  :: Parsec.Parser (TxOutDatumAnyEra -> ReferenceScriptAnyEra -> TxOutShelleyBasedEra)
parseTxOutShelleyBasedEra = do
  addr <- parseShelleyAddress
  Parsec.spaces
  -- Accept the old style of separating the address and value in a
  -- transaction output:
  Parsec.option () (Parsec.char '+' >> Parsec.spaces)
  val <- parseValue RoleUTxO -- UTxO role works for transaction output
  return (TxOutShelleyBasedEra addr val)

parseShelleyAddress :: Parsec.Parser (Address ShelleyAddr)
parseShelleyAddress = do
  str <- lexPlausibleAddressString
  case deserialiseAddress AsShelleyAddress str of
    Nothing -> fail $ "invalid address: " <> Text.unpack str
    Just addr -> pure addr

parseTxOutAnyEra
  :: Parsec.Parser (TxOutDatumAnyEra -> ReferenceScriptAnyEra -> TxOutAnyEra)
parseTxOutAnyEra = do
  addr <- parseAddressAny
  Parsec.spaces
  -- Accept the old style of separating the address and value in a
  -- transaction output:
  Parsec.option () (Parsec.char '+' >> Parsec.spaces)
  val <- parseValue RoleUTxO -- UTxO role works for transaction output
  return (TxOutAnyEra addr val)

--------------------------------------------------------------------------------

pVoteChoice :: Parser Vote
pVoteChoice =
  asum
    [ flag' Yes $ long "yes"
    , flag' No $ long "no"
    , flag' Abstain $ long "abstain"
    ]

pVoterType :: Parser VType
pVoterType =
  asum
    [ flag' VCC $
        mconcat [long "constitutional-committee-member", Opt.help "Member of the constiutional committee"]
    , flag' VDR $ mconcat [long "drep", Opt.help "Delegated representative"]
    , flag' VSP $ mconcat [long "spo", Opt.help "Stake pool operator"]
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
  Opt.flag' () $
    mconcat
      [ Opt.long "always-abstain"
      , Opt.help "Abstain from voting on all proposals."
      ]

pVoteAnchor :: Parser (VoteUrl, L.SafeHash L.StandardCrypto L.AnchorData)
pVoteAnchor =
  ((,) . VoteUrl <$> pUrl "anchor-url" "Vote anchor URL")
    <*> pVoteAnchorDataHash

pVoteAnchorDataHash :: Parser (L.SafeHash L.StandardCrypto L.AnchorData)
pVoteAnchorDataHash =
  Opt.option readSafeHash $
    mconcat
      [ Opt.long "anchor-data-hash"
      , Opt.metavar "HASH"
      , Opt.help "Hash of the vote anchor data (obtain it with \"cardano-cli hash anchor-data ...\")."
      ]

pAlwaysNoConfidence :: Parser ()
pAlwaysNoConfidence =
  Opt.flag' () $
    mconcat
      [ Opt.long "always-no-confidence"
      , Opt.help "Always vote no confidence"
      ]

pDrepRefund :: Parser (DRepHashSource, Lovelace)
pDrepRefund =
  (,)
    <$> pDRepHashSource
    <*> pDepositRefund

pDepositRefund :: Parser Lovelace
pDepositRefund =
  Opt.option (readerFromParsecParser parseLovelace) $
    mconcat
      [ Opt.long "deposit-refund"
      , Opt.metavar "LOVELACE"
      , Opt.help "Deposit refund amount."
      ]

pDRepHashSource :: Parser DRepHashSource
pDRepHashSource =
  asum
    [ DRepHashSourceScript <$> pDRepScriptHash
    , DRepHashSourceVerificationKey <$> pDRepVerificationKeyOrHashOrFile
    ]

pSPOHashSource :: Parser SPOHashSource
pSPOHashSource = SPOHashSourceVerificationKey <$> pSPOVerificationKeyOrHashOrFile

pDRepScriptHash :: Parser ScriptHash
pDRepScriptHash =
  pScriptHash
    "drep-script-hash"
    "DRep script hash (hex-encoded). Obtain it with \"cardano-cli hash script ...\"."

pSPOScriptHash :: Parser ScriptHash
pSPOScriptHash =
  pScriptHash
    "spo-script-hash"
    "Stake pool operator script hash (hex-encoded). Obtain it with \"cardano-cli hash script ...\"."

pConstitutionScriptHash :: Parser ScriptHash
pConstitutionScriptHash =
  pScriptHash
    "constitution-script-hash"
    "Constitution script hash (hex-encoded). Obtain it with \"cardano-cli hash script ...\"."

pDRepVerificationKeyOrHashOrFile
  :: Parser (VerificationKeyOrHashOrFile DRepKey)
pDRepVerificationKeyOrHashOrFile =
  asum
    [ VerificationKeyOrFile <$> pDRepVerificationKeyOrFile
    , VerificationKeyHash <$> pDRepVerificationKeyHash
    ]

pSPOVerificationKeyOrHashOrFile
  :: Parser (VerificationKeyOrHashOrFile StakePoolKey)
pSPOVerificationKeyOrHashOrFile =
  asum
    [ VerificationKeyOrFile <$> pSPOVerificationKeyOrFile
    , VerificationKeyHash <$> pSPOVerificationKeyHash
    ]

pDRepVerificationKeyOrHashOrFileOrScriptHash
  :: Parser (VerificationKeyOrHashOrFileOrScriptHash DRepKey)
pDRepVerificationKeyOrHashOrFileOrScriptHash =
  asum
    [ VkhfshKeyHashFile . VerificationKeyOrFile <$> pDRepVerificationKeyOrFile
    , VkhfshKeyHashFile . VerificationKeyHash <$> pDRepVerificationKeyHash
    , VkhfshScriptHash
        <$> pScriptHash
          "drep-script-hash"
          "Cold Native or Plutus script file hash (hex-encoded). Obtain it with \"cardano-cli hash script ...\"."
    ]

pAllOrOnlyDRepHashSource
  :: Parser (AllOrOnly DRepHashSource)
pAllOrOnlyDRepHashSource = pAll <|> pOnly
 where
  pOnly = Only <$> some pDRepHashSource
  pAll =
    Opt.flag' All $
      mconcat
        [ Opt.long "all-dreps"
        , Opt.help "Query for all DReps."
        ]

pAllOrOnlySPOHashSource :: Parser (AllOrOnly SPOHashSource)
pAllOrOnlySPOHashSource = pAll <|> pOnly
 where
  pOnly = Only <$> some pSPOHashSource
  pAll =
    Opt.flag' All $
      mconcat
        [ Opt.long "all-spos"
        , Opt.help "Query for all DReps."
        ]

pAllOrOnlyGovActionIds
  :: ()
  => ConwayEraOnwards era
  -> Parser (AllOrOnly (L.GovActionId L.StandardCrypto))
pAllOrOnlyGovActionIds era = pAll <|> pOnly
 where
  pOnly = Only <$> pGovActionIds era
  pAll =
    Opt.flag' All $
      mconcat
        [ Opt.long "all-proposals"
        , Opt.help "Query for all governance proposals."
        ]

pGovActionIds
  :: forall era
   . ()
  => ConwayEraOnwards era
  -> Parser [L.GovActionId L.StandardCrypto]
pGovActionIds era = conwayEraOnwardsConstraints era (some pLedgerGovernanceAction)
 where
  pLedgerGovernanceAction :: Parser (L.GovActionId L.StandardCrypto)
  pLedgerGovernanceAction = uncurry L.GovActionId <$> pairParser

  pairParser :: Parser (L.TxId L.StandardCrypto, L.GovActionIx)
  pairParser = bimap toShelleyTxId L.GovActionIx <$> pGovernanceActionId

pDRepVerificationKeyHash :: Parser (Hash DRepKey)
pDRepVerificationKeyHash =
  Opt.option (rBech32KeyHash AsDRepKey <|> rHexHash AsDRepKey Nothing) $
    mconcat
      [ Opt.long "drep-key-hash"
      , Opt.metavar "HASH"
      , Opt.help "DRep verification key hash (either Bech32-encoded or hex-encoded)."
      ]

pDRepVerificationKey :: Parser (VerificationKey DRepKey)
pDRepVerificationKey =
  Opt.option (readVerificationKey AsDRepKey) $
    mconcat
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
  File <$> parseFilePath "drep-verification-key-file" "Filepath of the DRep verification key."

pSPOVerificationKeyHash :: Parser (Hash StakePoolKey)
pSPOVerificationKeyHash =
  Opt.option (rBech32KeyHash AsStakePoolKey <|> rHexHash AsStakePoolKey Nothing) $
    mconcat
      [ Opt.long "spo-key-hash"
      , Opt.metavar "HASH"
      , Opt.help "SPO verification key hash (either Bech32-encoded or hex-encoded)."
      ]

pSPOVerificationKey :: Parser (VerificationKey StakePoolKey)
pSPOVerificationKey =
  Opt.option (readVerificationKey AsStakePoolKey) $
    mconcat
      [ Opt.long "spo-verification-key"
      , Opt.metavar "STRING"
      , Opt.help "SPO verification key (Bech32 or hex-encoded)."
      ]

pSPOVerificationKeyOrFile :: Parser (VerificationKeyOrFile StakePoolKey)
pSPOVerificationKeyOrFile =
  asum
    [ VerificationKeyValue <$> pSPOVerificationKey
    , VerificationKeyFilePath <$> pSPOVerificationKeyFile
    ]

pSPOVerificationKeyFile :: Parser (VerificationKeyFile In)
pSPOVerificationKeyFile =
  File <$> parseFilePath "spo-verification-key-file" "Filepath of the SPO verification key."

pAnchorUrl :: Parser ProposalUrl
pAnchorUrl =
  ProposalUrl
    <$> pUrl "anchor-url" "Anchor URL"

pExpectedAnchorDataHash :: Parser (L.SafeHash L.StandardCrypto L.AnchorData)
pExpectedAnchorDataHash = pExpectedHash id "anchor data"

pExpectedHash :: (L.SafeHash L.StandardCrypto L.AnchorData -> a) -> String -> Parser a
pExpectedHash adaptor hashedDataName =
  Opt.option (adaptor <$> readSafeHash) $
    mconcat
      [ Opt.long "expected-hash"
      , Opt.metavar "HASH"
      , Opt.help $
          mconcat
            [ "Expected hash for the " ++ hashedDataName ++ ", for verification purposes. "
            , "If provided, the hash of the " ++ hashedDataName ++ " will be compared to this value."
            ]
      ]

pAnchorDataHash :: Parser (L.SafeHash L.StandardCrypto L.AnchorData)
pAnchorDataHash =
  Opt.option readSafeHash $
    mconcat
      [ Opt.long "anchor-data-hash"
      , Opt.metavar "HASH"
      , Opt.help "Proposal anchor data hash (obtain it with \"cardano-cli hash anchor-data ...\")"
      ]

pMustCheckHash :: String -> String -> String -> String -> Parser (MustCheckHash anchorData)
pMustCheckHash flagSuffix' dataName' hashParamName' urlParamName' =
  Opt.flag TrustHash CheckHash $
    mconcat
      [ Opt.long ("check-" ++ flagSuffix')
      , Opt.help
          ( "Verify that the expected "
              ++ dataName'
              ++ " hash provided in "
              ++ hashParamName'
              ++ " matches the hash of the file downloaded from the URL provided in "
              ++ urlParamName'
              ++ " (this parameter will download the file from the URL)"
          )
      ]

pPotentiallyCheckedAnchorData
  :: Parser (MustCheckHash anchorType)
  -> Parser anchor
  -> Parser (PotentiallyCheckedAnchor anchorType anchor)
pPotentiallyCheckedAnchorData mustCheckHash anchorData =
  PotentiallyCheckedAnchor
    <$> anchorData
    <*> mustCheckHash

pMustCheckProposalHash :: Parser (MustCheckHash ProposalUrl)
pMustCheckProposalHash = pMustCheckHash "anchor-data" "proposal" "--anchor-data-hash" "--anchor-url"

pMustCheckConstitutionHash :: Parser (MustCheckHash ConstitutionUrl)
pMustCheckConstitutionHash = pMustCheckHash "constitution-hash" "constitution" "--constitution-hash" "--constitution-url"

pMustCheckMetadataHash :: Parser (MustCheckHash DRepMetadataUrl)
pMustCheckMetadataHash = pMustCheckHash "drep-metadata-hash" "DRep metadata" "--drep-metadata-hash" "--drep-metadata-url"

pMustCheckStakeMetadataHash :: Parser (MustCheckHash StakePoolMetadataReference)
pMustCheckStakeMetadataHash = pMustCheckHash "metadata-hash" "stake pool metadata" "--metadata-hash" "--metadata-url"

pMustCheckVoteUrl :: Parser (MustCheckHash VoteUrl)
pMustCheckVoteUrl = pMustCheckHash "anchor-data-hash" "vote anchor data" "--anchor-data-hash" "--anchor-url"

pMustCheckResignationMetadataHash :: Parser (MustCheckHash ResignationMetadataUrl)
pMustCheckResignationMetadataHash =
  pMustCheckHash
    "resignation-metadata-hash"
    "Constitutional Committee cold key resignation certificate metadata"
    "--resignation-metadata-hash"
    "--resignation-metadata-url"

pPreviousGovernanceAction :: Parser (Maybe (TxId, Word16))
pPreviousGovernanceAction =
  optional $
    (,)
      <$> pTxId "prev-governance-action-tx-id" "Txid of the previous governance action."
      <*> pWord16 "prev-governance-action-index" "Action index of the previous governance action."

pGovernanceActionId :: Parser (TxId, Word16)
pGovernanceActionId =
  (,)
    <$> pTxId "governance-action-tx-id" "Txid of the governance action."
    <*> pWord16 "governance-action-index" "Tx's governance action index."

pWord16 :: String -> String -> Parser Word16
pWord16 l h =
  Opt.option integralReader $
    mconcat
      [ Opt.long l
      , Opt.metavar "WORD16"
      , Opt.help h
      ]

pTxId :: String -> String -> Parser TxId
pTxId l h =
  Opt.option (readerFromParsecParser parseTxId) $
    mconcat
      [ Opt.long l
      , Opt.metavar "TXID"
      , Opt.help h
      ]

pNetworkIdForTestnetData :: EnvCli -> Parser NetworkId
pNetworkIdForTestnetData envCli =
  asum $
    mconcat
      [
        [ fmap (Testnet . NetworkMagic) $
            Opt.option (bounded "TESTNET_MAGIC") $
              mconcat
                [ Opt.long "testnet-magic"
                , Opt.metavar "NATURAL"
                , Opt.help $
                    mconcat
                      [ "Specify a testnet magic id for the cluster. "
                      , "This overrides both the network magic from the "
                      , "spec file and CARDANO_NODE_NETWORK_ID environment variable."
                      ]
                ]
        ]
      , -- Default to the network id specified by the environment variable if it is available.
        pure <$> maybeToList (envCliNetworkId envCli)
      ]

pReferenceScriptSize :: Parser ReferenceScriptSize
pReferenceScriptSize =
  fmap ReferenceScriptSize $
    Opt.option integralReader $
      mconcat
        [ Opt.long "reference-script-size"
        , Opt.metavar "NATURAL"
        , Opt.help "Total size in bytes of transaction reference scripts (default is 0)."
        , Opt.value 0
        ]

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

pFeatured
  :: ()
  => Eon eon
  => ToCardanoEra f
  => f era
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
