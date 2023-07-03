{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.Common.Parsers where

import           Cardano.Api

import           Cardano.CLI.Environment (EnvCli (..))
import           Cardano.CLI.Shelley.Key
import           Cardano.CLI.Types

import           Data.Bifunctor
import qualified Data.ByteString.Char8 as BSC
import           Data.Foldable
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import           Data.Maybe (maybeToList)
import qualified Data.Text as Text
import           Data.Word (Word64)
import           Options.Applicative
import qualified Options.Applicative as Opt
import qualified Text.Parsec as Parsec
import           Text.Parsec ((<?>))
import qualified Text.Parsec.Error as Parsec
import qualified Text.Parsec.Language as Parsec
import qualified Text.Parsec.String as Parsec
import qualified Text.Parsec.Token as Parsec

pCardanoEra :: Parser AnyCardanoEra
pCardanoEra = asum
  [ Opt.flag' (AnyCardanoEra ByronEra) $ mconcat
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

    -- Default for now:
  , pure (AnyCardanoEra BabbageEra)
  ]
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
    Opt.option (readerFromParsecParser parseStakeAddress)
      (  Opt.long "stake-address"
      <> Opt.metavar "ADDRESS"
      <> Opt.help "Target stake address (bech32 format)."
      )

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
  ScriptFile <$> Opt.strOption
    (  Opt.long name
    <> Opt.metavar "FILE"
    <> Opt.help help'
    <> Opt.completer (Opt.bashCompleter "file")
    )

pScriptFor name (Just deprecated) help' =
      pScriptFor name Nothing help'
  <|> ScriptFile <$> Opt.strOption
        (  Opt.long deprecated
        <> Opt.internal
        )

pStakeVerificationKey :: Parser (VerificationKey StakeKey)
pStakeVerificationKey =
  Opt.option
    (readVerificationKey AsStakeKey)
      (  Opt.long "stake-verification-key"
      <> Opt.metavar "STRING"
      <> Opt.help "Stake verification key (Bech32 or hex-encoded)."
      )

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
