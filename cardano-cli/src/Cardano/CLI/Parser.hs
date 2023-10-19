{-# LANGUAGE TypeApplications #-}

module Cardano.CLI.Parser
  ( readerFromAttoParser
  , readFractionAsRational
  , readKeyOutputFormat
  , readIdOutputFormat
  , readProposalFileFormat
  , readTxViewOutputFormat
  , readRational
  , readRationalUnitInterval
  , readStringOfMaxLength
  , readURIOfMaxLength
  , eDNSName
  , readGovernanceActionViewOutputFormat
  ) where

import           Cardano.CLI.Types.Common
import qualified Cardano.Ledger.BaseTypes as Shelley

import qualified Data.Attoparsec.ByteString.Char8 as Atto
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC
import           Data.Foldable
import           Data.Ratio ((%))
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Options.Applicative as Opt

readIdOutputFormat :: Opt.ReadM IdOutputFormat
readIdOutputFormat = do
  s <- Opt.str @String
  case s of
    "hex" -> pure IdOutputFormatHex
    "bech32" -> pure IdOutputFormatBech32
    _ ->
      fail $ mconcat
        [ "Invalid output format: " <> show s
        , ". Accepted output formats are \"hex\" and \"bech32\"."
        ]

readProposalFileFormat :: Opt.ReadM ProposalFileFormat
readProposalFileFormat = do
  s <- Opt.str @String
  case s of
    "text" -> pure ProposalFileFormatText
    "binary" -> pure ProposalFileFormatBinary
    _ ->
      fail $ mconcat
        [ "Invalid proposal file format: " <> show s
        , ". Accepted output formats are \"text\" and \"binary\"."
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

readTxViewOutputFormat :: Opt.ReadM TxViewOutputFormat
readTxViewOutputFormat = do
  s <- Opt.str @String
  case s of
    "json" -> pure TxViewOutputFormatJson
    "yaml" -> pure TxViewOutputFormatYaml
    _ ->
      fail $ mconcat
        [ "Invalid transaction view output format: " <> show s
        , ". Accepted output formats are \"json\" and \"yaml\"."
        ]

readGovernanceActionViewOutputFormat :: Opt.ReadM GovernanceActionViewOutputFormat
readGovernanceActionViewOutputFormat = do
  s <- Opt.str @String
  case s of
    "json" -> pure GovernanceActionViewOutputFormatJson
    "yaml" -> pure GovernanceActionViewOutputFormatYaml
    _ ->
      fail $ mconcat
        [ "Invalid governance action view output format: " <> show s
        , ". Accepted output formats are \"json\" and \"yaml\"."
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

eDNSName :: String -> Either String ByteString
eDNSName str =
  -- We're using 'Shelley.textToDns' to validate the string.
  case Shelley.textToDns (Text.pack str) of
    Nothing -> Left $ "DNS name is more than 64 bytes: " <> str
    Just dnsName -> Right . Text.encodeUtf8 . Shelley.dnsToText $ dnsName
