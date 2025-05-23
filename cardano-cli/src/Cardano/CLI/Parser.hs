{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.CLI.Parser
  ( readerFromAttoParser
  , readFractionAsRational
  , readIdOutputFormat
  , readRational
  , readRationalUnitInterval
  , readStringOfMaxLength
  , readURIOfMaxLength
  , commandWithMetavar
  , eDNSName
  , stringToAnchorScheme
  , deprecatedReadKeyOutputFormat
  )
where

import Cardano.Api.Ledger qualified as L

import Cardano.CLI.Type.Common

import Data.Attoparsec.ByteString.Char8 qualified as Atto
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BSC
import Data.Char (toLower)
import Data.Foldable
import Data.Ratio ((%))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Options.Applicative qualified as Opt
import Vary

readIdOutputFormat :: Opt.ReadM (Vary [FormatBech32, FormatHex])
readIdOutputFormat = do
  s <- Opt.str @String
  case s of
    "hex" -> pure $ Vary.from FormatHex
    "bech32" -> pure $ Vary.from FormatBech32
    _ ->
      fail $
        mconcat
          [ "Invalid output format: " <> show s
          , ". Accepted output formats are \"hex\" and \"bech32\"."
          ]

deprecatedReadKeyOutputFormat :: Opt.ReadM (Vary [FormatBech32, FormatTextEnvelope])
deprecatedReadKeyOutputFormat = do
  s <- Opt.str @String
  case s of
    "text-envelope" -> pure (Vary.from FormatTextEnvelope)
    "bech32" -> pure (Vary.from FormatBech32)
    _ ->
      fail $
        mconcat
          [ "Invalid key output format: " <> show s
          , ". Accepted output formats are \"text-envelope\" and \"bech32\"."
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
      fail $
        mconcat
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
    | otherwise = fail "Please enter a value in the range [0,1]"

readFractionAsRational :: Opt.ReadM Rational
readFractionAsRational = readerFromAttoParser fractionalAsRational
 where
  fractionalAsRational :: Atto.Parser Rational
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

commandWithMetavar :: String -> Opt.ParserInfo a -> Opt.Mod Opt.CommandFields a
commandWithMetavar cmdName pInfo = Opt.command cmdName pInfo <> Opt.metavar cmdName

-- | Converts a string to an 'AnchorScheme' if it is a valid scheme and is in the
-- 'SupportedScheme' list, otherwise it returns 'Left'.
stringToAnchorScheme :: SupportedSchemes -> String -> Either String AnchorScheme
stringToAnchorScheme supportedSchemes schemaString = do
  case convertToAnchorScheme $ map toLower schemaString of
    Just scheme | scheme `elem` supportedSchemes -> pure scheme
    _ -> Left $ "Unsupported URL scheme: " <> schemaString
 where
  convertToAnchorScheme :: String -> Maybe AnchorScheme
  convertToAnchorScheme "file:" = Just FileScheme
  convertToAnchorScheme "http:" = Just HttpScheme
  convertToAnchorScheme "https:" = Just HttpsScheme
  convertToAnchorScheme "ipfs:" = Just IpfsScheme
  convertToAnchorScheme _ = Nothing

eDNSName :: String -> Either String ByteString
eDNSName str =
  -- We're using 'Shelley.textToDns' to validate the string.
  case L.textToDns 128 (Text.pack str) of
    Nothing -> Left $ "DNS name is more than 64 bytes: " <> str
    Just dnsName -> Right . Text.encodeUtf8 . L.dnsToText $ dnsName
