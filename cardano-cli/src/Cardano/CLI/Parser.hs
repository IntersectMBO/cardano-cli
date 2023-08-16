{-# LANGUAGE TypeApplications #-}

module Cardano.CLI.Parser
  ( readerFromAttoParser
  , readFractionAsRational
  , readKeyOutputFormat
  , readPoolIdOutputFormat
  , readRational
  , readRationalUnitInterval
  , readStringOfMaxLength
  , readURIOfMaxLength
  , readLedgerProtVer
  , eDNSName
  ) where

import           Cardano.CLI.Types.Common
import qualified Cardano.Ledger.BaseTypes as L
import           Cardano.Prelude (ConvertText (..))

import           Control.Monad (void)
import qualified Data.Attoparsec.ByteString.Char8 as Atto
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC
import           Data.Foldable
import           Data.Ratio ((%))
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           GHC.Natural (Natural)
import qualified Options.Applicative as Opt

readPoolIdOutputFormat :: Opt.ReadM PoolIdOutputFormat
readPoolIdOutputFormat = do
  s <- Opt.str @String
  case s of
    "hex" -> pure PoolIdOutputFormatHex
    "bech32" -> pure PoolIdOutputFormatBech32
    _ ->
      fail $ mconcat
        [ "Invalid output format: " <> show s
        , ". Accepted output formats are \"hex\" and \"bech32\"."
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

readLedgerProtVer :: Opt.ReadM L.ProtVer
readLedgerProtVer = readerFromAttoParser pAttoProtVer
  where
    pAttoProtVer :: Atto.Parser L.ProtVer
    pAttoProtVer = do
      major <- Atto.decimal @Natural >>= L.mkVersion
      void $ Atto.char ','
      minor <- Atto.decimal @Natural
      pure $ L.ProtVer major minor

readerFromAttoParser :: Atto.Parser a -> Opt.ReadM a
readerFromAttoParser p =
  Opt.eitherReader (Atto.parseOnly (p <* Atto.endOfInput) . BSC.pack)

eDNSName :: String -> Either String ByteString
eDNSName str =
  -- We're using 'L.textToDns' to validate the string.
  case L.textToDns (toS str) of
    Nothing -> Left $ "DNS name is more than 64 bytes: " <> str
    Just dnsName -> Right . Text.encodeUtf8 . L.dnsToText $ dnsName
