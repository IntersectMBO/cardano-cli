{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cli.Parser
  ( hprop_integral_reader
  , hprop_integral_pair_reader_positive
  , hprop_integral_pair_reader_negative
  , hprop_lovelace_reader
  , hprop_url_reader
  )
where

import Cardano.Api.Ledger qualified as L
import Cardano.Api.Parser.Text qualified as P
import Cardano.Api.Pretty (textShow)

import Cardano.CLI.EraBased.Common.Option
  ( integralParsecParser
  , pUrl
  , pairIntegralParsecParser
  , parseLovelace
  )

import Data.Bits (Bits)
import Data.Data (Proxy (..), Typeable)
import Data.Either (isLeft, isRight)
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import Data.Word (Word16)
import Options.Applicative qualified as Opt

import Test.Cardano.CLI.Util (watchdogProp)

import Hedgehog (Gen, Property, assert, property, (===))
import Hedgehog.Extras (assertWith, propertyOnce)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Internal.Property (forAll)
import Hedgehog.Range qualified as Gen
import Hedgehog.Range qualified as Range

-- | Execute me with:
-- @cabal test cardano-cli-test --test-options '-p "/integral reader/"'@
hprop_integral_reader :: Property
hprop_integral_reader = watchdogProp . property $ do
  parse @Word "0" === Right 0
  parse @Word "42" === Right 42
  assertWith (parse @Word "-1") isLeft
  assertWith (parse @Word "18446744073709551616") isLeft
  assertWith (parse @Word "-1987090") isLeft

  w <- forAll $ Gen.word $ Gen.linear minBound maxBound
  parse @Word (textShow w) === Right w

  parse @Word16 "0" === Right 0
  parse @Word16 "42" === Right 42
  assertWith (parse @Word16 "-1") isLeft
  assertWith (parse @Word16 "65536") isLeft
  assertWith (parse @Word16 "298709870987") isLeft
  assertWith (parse @Word16 "-1987090") isLeft
 where
  parse :: (Typeable a, Integral a, Bits a) => Text -> Either String a
  parse = P.runParser integralParsecParser

-- | Execute me with:
-- @cabal test cardano-cli-test --test-options '-p "/integral pair reader positive/"'@
hprop_integral_pair_reader_positive :: Property
hprop_integral_pair_reader_positive = watchdogProp . property $ do
  validArbitraryTuple <- forAll $ genNumberTuple (Proxy :: Proxy Word)
  assert $ isRight $ parse @Word validArbitraryTuple
 where
  parse :: (Typeable a, Integral a, Bits a) => Text -> Either String (a, a)
  parse = P.runParser pairIntegralParsecParser

genNumberTuple :: forall a. Integral a => Show a => Proxy a -> Gen Text
genNumberTuple _ = do
  x :: a <- Gen.integral (Range.linear 0 100)
  y :: a <- Gen.integral (Range.linear 0 100)
  space0 <- genArbitrarySpace
  space1 <- genArbitrarySpace
  space2 <- genArbitrarySpace
  space3 <- genArbitrarySpace
  pure $
    mconcat
      [space0, "(", space2, textShow x, space1, ",", space2, textShow y, space1, ")", space3]

genArbitrarySpace :: Gen Text
genArbitrarySpace = Gen.text (Range.linear 0 5) (return ' ')

-- | Execute me with:
-- @cabal test cardano-cli-test --test-options '-p "/integral pair reader negative/"'@
hprop_integral_pair_reader_negative :: Property
hprop_integral_pair_reader_negative =
  watchdogProp . propertyOnce $ do
    assertWith (parse @Word "(0, 0, 0)") isLeft
    assertWith (parse @Word "(-1, 0)") isLeft
    assertWith (parse @Word "(18446744073709551616, 0)") isLeft
    assertWith (parse @Word "(0, 18446744073709551616)") isLeft
    assertWith (parse @Word "(0, -1)") isLeft
    assertWith (parse @Word "0, 0)") isLeft
    assertWith (parse @Word "(0, 0") isLeft
    assertWith (parse @Word "(0 0)") isLeft
    assertWith (parse @Word "(   0, 0") isLeft
 where
  parse :: (Typeable a, Integral a, Bits a) => Text -> Either String (a, a)
  parse = P.runParser pairIntegralParsecParser

-- | Execute me with:
-- @cabal test cardano-cli-test --test-options '-p "/lovelace reader/"'@
hprop_lovelace_reader :: Property
hprop_lovelace_reader =
  watchdogProp . propertyOnce $ do
    parse "0" === Right 0
    parse "42" === Right 42
    parse "1000000" === Right 1000000
    parse "18446744073709551615" === Right (L.Coin 18446744073709551615)
    assertWith (parse "18446744073709551616") isLeft
    assertWith (parse "-1") isLeft
    assertWith (parse "abc") isLeft
    assertWith (parse "") isLeft
 where
  parse :: Text -> Either String L.Coin
  parse = P.runParser parseLovelace

-- | Execute me with:
-- @cabal test cardano-cli-test --test-options '-p "/url reader/"'@
hprop_url_reader :: Property
hprop_url_reader =
  watchdogProp . propertyOnce $ do
    -- Valid short URL
    assertWith (parseUrl "http://example.com") isJust
    -- Exactly 128 bytes should succeed
    assertWith (parseUrl (replicate 128 'x')) isJust
    -- 129 bytes should fail
    assertWith (parseUrl (replicate 129 'x')) isNothing
    -- Empty URL should succeed (the ledger allows it)
    assertWith (parseUrl "") isJust
 where
  parseUrl :: String -> Maybe L.Url
  parseUrl url =
    Opt.getParseResult $
      Opt.execParserPure Opt.defaultPrefs urlParserInfo ["--test-url", url]
  urlParserInfo :: Opt.ParserInfo L.Url
  urlParserInfo = Opt.info (pUrl "test-url" "Test URL") mempty
