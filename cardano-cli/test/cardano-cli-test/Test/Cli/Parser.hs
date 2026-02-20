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

import Cardano.Api (Lovelace)
import qualified Cardano.Api.Ledger as L
import Cardano.CLI.EraBased.Options.Common
  ( integralParsecParser
  , pUrl
  , pairIntegralParsecParser
  , parseLovelace
  )
import Data.Bits (Bits)
import Data.Data (Proxy (..), Typeable)
import Data.Either (isLeft, isRight)
import Data.Maybe (isJust, isNothing)
import Data.Word (Word16, Word64)
import Hedgehog (Gen, Property, assert, property, (===))
import Hedgehog.Extras (assertWith, propertyOnce)
import qualified Hedgehog.Gen as Gen
import Hedgehog.Internal.Property (forAll)
import qualified Hedgehog.Range as Gen
import qualified Hedgehog.Range as Range
import Options.Applicative (defaultPrefs, execParserPure, getParseResult, info)
import qualified Text.Parsec as Parsec

-- | Execute me with:
-- @cabal test cardano-cli-test --test-options '-p "/integral reader/"'@
hprop_integral_reader :: Property
hprop_integral_reader = property $ do
  parse @Word "0" === Right 0
  parse @Word "42" === Right 42
  assertWith (parse @Word "-1") isLeft
  assertWith (parse @Word "18446744073709551616") isLeft
  assertWith (parse @Word "-1987090") isLeft

  w <- forAll $ Gen.word $ Gen.linear minBound maxBound
  parse @Word (show w) === Right w

  parse @Word16 "0" === Right 0
  parse @Word16 "42" === Right 42
  assertWith (parse @Word16 "-1") isLeft
  assertWith (parse @Word16 "65536") isLeft
  assertWith (parse @Word16 "298709870987") isLeft
  assertWith (parse @Word16 "-1987090") isLeft
 where
  parse :: (Typeable a, Integral a, Bits a) => String -> Either String a
  parse s =
    case Parsec.runParser integralParsecParser () "" s of
      Left parsecError -> Left $ show parsecError
      Right x -> Right x

-- | Execute me with:
-- @cabal test cardano-cli-test --test-options '-p "/integral pair reader positive/"'@
hprop_integral_pair_reader_positive :: Property
hprop_integral_pair_reader_positive = property $ do
  validArbitraryTuple <- forAll $ genNumberTuple (Proxy :: Proxy Word)
  assert $ isRight $ parse @Word validArbitraryTuple
 where
  parse :: (Typeable a, Integral a, Bits a) => String -> Either String (a, a)
  parse s =
    case Parsec.runParser pairIntegralParsecParser () "" s of
      Left parsecError -> Left $ show parsecError
      Right x -> Right x

genNumberTuple :: forall a. Integral a => Show a => Proxy a -> Gen String
genNumberTuple _ = do
  x :: a <- Gen.integral (Range.linear 0 100)
  y :: a <- Gen.integral (Range.linear 0 100)
  space0 <- genArbitrarySpace
  space1 <- genArbitrarySpace
  space2 <- genArbitrarySpace
  space3 <- genArbitrarySpace
  return $
    space0 ++ "(" ++ space2 ++ show x ++ space1 ++ "," ++ space2 ++ show y ++ space1 ++ ")" ++ space3

genArbitrarySpace :: Gen String
genArbitrarySpace = Gen.string (Range.linear 0 5) (return ' ')

-- | Execute me with:
-- @cabal test cardano-cli-test --test-options '-p "/integral pair reader negative/"'@
hprop_integral_pair_reader_negative :: Property
hprop_integral_pair_reader_negative = propertyOnce $ do
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
  parse :: (Typeable a, Integral a, Bits a) => String -> Either String (a, a)
  parse s =
    case Parsec.runParser pairIntegralParsecParser () "" s of
      Left parsecError -> Left $ show parsecError
      Right x -> Right x

-- | Execute me with:
-- @cabal test cardano-cli-test --test-options '-p "/lovelace reader/"'@
hprop_lovelace_reader :: Property
hprop_lovelace_reader = property $ do
  parse "0" === Right (L.Coin 0)
  parse "1000000" === Right (L.Coin 1000000)
  parse (show (maxBound :: Word64)) === Right (L.Coin (fromIntegral (maxBound :: Word64)))
  assertWith (parse "-1") isLeft
  assertWith (parse "18446744073709551616") isLeft -- maxBound + 1
  assertWith (parse "not-a-number") isLeft

  n <- forAll $ Gen.word64 Range.constantBounded
  parse (show n) === Right (L.Coin (fromIntegral n))
 where
  parse :: String -> Either String Lovelace
  parse s =
    case Parsec.runParser parseLovelace () "" s of
      Left parsecError -> Left $ show parsecError
      Right x -> Right x

-- | Execute me with:
-- @cabal test cardano-cli-test --test-options '-p "/url reader/"'@
--
-- Verifies that @pUrl@ rejects URLs longer than 64 bytes via a clean parser
-- failure rather than a runtime 'error' crash.
hprop_url_reader :: Property
hprop_url_reader = propertyOnce $ do
  assertWith (runUrlParser "http://example.com") isJust
  assertWith (runUrlParser (replicate 64 'x')) isJust
  assertWith (runUrlParser (replicate 65 'x')) isJust
  assertWith (runUrlParser (replicate 100 'a')) isJust
  assertWith (runUrlParser (replicate 129 'a')) isNothing
 where
  runUrlParser :: String -> Maybe L.Url
  runUrlParser url =
    getParseResult $
      execParserPure defaultPrefs (info (pUrl "url" "A URL.") mempty) ["--url", url]
