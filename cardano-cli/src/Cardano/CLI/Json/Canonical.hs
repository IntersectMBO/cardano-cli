{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Instances used in the canonical JSON encoding of `GenesisData`

module Cardano.CLI.Json.Canonical
  ( SchemaError(..)
  , canonicalDecodePretty
  , canonicalEncodePretty
  )
where

-- import           Cardano.Prelude.Base
-- import qualified Cardano.Prelude.Compat as I
-- import           Cardano.Prelude.Json.Parse (parseJSString)

import           Cardano.Prelude (toS)

import           Data.Bifunctor
import qualified Data.ByteString.Lazy as LB
import           Data.Functor.Identity
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Text.JSON.Canonical as CanonicalJSON

-- import           Control.Monad.Error
-- import           Data.Fixed (E12, resolution)
-- import           Data.Int
-- import qualified Data.Text.Lazy.Builder as Builder
-- import           Data.Time (NominalDiffTime, UTCTime)
-- import           Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
-- import           Data.Word
-- import           Formatting (bprint, builder)
-- import           Formatting.Buildable (Buildable (build))
-- import           GHC.Natural (Natural)
-- import           Text.JSON.Canonical (FromJSON (fromJSON), Int54, JSValue (JSNum, JSString),
--                    ReportSchemaErrors (expected), ToJSON (toJSON))

data SchemaError = SchemaError
  { seExpected :: !Text
  , seActual   :: !(Maybe Text)
  } deriving (Show, Eq)

-- instance Buildable SchemaError where
--   build se = mconcat
--     [ bprint ("expected " . builder) $ Builder.fromText (seExpected se)
--     , case seActual se of
--       Nothing     -> mempty
--       Just actual -> bprint (" but got " . builder) $ Builder.fromText actual
--     ]

-- instance
--     (Applicative m, Monad m, MonadError SchemaError m)
--     => ReportSchemaErrors m
--   where
--   expected expec actual = throwError SchemaError
--     { seExpected = toS expec
--     , seActual   = fmap toS actual
--     }

-- instance Monad m => ToJSON m Int32 where
--   toJSON = pure . JSNum . fromIntegral

-- instance Monad m => ToJSON m Word16 where
--   toJSON = pure . JSNum . fromIntegral

-- instance Monad m => ToJSON m Word32 where
--   toJSON = pure . JSNum . fromIntegral

-- instance Monad m => ToJSON m Word64 where
--   toJSON = pure . JSString . CanonicalJSON.toJSString . show

-- instance Monad m => ToJSON m Integer where
--   toJSON = pure . JSString . CanonicalJSON.toJSString . show

-- instance Monad m => ToJSON m Natural where
--   toJSON = pure . JSString . CanonicalJSON.toJSString . show

-- -- | For backwards compatibility we convert this to seconds
-- instance Monad m => ToJSON m UTCTime where
--   toJSON = pure . JSNum . round . utcTimeToPOSIXSeconds

-- -- | For backwards compatibility we convert this to microseconds
-- instance Monad m => ToJSON m NominalDiffTime where
--   toJSON = toJSON . (`div` 1e6) . toPicoseconds
--    where
--     toPicoseconds :: NominalDiffTime -> Integer
--     toPicoseconds t =
--       numerator (toRational t * toRational (resolution $ Proxy @E12))

-- instance ReportSchemaErrors m => FromJSON m Int32 where
--   fromJSON (JSNum i) = pure . fromIntegral $ i
--   fromJSON val       = CanonicalJSON.expectedButGotValue "Int32" val

-- instance ReportSchemaErrors m => FromJSON m Word16 where
--   fromJSON (JSNum i) = pure . fromIntegral $ i
--   fromJSON val       = CanonicalJSON.expectedButGotValue "Word16" val

-- instance ReportSchemaErrors m => FromJSON m Word32 where
--   fromJSON (JSNum i) = pure . fromIntegral $ i
--   fromJSON val       = CanonicalJSON.expectedButGotValue "Word32" val

-- instance ReportSchemaErrors m => FromJSON m Word64 where
--   fromJSON = parseJSString (I.readEither @Word64 @Text . toS)

-- instance ReportSchemaErrors m => FromJSON m Integer where
--   fromJSON = parseJSString (I.readEither @Integer @Text . toS)

-- instance MonadError SchemaError m => FromJSON m Natural where
--   fromJSON = parseJSString (I.readEither @Natural @Text . toS)

-- instance MonadError SchemaError m => FromJSON m UTCTime where
--   fromJSON = fmap (posixSecondsToUTCTime . fromIntegral) . fromJSON @_ @Int54

-- instance MonadError SchemaError m => FromJSON m NominalDiffTime where
--   fromJSON = fmap (fromRational . (% 1e6)) . fromJSON

canonicalDecodePretty
  :: forall a
   . CanonicalJSON.FromJSON (Either SchemaError) a
  => LB.ByteString
  -> Either Text a
canonicalDecodePretty y = do
  eVal <- first toS (CanonicalJSON.parseCanonicalJSON y)
  first (Text.pack . show) (CanonicalJSON.fromJSON eVal :: Either SchemaError a)

canonicalEncodePretty
  :: forall a . CanonicalJSON.ToJSON Identity a => a -> LB.ByteString
canonicalEncodePretty x =
  LB.fromStrict
    . Text.encodeUtf8
    . toS
    $ CanonicalJSON.prettyCanonicalJSON
    $ runIdentity
    $ CanonicalJSON.toJSON x
