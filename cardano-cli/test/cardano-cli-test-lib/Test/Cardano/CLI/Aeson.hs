module Test.Cardano.CLI.Aeson (
    assertHasKeys,
    assertHasMappings,
  ) where

import           Control.Monad (forM_)
import           Control.Monad.IO.Class
import           Data.Aeson hiding (pairs)
import qualified Data.Aeson.Key as Aeson
import qualified Data.Aeson.KeyMap as Aeson.KeyMap
import qualified Data.ByteString.Lazy as LBS
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Stack (HasCallStack)
import qualified GHC.Stack as GHC

import           Hedgehog
import qualified Hedgehog as H
import qualified Hedgehog.Extras as H

{- HLINT ignore "Use uncurry" -}

-- | @assertHasKeys keys path@ succeeds if @path@ is a file containing a JSON object
-- whose keys is a superset of @keys@.
--
-- For example. if @path@ contains @"{ "a":0, "b":1.0, "c": "foo"}"@,
-- @hasKeys ["b", "a"] path@ succeeds.
assertHasKeys :: ()
  => HasCallStack
  => MonadTest m
  => MonadIO m
  => [Text]
  -> FilePath
  -> m ()
assertHasKeys keys jsonFile = GHC.withFrozenCallStack $ do
  content <- liftIO $ LBS.readFile jsonFile
  case decode content of
    Nothing -> do
      H.note_ $ "Cannot read JSON file: " <> jsonFile
      H.failure
    Just obj -> do
      forM_ keys $ \key -> assertHasKey jsonFile obj key

-- | @assertHasKey file obj key@ checks that @obj@ has @key@ as a top-level key.
-- @file@ is only used for logging in case of failure: it is assumed to be
-- the path from which @obj@ was loaded.
--
-- Having this functions allows for good feedback in case of a test failure.
assertHasKey :: ()
  => HasCallStack
  => MonadTest m
  => FilePath
  -> Object
  -> Text
  -> m ()
assertHasKey file obj key = GHC.withFrozenCallStack $ do
  case Aeson.KeyMap.lookup (Aeson.fromText key) obj of
    Nothing -> do
      H.note_ $ "JSON file at " <> file <> " is missing key: \"" <> T.unpack key <> "\""
      H.failure
    Just _ -> H.success

-- | @assertHasMappings pairs path@ succeeds if @path@ is a file containing a JSON object
-- whose mappings is a superset of @pairs@.
--
-- For example, if @path@ contains @"{ "a":"bar", "b":"buzz", "c":"foo"}"@,
-- @assertHasMappings "[("b", "buzz"), ("a", "bar")] path@ succeeds.
assertHasMappings :: ()
  => HasCallStack
  => MonadTest m
  => MonadIO m
  => [(Text, Text)]
  -> FilePath
  -> m ()
assertHasMappings pairs jsonFile = GHC.withFrozenCallStack $ do
  content <- liftIO $ LBS.readFile jsonFile
  case decode content of
    Nothing -> do
      H.note_ $ "Cannot read JSON file: " <> jsonFile
      H.failure
    Just obj -> do
      forM_ pairs $ \(key, value) -> assertHasMapping jsonFile obj key value

-- | @assertHasMapping file obj key value@ checks that @obj@ has the @key->value@
-- at its top-level. @file@ is only used for logging in case of failure: it is assumed to be
-- the path from which @obj@ was loaded.
--
-- Having this functions allows for good feedback in case of a test failure.
assertHasMapping :: ()
  => HasCallStack
  => MonadTest m
  => FilePath
  -> Object
  -> Text
  -> Text
  -> m ()
assertHasMapping file obj key value = GHC.withFrozenCallStack $ do
  case Aeson.KeyMap.lookup (Aeson.fromText key) obj of
    Nothing -> do
      H.note_ $ "JSON file at " <> file <> " is missing key: \"" <> T.unpack key <> "\""
      H.failure
    Just inThere ->
      case inThere of
        String textInThere | value == textInThere -> H.success
        String textInThere -> do
            H.note_ $ "JSON file at " <> file <> " has the mapping \"" <> T.unpack key <> "\"->\"" <> T.unpack textInThere <> "\""
            H.note_ $ "whereas it was expected to be \"" <> T.unpack key <> "\"->\"" <> T.unpack value <> "\""
            H.failure
        Object _ -> failWrongType "object"
        Array _  -> failWrongType "array"
        Number _ -> failWrongType "number"
        Bool _   -> failWrongType "bool"
        Null     -> failWrongType "null"
   where
     failWrongType got = do
       H.note_ $ "JSON file at " <> file <> " has wrong type for key: \"" <> T.unpack key <> "\""
       H.note_ $ "Expected string but got: " <> got
       H.failure


