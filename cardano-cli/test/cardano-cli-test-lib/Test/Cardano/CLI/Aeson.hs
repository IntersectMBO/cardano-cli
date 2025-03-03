{-# LANGUAGE TypeApplications #-}

module Test.Cardano.CLI.Aeson
  ( assertEqualModuloDesc
  , assertHasKeys
  , assertHasMappings
  , redactJsonFieldsInFile
  )
where

import Control.Monad (forM_)
import Control.Monad.IO.Class
import Data.Aeson hiding (pairs)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.Key
import Data.Aeson.Key qualified as Aeson
import Data.Aeson.KeyMap qualified as Aeson.KeyMap
import Data.ByteString.Lazy qualified as LBS
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as Vector
import GHC.Stack (HasCallStack)
import GHC.Stack qualified as GHC

import Hedgehog
import Hedgehog qualified as H
import Hedgehog.Extras qualified as H

{- HLINT ignore "Use uncurry" -}

-- | @assertHasKeys keys path@ succeeds if @path@ is a file containing a JSON object
-- whose keys is a superset of @keys@.
--
-- For example. if @path@ contains @"{ "a":0, "b":1.0, "c": "foo"}"@,
-- @hasKeys ["b", "a"] path@ succeeds.
assertHasKeys
  :: ()
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
assertHasKey
  :: ()
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
assertHasMappings
  :: ()
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
assertHasMapping
  :: ()
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
          H.note_ $
            "JSON file at "
              <> file
              <> " has the mapping \""
              <> T.unpack key
              <> "\"->\""
              <> T.unpack textInThere
              <> "\""
          H.note_ $ "whereas it was expected to be \"" <> T.unpack key <> "\"->\"" <> T.unpack value <> "\""
          H.failure
        Object _ -> failWrongType "object"
        Array _ -> failWrongType "array"
        Number _ -> failWrongType "number"
        Bool _ -> failWrongType "bool"
        Null -> failWrongType "null"
 where
  failWrongType got = do
    H.note_ $ "JSON file at " <> file <> " has wrong type for key: \"" <> T.unpack key <> "\""
    H.note_ $ "Expected string but got: " <> got
    H.failure

-- | @assertEqualModuloDesc file1 file2@ loads @file1@ and @file2@ from disk,
-- then it strips the field @description@ from the loaded content, and finally compare
-- the two values. The values must be equal, otherwise the test is failed.
--
-- Required, because command @"key" "verification-key"@ generates keys without descriptions.
-- Note that it would be better to write descriptions, see:
-- https://github.com/IntersectMBO/cardano-cli/issues/429#issuecomment-2003880575
assertEqualModuloDesc
  :: ()
  => (HasCallStack, MonadIO m, MonadTest m)
  => FilePath
  -- ^ The file of the first generated verification key
  -> FilePath
  -- ^ The file of the second generated verification key, i.e. the one
  --   generated by calling "key verification-key"
  -> m ()
assertEqualModuloDesc file1 file2 = GHC.withFrozenCallStack $ do
  value1 <- H.readJsonFileOk @Value file1
  value1' <- removeDescription value1

  value2 <- H.readJsonFileOk @Value file2
  value2' <- removeDescription value2

  value1' H.=== value2'

-- | Removes the @description@ field from a JSON object.
removeDescription
  :: ()
  => (HasCallStack, MonadTest m)
  => Value
  -> m Value
removeDescription v =
  case v of
    Object inner ->
      return $ Object $ Aeson.KeyMap.delete (Aeson.fromText "description") inner
    Array _ -> failWrongType "array"
    Number _ -> failWrongType "number"
    Bool _ -> failWrongType "bool"
    String _ -> failWrongType "string"
    Null -> failWrongType "null"
 where
  failWrongType got = do
    H.note_ $ "Expected object but got: " <> got
    H.failure

-- | @redactJsonStringFieldInFile [(k0, v0), (k1, v1), ..] sourceFilePath targetFilePath@ reads the JSON at @sourceFilePath@, and then
-- replaces the value associated to @k0@ by @v0@, replaces the value associated to @k1@ by @v1@, etc.
-- Then the obtained JSON is written to @targetFilePath@. This replacement is done recursively
-- so @k0@, @k1@, etc. can appear at any depth within the JSON.
redactJsonFieldsInFile
  :: ()
  => MonadTest m
  => MonadIO m
  => HasCallStack
  => Map.Map Text Text
  -- ^ Map from key name, to the new (String) value to attach to this key
  -> FilePath
  -> FilePath
  -> m ()
redactJsonFieldsInFile changes sourceFilePath targetFilePath = GHC.withFrozenCallStack $ do
  contents <- H.evalIO $ LBS.readFile sourceFilePath
  case eitherDecode contents :: Either String Value of
    Left err -> do
      H.note_ $ "Failed to decode JSON: " <> err
      H.success
    Right json -> do
      let redactedJson = redactJsonFields changes json
      H.evalIO $ LBS.writeFile targetFilePath $ encodePretty redactedJson

redactJsonFields :: () => Map.Map Text Text -> Value -> Value
redactJsonFields changes v =
  case v of
    Object obj ->
      let obj' =
            Aeson.KeyMap.mapWithKey
              ( \k v' ->
                  case Map.lookup (toText k) changes of
                    Just replacement -> String replacement
                    Nothing -> recurse v'
              )
              obj
       in Object obj'
    Array vector ->
      Array $ Vector.map recurse vector
    _ -> v
 where
  recurse = redactJsonFields changes
