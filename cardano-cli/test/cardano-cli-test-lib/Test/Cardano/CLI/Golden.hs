{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.CLI.Golden
  ( testAllErrorMessages
  , testAllErrorMessages_
  , testErrorMessage
  )
where

import Cardano.Api (Error (..))
import Cardano.Api.Pretty

import Data.Data
import GHC.Stack (HasCallStack, withFrozenCallStack)
import System.FilePath ((</>))

import Test.Cardano.CLI.Util

import Hedgehog
import Hedgehog.Extras.Test qualified as H
import Test.Tasty
import Test.Tasty.Hedgehog

-- | Generate test tree for the list of values. This 'TestTree' will serialize the values using 'Error'
-- instance and compare them against golden files in the provided location.
testAllErrorMessages
  :: forall a
   . (HasCallStack, Data a, Error a)
  => FilePath
  -- ^ golden files location
  -> [a]
  -- ^ list of values to test against
  -> TestTree
testAllErrorMessages goldenFilesLocation errs = withFrozenCallStack $ do
  -- 'err' here is only needed for its 'Data' instance and it's never evaluated
  -- it's equivalent of having @err = undefined :: a@
  let err = undefined :: a
      typeName = show $ typeOf err
      testedConstructors = map toConstr errs
      allConstructors = dataTypeConstrs $ dataTypeOf err
      notTestedConstructors = [c | c <- allConstructors, c `notElem` testedConstructors]
      testAllConstructors =
        testProperty "check if all constructors are tested" . withTests 1 . watchdogProp . property $ do
          H.note_ $ "Untested constructors: " <> show notTestedConstructors
          notTestedConstructors === []

  testGroup typeName $
    testAllConstructors : map (testErrorMessage goldenFilesLocation) errs

-- | Creates error messages for all values and tests them against the golden files.
--
-- An escape hatch when adding of 'Data a' instance gets impossible (like when we embed 'TypeRep' in our error
-- data types) or requires significant multi-package changes and outweighs the benefits here.
testAllErrorMessages_
  :: forall a
   . (HasCallStack, Error a)
  => FilePath
  -- ^ golden files path
  -> String
  -- ^ module name
  -> String
  -- ^ type name
  -> [(String, a)]
  -- ^ list of constructor names and values
  -> TestTree
testAllErrorMessages_ goldenFilesLocation moduleName typeName errs = withFrozenCallStack $ do
  testGroup typeName $
    fmap (uncurry $ testErrorMessage_ goldenFilesLocation moduleName typeName) errs

-- | Create 'TestTree' validating serialized value @a@ using 'Error' against the golden files.
testErrorMessage
  :: (HasCallStack, Data a, Error a)
  => FilePath
  -- ^ golden files path
  -> a
  -- ^ value to test
  -> TestTree
testErrorMessage goldenFilesLocation err = withFrozenCallStack $ do
  let errTypeRep = typeOf err
      typeName = show errTypeRep
      moduleName = tyConModule $ typeRepTyCon errTypeRep
      constructorName = show $ toConstr err
  testErrorMessage_ goldenFilesLocation moduleName typeName constructorName err

-- | Create 'TestTree' validating serialized value @a@ using 'Error' against the golden files.
--
-- Requires providing a module name, a type name and a constructor name of @a@. Useful when 'Data a'
-- instance is not available.
testErrorMessage_
  :: (HasCallStack, Error a)
  => FilePath
  -- ^ golden files path
  -> String
  -- ^ module name
  -> String
  -- ^ type name
  -> String
  -- ^ constructor name
  -> a
  -- ^ value to test
  -> TestTree
testErrorMessage_ goldenFilesLocation moduleName typeName constructorName err = withFrozenCallStack $ do
  let fqtn = moduleName <> "." <> typeName
  testProperty constructorName . withTests 1 . watchdogProp . property $ do
    H.note_ "Incorrect error message in golden file"
    H.note_ "What the value looks like in memory"
    let pErr = docToString (prettyError err)
    H.note_ $ show pErr
    H.diffVsGoldenFile
      pErr
      (goldenFilesLocation </> fqtn </> constructorName <> ".txt")
