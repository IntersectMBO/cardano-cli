module Test.Golden.Util where

import Data.Text qualified as Text
import GHC.Exts qualified as GHC
import GHC.Stack (HasCallStack)
import GHC.Stack qualified as GHC
import System.FilePath (joinPath)

import Hedgehog.Extras (UnitIO)
import Hedgehog.Extras qualified as H

noteGoldenFile
  :: HasCallStack
  => FilePath
  -> UnitIO FilePath
noteGoldenFile fileName =
  GHC.withFrozenCallStack $ do
    case GHC.toList GHC.callStack of
      top : _ -> do
        let dirs = Text.unpack $ Text.intercalate "/" $ Text.splitOn "." $ Text.pack $ GHC.srcLocModule (snd top)

        H.note $ joinPath ["test/cardano-cli-golden/files/golden", dirs, fileName]
      _ -> H.failMessage GHC.callStack $ "No call stack available for golden file: " <> fileName
