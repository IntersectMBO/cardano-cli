{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cli.Shelley.Genesis.Create
  ( hprop_shelleyGenesisCreate
  )
where

import Control.Monad (void)
import Data.Aeson qualified as J
import Data.Aeson.Key qualified as J
import Data.Aeson.Types qualified as J
import Data.Bifunctor (Bifunctor (..))
import Data.ByteString.Lazy qualified as LBS
import Data.Foldable (for_)
import Data.HashMap.Lazy qualified as HM
import Data.Set qualified as S
import Data.Time.Clock qualified as DT
import GHC.Exts (IsList (..))

import Test.Cardano.CLI.Aeson (assertHasMappings)
import Test.Cardano.CLI.Util as OP

import Hedgehog (Property, forAll, (===))
import Hedgehog qualified as H
import Hedgehog.Extras.Stock.Time qualified as H
import Hedgehog.Extras.Test.Base qualified as H
import Hedgehog.Extras.Test.File qualified as H
import Hedgehog.Gen qualified as G
import Hedgehog.Range qualified as R

{- HLINT ignore "Use <$>" -}
{- HLINT ignore "Use camelCase" -}
{- HLINT ignore "Use mapM" -}
{- HLINT ignore "Reduce duplication" -}

parseMaxLovelaceSupply :: J.Value -> J.Parser Int
parseMaxLovelaceSupply = J.withObject "Object" $ \o -> o J..: "maxLovelaceSupply"

parseSystemStart :: J.Value -> J.Parser String
parseSystemStart = J.withObject "Object" $ \o -> o J..: "systemStart"

parseHashMap :: J.Value -> J.Parser (HM.HashMap String J.Value)
parseHashMap (J.Object hm) = pure $ fromList $ fmap (first J.toString) (toList hm)
parseHashMap v = J.typeMismatch "Object" v

parseDelegateCount :: J.Value -> J.Parser Int
parseDelegateCount = J.withObject "Object" $ \o -> do
  delegates <- (o J..: "genDelegs") >>= parseHashMap
  pure $ HM.size delegates

parseDelegateKey :: J.Value -> J.Parser String
parseDelegateKey = J.withObject "Object" $ \o -> o J..: "delegate"

parseDelegateKeys :: J.Value -> J.Parser [String]
parseDelegateKeys = J.withObject "Object" $ \o -> do
  delegates <- (o J..: "genDelegs") >>= parseHashMap
  sequence $ fmap (parseDelegateKey . snd) (toList delegates)

parseHashKeys :: J.Value -> J.Parser [String]
parseHashKeys = J.withObject "Object" $ \o -> do
  delegates <- (o J..: "genDelegs") >>= parseHashMap
  pure $ fmap fst (toList delegates)

parseTotalSupply :: J.Value -> J.Parser Int
parseTotalSupply = J.withObject "Object" $ \o -> do
  initialFunds <- (o J..: "initialFunds") >>= parseHashMap
  fmap sum (sequence (fmap (J.parseJSON @Int . snd) (toList initialFunds)))

hprop_shelleyGenesisCreate :: Property
hprop_shelleyGenesisCreate =
  watchdogProp . propertyOnce $ do
    H.moduleWorkspace "tmp" $ \tempDir -> do
      sourceGenesisSpecFile <-
        noteInputFile "test/cardano-cli-test/files/input/shelley/genesis/genesis.spec.json"
      sourceAlonzoGenesisSpecFile <-
        noteInputFile "test/cardano-cli-test/files/input/alonzo/genesis.alonzo.spec.json"
      sourceConwayGenesisSpecFile <-
        noteInputFile "test/cardano-cli-test/files/input/conway/genesis.conway.spec.json"

      genesisSpecFile <- noteTempFile tempDir "genesis.spec.json"
      alonzoSpecFile <- noteTempFile tempDir "genesis.alonzo.spec.json"
      conwaySpecFile <- noteTempFile tempDir "genesis.conway.spec.json"

      H.copyFile sourceGenesisSpecFile genesisSpecFile
      H.copyFile sourceAlonzoGenesisSpecFile alonzoSpecFile
      H.copyFile sourceConwayGenesisSpecFile conwaySpecFile

      let genesisFile = tempDir <> "/genesis.json"

      fmtStartTime <- fmap H.formatIso8601 $ H.evalIO DT.getCurrentTime

      (supply, fmtSupply) <- fmap (OP.withSnd show) $ forAll $ G.int (R.linear 10_000_000 4_000_000_000)
      (delegateCount, fmtDelegateCount) <- fmap (OP.withSnd show) $ forAll $ G.int (R.linear 4 19)
      (utxoCount, fmtUtxoCount) <- fmap (OP.withSnd show) $ forAll $ G.int (R.linear 4 19)

      -- Create the genesis json file and required keys
      void $
        execCardanoCLI
          [ "latest"
          , "genesis"
          , "create"
          , "--testnet-magic"
          , "12"
          , "--start-time"
          , fmtStartTime
          , "--supply"
          , fmtSupply
          , "--gen-genesis-keys"
          , fmtDelegateCount
          , "--gen-utxo-keys"
          , fmtUtxoCount
          , "--genesis-dir"
          , tempDir
          ]

      H.assertFilesExist [genesisFile]

      genesisContents <- H.evalIO $ LBS.readFile genesisFile

      actualJson <- H.evalEither $ J.eitherDecode genesisContents
      actualSupply <- H.evalEither $ J.parseEither parseMaxLovelaceSupply actualJson
      actualStartTime <- H.evalEither $ J.parseEither parseSystemStart actualJson
      actualDelegateCount <- H.evalEither $ J.parseEither parseDelegateCount actualJson
      actualTotalSupply <- H.evalEither $ J.parseEither parseTotalSupply actualJson
      actualHashKeys <- H.evalEither $ J.parseEither parseHashKeys actualJson
      actualDelegateKeys <- H.evalEither $ J.parseEither parseDelegateKeys actualJson

      actualSupply === supply
      actualStartTime === fmtStartTime
      actualDelegateCount === delegateCount
      actualDelegateCount === utxoCount
      actualTotalSupply === supply - 1_000_000 -- Check that the sum of the initial fund amounts matches the total supply
      -- We don't use the entire supply so there is ada in the treasury. This is
      -- required for stake pool rewards.

      -- Check uniqueness and count of hash keys
      S.size (fromList actualHashKeys) === length actualHashKeys -- This isn't strictly necessary because we use aeson which guarantees uniqueness of keys
      S.size (fromList actualHashKeys) === delegateCount

      -- Check uniqueness and count of hash keys
      S.size (fromList actualDelegateKeys) === length actualDelegateKeys
      S.size (fromList actualDelegateKeys) === delegateCount

      for_ [1 .. delegateCount] $ \i -> do
        -- Check Genesis keys
        assertHasMappings [("type", "GenesisVerificationKey_ed25519")] $
          tempDir <> "/genesis-keys/genesis" <> show i <> ".vkey"
        assertHasMappings [("type", "GenesisSigningKey_ed25519")] $
          tempDir <> "/genesis-keys/genesis" <> show i <> ".skey"

        H.assertEndsWithSingleNewline $ tempDir <> "/genesis-keys/genesis" <> show i <> ".skey"
        H.assertEndsWithSingleNewline $ tempDir <> "/genesis-keys/genesis" <> show i <> ".vkey"

        -- Check delegate keys
        assertHasMappings [("type", "GenesisDelegateSigningKey_ed25519")] $
          tempDir <> "/delegate-keys/delegate" <> show i <> ".skey"
        assertHasMappings [("type", "GenesisDelegateVerificationKey_ed25519")] $
          tempDir <> "/delegate-keys/delegate" <> show i <> ".vkey"
        assertHasMappings [("type", "NodeOperationalCertificateIssueCounter")] $
          tempDir <> "/delegate-keys/delegate" <> show i <> ".counter"

        H.assertEndsWithSingleNewline $ tempDir <> "/delegate-keys/delegate" <> show i <> ".skey"
        H.assertEndsWithSingleNewline $ tempDir <> "/delegate-keys/delegate" <> show i <> ".vkey"
        H.assertEndsWithSingleNewline $ tempDir <> "/delegate-keys/delegate" <> show i <> ".counter"

        -- Check utxo keys
        assertHasMappings [("type", "GenesisUTxOSigningKey_ed25519")] $
          tempDir <> "/utxo-keys/utxo" <> show i <> ".skey"
        assertHasMappings [("type", "GenesisUTxOVerificationKey_ed25519")] $
          tempDir <> "/utxo-keys/utxo" <> show i <> ".vkey"

        H.assertEndsWithSingleNewline $ tempDir <> "/utxo-keys/utxo" <> show i <> ".skey"
        H.assertEndsWithSingleNewline $ tempDir <> "/utxo-keys/utxo" <> show i <> ".vkey"

    H.moduleWorkspace "tmp" $ \tempDir -> do
      let genesisFile = tempDir <> "/genesis.json"

      fmtStartTime <- fmap H.formatIso8601 $ H.evalIO DT.getCurrentTime

      (supply, fmtSupply) <- fmap (OP.withSnd show) $ forAll $ G.int (R.linear 10_000_000 4_000_000_000)
      (delegateCount, fmtDelegateCount) <- fmap (OP.withSnd show) $ forAll $ G.int (R.linear 4 19)
      (utxoCount, fmtUtxoCount) <- fmap (OP.withSnd show) $ forAll $ G.int (R.linear 4 19)

      sourceAlonzoGenesisSpecFile <-
        noteInputFile "test/cardano-cli-test/files/input/alonzo/genesis.alonzo.spec.json"
      alonzoSpecFile <- noteTempFile tempDir "genesis.alonzo.spec.json"
      H.copyFile sourceAlonzoGenesisSpecFile alonzoSpecFile

      sourceConwayGenesisSpecFile <-
        noteInputFile "test/cardano-cli-test/files/input/conway/genesis.conway.spec.json"
      conwaySpecFile <- noteTempFile tempDir "genesis.conway.spec.json"
      H.copyFile sourceConwayGenesisSpecFile conwaySpecFile

      -- Create the genesis json file and required keys
      void $
        execCardanoCLI
          [ "latest"
          , "genesis"
          , "create"
          , "--testnet-magic"
          , "12"
          , "--start-time"
          , fmtStartTime
          , "--supply"
          , fmtSupply
          , "--gen-genesis-keys"
          , fmtDelegateCount
          , "--gen-utxo-keys"
          , fmtUtxoCount
          , "--genesis-dir"
          , tempDir
          ]

      H.assertFilesExist [genesisFile]

      genesisContents <- H.evalIO $ LBS.readFile genesisFile

      actualJson <- H.evalEither $ J.eitherDecode genesisContents
      actualSupply <- H.evalEither $ J.parseEither parseMaxLovelaceSupply actualJson
      actualStartTime <- H.evalEither $ J.parseEither parseSystemStart actualJson
      actualDelegateCount <- H.evalEither $ J.parseEither parseDelegateCount actualJson
      actualTotalSupply <- H.evalEither $ J.parseEither parseTotalSupply actualJson
      actualHashKeys <- H.evalEither $ J.parseEither parseHashKeys actualJson
      actualDelegateKeys <- H.evalEither $ J.parseEither parseDelegateKeys actualJson

      actualSupply === supply
      actualStartTime === fmtStartTime
      actualDelegateCount === delegateCount
      actualDelegateCount === utxoCount
      actualTotalSupply === supply - 1_000_000 -- Check that the sum of the initial fund amounts matches the total supply
      -- We don't use the entire supply so there is ada in the treasury. This is
      -- required for stake pool rewards.
      -- Check uniqueness and count of hash keys
      S.size (fromList actualHashKeys) === length actualHashKeys -- This isn't strictly necessary because we use aeson which guarantees uniqueness of keys
      S.size (fromList actualHashKeys) === delegateCount

      -- Check uniqueness and count of hash keys
      S.size (fromList actualDelegateKeys) === length actualDelegateKeys
      S.size (fromList actualDelegateKeys) === delegateCount

      for_ [1 .. delegateCount] $ \i -> do
        -- Check Genesis keys
        assertHasMappings [("type", "GenesisSigningKey_ed25519")] $
          tempDir <> "/genesis-keys/genesis" <> show i <> ".skey"
        assertHasMappings [("type", "GenesisVerificationKey_ed25519")] $
          tempDir <> "/genesis-keys/genesis" <> show i <> ".vkey"

        H.assertEndsWithSingleNewline $ tempDir <> "/genesis-keys/genesis" <> show i <> ".skey"
        H.assertEndsWithSingleNewline $ tempDir <> "/genesis-keys/genesis" <> show i <> ".vkey"

        -- Check delegate keys
        assertHasMappings [("type", "GenesisDelegateSigningKey_ed25519")] $
          tempDir <> "/delegate-keys/delegate" <> show i <> ".skey"
        assertHasMappings [("type", "GenesisDelegateVerificationKey_ed25519")] $
          tempDir <> "/delegate-keys/delegate" <> show i <> ".vkey"
        assertHasMappings [("type", "NodeOperationalCertificateIssueCounter")] $
          tempDir <> "/delegate-keys/delegate" <> show i <> ".counter"

        H.assertEndsWithSingleNewline $ tempDir <> "/delegate-keys/delegate" <> show i <> ".skey"
        H.assertEndsWithSingleNewline $ tempDir <> "/delegate-keys/delegate" <> show i <> ".vkey"
        H.assertEndsWithSingleNewline $ tempDir <> "/delegate-keys/delegate" <> show i <> ".counter"

        -- Check utxo keys
        assertHasMappings [("type", "GenesisUTxOSigningKey_ed25519")] $
          tempDir <> "/utxo-keys/utxo" <> show i <> ".skey"
        assertHasMappings [("type", "GenesisUTxOVerificationKey_ed25519")] $
          tempDir <> "/utxo-keys/utxo" <> show i <> ".vkey"

        H.assertEndsWithSingleNewline $ tempDir <> "/utxo-keys/utxo" <> show i <> ".skey"
        H.assertEndsWithSingleNewline $ tempDir <> "/utxo-keys/utxo" <> show i <> ".vkey"
