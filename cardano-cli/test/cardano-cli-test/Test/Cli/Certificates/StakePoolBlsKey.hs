{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cli.Certificates.StakePoolBlsKey where

import Cardano.Api
import Cardano.Api.Experimental qualified as Exp
import Cardano.Api.Ledger qualified as L

import Cardano.Ledger.BaseTypes (Port (..), textToDns)

import Control.Monad (void)
import Control.Monad.Morph (hoist)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Array.Byte (ByteArray)
import GHC.Exts (fromList)

import Test.Gen.Cardano.Api.Typed (genCertificate, genSigningKey)

import Test.Cardano.CLI.Util (execCardanoCLI, noteTempFile)

import Hedgehog (Gen, Property, forAll, (===))
import Hedgehog qualified as H
import Hedgehog.Extras qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

-- | The era the command under test runs in.
type TestEra = Exp.DijkstraEra

-- | Running the command on a registration certificate with any pool parameters yields a
-- certificate with the same parameters and the new BLS key.
-- Execute me with:
-- @cabal test cardano-cli-test --test-options '-p "/stake pool bls registration certificate keeps params/"'@
hprop_stake_pool_bls_registration_certificate_keeps_params :: Property
hprop_stake_pool_bls_registration_certificate_keeps_params =
  H.withTests 20 . H.property . hoist runResourceT . H.moduleWorkspace "tmp" $ \tempDir -> do
    oldParams <- forAll $ genStakePoolParams @TestEra
    newSigningKey <- forAll $ genSigningKey AsBlsKey

    certFile <- noteTempFile tempDir "registration.cert"
    blsSigningKeyFile <- noteTempFile tempDir "bls.skey"
    outFile <- noteTempFile tempDir "bls-registration.cert"

    writeRegistrationCertificate @TestEra certFile oldParams
    H.leftFailM . H.evalIO $ writeFileTextEnvelope (File blsSigningKeyFile) Nothing newSigningKey

    void . execCardanoCLI $
      [ "dijkstra"
      , "stake-pool"
      , "bls-registration-certificate"
      , "offline"
      , "--registration-certificate-file"
      , certFile
      , "--bls-signing-key-file"
      , blsSigningKeyFile
      , "--out-file"
      , outFile
      ]

    newParams <- readPoolParams @TestEra outFile

    newParams === oldParams{L.sppBlsKey = L.SJust (toLedgerBlsKey newSigningKey)}

readPoolParams
  :: forall era m
   . (Exp.IsEra era, H.MonadTest m, MonadIO m)
  => FilePath -> m (L.StakePoolParams (Exp.LedgerEra era))
readPoolParams file = Exp.obtainCommonConstraints (Exp.useEra @era) $ do
  Exp.Certificate cert <-
    H.leftFailM . H.evalIO $
      readFileTextEnvelope @(Exp.Certificate (Exp.LedgerEra era)) (File file)
  H.evalMaybe $ L.getRegPoolTxCert cert

writeRegistrationCertificate
  :: forall era m
   . (Exp.IsEra era, H.MonadTest m, MonadIO m)
  => FilePath -> L.StakePoolParams (Exp.LedgerEra era) -> m ()
writeRegistrationCertificate file params = Exp.obtainCommonConstraints (Exp.useEra @era) $ do
  let cert :: Exp.Certificate (Exp.LedgerEra era)
      cert = Exp.makeStakePoolRegistrationCertificate params
      write :: IO (Either (FileError ()) ())
      write =
        writeLazyByteStringFile
          (File file)
          (textEnvelopeToJSON (Just "Stake Pool Registration Certificate") cert)
  H.leftFailM $ H.evalIO write

-- | Parameters from the ledger generator, with a BLS key that is present or absent, a 32 byte
-- metadata hash and relays of every kind that needs no IP address added.
genStakePoolParams
  :: forall era
   . Exp.IsEra era
  => Gen (L.StakePoolParams (Exp.LedgerEra era))
genStakePoolParams = Exp.obtainCommonConstraints (Exp.useEra @era) $ do
  baseParams <-
    Gen.just $
      (\(Exp.Certificate cert) -> L.getRegPoolTxCert cert)
        <$> genCertificate (convert $ Exp.useEra @era)
  blsKey <- Gen.maybe $ toLedgerBlsKey <$> genSigningKey AsBlsKey
  metadata <- traverse genMetadataHash $ L.sppMetadata baseParams
  dnsName <- textToDns 64 "relay.example.com"
  extraRelays <- Gen.subsequence $ extraRelayCandidates dnsName
  pure
    baseParams
      { L.sppBlsKey = maybe L.SNothing L.SJust blsKey
      , L.sppMetadata = metadata
      , L.sppRelays = L.sppRelays baseParams <> fromList extraRelays
      }
 where
  genMetadataHash poolMetadata = do
    hashBytes <- Gen.list (Range.singleton 32) $ Gen.word8 Range.constantBounded
    pure poolMetadata{L.pmHash = fromList hashBytes :: ByteArray}

  extraRelayCandidates :: L.DnsName -> [L.StakePoolRelay]
  extraRelayCandidates dnsName =
    [ L.SingleHostAddr L.SNothing L.SNothing L.SNothing
    , L.SingleHostName (L.SJust $ Port 3001) dnsName
    , L.MultiHostName dnsName
    ]
