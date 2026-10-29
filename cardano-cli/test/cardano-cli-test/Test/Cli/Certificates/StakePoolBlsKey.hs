{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cli.Certificates.StakePoolBlsKey where

import Cardano.Api
import Cardano.Api.Experimental qualified as Exp
import Cardano.Api.Ledger qualified as L

import Control.Monad (void)
import Control.Monad.Morph (hoist)
import Control.Monad.Trans.Resource (runResourceT)

import Test.Gen.Cardano.Api.Typed (genSigningKey)

import Test.Cardano.CLI.Util (execCardanoCLI, noteTempFile)
import Test.Gen.Cardano.CLI.StakePool (genStakePoolParams)

import Hedgehog (Property, forAll, (===))
import Hedgehog qualified as H
import Hedgehog.Extras qualified as H

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
