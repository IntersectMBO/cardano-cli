{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.CLI.EraIndependent.Debug.CertificateView.Run
  ( runCertificateViewCmd
  )
where

import Cardano.Api
import Cardano.Api.Experimental qualified as Exp

import Cardano.CLI.Compatible.Exception
  ( CIO
  , fromEitherIOCli
  )
import Cardano.CLI.Compatible.Json.Friendly
  ( friendlyCertificate
  )
import Cardano.CLI.EraIndependent.Debug.CertificateView.Command
import Cardano.CLI.Read
import Cardano.CLI.Type.Common

runCertificateViewCmd
  :: ()
  => CertificateViewCmdArgs
  -> CIO e ()
runCertificateViewCmd
  CertificateViewCmdArgs
    { outputFormat
    , mOutFile
    , inputCertificateFile
    } = do
    certificateFile <-
      liftIO $ fileOrPipe $ unCertificateFile inputCertificateFile
    AnyCertificate era certificate <-
      fromEitherIOCli $ readCertificate certificateFile
    fromEitherIOCli @(FileError ()) $
      friendlyCertificate outputFormat mOutFile era certificate

data AnyCertificate where
  AnyCertificate
    :: Exp.Era era
    -> Exp.Certificate (Exp.LedgerEra era)
    -> AnyCertificate

-- | Every era writes the same text envelope type. Try Dijkstra first, then
-- Conway, which also reads registration certificates without a deposit.
readCertificate
  :: FileOrPipe
  -> IO (Either (FileError TextEnvelopeError) AnyCertificate)
readCertificate file = do
  result <- readAs Exp.DijkstraEra
  case result of
    Left (FileError _ (TextEnvelopeDecodeError _)) -> readAs Exp.ConwayEra
    _ -> pure result
 where
  readAs :: Exp.Era era -> IO (Either (FileError TextEnvelopeError) AnyCertificate)
  readAs era =
    readFileOrPipeTextEnvelopeAnyOf
      [ Exp.obtainCommonConstraints era $
          FromSomeType Exp.AsCertificate (AnyCertificate era)
      ]
      file
