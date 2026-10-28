{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
      friendlyCertificate era outputFormat mOutFile certificate

data AnyCertificate where
  AnyCertificate
    :: Exp.Era era
    -> Exp.Certificate (Exp.LedgerEra era)
    -> AnyCertificate

readCertificate
  :: FileOrPipe
  -> IO (Either (FileError TextEnvelopeError) AnyCertificate)
readCertificate =
  readFileOrPipeTextEnvelopeAnyOf
    [ Exp.obtainCommonConstraints Exp.ConwayEra $
        FromSomeType Exp.AsCertificate (AnyCertificate Exp.ConwayEra)
    , Exp.obtainCommonConstraints Exp.DijkstraEra $
        FromSomeType Exp.AsCertificate (AnyCertificate Exp.DijkstraEra)
    ]
