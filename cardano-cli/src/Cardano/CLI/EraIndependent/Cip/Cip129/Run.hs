{-# LANGUAGE RankNTypes #-}

module Cardano.CLI.EraIndependent.Cip.Cip129.Run
  ( runCip129
  )
where

import Cardano.Api

import Cardano.CLI.Compatible.Exception
import Cardano.CLI.EraIndependent.Cip.Cip129.Command
import Cardano.CLI.EraIndependent.Cip.Cip129.Conversion
import Cardano.CLI.EraIndependent.Cip.Common
import Cardano.CLI.Orphan ()
import Cardano.CLI.Read
import Cardano.CLI.Read.DRep

import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.Text.Encoding qualified as Text
import Data.Validation qualified as Valid
import System.IO

runCip129 :: Cip129 -> CIO e ()
runCip129 (Cip129DRep inp out) = do
  k <- case inp of
    InputTextEnvelopeFile (File textEnvFp) -> do
      f <- liftIO $ fileOrPipe textEnvFp
      fromEitherIOCli $ readDrepVerificationKeyFile f
    InputHexText t -> do
      fromEitherCli . Valid.toEither $ readDRepHexVerificationKeyText t
    InputBech32Text t -> do
      fromEitherCli . Valid.toEither $ readDRepBech32VerificationKeyText t
  let cip129Output = Text.encodeUtf8 $ encodeCip129DrepVerficationKeyText k
  case out of
    OutputText ->
      liftIO $ BSC.hPutStrLn stdout cip129Output
    OutputFile (File fp) ->
      liftIO $ BS.writeFile fp cip129Output
runCip129 (Cip129CommitteeHotKey _inp _out) = undefined
runCip129 (Cip129CommitteeColdKey _inp _out) = undefined
runCip129 (Cip129GovernanceAction _inp _out) = undefined
