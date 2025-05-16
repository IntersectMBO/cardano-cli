{-# LANGUAGE RankNTypes #-}

module Cardano.CLI.EraIndependent.Cip.Cip129.Run
  ( runCip129
  )
where

import Cardano.Api hiding (Cip129)

import Cardano.CLI.Compatible.Exception
import Cardano.CLI.EraIndependent.Cip.Cip129.Command
import Cardano.CLI.EraIndependent.Cip.Cip129.Internal.Conversion
import Cardano.CLI.EraIndependent.Cip.Common
import Cardano.CLI.Orphan ()
import Cardano.CLI.Read
import Cardano.CLI.Read.Committee.ColdKey
import Cardano.CLI.Read.Committee.HotKey
import Cardano.CLI.Read.DRep
import Cardano.CLI.Read.GovernanceActionId

import Data.ByteString (ByteString)
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
  renderOutput cip129Output out
runCip129 (Cip129CommitteeHotKey inp out) = do
  k <- case inp of
    InputTextEnvelopeFile (File textEnvFp) -> do
      f <- liftIO $ fileOrPipe textEnvFp
      fromEitherIOCli $ readCommitteeHotVerificationKeyFile f
    InputHexText t ->
      fromEitherCli . Valid.toEither $ readCommitteeHotHexVerificationKeyText t
    InputBech32Text t ->
      fromEitherCli . Valid.toEither $ readCommitteeHotBech32VerificationKeyText t
  let cip129Output = Text.encodeUtf8 $ encodeCip129CommitteeHotVerficationKeyText k
  renderOutput cip129Output out
runCip129 (Cip129CommitteeColdKey inp out) = do
  k <- case inp of
    InputTextEnvelopeFile (File textEnvFp) -> do
      f <- liftIO $ fileOrPipe textEnvFp
      fromEitherIOCli $ readCommitteeColdVerificationKeyFile f
    InputHexText t ->
      fromEitherCli . Valid.toEither $ readCommitteeColdHexVerificationKeyText t
    InputBech32Text t ->
      fromEitherCli . Valid.toEither $ readCommitteeColdBech32VerificationKeyText t
  let cip129Output = Text.encodeUtf8 $ encodeCip129CommitteeColdVerficationKeyText k
  renderOutput cip129Output out
runCip129 (Cip129GovernanceAction inp out) =
  case inp of
    InputHexText t -> do
      govId <- fromEitherCli $ readGoveranceActionIdHexText t
      let cip129Output = Text.encodeUtf8 $ encodeCip129GovernanceActionIdText govId
      renderOutput cip129Output out
    InputBech32Text{} ->
      throwCliError $ InputError "Bech32 encoded Governance Action Id is not supported"
    InputTextEnvelopeFile{} ->
      throwCliError $ InputError "TextEnvelope encoded Governance Action Id is not supported"

renderOutput :: ByteString -> Output -> CIO e ()
renderOutput cip129Output out = case out of
  OutputText -> do
    liftIO $ BSC.hPutStrLn stdout cip129Output
  OutputFile (File fp) -> do
    liftIO $ BS.writeFile fp cip129Output
