{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

{- HLINT ignore "Redundant id" -}

module Cardano.CLI.EraBased.TextView.Run
  ( runTextViewCmds
  , runTextViewInfoCmd
  )
where

import Cardano.Api

import Cardano.CLI.EraBased.TextView.Command
import Cardano.CLI.Helper (cborToText)
import Cardano.CLI.Json.Encode qualified as Json
import Cardano.CLI.Type.Common
import Cardano.CLI.Type.Error.TextViewFileError

import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Function ((&))
import Data.Text.Encoding qualified as Text
import Vary qualified

runTextViewCmds :: TextViewCmds era -> ExceptT TextViewFileError IO ()
runTextViewCmds = \case
  TextViewDecodeCborCmd cmd -> runTextViewInfoCmd cmd

runTextViewInfoCmd
  :: ()
  => TextViewDecodeCborCmdArgs
  -> ExceptT TextViewFileError IO ()
runTextViewInfoCmd
  TextViewDecodeCborCmdArgs
    { inputFile
    , outputFormat
    , mOutFile
    } = do
    tv <- firstExceptT TextViewReadFileError $ newExceptT (readTextEnvelopeFromFile inputFile)
    let lbCBOR = LBS.fromStrict (textEnvelopeRawCBOR tv)

    output <-
      outputFormat
        & ( id
              . Vary.on (\FormatCborHex -> pure lbCBOR)
              . Vary.on (\FormatJson -> pure $ Json.encodeJson tv)
              . Vary.on (\FormatText -> LBS.fromStrict . Text.encodeUtf8 <$> cborToText lbCBOR)
              . Vary.on (\FormatYaml -> pure $ Json.encodeYaml tv)
              $ Vary.exhaustiveCase
          )
        & firstExceptT TextViewCBORPrettyPrintError

    firstExceptT TextViewWriteFileError
      . newExceptT
      $ writeLazyByteStringOutput mOutFile output
