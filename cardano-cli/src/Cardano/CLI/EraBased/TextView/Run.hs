{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.CLI.EraBased.TextView.Run
  ( runTextViewCmds
  , runTextViewInfoCmd
  )
where

import Cardano.Api

import Cardano.CLI.Compatible.Exception
import Cardano.CLI.EraBased.TextView.Command
import Cardano.CLI.Helper (cborToText)
import Cardano.CLI.Json.Encode qualified as Json
import Cardano.CLI.Type.Common

import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Function ((&))
import Data.Text.Encoding qualified as Text
import Vary qualified

runTextViewCmds :: TextViewCmds era -> CIO e ()
runTextViewCmds = \case
  TextViewDecodeCborCmd cmd -> runTextViewInfoCmd cmd

runTextViewInfoCmd
  :: ()
  => TextViewDecodeCborCmdArgs
  -> CIO e ()
runTextViewInfoCmd
  TextViewDecodeCborCmdArgs
    { inputFile
    , outputFormat
    , mOutFile
    } = do
    tv <- fromEitherIOCli $ readTextEnvelopeFromFile inputFile
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

    fromEitherIOCli @(FileError ()) $
      writeLazyByteStringOutput mOutFile output
