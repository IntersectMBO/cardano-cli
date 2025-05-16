{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.Helper
  ( HelpersError (..)
  , cborToText
  , cborToTextByteString
  , cborToTextLazyByteString
  , printWarning
  , deprecationWarning
  , ensureNewFile
  , ensureNewFileLBS
  , pPrintCBOR
  , readCBOR
  , renderHelpersError
  , validateCBOR
  , printEraDeprecationWarning
  )
where

import Cardano.Api
import Cardano.Api.Byron qualified as Byron
import Cardano.Api.Ledger qualified as L

import Cardano.CLI.Type.Common

import Codec.CBOR.Pretty (prettyHexEnc)
import Codec.CBOR.Read (DeserialiseFailure, deserialiseFromBytes)
import Codec.CBOR.Term (decodeTerm, encodeTerm)
import Control.Exception (Exception (..), IOException)
import Control.Monad (unless, when)
import Data.Bifunctor (Bifunctor (..))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LB
import Data.ByteString.Lazy qualified as LBS
import Data.Functor (void)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.IO qualified as Text
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Encoding qualified as LT
import Data.Typeable (Typeable)
import System.Console.ANSI
import System.Console.ANSI qualified as ANSI
import System.Directory qualified as IO
import System.IO qualified as IO

data HelpersError
  = CBORPrettyPrintError !DeserialiseFailure
  | CBORDecodingError !DeserialiseFailure
  | IOError' !FilePath !IOException
  | OutputMustNotAlreadyExist FilePath
  | ReadCBORFileFailure !FilePath !Text
  deriving Show

instance Error HelpersError where
  prettyError = renderHelpersError

renderHelpersError :: HelpersError -> Doc ann
renderHelpersError = \case
  OutputMustNotAlreadyExist fp ->
    "Output file/directory must not already exist: " <> pretty fp
  ReadCBORFileFailure fp err' ->
    "CBOR read failure at: " <> pretty fp <> pshow err'
  CBORPrettyPrintError err' ->
    "Error with CBOR decoding: " <> pshow err'
  CBORDecodingError err' ->
    "Error with CBOR decoding: " <> pshow err'
  IOError' fp ioE ->
    "Error at: " <> pretty fp <> " Error: " <> pshow ioE

decodeCBOR
  :: LB.ByteString
  -> (forall s. L.Decoder s a)
  -> Either HelpersError (LB.ByteString, a)
decodeCBOR bs decoder =
  first CBORDecodingError $ deserialiseFromBytes decoder bs

printWarning :: MonadIO m => String -> m ()
printWarning warning = liftIO $ do
  ANSI.hSetSGR IO.stderr [SetColor Foreground Vivid Yellow]
  IO.hPutStrLn IO.stderr $ "WARNING: " <> warning
  ANSI.hSetSGR IO.stderr [Reset]
  -- We need to flush, or otherwise what's on stdout may have the wrong colour
  -- since it's likely sharing a console with stderr
  IO.hFlush IO.stderr

deprecationWarning :: String -> IO ()
deprecationWarning cmd =
  printWarning $
    "This CLI command is deprecated.  Please use " <> cmd <> " command instead."

-- | Checks if a path exists and throws and error if it does.
ensureNewFile :: (FilePath -> a -> IO ()) -> FilePath -> a -> ExceptT HelpersError IO ()
ensureNewFile writer outFile blob = do
  exists <- liftIO $ IO.doesPathExist outFile
  when exists $
    left $
      OutputMustNotAlreadyExist outFile
  liftIO $ writer outFile blob

ensureNewFileLBS :: FilePath -> ByteString -> ExceptT HelpersError IO ()
ensureNewFileLBS = ensureNewFile BS.writeFile

pPrintCBOR :: LB.ByteString -> ExceptT HelpersError IO ()
pPrintCBOR bs = do
  case deserialiseFromBytes decodeTerm bs of
    Left err -> left $ CBORPrettyPrintError err
    Right (remaining, decodedVal) -> do
      liftIO . Text.putStrLn . Text.pack . prettyHexEnc $ encodeTerm decodedVal
      unless (LB.null remaining) $
        pPrintCBOR remaining

cborToTextByteString :: LB.ByteString -> ExceptT HelpersError IO LB.ByteString
cborToTextByteString bs = do
  text <- cborToText bs
  pure $ LB.fromStrict $ Text.encodeUtf8 text

cborToTextLazyByteString :: LB.ByteString -> ExceptT HelpersError IO LBS.ByteString
cborToTextLazyByteString =
  fmap (LT.encodeUtf8 . LT.fromStrict) . cborToText

cborToText :: LB.ByteString -> ExceptT HelpersError IO Text
cborToText bs = do
  as <- cborToTextList bs
  let cs = filter (not . Text.null) as
  pure $ mconcat $ fmap (<> "\n") cs

cborToTextList :: LB.ByteString -> ExceptT HelpersError IO [Text]
cborToTextList bs = do
  case deserialiseFromBytes decodeTerm bs of
    Left err -> left $ CBORPrettyPrintError err
    Right (remaining, decodedVal) -> do
      let text = Text.pack . prettyHexEnc $ encodeTerm decodedVal

      if LB.null remaining
        then pure [text]
        else do
          extraText <- cborToTextList remaining
          pure $ text : extraText

readCBOR :: FilePath -> ExceptT HelpersError IO LB.ByteString
readCBOR fp =
  handleIOExceptT
    (ReadCBORFileFailure fp . Text.pack . displayException)
    (LB.readFile fp)

validateCBOR :: CBORObject -> LB.ByteString -> Either HelpersError Text
validateCBOR cborObject bs =
  case cborObject of
    CBORBlockByron epochSlots -> do
      void $
        decodeCBOR bs (L.toPlainDecoder Nothing L.byronProtVer (Byron.decCBORABlockOrBoundary epochSlots))
      Right "Valid Byron block."
    CBORDelegationCertificateByron -> do
      void $ decodeCBOR bs (L.fromCBOR :: L.Decoder s Byron.Certificate)
      Right "Valid Byron delegation certificate."
    CBORTxByron -> do
      void $ decodeCBOR bs (L.fromCBOR :: L.Decoder s Byron.Tx)
      Right "Valid Byron Tx."
    CBORUpdateProposalByron -> do
      void $ decodeCBOR bs (L.fromCBOR :: L.Decoder s Byron.Proposal)
      Right "Valid Byron update proposal."
    CBORVoteByron -> do
      void $ decodeCBOR bs (L.fromCBOR :: L.Decoder s Byron.Vote)
      Right "Valid Byron vote."

printEraDeprecationWarning :: Typeable era => MonadIO m => ToCardanoEra eon => eon era -> m ()
printEraDeprecationWarning era = do
  let selectedEraNum = fromEnum $ AnyCardanoEra (toCardanoEra era)
      currentEraNum = fromEnum $ AnyCardanoEra ConwayEra
  when (selectedEraNum < currentEraNum) $
    printWarning "Selected era is deprecated and will be removed in the future."
