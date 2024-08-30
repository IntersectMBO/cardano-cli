{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.Helpers
  ( HelpersError (..)
  , printWarning
  , deprecationWarning
  , ensureNewFile
  , ensureNewFileLBS
  , pPrintCBOR
  , readCBOR
  , renderHelpersError
  , validateCBOR
  )
where

import qualified Cardano.Api.Ledger as L

import           Cardano.Chain.Block (decCBORABlockOrBoundary)
import qualified Cardano.Chain.Delegation as Delegation
import qualified Cardano.Chain.Update as Update
import qualified Cardano.Chain.UTxO as UTxO
import           Cardano.CLI.Pretty (Doc, pretty, pshow)
import           Cardano.CLI.Types.Common

import           Codec.CBOR.Pretty (prettyHexEnc)
import           Codec.CBOR.Read (DeserialiseFailure, deserialiseFromBytes)
import           Codec.CBOR.Term (decodeTerm, encodeTerm)
import           Control.Exception (Exception (..), IOException)
import           Control.Monad (unless, when)
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (handleIOExceptT, left)
import           Data.Bifunctor (Bifunctor (..))
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LB
import           Data.Functor (void)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified System.Console.ANSI as ANSI
import           System.Console.ANSI
import qualified System.Directory as IO
import qualified System.IO as IO

data HelpersError
  = CBORPrettyPrintError !DeserialiseFailure
  | CBORDecodingError !DeserialiseFailure
  | IOError' !FilePath !IOException
  | OutputMustNotAlreadyExist FilePath
  | ReadCBORFileFailure !FilePath !Text
  deriving Show

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

readCBOR :: FilePath -> ExceptT HelpersError IO LB.ByteString
readCBOR fp =
  handleIOExceptT
    (ReadCBORFileFailure fp . Text.pack . displayException)
    (LB.readFile fp)

validateCBOR :: CBORObject -> LB.ByteString -> Either HelpersError Text
validateCBOR cborObject bs =
  case cborObject of
    CBORBlockByron epochSlots -> do
      void $ decodeCBOR bs (L.toPlainDecoder L.byronProtVer (decCBORABlockOrBoundary epochSlots))
      Right "Valid Byron block."
    CBORDelegationCertificateByron -> do
      void $ decodeCBOR bs (L.fromCBOR :: L.Decoder s Delegation.Certificate)
      Right "Valid Byron delegation certificate."
    CBORTxByron -> do
      void $ decodeCBOR bs (L.fromCBOR :: L.Decoder s UTxO.Tx)
      Right "Valid Byron Tx."
    CBORUpdateProposalByron -> do
      void $ decodeCBOR bs (L.fromCBOR :: L.Decoder s Update.Proposal)
      Right "Valid Byron update proposal."
    CBORVoteByron -> do
      void $ decodeCBOR bs (L.fromCBOR :: L.Decoder s Update.Vote)
      Right "Valid Byron vote."
