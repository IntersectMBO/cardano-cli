{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Cardano.CLI.Run.Mnemonic (generateMnemonic, extendedSigningKeyFromMnemonicImpl) where

import           Cardano.Api
                   (AsType (AsCommitteeColdExtendedKey, AsCommitteeHotExtendedKey, AsDRepExtendedKey, AsPaymentExtendedKey, AsStakeExtendedKey),
                   ExceptT, File, FileDirection (Out), HasTextEnvelope, Key (SigningKey),
                   MnemonicSize (..), MnemonicToSigningKeyError, MonadIO (..), SerialiseAsBech32,
                   except, findMnemonicWordsWithPrefix, firstExceptT, left, newExceptT,
                   readTextFile, serialiseToBech32, signingKeyFromMnemonic,
                   signingKeyFromMnemonicWithPaymentKeyIndex, textEnvelopeToJSON,
                   writeLazyByteStringFile, writeTextFile)
import qualified Cardano.Api as Api

import qualified Cardano.CLI.Commands.Key as Cmd
import           Cardano.CLI.Types.Common (KeyOutputFormat (..), SigningKeyFile)
import           Cardano.CLI.Types.Errors.KeyCmdError
                   (KeyCmdError (KeyCmdMnemonicError, KeyCmdReadMnemonicFileError, KeyCmdWriteFileError, KeyCmdWrongNumOfMnemonics))
import           Cardano.Prelude (catch, stdout)

import           Control.Monad (when)
import           Data.Bifunctor (Bifunctor (..))
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Word (Word32)
import           System.IO.Error (isEOFError)
import Cardano.CLI.OS.Posix (hFlush)

-- | Generate a mnemonic and write it to a file or stdout.
generateMnemonic
  :: MonadIO m
  => MnemonicSize
  -- ^ The number of words in the mnemonic.
  -> Maybe (File () Out)
  -- ^ The file to write the mnemonic to. If 'Nothing', write to stdout.
  -> ExceptT KeyCmdError m ()
generateMnemonic mnemonicWords mnemonicOutputFormat = do
  mnemonic <- firstExceptT KeyCmdMnemonicError $ Api.generateMnemonic mnemonicWords
  let expectedNumOfMnemonicWords = mnemonicSizeToInt mnemonicWords
      obtainedNumOfMnemonicWords = length mnemonic
  when (obtainedNumOfMnemonicWords /= expectedNumOfMnemonicWords) $
    left $
      KeyCmdWrongNumOfMnemonics expectedNumOfMnemonicWords obtainedNumOfMnemonicWords
  case mnemonicOutputFormat of
    Just outFile ->
      firstExceptT KeyCmdWriteFileError . newExceptT $
        writeTextFile outFile (T.unwords mnemonic)
    Nothing -> liftIO $ putStrLn $ T.unpack (T.unwords mnemonic)
 where
  mnemonicSizeToInt :: MnemonicSize -> Int
  mnemonicSizeToInt MS12 = 12
  mnemonicSizeToInt MS15 = 15
  mnemonicSizeToInt MS18 = 18
  mnemonicSizeToInt MS21 = 21
  mnemonicSizeToInt MS24 = 24

-- | Derive an extended signing key from a mnemonic and write it to a file.
extendedSigningKeyFromMnemonicImpl
  :: KeyOutputFormat
  -- ^ The format in which to write the signing key.
  -> Cmd.ExtendedSigningType
  -- ^ The type of the extended signing key to derive with an optional payment key index.
  -> Word32
  -- ^ The account index.
  -> Cmd.MnemonicSource
  -- ^ The source of the mnemonic (either file or stdin).
  -> SigningKeyFile Out
  -- ^ The file to write the signing key to.
  -> ExceptT KeyCmdError IO ()
extendedSigningKeyFromMnemonicImpl keyOutputFormat derivedExtendedSigningKeyType derivationAccountNo mnemonicSource signingKeyFileOut =
  do
    let writeKeyToFile
          :: (HasTextEnvelope (SigningKey a), SerialiseAsBech32 (SigningKey a))
          => SigningKey a -> ExceptT KeyCmdError IO ()
        writeKeyToFile = writeSigningKeyFile keyOutputFormat signingKeyFileOut

        wrapException :: Either MnemonicToSigningKeyError a -> ExceptT KeyCmdError IO a
        wrapException = except . first KeyCmdMnemonicError

    mnemonicWords <- readMnemonic mnemonicSource

    case derivedExtendedSigningKeyType of
      Cmd.ExtendedSigningPaymentKey paymentKeyNo ->
        writeKeyToFile
          =<< wrapException
            ( signingKeyFromMnemonicWithPaymentKeyIndex
                AsPaymentExtendedKey
                mnemonicWords
                derivationAccountNo
                paymentKeyNo
            )
      Cmd.ExtendedSigningStakeKey paymentKeyNo ->
        writeKeyToFile
          =<< wrapException
            ( signingKeyFromMnemonicWithPaymentKeyIndex
                AsStakeExtendedKey
                mnemonicWords
                derivationAccountNo
                paymentKeyNo
            )
      Cmd.ExtendedSigningDRepKey ->
        writeKeyToFile
          =<< wrapException (signingKeyFromMnemonic AsDRepExtendedKey mnemonicWords derivationAccountNo)
      Cmd.ExtendedSigningCCColdKey ->
        writeKeyToFile
          =<< wrapException
            (signingKeyFromMnemonic AsCommitteeColdExtendedKey mnemonicWords derivationAccountNo)
      Cmd.ExtendedSigningCCHotKey ->
        writeKeyToFile
          =<< wrapException
            (signingKeyFromMnemonic AsCommitteeHotExtendedKey mnemonicWords derivationAccountNo)
 where
  writeSigningKeyFile
    :: (HasTextEnvelope (SigningKey a), SerialiseAsBech32 (SigningKey a))
    => KeyOutputFormat -> SigningKeyFile Out -> SigningKey a -> ExceptT KeyCmdError IO ()
  writeSigningKeyFile fmt sKeyPath skey =
    firstExceptT KeyCmdWriteFileError $
      case fmt of
        KeyOutputFormatTextEnvelope ->
          newExceptT $
            writeLazyByteStringFile sKeyPath $
              textEnvelopeToJSON Nothing skey
        KeyOutputFormatBech32 ->
          newExceptT $
            writeTextFile sKeyPath $
              serialiseToBech32 skey

  readMnemonic :: Cmd.MnemonicSource -> ExceptT KeyCmdError IO [Text]
  readMnemonic (Cmd.MnemonicFromFile filePath) = do
    fileText <- firstExceptT KeyCmdReadMnemonicFileError $ except =<< readTextFile filePath
    return $ map T.pack $ words $ T.unpack fileText
  readMnemonic Cmd.MnemonicFromInteractivePrompt =
    liftIO $ do
      putStrLn $
        unlines
          [ ""
          , "Please enter your mnemonic sentence."
          , ""
          , " - It should consist of either: 12, 15, 18, 21, or 24 words."
          , " - To terminate, press enter on an empty line."
          , " - To abort you can press CTRL+C."
          , ""
          ]
      inputMnemonic []
   where
    inputMnemonic :: [Text] -> IO [Text]
    inputMnemonic mnemonic = do
      minput <- getLineWithPrompt (show (length mnemonic + 1) <> ". ")
      case minput of
        Nothing -> return $ reverse mnemonic
        Just "" -> return $ reverse mnemonic
        Just input ->
          let newWords = map (T.toLower . T.pack) $ filter (not . null) $ words input
           in case span isValidMnemonicWord newWords of
                (allWords, []) -> inputMnemonic (reverse allWords ++ mnemonic)
                (validWords, invalidWord : _notValidatedWords) -> do
                  liftIO $ putStrLn $ "The word \"" <> T.unpack invalidWord <> "\" is not in the memonic dictionary"
                  inputMnemonic (reverse validWords ++ mnemonic)

    isValidMnemonicWord :: Text -> Bool
    isValidMnemonicWord word = word `elem` map fst (findMnemonicWordsWithPrefix word)

    getLineWithPrompt :: String -> IO (Maybe String)
    getLineWithPrompt prompt = do
      catch
        ( do
            putStr prompt
            hFlush stdout
            Just <$> getLine
        )
        handleEOF

    handleEOF :: IOError -> IO (Maybe String)
    handleEOF eofERRor | isEOFError eofERRor = return Nothing
    handleEOF otherError = ioError otherError
