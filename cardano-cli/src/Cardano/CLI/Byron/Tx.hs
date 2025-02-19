{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Cardano.CLI.Byron.Tx
  ( ByronTxError (..)
  , Tx
  , TxFile
  , NewTxFile (..)
  , prettyAddress
  , readByronTx
  , normalByronTxToGenTx
  , txSpendGenesisUTxOByronPBFT
  , txSpendUTxOByronPBFT
  , nodeSubmitTx
  , renderByronTxError
  -- TODO: remove when they are exported from the ledger
  , fromCborTxAux
  , toCborTxAux
  , ScriptValidity (..)
  )
where

import Cardano.Api
import Cardano.Api.Byron qualified as Byron
import Cardano.Api.Consensus (ByronBlock, EraMismatch (..), GenTx (..))
import Cardano.Api.Consensus qualified as Byron
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Network qualified as Net.Tx

import Cardano.Binary qualified as Binary
import Cardano.CLI.Byron.Key (byronWitnessToVerKey)
import Cardano.CLI.Types.Common (TxFile)
import Cardano.Crypto.Signing qualified as Crypto

import Data.Bifunctor (Bifunctor (..))
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as LB
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Formatting (sformat, (%))
import GHC.Exts (IsList (..))

data ByronTxError
  = TxDeserialisationFailed !FilePath !Binary.DecoderError
  | ByronTxSubmitError !Text
  | ByronTxSubmitErrorEraMismatch !EraMismatch
  deriving Show

renderByronTxError :: ByronTxError -> Doc ann
renderByronTxError = \case
  ByronTxSubmitError res -> "Error while submitting tx: " <> pretty res
  ByronTxSubmitErrorEraMismatch EraMismatch{ledgerEraName, otherEraName} ->
    "The era of the node and the tx do not match. "
      <> "The node is running in the "
      <> pretty ledgerEraName
      <> " era, but the transaction is for the "
      <> pretty otherEraName
      <> " era."
  TxDeserialisationFailed txFp decErr ->
    "Transaction deserialisation failed at " <> pshow txFp <> " Error: " <> pshow decErr

newtype NewTxFile
  = NewTxFile FilePath
  deriving (Eq, Ord, Show, IsString)

-- | Pretty-print an address in its Base58 form, and also
--   its full structure.
prettyAddress :: Address ByronAddr -> Text
prettyAddress (ByronAddress addr) =
  sformat
    (Byron.addressF % "\n" % Byron.addressDetailedF)
    addr
    addr

-- TODO: Move to cardano-api
readByronTx :: TxFile In -> ExceptT ByronTxError IO (Byron.ATxAux ByteString)
readByronTx (File fp) = do
  txBS <- liftIO $ LB.readFile fp
  case fromCborTxAux txBS of
    Left e -> left $ TxDeserialisationFailed fp e
    Right tx -> pure tx

-- | The 'GenTx' is all the kinds of transactions that can be submitted
-- and \"normal\" Byron transactions are just one of the kinds.
normalByronTxToGenTx :: Byron.ATxAux ByteString -> GenTx ByronBlock
normalByronTxToGenTx tx' = Byron.ByronTx (Byron.byronIdTx tx') tx'

-- | Given a genesis, and a pair of a genesis public key and address,
--   reconstruct a TxIn corresponding to the genesis UTxO entry.
genesisUTxOTxIn :: Byron.Config -> Crypto.VerificationKey -> Byron.Address -> Byron.TxIn
genesisUTxOTxIn gc vk genAddr =
  handleMissingAddr $ fst <$> Map.lookup genAddr initialUtxo
 where
  initialUtxo :: Map Byron.Address (Byron.TxIn, Byron.TxOut)
  initialUtxo =
    fromList
      . mapMaybe (\(inp, out) -> mkEntry inp genAddr <$> keyMatchesUTxO vk out)
      . fromCompactTxInTxOutList
      . toList
      . Byron.unUTxO
      . Byron.genesisUtxo
      $ gc
   where
    mkEntry
      :: Byron.TxIn
      -> Byron.Address
      -> Byron.TxOut
      -> (Byron.Address, (Byron.TxIn, Byron.TxOut))
    mkEntry inp addr out = (addr, (inp, out))

  fromCompactTxInTxOutList
    :: [(Byron.CompactTxIn, Byron.CompactTxOut)]
    -> [(Byron.TxIn, Byron.TxOut)]
  fromCompactTxInTxOutList =
    map (bimap Byron.fromCompactTxIn Byron.fromCompactTxOut)

  keyMatchesUTxO :: Crypto.VerificationKey -> Byron.TxOut -> Maybe Byron.TxOut
  keyMatchesUTxO key out =
    if Byron.checkVerKeyAddress key (Byron.txOutAddress out)
      then Just out
      else Nothing

  handleMissingAddr :: Maybe Byron.TxIn -> Byron.TxIn
  handleMissingAddr =
    fromMaybe . error $
      "\nGenesis UTxO has no address\n"
        <> Text.unpack (prettyAddress (ByronAddress genAddr))
        <> "\n\nIt has the following, though:\n\n"
        <> List.concatMap (Text.unpack . prettyAddress . ByronAddress) (Map.keys initialUtxo)

-- | Generate a transaction spending genesis UTxO at a given address,
--   to given outputs, signed by the given key.
txSpendGenesisUTxOByronPBFT
  :: Byron.Config
  -> NetworkId
  -> Byron.SomeByronSigningKey
  -> Address ByronAddr
  -> [TxOut CtxTx ByronEra]
  -> Byron.ATxAux ByteString
txSpendGenesisUTxOByronPBFT gc nId sk (ByronAddress bAddr) outs =
  let txins = [(Byron.fromByronTxIn txIn, BuildTxWith (KeyWitness KeyWitnessForSpending))]
   in case makeByronTransactionBody txins outs of
        Left err -> error $ "Error occurred while creating a Byron genesis based UTxO transaction: " <> show err
        Right txBody ->
          let bWit = fromByronWitness sk nId txBody
           in makeSignedByronTransaction [bWit] txBody
 where
  ByronVerificationKey vKey = byronWitnessToVerKey sk

  txIn :: Byron.TxIn
  txIn = genesisUTxOTxIn gc vKey bAddr

-- | Generate a transaction from given Tx inputs to outputs,
--   signed by the given key.
txSpendUTxOByronPBFT
  :: NetworkId
  -> Byron.SomeByronSigningKey
  -> [TxIn]
  -> [TxOut CtxTx ByronEra]
  -> Byron.ATxAux ByteString
txSpendUTxOByronPBFT nId sk txIns outs = do
  let apiTxIns = [(txIn, BuildTxWith (KeyWitness KeyWitnessForSpending)) | txIn <- txIns]

  case makeByronTransactionBody apiTxIns outs of
    Left err -> error $ "Error occurred while creating a Byron genesis based UTxO transaction: " <> show err
    Right txBody ->
      let bWit = fromByronWitness sk nId txBody
       in makeSignedByronTransaction [bWit] txBody

fromByronWitness
  :: Byron.SomeByronSigningKey -> NetworkId -> L.Annotated Byron.Tx ByteString -> KeyWitness ByronEra
fromByronWitness bw nId txBody =
  case bw of
    Byron.AByronSigningKeyLegacy sk -> makeByronKeyWitness nId txBody sk
    Byron.AByronSigningKey sk' -> makeByronKeyWitness nId txBody sk'

-- | Submit a transaction to a node specified by topology info.
nodeSubmitTx
  :: SocketPath
  -> NetworkId
  -> GenTx ByronBlock
  -> ExceptT ByronTxError IO ()
nodeSubmitTx nodeSocketPath network gentx = do
  let connctInfo =
        LocalNodeConnectInfo
          { localNodeSocketPath = nodeSocketPath
          , localNodeNetworkId = network
          , localConsensusModeParams = CardanoModeParams (EpochSlots 21600)
          }
  res <- liftIO $ submitTxToNodeLocal connctInfo (TxInByronSpecial gentx)
  case res of
    Net.Tx.SubmitSuccess -> liftIO $ Text.putStrLn "Transaction successfully submitted."
    Net.Tx.SubmitFail reason ->
      case reason of
        TxValidationErrorInCardanoMode err -> left . ByronTxSubmitError . Text.pack $ show err
        TxValidationEraMismatch mismatchErr -> left $ ByronTxSubmitErrorEraMismatch mismatchErr

  return ()

-- TODO: remove these local definitions when the updated ledger lib is available
fromCborTxAux :: LB.ByteString -> Either Binary.DecoderError (Byron.ATxAux B.ByteString)
fromCborTxAux lbs =
  annotationBytes lbs
    <$> Binary.decodeFullDecoder
      "Cardano.Chain.UTxO.TxAux.fromCborTxAux"
      Binary.fromCBOR
      lbs
 where
  annotationBytes :: Functor f => LB.ByteString -> f L.ByteSpan -> f B.ByteString
  annotationBytes bytes = fmap (LB.toStrict . L.slice bytes)

toCborTxAux :: Byron.ATxAux ByteString -> LB.ByteString
toCborTxAux = LB.fromStrict . Byron.aTaAnnotation -- The ByteString anotation is the CBOR encoded version.
