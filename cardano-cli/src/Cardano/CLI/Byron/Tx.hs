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

import           Cardano.Api
import           Cardano.Api.Byron
import qualified Cardano.Api.Ledger as L

import qualified Cardano.Binary as Binary
import           Cardano.CLI.Byron.Key (byronWitnessToVerKey)
import           Cardano.CLI.Types.Common (TxFile)
import qualified Cardano.Crypto.Signing as Crypto
import           Ouroboros.Consensus.Byron.Ledger (ByronBlock, GenTx (..))
import qualified Ouroboros.Consensus.Byron.Ledger as Byron
import           Ouroboros.Consensus.Cardano.Block (EraMismatch (..))
import qualified Ouroboros.Network.Protocol.LocalTxSubmission.Client as Net.Tx

import           Data.Bifunctor (Bifunctor (..))
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.List as List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe, mapMaybe)
import           Data.String (IsString)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Formatting (sformat, (%))
import           GHC.Exts (IsList (..))

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
    (L.addressF % "\n" % L.addressDetailedF)
    addr
    addr

-- TODO: Move to cardano-api
readByronTx :: TxFile In -> ExceptT ByronTxError IO (ATxAux ByteString)
readByronTx (File fp) = do
  txBS <- liftIO $ LB.readFile fp
  case fromCborTxAux txBS of
    Left e -> left $ TxDeserialisationFailed fp e
    Right tx -> pure tx

-- | The 'GenTx' is all the kinds of transactions that can be submitted
-- and \"normal\" Byron transactions are just one of the kinds.
normalByronTxToGenTx :: ATxAux ByteString -> GenTx ByronBlock
normalByronTxToGenTx tx' = Byron.ByronTx (Byron.byronIdTx tx') tx'

-- | Given a genesis, and a pair of a genesis public key and address,
--   reconstruct a TxIn corresponding to the genesis UTxO entry.
genesisUTxOTxIn :: L.Config -> Crypto.VerificationKey -> L.Address -> ByronTxIn
genesisUTxOTxIn gc vk genAddr =
  handleMissingAddr $ fst <$> Map.lookup genAddr initialUtxo
 where
  initialUtxo :: Map L.Address (ByronTxIn, ByronTxOut)
  initialUtxo =
    fromList
      . mapMaybe (\(inp, out) -> mkEntry inp genAddr <$> keyMatchesUTxO vk out)
      . fromCompactTxInTxOutList
      . toList
      . L.unUTxO
      . L.genesisUtxo
      $ gc
   where
    mkEntry
      :: ByronTxIn
      -> L.Address
      -> ByronTxOut
      -> (L.Address, (ByronTxIn, ByronTxOut))
    mkEntry inp addr out = (addr, (inp, out))

  fromCompactTxInTxOutList
    :: [(L.CompactTxIn, L.CompactTxOut)]
    -> [(ByronTxIn, ByronTxOut)]
  fromCompactTxInTxOutList =
    map (bimap L.fromCompactTxIn L.fromCompactTxOut)

  keyMatchesUTxO :: Crypto.VerificationKey -> ByronTxOut -> Maybe ByronTxOut
  keyMatchesUTxO key out =
    if L.checkVerKeyAddress key (L.txOutAddress out)
      then Just out
      else Nothing

  handleMissingAddr :: Maybe ByronTxIn -> ByronTxIn
  handleMissingAddr =
    fromMaybe . error $
      "\nGenesis UTxO has no address\n"
        <> Text.unpack (prettyAddress (ByronAddress genAddr))
        <> "\n\nIt has the following, though:\n\n"
        <> List.concatMap (Text.unpack . prettyAddress . ByronAddress) (Map.keys initialUtxo)

-- | Generate a transaction spending genesis UTxO at a given address,
--   to given outputs, signed by the given key.
txSpendGenesisUTxOByronPBFT
  :: L.Config
  -> NetworkId
  -> SomeByronSigningKey
  -> Address ByronAddr
  -> [TxOut CtxTx ByronEra]
  -> ATxAux ByteString
txSpendGenesisUTxOByronPBFT gc nId sk (ByronAddress bAddr) outs =
  let txins = [(fromByronTxIn txIn, BuildTxWith (KeyWitness KeyWitnessForSpending))]
   in case makeByronTransactionBody txins outs of
        Left err -> error $ "Error occurred while creating a Byron genesis based UTxO transaction: " <> show err
        Right txBody ->
          let bWit = fromByronWitness sk nId txBody
           in makeSignedByronTransaction [bWit] txBody
 where
  ByronVerificationKey vKey = byronWitnessToVerKey sk

  txIn :: ByronTxIn
  txIn = genesisUTxOTxIn gc vKey bAddr

-- | Generate a transaction from given Tx inputs to outputs,
--   signed by the given key.
txSpendUTxOByronPBFT
  :: NetworkId
  -> SomeByronSigningKey
  -> [TxIn]
  -> [TxOut CtxTx ByronEra]
  -> ATxAux ByteString
txSpendUTxOByronPBFT nId sk txIns outs = do
  let apiTxIns = [(txIn, BuildTxWith (KeyWitness KeyWitnessForSpending)) | txIn <- txIns]

  case makeByronTransactionBody apiTxIns outs of
    Left err -> error $ "Error occurred while creating a Byron genesis based UTxO transaction: " <> show err
    Right txBody ->
      let bWit = fromByronWitness sk nId txBody
       in makeSignedByronTransaction [bWit] txBody

fromByronWitness
  :: SomeByronSigningKey -> NetworkId -> L.Annotated L.Tx ByteString -> KeyWitness ByronEra
fromByronWitness bw nId txBody =
  case bw of
    AByronSigningKeyLegacy sk -> makeByronKeyWitness nId txBody sk
    AByronSigningKey sk' -> makeByronKeyWitness nId txBody sk'

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
fromCborTxAux :: LB.ByteString -> Either Binary.DecoderError (ATxAux B.ByteString)
fromCborTxAux lbs =
  annotationBytes lbs
    <$> Binary.decodeFullDecoder
      "Cardano.Chain.UTxO.TxAux.fromCborTxAux"
      Binary.fromCBOR
      lbs
 where
  annotationBytes :: Functor f => LB.ByteString -> f L.ByteSpan -> f B.ByteString
  annotationBytes bytes = fmap (LB.toStrict . L.slice bytes)

toCborTxAux :: ATxAux ByteString -> LB.ByteString
toCborTxAux = LB.fromStrict . L.aTaAnnotation -- The ByteString anotation is the CBOR encoded version.
