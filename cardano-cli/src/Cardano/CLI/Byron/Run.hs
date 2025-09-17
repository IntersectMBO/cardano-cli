{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

{- HLINT ignore "Use newtype instead of data" -}

module Cardano.CLI.Byron.Run
  ( ByronClientCmdError
  , renderByronClientCmdError
  , runByronClientCommand
  )
where

import Cardano.Api hiding (GenesisParameters, UpdateProposal, txId)
import Cardano.Api.Byron
  ( SigningKey (ByronSigningKey, ByronSigningKeyLegacy)
  , SomeByronSigningKey (..)
  , VerificationKey (ByronVerificationKey)
  )
import Cardano.Api.Byron qualified as Byron

import Cardano.CLI.Byron.Command
import Cardano.CLI.Byron.Delegation
import Cardano.CLI.Byron.Genesis
import Cardano.CLI.Byron.Key
import Cardano.CLI.Byron.Tx
import Cardano.CLI.Byron.UpdateProposal
import Cardano.CLI.Byron.Vote
import Cardano.CLI.Compatible.Exception
import Cardano.CLI.Helper
import Cardano.CLI.Type.Common
import Cardano.Crypto.Hashing qualified as Crypto
import Cardano.Crypto.Signing qualified as Crypto

import Data.ByteString.Char8 qualified as BS
import Data.Text (Text)
import Data.Text.IO qualified as Text
import Data.Text.Lazy.Builder qualified as Builder
import Data.Text.Lazy.IO qualified as TL
import Formatting qualified as F

-- | Data type that encompasses all the possible errors of the
-- Byron client.
data ByronClientCmdError
  = ByronCmdKeyFailure !ByronKeyFailure
  deriving Show

instance Error ByronClientCmdError where
  prettyError = renderByronClientCmdError

renderByronClientCmdError :: ByronClientCmdError -> Doc ann
renderByronClientCmdError = \case
  ByronCmdKeyFailure e -> renderByronKeyFailure e

runByronClientCommand :: ByronCommand -> CIO e ()
runByronClientCommand c =
  case c of
    NodeCmds bc -> runNodeCmds bc
    Genesis outDir params -> runGenesisCommand outDir params
    ValidateCBOR cborObject fp -> runValidateCBOR cborObject fp
    PrettyPrintCBOR fp -> runPrettyPrintCBOR fp
    PrettySigningKeyPublic bKeyFormat skF -> runPrettySigningKeyPublic bKeyFormat skF
    MigrateDelegateKeyFrom oldKey nskf ->
      runMigrateDelegateKeyFrom oldKey nskf
    PrintGenesisHash genFp -> runPrintGenesisHash genFp
    PrintSigningKeyAddress bKeyFormat networkid skF -> runPrintSigningKeyAddress bKeyFormat networkid skF
    Keygen nskf -> runKeygen nskf
    ToVerification bKeyFormat skFp nvkFp -> runToVerification bKeyFormat skFp nvkFp
    SubmitTx socketPath network fp -> runSubmitTx socketPath network fp
    GetTxId fp -> runGetTxId fp
    SpendGenesisUTxO genFp nw era nftx ctKey genRichAddr outs ->
      runSpendGenesisUTxO genFp nw era nftx ctKey genRichAddr outs
    SpendUTxO nw era nftx ctKey ins outs ->
      runSpendUTxO nw era nftx ctKey ins outs

runNodeCmds :: NodeCmds -> CIO e ()
runNodeCmds (CreateVote nw sKey upPropFp voteBool outputFp) =
  runVoteCreation nw sKey upPropFp voteBool outputFp
runNodeCmds (SubmitUpdateProposal nodeSocketPath network proposalFp) = do
  submitByronUpdateProposal nodeSocketPath network proposalFp
runNodeCmds (SubmitVote nodeSocketPath network voteFp) = do
  submitByronVote nodeSocketPath network voteFp
runNodeCmds (UpdateProposal nw sKey pVer sVer sysTag insHash outputFp params) =
  runProposalCreation nw sKey pVer sVer sysTag insHash outputFp params

runGenesisCommand :: NewDirectory -> GenesisParameters -> CIO e ()
runGenesisCommand outDir params = do
  (genData, genSecrets) <- mkGenesis params
  fromExceptTCli $ dumpGenesis outDir genData genSecrets

runValidateCBOR :: CBORObject -> FilePath -> CIO e ()
runValidateCBOR cborObject fp = do
  bs <- fromExceptTCli $ readCBOR fp
  res <- fromEitherCli $ validateCBOR cborObject bs
  liftIO $ Text.putStrLn res

runPrettyPrintCBOR :: FilePath -> CIO e ()
runPrettyPrintCBOR fp = do
  bs <- fromExceptTCli $ readCBOR fp
  fromExceptTCli $ pPrintCBOR bs

runPrettySigningKeyPublic
  :: ByronKeyFormat -> SigningKeyFile In -> CIO e ()
runPrettySigningKeyPublic bKeyFormat skF = do
  sK <- fromExceptTCli $ readByronSigningKey bKeyFormat skF
  liftIO . Text.putStrLn . prettyPublicKey $ byronWitnessToVerKey sK

runMigrateDelegateKeyFrom
  :: SigningKeyFile In
  -- ^ Legacy Byron signing key
  -> NewSigningKeyFile
  -> CIO e ()
runMigrateDelegateKeyFrom oldKey@(File fp) (NewSigningKeyFile newKey) = do
  sk <- fromExceptTCli $ readByronSigningKey LegacyByronKeyFormat oldKey
  migratedWitness <- case sk of
    AByronSigningKeyLegacy (ByronSigningKeyLegacy sKey) ->
      return . AByronSigningKey $ ByronSigningKey sKey
    AByronSigningKey _ ->
      throwCliError . ByronCmdKeyFailure $ CannotMigrateFromNonLegacySigningKey fp
  fromExceptTCli $ ensureNewFileLBS newKey $ serialiseByronWitness migratedWitness

runPrintGenesisHash :: GenesisFile -> CIO e ()
runPrintGenesisHash genFp = do
  genesis <-
    fromExceptTCli $ readGenesis genFp dummyNetwork
  liftIO . Text.putStrLn $ formatter genesis
 where
  -- For this purpose of getting the hash, it does not matter what network
  -- value we use here.
  dummyNetwork :: NetworkId
  dummyNetwork = Mainnet

  formatter :: Byron.Config -> Text
  formatter =
    F.sformat Crypto.hashHexF
      . Byron.unGenesisHash
      . Byron.configGenesisHash

runPrintSigningKeyAddress
  :: ByronKeyFormat
  -> NetworkId
  -> SigningKeyFile In
  -> CIO e ()
runPrintSigningKeyAddress bKeyFormat networkid skF = do
  sK <- fromExceptTCli $ readByronSigningKey bKeyFormat skF
  let sKeyAddr = prettyAddress . makeByronAddress networkid $ byronWitnessToVerKey sK
  liftIO $ Text.putStrLn sKeyAddr

runKeygen :: NewSigningKeyFile -> CIO e ()
runKeygen (NewSigningKeyFile skF) = do
  sK <- generateSigningKey AsByronKey
  fromExceptTCli $ ensureNewFileLBS skF $ serialiseToRawBytes sK

runToVerification
  :: ByronKeyFormat -> SigningKeyFile In -> NewVerificationKeyFile -> CIO e ()
runToVerification bKeyFormat skFp (NewVerificationKeyFile vkFp) = do
  sk <- fromExceptTCli $ readByronSigningKey bKeyFormat skFp
  let ByronVerificationKey vK = byronWitnessToVerKey sk
  let vKey = Builder.toLazyText $ Crypto.formatFullVerificationKey vK
  fromExceptTCli $ ensureNewFile TL.writeFile vkFp vKey

runSubmitTx :: SocketPath -> NetworkId -> TxFile In -> CIO e ()
runSubmitTx nodeSocketPath network fp = do
  tx <- fromExceptTCli $ readByronTx fp

  fromExceptTCli $ nodeSubmitTx nodeSocketPath network (normalByronTxToGenTx tx)

runGetTxId :: TxFile In -> CIO e ()
runGetTxId fp = do
  tx <- fromExceptTCli $ readByronTx fp
  let txId = getTxIdByron tx
  liftIO . BS.putStrLn $ serialiseToRawBytesHex txId

runSpendGenesisUTxO
  :: GenesisFile
  -> NetworkId
  -> ByronKeyFormat
  -> NewTxFile
  -> SigningKeyFile In
  -> Address ByronAddr
  -> [TxOut CtxTx ByronEra]
  -> CIO e ()
runSpendGenesisUTxO genesisFile nw bKeyFormat (NewTxFile ctTx) ctKey genRichAddr outs = do
  genesis <- fromExceptTCli $ readGenesis genesisFile nw
  sk <- fromExceptTCli $ readByronSigningKey bKeyFormat ctKey

  let tx = txSpendGenesisUTxOByronPBFT genesis nw sk genRichAddr outs
  fromExceptTCli $
    ensureNewFileLBS ctTx $
      teRawCBOR $
        serialiseByronTx tx

-- Construct a Byron era tx
runSpendUTxO
  :: NetworkId
  -> ByronKeyFormat
  -> NewTxFile
  -> SigningKeyFile In
  -> [TxIn]
  -> [TxOut CtxTx ByronEra]
  -> CIO e ()
runSpendUTxO nw bKeyFormat (NewTxFile ctTx) ctKey ins outs = do
  sk <- fromExceptTCli $ readByronSigningKey bKeyFormat ctKey

  let gTx = txSpendUTxOByronPBFT nw sk ins outs
  fromExceptTCli $
    ensureNewFileLBS ctTx $
      teRawCBOR $
        serialiseByronTx gTx
