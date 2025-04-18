{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.Compatible.Transaction.Option
  ( pAllCompatibleTransactionCommands
  )
where

import Cardano.Api

import Cardano.CLI.Compatible.Transaction.Command
import Cardano.CLI.Environment
import Cardano.CLI.EraBased.Common.Option hiding (pRefScriptFp, pTxOutDatum)
import Cardano.CLI.Parser
import Cardano.CLI.Type.Common

import Data.Foldable hiding (toList)
import Options.Applicative
import Options.Applicative qualified as Opt

pAllCompatibleTransactionCommands
  :: EnvCli -> ShelleyBasedEra era -> Parser (CompatibleTransactionCmds era)
pAllCompatibleTransactionCommands envCli sbe =
  let allCommannds =
        asum
          [ pCompatibleSignedTransactionCommand envCli sbe
          ]
   in Opt.hsubparser $
        commandWithMetavar "transaction" $
          Opt.info allCommannds $
            Opt.progDesc "Transaction commands."

pCompatibleSignedTransactionCommand
  :: EnvCli -> ShelleyBasedEra era -> Parser (CompatibleTransactionCmds era)
pCompatibleSignedTransactionCommand envCli sbe =
  Opt.hsubparser $
    commandWithMetavar "signed-transaction" $
      Opt.info (pCompatibleSignedTransaction envCli sbe) $
        Opt.progDesc "Create a simple signed transaction."

pCompatibleSignedTransaction
  :: EnvCli -> ShelleyBasedEra era -> Parser (CompatibleTransactionCmds era)
pCompatibleSignedTransaction env sbe =
  CreateCompatibleSignedTransaction sbe
    <$> many pTxInOnly
    <*> many (pTxOutEraAware sbe)
    <*> pFeatured (toCardanoEra sbe) (optional pUpdateProposalFile)
    <*> pFeatured (toCardanoEra sbe) (many (pProposalFile ManualBalance))
    <*> pVoteFiles sbe ManualBalance
    <*> many pWitnessSigningData
    <*> optional (pNetworkId env)
    <*> pTxFee
    <*> many (pCertificateFile ManualBalance)
    <*> pOutputFile

pTxInOnly :: Parser TxIn
pTxInOnly =
  Opt.option
    (readerFromParsecParser parseTxIn)
    ( Opt.long "tx-in"
        <> Opt.metavar "TX_IN"
        <> Opt.help "TxId#TxIx"
    )

-- This parser renders the appropriate parsers depending on what
-- functionality is available per era.
pTxOutEraAware :: ShelleyBasedEra era -> Parser TxOutAnyEra
pTxOutEraAware sbe =
  Opt.option
    (readerFromParsecParser parseTxOutAnyEra)
    ( Opt.long "tx-out"
        <> Opt.metavar "ADDRESS VALUE"
        -- TODO alonzo: Update the help text to describe the new syntax as well.
        <> Opt.help
          "The transaction output as ADDRESS VALUE where ADDRESS is \
          \the Bech32-encoded address followed by the value in \
          \the multi-asset syntax (including simply Lovelace)."
    )
    <*> pTxOutDatum sbe
    <*> pRefScriptFp sbe

pTxOutDatum :: ShelleyBasedEra era -> Parser TxOutDatumAnyEra
pTxOutDatum =
  caseShelleyToMaryOrAlonzoEraOnwards
    (const $ pure TxOutDatumByNone)
    ( \case
        AlonzoEraOnwardsAlonzo ->
          pAlonzoDatumFunctionality <|> pure TxOutDatumByNone
        AlonzoEraOnwardsBabbage ->
          pBabbageDatumFunctionality <|> pure TxOutDatumByNone
        AlonzoEraOnwardsConway -> pConwayDatumFunctionality <|> pure TxOutDatumByNone
    )
 where
  pAlonzoDatumFunctionality =
    asum
      [ pTxOutDatumByHashOnly
      , pTxOutDatumByHashOf
      , pTxOutDatumByValue
      ]
  pBabbageDatumFunctionality =
    asum
      [ pAlonzoDatumFunctionality
      , pTxOutInlineDatumByValue
      ]

  pConwayDatumFunctionality = pBabbageDatumFunctionality

  pTxOutDatumByHashOnly =
    fmap TxOutDatumByHashOnly $
      Opt.option (readerFromParsecParser $ parseHash (AsHash AsScriptData)) $
        mconcat
          [ Opt.long "tx-out-datum-hash"
          , Opt.metavar "HASH"
          , Opt.help $
              mconcat
                [ "The script datum hash for this tx output, as "
                , "the raw datum hash (in hex)."
                ]
          ]

  pTxOutDatumByHashOf =
    TxOutDatumByHashOf
      <$> pScriptDataOrFile
        "tx-out-datum-hash"
        "The script datum hash for this tx output, by hashing the script datum given here."
        "The script datum hash for this tx output, by hashing the script datum in the file."

  pTxOutDatumByValue =
    TxOutDatumByValue
      <$> pScriptDataOrFile
        "tx-out-datum-embed"
        "The script datum to embed in the tx for this output, given here."
        "The script datum to embed in the tx for this output, in the given file."

  pTxOutInlineDatumByValue =
    TxOutInlineDatumByValue
      <$> pScriptDataOrFile
        "tx-out-inline-datum"
        "The script datum to embed in the tx output as an inline datum, given here."
        "The script datum to embed in the tx output as an inline datum, in the given file."

pRefScriptFp :: ShelleyBasedEra era -> Parser ReferenceScriptAnyEra
pRefScriptFp =
  caseShelleyToBabbageOrConwayEraOnwards
    (const $ pure ReferenceScriptAnyEraNone)
    ( const $
        ReferenceScriptAnyEra
          <$> parseFilePath "tx-out-reference-script-file" "Reference script input file."
            <|> pure ReferenceScriptAnyEraNone
    )
