{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.CLI.Compatible.Transaction.Option
  ( pAllCompatibleTransactionCommands
  )
where

import Cardano.Api
import Cardano.Api.Experimental qualified as Exp

import Cardano.CLI.Compatible.Transaction.Command
import Cardano.CLI.Environment
import Cardano.CLI.EraBased.Common.Option hiding (pRefScriptFp, pTxOutDatum)
import Cardano.CLI.EraBased.Script.Type
import Cardano.CLI.Parser
import Cardano.CLI.Read
import Cardano.CLI.Type.Common
import Cardano.CLI.Type.Governance

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
    <*> pTxInsCollateral sbe
    <*> many (pTxOutEraAware sbe)
    <*> pOptionalUpdateProposalFile sbe
    <*> pProposalFiles sbe ManualBalance
    <*> pVoteFiles sbe ManualBalance
    <*> many pWitnessSigningData
    <*> optional (pNetworkId env)
    <*> pTxFee
    <*> pCertificateFiles sbe ManualBalance
    <*> optional pProtocolParamsFile
    <*> pOutputFile

pTxInOnly :: Parser TxIn
pTxInOnly =
  Opt.option
    (readerFromParsecParser parseTxIn)
    ( Opt.long "tx-in"
        <> Opt.metavar "TX_IN"
        <> Opt.help "TxId#TxIx"
    )

pTxInsCollateral :: ShelleyBasedEra era -> Parser [TxIn]
pTxInsCollateral = \case
  ShelleyBasedEraShelley -> pure []
  ShelleyBasedEraAllegra -> pure []
  ShelleyBasedEraMary -> pure []
  ShelleyBasedEraAlonzo -> many pTxInCollateral
  ShelleyBasedEraBabbage -> many pTxInCollateral
  ShelleyBasedEraConway -> many pTxInCollateral
  ShelleyBasedEraDijkstra -> many pTxInCollateral

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
pTxOutDatum = \case
  ShelleyBasedEraShelley -> pure TxOutDatumByNone
  ShelleyBasedEraAllegra -> pure TxOutDatumByNone
  ShelleyBasedEraMary -> pure TxOutDatumByNone
  ShelleyBasedEraAlonzo -> pAlonzoDatumFunctionality <|> pure TxOutDatumByNone
  ShelleyBasedEraBabbage -> pBabbageDatumFunctionality <|> pure TxOutDatumByNone
  ShelleyBasedEraConway -> pConwayDatumFunctionality <|> pure TxOutDatumByNone
  ShelleyBasedEraDijkstra -> pConwayDatumFunctionality <|> pure TxOutDatumByNone
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
      Opt.option (readerFromParsecParser parseScriptDataHash) $
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
pRefScriptFp = \case
  ShelleyBasedEraShelley -> pure ReferenceScriptAnyEraNone
  ShelleyBasedEraAllegra -> pure ReferenceScriptAnyEraNone
  ShelleyBasedEraMary -> pure ReferenceScriptAnyEraNone
  ShelleyBasedEraAlonzo -> pure ReferenceScriptAnyEraNone
  ShelleyBasedEraBabbage -> pure ReferenceScriptAnyEraNone
  ShelleyBasedEraConway -> pReferenceScript
  ShelleyBasedEraDijkstra -> pReferenceScript
 where
  pReferenceScript =
    ReferenceScriptAnyEra
      <$> parseFilePath "tx-out-reference-script-file" "Reference script input file."
        <|> pure ReferenceScriptAnyEraNone

pOptionalUpdateProposalFile :: ShelleyBasedEra era -> Parser (Maybe UpdateProposalFile)
pOptionalUpdateProposalFile = \case
  ShelleyBasedEraShelley -> optional pUpdateProposalFile
  ShelleyBasedEraAllegra -> optional pUpdateProposalFile
  ShelleyBasedEraMary -> optional pUpdateProposalFile
  ShelleyBasedEraAlonzo -> optional pUpdateProposalFile
  ShelleyBasedEraBabbage -> optional pUpdateProposalFile
  ShelleyBasedEraConway -> pure Nothing
  ShelleyBasedEraDijkstra -> pure Nothing

pVoteFiles
  :: forall era
   . ShelleyBasedEra era
  -> BalanceTxExecUnits
  -> Parser [(VoteFile In, Maybe AnyNonAssetScript)]
pVoteFiles sbe bExUnits =
  case sbe of
    ShelleyBasedEraShelley -> pure []
    ShelleyBasedEraAllegra -> pure []
    ShelleyBasedEraMary -> pure []
    ShelleyBasedEraAlonzo -> pure []
    ShelleyBasedEraBabbage -> pure []
    ShelleyBasedEraConway -> pVoteFilesForEra (Exp.useEra @era) bExUnits
    ShelleyBasedEraDijkstra -> pVoteFilesForEra (Exp.useEra @era) bExUnits

pProposalFiles
  :: forall era
   . ShelleyBasedEra era
  -> BalanceTxExecUnits
  -> Parser [(ProposalFile In, Maybe AnyNonAssetScript)]
pProposalFiles sbe bExUnits =
  case sbe of
    ShelleyBasedEraShelley -> pure []
    ShelleyBasedEraAllegra -> pure []
    ShelleyBasedEraMary -> pure []
    ShelleyBasedEraAlonzo -> pure []
    ShelleyBasedEraBabbage -> pure []
    ShelleyBasedEraConway -> pProposalFilesForEra (Exp.useEra @era) bExUnits
    ShelleyBasedEraDijkstra -> pProposalFilesForEra (Exp.useEra @era) bExUnits

pCertificateFiles
  :: forall era
   . ShelleyBasedEra era
  -> BalanceTxExecUnits
  -> Parser [(CertificateFile, Maybe AnyNonAssetScript)]
pCertificateFiles sbe bExUnits =
  many $ case sbe of
    ShelleyBasedEraShelley -> pCertificateFile
    ShelleyBasedEraAllegra -> pCertificateFile
    ShelleyBasedEraMary -> pCertificateFile
    ShelleyBasedEraAlonzo -> pCertificateFile
    ShelleyBasedEraBabbage -> pCertificateFile
    ShelleyBasedEraConway -> pCertificateFileForEra (Exp.useEra @era) bExUnits
    ShelleyBasedEraDijkstra -> pCertificateFileForEra (Exp.useEra @era) bExUnits
 where
  pCertificateFile :: Parser (CertificateFile, Maybe AnyNonAssetScript)
  pCertificateFile =
    pCertificateFileWithPlutusReferenceLanguage
      (pAnyPlutusSLanguage "certificate-")
      "the use of the certificate."
      bExUnits
