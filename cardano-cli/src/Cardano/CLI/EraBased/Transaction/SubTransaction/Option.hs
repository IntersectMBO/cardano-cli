{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Parsers for Dijkstra sub-transactions: the @transaction sub-transaction@
-- command group.
module Cardano.CLI.EraBased.Transaction.SubTransaction.Option
  ( pTransactionSubTransactionCmds
  )
where

import Cardano.Api
import Cardano.Api.Experimental qualified as Exp

import Cardano.CLI.EraBased.Common.Option
import Cardano.CLI.EraBased.Transaction.Command (TransactionCmds (..))
import Cardano.CLI.EraBased.Transaction.SubTransaction.Command
import Cardano.CLI.Option.Flag
import Cardano.CLI.Parser
import Cardano.CLI.Read
import Cardano.CLI.Type.Common
import Cardano.CLI.Type.Key

import Control.Monad (join)
import Data.Foldable (asum)
import Data.Function ((&))
import Options.Applicative (Parser, many, optional, some)
import Options.Applicative qualified as Opt
import Options.Applicative.Help qualified as H
import Prettyprinter (line)

-- | The @sub-transaction@ command group. Sub-transactions exist from Dijkstra
-- onwards, so the group is absent in Conway.
pTransactionSubTransactionCmds
  :: forall era. Exp.IsEra era => Maybe (Parser (TransactionCmds era))
pTransactionSubTransactionCmds =
  case Exp.useEra @era of
    Exp.ConwayEra -> Nothing
    Exp.DijkstraEra ->
      fmap TransactionSubTransactionCmds
        <$> subInfoParser
          "sub-transaction"
          ( Opt.progDesc $
              mconcat
                [ "Sub-transaction commands. A sub-transaction is built and signed on its own, "
                , "then embedded in a top-level transaction."
                ]
          )
          [ Just $
              Opt.hsubparser $
                commandWithMetavar "build-raw" $
                  Opt.info pSubTransactionBuildRaw $
                    Opt.progDescDoc $
                      Just $
                        mconcat
                          [ pretty @String "Build an unsigned sub-transaction (low-level, inconvenient)"
                          , line
                          , line
                          , H.yellow $
                              mconcat
                                [ "Please note "
                                , H.underline "the order"
                                , " of some cmd options is crucial. If used incorrectly may produce "
                                , "undesired sub-transaction body. See nested [] notation above for details."
                                ]
                          ]
          , Just $
              Opt.hsubparser $
                commandWithMetavar "sign" $
                  Opt.info pSubTransactionSign $
                    Opt.progDesc "Sign a sub-transaction"
          , Just $
              Opt.hsubparser $
                commandWithMetavar "txid" $
                  Opt.info pSubTransactionTxId $
                    Opt.progDesc "Print a sub-transaction identifier."
          ]

pSubTransactionBuildRaw :: forall era. Exp.IsEra era => Parser (SubTransactionCmds era)
pSubTransactionBuildRaw =
  fmap SubTransactionBuildRawCmd $
    SubTransactionBuildRawCmdArgs Exp.useEra
      <$> some (pTxIn ManualBalance)
      <*> many pReadOnlyReferenceTxIn
      <*> many pTxOut
      <*> (fmap join . optional $ pMintMultiAsset @era ManualBalance)
      <*> optional pInvalidBefore
      <*> optional pInvalidHereafterSlot
      <*> many (pCertificateFile ManualBalance)
      <*> many (pWithdrawal ManualBalance)
      <*> pTxMetadataJsonSchema
      <*> many (pScriptFor "auxiliary-script-file" Nothing "Filepath of auxiliary script(s)")
      <*> many pMetadataFile
      <*> optional pProtocolParamsFile
      <*> pVoteFiles ManualBalance
      <*> pProposalFiles ManualBalance
      <*> pCurrentTreasuryValue
      <*> pTreasuryDonation
      <*> many pGuard
      <*> pUnsignedSubTxFileOut

pSubTransactionSign :: Parser (SubTransactionCmds era)
pSubTransactionSign =
  fmap SubTransactionSignCmd $
    SubTransactionSignCmdArgs
      <$> pUnsignedSubTxFileIn
      <*> many pWitnessSigningData
      <*> pSignedSubTxFileOut

pSubTransactionTxId :: Parser (SubTransactionCmds era)
pSubTransactionTxId =
  fmap SubTransactionTxIdCmd $
    SubTransactionTxIdCmdArgs
      <$> pInputSubTxFile
      <*> pFormatFlags
        "output"
        [ flagFormatJson & setDefault
        , flagFormatText
        , flagFormatYaml
        ]

-- Leaf parsers

-- | A guard credential: the sub-transaction requires this key's signature or
-- this script's approval. Guards replace required signers in Dijkstra.
pGuard :: Parser (VerificationKeyOrHashOrFileOrScriptHash PaymentKey)
pGuard =
  asum
    [ VkhfshKeyHashFile . VerificationKeyOrFile <$> pGuardVerificationKeyOrFile
    , VkhfshKeyHashFile . VerificationKeyHash <$> pGuardKeyHash
    , VkhfshScriptHash
        <$> pScriptHash
          "guard-script-hash"
          "Hash of a native or Plutus guard script (hex-encoded). Obtain it with \"cardano-cli hash script ...\"."
    ]

pGuardVerificationKeyOrFile :: Parser (VerificationKeyOrFile PaymentKey)
pGuardVerificationKeyOrFile =
  asum
    [ VerificationKeyValue <$> pGuardVerificationKey
    , VerificationKeyFilePath <$> pGuardVerificationKeyFile
    ]

pGuardVerificationKey :: Parser (VerificationKey PaymentKey)
pGuardVerificationKey =
  Opt.option (rVerificationKey $ Just "Invalid guard verification key") $
    mconcat
      [ Opt.long "guard-verification-key"
      , Opt.metavar "STRING"
      , Opt.help "Payment verification key whose signature the sub-transaction requires (hex-encoded)."
      ]

pGuardVerificationKeyFile :: Parser (VerificationKeyFile In)
pGuardVerificationKeyFile =
  File
    <$> parseFilePath
      "guard-verification-key-file"
      "Filepath of a payment verification key whose signature the sub-transaction requires."

pGuardKeyHash :: Parser (Hash PaymentKey)
pGuardKeyHash =
  Opt.option (readerFromParsecParser parseHexHash) $
    mconcat
      [ Opt.long "guard-key-hash"
      , Opt.metavar "HASH"
      , Opt.help "Hash of a payment verification key whose signature the sub-transaction requires."
      ]

-- | Like 'pInvalidHereafter' without the deprecated aliases and without the
-- era-indexed wrapper; sub-transactions carry a plain slot.
pInvalidHereafterSlot :: Parser SlotNo
pInvalidHereafterSlot =
  fmap SlotNo $
    Opt.option (bounded "SLOT") $
      mconcat
        [ Opt.long "invalid-hereafter"
        , Opt.metavar "SLOT"
        , Opt.help "Time that transaction is valid until (in slots)."
        ]

pUnsignedSubTxFileOut :: Parser (UnsignedSubTxFile Out)
pUnsignedSubTxFileOut =
  File <$> parseFilePath "out-file" "Output filepath of the JSON unsigned sub-transaction."

pSignedSubTxFileOut :: Parser (SignedSubTxFile Out)
pSignedSubTxFileOut =
  File <$> parseFilePath "out-file" "Output filepath of the JSON signed sub-transaction."

pUnsignedSubTxFileIn :: Parser (UnsignedSubTxFile In)
pUnsignedSubTxFileIn =
  File <$> parseFilePath "sub-tx-file" "Input filepath of the JSON unsigned sub-transaction."

pSignedSubTxFileIn :: Parser (SignedSubTxFile In)
pSignedSubTxFileIn =
  File <$> parseFilePath "signed-sub-tx-file" "Input filepath of the JSON signed sub-transaction."

pInputSubTxFile :: Parser InputSubTxFile
pInputSubTxFile =
  asum
    [ InputUnsignedSubTxFile <$> pUnsignedSubTxFileIn
    , InputSignedSubTxFile <$> pSignedSubTxFileIn
    ]
