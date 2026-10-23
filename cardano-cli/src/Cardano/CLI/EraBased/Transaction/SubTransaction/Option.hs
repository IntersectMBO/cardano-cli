{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Parsers for Dijkstra sub-transactions: the @sub-transaction@
-- command group and the @--signed-sub-tx-file@ option with which a top-level
-- transaction embeds signed sub-transactions.
module Cardano.CLI.EraBased.Transaction.SubTransaction.Option
  ( pSubTransactionCmds
  , pSubTransactionFiles
  )
where

import Cardano.Api
import Cardano.Api.Experimental qualified as Exp

import Cardano.CLI.EraBased.Common.Option
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
pSubTransactionCmds
  :: forall era. Exp.IsEra era => Maybe (Parser SubTransactionCmds)
pSubTransactionCmds =
  case Exp.useEra @era of
    Exp.ConwayEra -> Nothing
    Exp.DijkstraEra ->
      subInfoParser
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
                        , "Canonical output fails if it would change sub-transaction data covered by an existing hash."
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

pSubTransactionBuildRaw :: Parser SubTransactionCmds
pSubTransactionBuildRaw =
  fmap SubTransactionBuildRawCmd $
    SubTransactionBuildRawCmdArgs
      <$> some
        ( pTxInWithPlutusReferenceLanguage
            (pPlutusV4ReferenceLanguage Exp.DijkstraEra "spending-")
            "the spending of the transaction input. Only native scripts or Plutus V4 scripts are supported."
            ManualBalance
        )
      <*> many pReadOnlyReferenceTxIn
      <*> many pTxOut
      <*> ( fmap join . optional $
              pMintMultiAssetWithPlutusReferenceLanguage @Exp.DijkstraEra
                (pPlutusV4ReferenceLanguage Exp.DijkstraEra "mint-")
                "the minting of assets for a particular policy Id. Only native scripts or Plutus V4 scripts are supported."
                ManualBalance
          )
      <*> optional pInvalidBefore
      <*> optional pInvalidHereafterSlot
      <*> many
        ( pCertificateFileWithPlutusReferenceLanguage
            (pPlutusV4ReferenceLanguage Exp.DijkstraEra "certificate-")
            "the use of the certificate. Only native scripts or Plutus V4 scripts are supported."
            ManualBalance
        )
      <*> many
        ( pWithdrawalWithPlutusReferenceLanguage
            (pPlutusV4ReferenceLanguage Exp.DijkstraEra "withdrawal-")
            "the withdrawal of rewards. Only native scripts or Plutus V4 scripts are supported."
            ManualBalance
        )
      <*> pDirectDeposits Exp.DijkstraEra
      <*> pAccountBalanceIntervals Exp.DijkstraEra
      <*> pTxMetadataJsonSchema
      <*> many (pScriptFor "auxiliary-script-file" Nothing "Filepath of auxiliary script(s)")
      <*> many pMetadataFile
      <*> optional pProtocolParamsFile
      <*> many
        ( pVoteFileWithPlutusReferenceLanguage
            (pPlutusV4ReferenceLanguage Exp.DijkstraEra "vote-")
            "a vote. Only native scripts or Plutus V4 scripts are supported."
            ManualBalance
        )
      <*> many
        ( pProposalFileWithPlutusReferenceLanguage
            (pPlutusV4ReferenceLanguage Exp.DijkstraEra "proposal-")
            "a proposal. Only native scripts or Plutus V4 scripts are supported."
            ManualBalance
        )
      <*> pCurrentTreasuryValue
      <*> pTreasuryDonation
      <*> many pGuard
      <*> many pTopLevelGuard
      <*> pIsCborOutCanonical
      <*> pUnsignedSubTxFileOut

pSubTransactionSign :: Parser SubTransactionCmds
pSubTransactionSign =
  fmap SubTransactionSignCmd $
    SubTransactionSignCmdArgs
      <$> pUnsignedSubTxFileIn
      <*> many pSubTransactionSigningData
      <*> pSignedSubTxFileOut

pSubTransactionTxId :: Parser SubTransactionCmds
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

-- | A credential that the enclosing top-level transaction must include as a guard
-- when it embeds this sub-transaction. File inputs allow datum requirements to
-- be checked when reading the script; hashes leave that check to the ledger.
pTopLevelGuard :: Parser TopLevelGuard
pTopLevelGuard =
  asum
    [ TopLevelKeyGuard <$> pGuardKey "top-level-guard" "enclosing transaction"
    , TopLevelScriptGuard <$> pTopLevelGuardScript <*> optional pTopLevelGuardDatum
    ]

pTopLevelGuardScript :: Parser GuardScript
pTopLevelGuardScript =
  asum
    [ GuardScriptHash
        <$> pScriptHash
          "top-level-guard-script-hash"
          "Hash of a script required as a guard in the enclosing transaction (hex-encoded). Supply its witness in the enclosing transaction."
    , GuardScriptFile
        <$> pScriptFor
          "top-level-guard-script-file"
          Nothing
          "Script file whose hash is required as a guard in the enclosing transaction; no witness is added here. Plutus scripts must be V4 and require a datum; native scripts must not have one."
    ]

pTopLevelGuardDatum :: Parser ScriptDataOrFile
pTopLevelGuardDatum =
  pScriptDataOrFile
    "top-level-guard-datum"
    "Datum for the preceding script guard. Required only for Plutus scripts; must be omitted for key and native-script guards."
    "Datum file for the preceding script guard. Required only for Plutus scripts; must be omitted for key and native-script guards."

-- | A guard credential: the sub-transaction requires this key's signature or
-- this script's approval. Guards replace required signers in Dijkstra.
pGuard :: Parser Guard
pGuard =
  asum
    [ KeyGuard <$> pGuardKey "guard" "sub-transaction"
    , ScriptGuard <$> pGuardScript <*> optional pGuardRedeemer
    ]

pGuardScript :: Parser GuardScript
pGuardScript =
  asum
    [ GuardScriptHash
        <$> pScriptHash
          "guard-script-hash"
          "Hash of a native or Plutus V4 guard script (hex-encoded). Supply the script through another witness or a reference input. When supplying only a hash, you must provide --guard-redeemer-* and --guard-execution-units if the script is Plutus; the CLI cannot determine the script type from its hash."
    , GuardScriptFile
        <$> pScriptFor
          "guard-script-file"
          Nothing
          "Filepath of a native or Plutus V4 script whose approval the sub-transaction requires. Plutus scripts require --guard-redeemer-* and --guard-execution-units; native scripts accept neither."
    ]

pGuardRedeemer :: Parser (ScriptDataOrFile, ExecutionUnits)
pGuardRedeemer =
  (,)
    <$> pScriptDataOrFile
      "guard-redeemer"
      "Redeemer for the preceding script guard. Required only for Plutus V4 scripts, together with --guard-execution-units."
      "Redeemer file for the preceding script guard. Required only for Plutus V4 scripts, together with --guard-execution-units."
    <*> pExecutionUnits "guard"

-- | Share key forms while keeping the required transaction explicit in help.
pGuardKey :: String -> String -> Parser (VerificationKeyOrHashOrFile PaymentKey)
pGuardKey prefix transaction =
  asum
    [ VerificationKeyOrFile . VerificationKeyValue <$> key
    , VerificationKeyOrFile . VerificationKeyFilePath <$> keyFile
    , VerificationKeyHash <$> keyHash
    ]
 where
  otherRoles =
    " For other signing-key roles, hash the verification key first and use --" ++ prefix ++ "-key-hash."
  key =
    Opt.option (rVerificationKey $ Just "Invalid guard verification key") $
      mconcat
        [ Opt.long (prefix ++ "-verification-key")
        , Opt.metavar "STRING"
        , Opt.help $
            "Payment verification key whose signature the "
              ++ transaction
              ++ " requires (hex-encoded)."
              ++ otherRoles
        ]
  keyFile =
    File
      <$> parseFilePath
        (prefix ++ "-verification-key-file")
        ( "Filepath of a payment verification key whose signature the "
            ++ transaction
            ++ " requires."
            ++ otherRoles
        )
  keyHash =
    Opt.option (readerFromParsecParser parseHexHash) $
      mconcat
        [ Opt.long (prefix ++ "-key-hash")
        , Opt.metavar "HASH"
        , Opt.help $
            "Hash of a verification key whose signature the "
              ++ transaction
              ++ " requires (hex-encoded). Accepts hashes from any Shelley signing-key role, including stake, DRep and committee keys."
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

-- | Sub-transactions use Shelley witnesses, so no Byron address is accepted.
-- The signing-key file's type is checked when it is read.
pSubTransactionSigningData :: Parser WitnessSigningData
pSubTransactionSigningData =
  (`KeyWitnessSigningData` Nothing) <$> pSigningKeyFileIn

pInputSubTxFile :: Parser InputSubTxFile
pInputSubTxFile =
  asum
    [ InputUnsignedSubTxFile <$> pUnsignedSubTxFileIn
    , InputSignedSubTxFile <$> pSignedSubTxFileIn
    ]

-- | Signed sub-transactions to embed in a top-level transaction. Only Dijkstra
-- has sub-transactions, so the option does not exist in Conway.
pSubTransactionFiles :: forall era. Exp.IsEra era => Parser [SignedSubTxFile In]
pSubTransactionFiles = case Exp.useEra @era of
  Exp.ConwayEra -> pure []
  Exp.DijkstraEra ->
    many $
      File
        <$> parseFilePath
          "signed-sub-tx-file"
          "Filepath of a signed sub-transaction to embed in this transaction (zero or more)."
