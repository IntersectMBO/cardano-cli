{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.CLI.EraBased.Options.Transaction
  ( TransactionCmd(..)
  , pTransactionCmd
  , renderTransactionCmd
  ) where

import           Cardano.Api hiding (QueryInShelleyBasedEra (..))

import           Cardano.CLI.Commands.Legacy
import           Cardano.CLI.Environment (EnvCli (..))
import           Cardano.CLI.EraBased.Options.Common
import           Cardano.CLI.Types.Governance
import           Cardano.CLI.Types.Legacy

import qualified Data.Aeson as Aeson
import           Data.Foldable
import           Data.Functor (($>))
import           Data.Maybe
import           Data.Text (Text)
import           GHC.Natural (Natural)
import           Options.Applicative hiding (help, str)
import qualified Options.Applicative as Opt
import qualified Options.Applicative.Help as H
import           Prettyprinter (line, pretty)
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.String as Parsec

data TransactionCmd era
  = TxBuildRaw
      AnyCardanoEra
      (Maybe ScriptValidity) -- ^ Mark script as expected to pass or fail validation
      [(TxIn, Maybe (ScriptWitnessFiles WitCtxTxIn))]
      -- ^ Transaction inputs with optional spending scripts
      [TxIn]
      -- ^ Read only reference inputs
      [TxIn]
      -- ^ Transaction inputs for collateral, only key witnesses, no scripts.
      (Maybe TxOutAnyEra)
      -- ^ Return collateral
      (Maybe Lovelace)
      -- ^ Total collateral
      [RequiredSigner]
      -- ^ Required signers
      [TxOutAnyEra]
      (Maybe (Value, [ScriptWitnessFiles WitCtxMint]))
      -- ^ Multi-Asset value with script witness
      (Maybe SlotNo)
      -- ^ Transaction lower bound
      (Maybe SlotNo)
      -- ^ Transaction upper bound
      (Maybe Lovelace)
      -- ^ Tx fee
      [(CertificateFile, Maybe (ScriptWitnessFiles WitCtxStake))]
      -- ^ Certificates with potential script witness
      [(StakeAddress, Lovelace, Maybe (ScriptWitnessFiles WitCtxStake))]
      TxMetadataJsonSchema
      [ScriptFile]
      -- ^ Auxiliary scripts
      [MetadataFile]
      (Maybe ProtocolParamsFile)
      (Maybe UpdateProposalFile)
      (TxBodyFile Out)

    -- | Like 'TxBuildRaw' but without the fee, and with a change output.
  | TxBuild
      SocketPath
      (CardanoEra era)
      AnyConsensusModeParams
      NetworkId
      (Maybe ScriptValidity) -- ^ Mark script as expected to pass or fail validation
      (Maybe Word)
      -- ^ Override the required number of tx witnesses
      [(TxIn, Maybe (ScriptWitnessFiles WitCtxTxIn))]
      -- ^ Transaction inputs with optional spending scripts
      [TxIn]
      -- ^ Read only reference inputs
      [RequiredSigner]
      -- ^ Required signers
      [TxIn]
      -- ^ Transaction inputs for collateral, only key witnesses, no scripts.
      (Maybe TxOutAnyEra)
      -- ^ Return collateral
      (Maybe Lovelace)
      -- ^ Total collateral
      [TxOutAnyEra]
      -- ^ Normal outputs
      TxOutChangeAddress
      -- ^ A change output
      (Maybe (Value, [ScriptWitnessFiles WitCtxMint]))
      -- ^ Multi-Asset value with script witness
      (Maybe SlotNo)
      -- ^ Transaction lower bound
      (Maybe SlotNo)
      -- ^ Transaction upper bound
      [(CertificateFile, Maybe (ScriptWitnessFiles WitCtxStake))]
      -- ^ Certificates with potential script witness
      [(StakeAddress, Lovelace, Maybe (ScriptWitnessFiles WitCtxStake))]
      -- ^ Withdrawals with potential script witness
      TxMetadataJsonSchema
      [ScriptFile]
      -- ^ Auxiliary scripts
      [MetadataFile]
      (Maybe (Deprecated ProtocolParamsFile))
      (Maybe UpdateProposalFile)
      [File (ConwayVote era) In]
      [NewConstitutionFile In]
      TxBuildOutputOptions
  | TxSign InputTxBodyOrTxFile [WitnessSigningData] (Maybe NetworkId) (TxFile Out)
  | TxCreateWitness (TxBodyFile In) WitnessSigningData (Maybe NetworkId) (File () Out)
  | TxAssembleTxBodyWitness (TxBodyFile In) [WitnessFile] (File () Out)
  | TxSubmit SocketPath AnyConsensusModeParams NetworkId FilePath
  | TxMintedPolicyId ScriptFile
  | TxCalculateMinFee
      (TxBodyFile In)
      NetworkId
      ProtocolParamsFile
      TxInCount
      TxOutCount
      TxShelleyWitnessCount
      TxByronWitnessCount
  | TxCalculateMinRequiredUTxO
      AnyCardanoEra
      ProtocolParamsFile
      TxOutAnyEra
  | TxHashScriptData
      ScriptDataOrFile
  | TxGetTxId InputTxBodyOrTxFile
  | TxView InputTxBodyOrTxFile

renderTransactionCmd :: TransactionCmd era -> Text
renderTransactionCmd cmd =
  case cmd of
    TxBuild {} -> "transaction build"
    TxBuildRaw {} -> "transaction build-raw"
    TxSign {} -> "transaction sign"
    TxCreateWitness {} -> "transaction witness"
    TxAssembleTxBodyWitness {} -> "transaction sign-witness"
    TxSubmit {} -> "transaction submit"
    TxMintedPolicyId {} -> "transaction policyid"
    TxCalculateMinFee {} -> "transaction calculate-min-fee"
    TxCalculateMinRequiredUTxO {} -> "transaction calculate-min-value"
    TxHashScriptData {} -> "transaction hash-script-data"
    TxGetTxId {} -> "transaction txid"
    TxView {} -> "transaction view"

pTransactionCmd :: EnvCli -> CardanoEra era -> Parser (TransactionCmd era)
pTransactionCmd envCli era =
  asum $ catMaybes
    [ Just
        $ subParser "build-raw"
        $ Opt.info pTransactionBuildRaw $ Opt.progDescDoc $ Just $ mconcat
          [ pretty @String "Build a transaction (low-level, inconvenient)"
          , line
          , line
          , H.yellow $ mconcat
            [ "Please note the order of some cmd options is crucial. If used incorrectly may produce "
            , "undesired tx body. See nested [] notation above for details."
            ]
          ]
    , Just
        $ subParser "build"
        $ Opt.info (pTransactionBuild era)
        $ Opt.progDescDoc $ Just $ mconcat
          [ pretty @String "Build a balanced transaction (automatically calculates fees)"
          , line
          , line
          , H.yellow $ mconcat
            [ "Please note "
            , H.underline "the order"
            , " of some cmd options is crucial. If used incorrectly may produce "
            , "undesired tx body. See nested [] notation above for details."
            ]
          ]
    , Just
        $ subParser "sign"
        $ Opt.info pTransactionSign
        $ Opt.progDesc "Sign a transaction"
    , Just
        $ subParser "witness"
        $ Opt.info pTransactionCreateWitness
        $ Opt.progDesc "Create a transaction witness"
    , Just
        $ subParser "assemble"
        $ Opt.info pTransactionAssembleTxBodyWit
        $ Opt.progDesc "Assemble a tx body and witness(es) to form a transaction"
    , pSignWitnessBackwardCompatible
    , Just
        $ subParser "submit"
        $ Opt.info pTransactionSubmit . Opt.progDesc
        $ mconcat
            [ "Submit a transaction to the local node whose Unix domain socket "
            , "is obtained from the CARDANO_NODE_SOCKET_PATH environment variable."
            ]
    , Just
        $ subParser "policyid"
        $ Opt.info pTransactionPolicyId
        $ Opt.progDesc "Calculate the PolicyId from the monetary policy script."
    , Just
        $ subParser "calculate-min-fee"
        $ Opt.info pTransactionCalculateMinFee
        $ Opt.progDesc "Calculate the minimum fee for a transaction."
    , Just
        $ subParser "calculate-min-required-utxo"
        $ Opt.info pTransactionCalculateMinReqUTxO
        $ Opt.progDesc "Calculate the minimum required UTxO for a transaction output."
    , pCalculateMinRequiredUtxoBackwardCompatible
    , Just
        $ subParser "hash-script-data"
        $ Opt.info pTxHashScriptData
        $ Opt.progDesc "Calculate the hash of script data."
    , Just
        $ subParser "txid"
        $ Opt.info pTransactionId
        $ Opt.progDesc "Print a transaction identifier."
    , Just
        $ subParser "view"
        $ Opt.info pTransactionView
        $ Opt.progDesc "Print a transaction."
    ]
  where
    -- Backwards compatible parsers
    calcMinValueInfo :: ParserInfo (TransactionCmd era)
    calcMinValueInfo =
      Opt.info pTransactionCalculateMinReqUTxO
        $ Opt.progDesc "DEPRECATED: Use 'calculate-min-required-utxo' instead."

    pCalculateMinRequiredUtxoBackwardCompatible :: Maybe (Parser (TransactionCmd era))
    pCalculateMinRequiredUtxoBackwardCompatible =
      Just
        $ Opt.subparser
        $ Opt.command "calculate-min-value" calcMinValueInfo <> Opt.internal

    assembleInfo :: ParserInfo (TransactionCmd era)
    assembleInfo =
      Opt.info pTransactionAssembleTxBodyWit
        $ Opt.progDesc "Assemble a tx body and witness(es) to form a transaction"

    pSignWitnessBackwardCompatible :: Maybe (Parser (TransactionCmd era))
    pSignWitnessBackwardCompatible =
      Just
        $ Opt.subparser
        $ Opt.command "sign-witness" assembleInfo <> Opt.internal

    pScriptValidity :: Parser ScriptValidity
    pScriptValidity = asum
      [ Opt.flag' ScriptValid $ mconcat
        [ Opt.long "script-valid"
        , Opt.help "Assertion that the script is valid. (default)"
        ]
      , Opt.flag' ScriptInvalid $ mconcat
        [ Opt.long "script-invalid"
        , Opt.help $ mconcat
          [ "Assertion that the script is invalid.  "
          , "If a transaction is submitted with such a script, "
          , "the script will fail and the collateral will be taken."
          ]
        ]
      ]

    pTransactionBuild :: CardanoEra era -> Parser (TransactionCmd era)
    pTransactionBuild era' =
      TxBuild
        <$> pSocketPath envCli
        <*> pure era'
        <*> pConsensusModeParams
        <*> pNetworkId envCli
        <*> optional pScriptValidity
        <*> optional pWitnessOverride
        <*> some (pTxIn AutoBalance)
        <*> many pReadOnlyReferenceTxIn
        <*> many pRequiredSigner
        <*> many pTxInCollateral
        <*> optional pReturnCollateral
        <*> optional pTotalCollateral
        <*> many pTxOut
        <*> pChangeAddress
        <*> optional (pMintMultiAsset AutoBalance)
        <*> optional pInvalidBefore
        <*> optional pInvalidHereafter
        <*> many (pCertificateFile AutoBalance)
        <*> many (pWithdrawal AutoBalance)
        <*> pTxMetadataJsonSchema
        <*> many (pScriptFor
                    "auxiliary-script-file"
                    Nothing
                    "Filepath of auxiliary script(s)")
        <*> many pMetadataFile
        <*> optional pDeprecatedProtocolParamsFile
        <*> optional pUpdateProposalFile
        <*> many (pFileInDirection "vote-file" "Filepath of the vote.")
        <*> many (pFileInDirection "constitution-file" "Filepath of the constitution.")
        <*> (OutputTxBodyOnly <$> pTxBodyFileOut <|> pCalculatePlutusScriptCost)

    pChangeAddress :: Parser TxOutChangeAddress
    pChangeAddress =
      fmap TxOutChangeAddress $ Opt.option (readerFromParsecParser parseAddressAny) $ mconcat
        [ Opt.long "change-address"
        , Opt.metavar "ADDRESS"
        , Opt.help "Address where ADA in excess of the tx fee will go to."
        ]

    pTransactionBuildRaw :: Parser (TransactionCmd era)
    pTransactionBuildRaw =
      TxBuildRaw
        <$> pCardanoEra envCli
        <*> optional pScriptValidity
        <*> some (pTxIn ManualBalance)
        <*> many pReadOnlyReferenceTxIn
        <*> many pTxInCollateral
        <*> optional pReturnCollateral
        <*> optional pTotalCollateral
        <*> many pRequiredSigner
        <*> many pTxOut
        <*> optional (pMintMultiAsset ManualBalance)
        <*> optional pInvalidBefore
        <*> optional pInvalidHereafter
        <*> optional pTxFee
        <*> many (pCertificateFile ManualBalance )
        <*> many (pWithdrawal ManualBalance)
        <*> pTxMetadataJsonSchema
        <*> many (pScriptFor "auxiliary-script-file" Nothing "Filepath of auxiliary script(s)")
        <*> many pMetadataFile
        <*> optional pProtocolParamsFile
        <*> optional pUpdateProposalFile
        <*> pTxBodyFileOut

    pTransactionSign  :: Parser (TransactionCmd era)
    pTransactionSign =
      TxSign
        <$> pInputTxOrTxBodyFile
        <*> many pWitnessSigningData
        <*> optional (pNetworkId envCli)
        <*> pTxFileOut

    pTransactionCreateWitness :: Parser (TransactionCmd era)
    pTransactionCreateWitness =
      TxCreateWitness
        <$> pTxBodyFileIn
        <*> pWitnessSigningData
        <*> optional (pNetworkId envCli)
        <*> pOutputFile

    pTransactionAssembleTxBodyWit :: Parser (TransactionCmd era)
    pTransactionAssembleTxBodyWit =
      TxAssembleTxBodyWitness
        <$> pTxBodyFileIn
        <*> many pWitnessFile
        <*> pOutputFile

    pTransactionSubmit :: Parser (TransactionCmd era)
    pTransactionSubmit =
      TxSubmit
        <$> pSocketPath envCli
        <*> pConsensusModeParams
        <*> pNetworkId envCli
        <*> pTxSubmitFile

    pTransactionPolicyId :: Parser (TransactionCmd era)
    pTransactionPolicyId = TxMintedPolicyId <$> pScript

    pTransactionCalculateMinFee :: Parser (TransactionCmd era)
    pTransactionCalculateMinFee =
      TxCalculateMinFee
        <$> pTxBodyFileIn
        <*> pNetworkId envCli
        <*> pProtocolParamsFile
        <*> pTxInCount
        <*> pTxOutCount
        <*> pTxShelleyWitnessCount
        <*> pTxByronWitnessCount

    pTransactionCalculateMinReqUTxO :: Parser (TransactionCmd era)
    pTransactionCalculateMinReqUTxO = TxCalculateMinRequiredUTxO
      <$> pCardanoEra envCli
      <*> pProtocolParamsFile
      <*> pTxOut

    pTxHashScriptData :: Parser (TransactionCmd era)
    pTxHashScriptData =
      TxHashScriptData
        <$> pScriptDataOrFile
              "script-data"
              "The script data, in JSON syntax."
              "The script data, in the given JSON file."

    pTransactionId  :: Parser (TransactionCmd era)
    pTransactionId = TxGetTxId <$> pInputTxOrTxBodyFile

    pTransactionView :: Parser (TransactionCmd era)
    pTransactionView = TxView <$> pInputTxOrTxBodyFile


pScript :: Parser ScriptFile
pScript = pScriptFor "script-file" Nothing "Filepath of the script."

pReferenceTxIn :: String -> String -> Parser TxIn
pReferenceTxIn prefix scriptType =
  Opt.option (readerFromParsecParser parseTxIn) $ mconcat
    [ Opt.long (prefix ++ "tx-in-reference")
    , Opt.metavar "TX-IN"
    , Opt.help $ mconcat
      [ "TxId#TxIx - Specify a reference input. The reference input must have"
      , " a " <> scriptType <> " reference script attached."
      ]
    ]

pReadOnlyReferenceTxIn :: Parser TxIn
pReadOnlyReferenceTxIn =
  Opt.option (readerFromParsecParser parseTxIn) $ mconcat
    [ Opt.long "read-only-tx-in-reference"
    , Opt.metavar "TX-IN"
    , Opt.help $ mconcat
      [ "Specify a read only reference input. This reference input is not witnessing anything "
      , "it is simply provided in the plutus script context."
      ]
    ]


pScriptWitnessFiles :: forall witctx. ()
  => WitCtx witctx
  -> BalanceTxExecUnits -- ^ Use the @execution-units@ flag.
  -> String -- ^ Script flag prefix
  -> Maybe String
  -> String
  -> Parser (ScriptWitnessFiles witctx)
pScriptWitnessFiles witctx autoBalanceExecUnits scriptFlagPrefix scriptFlagPrefixDeprecated help =
    toScriptWitnessFiles
      <$> pScriptFor (scriptFlagPrefix ++ "-script-file")
                     ((++ "-script-file") <$> scriptFlagPrefixDeprecated)
                     ("The file containing the script to witness " ++ help)
      <*> optional ((,,) <$> pScriptDatumOrFile scriptFlagPrefix witctx
                         <*> pScriptRedeemerOrFile scriptFlagPrefix
                         <*> (case autoBalanceExecUnits of
                               AutoBalance -> pure (ExecutionUnits 0 0)
                               ManualBalance -> pExecutionUnits scriptFlagPrefix)
                   )
  where
    toScriptWitnessFiles :: ScriptFile
                         -> Maybe (ScriptDatumOrFile witctx,
                                   ScriptRedeemerOrFile,
                                   ExecutionUnits)
                         -> ScriptWitnessFiles witctx
    toScriptWitnessFiles sf Nothing        = SimpleScriptWitnessFile  sf
    toScriptWitnessFiles sf (Just (d,r, e)) = PlutusScriptWitnessFiles sf d r e


pExecutionUnits :: String -> Parser ExecutionUnits
pExecutionUnits scriptFlagPrefix =
  fmap (uncurry ExecutionUnits) $ Opt.option Opt.auto $ mconcat
    [ Opt.long (scriptFlagPrefix ++ "-execution-units")
    , Opt.metavar "(INT, INT)"
    , Opt.help "The time and space units needed by the script."
    ]

pScriptRedeemerOrFile :: String -> Parser ScriptDataOrFile
pScriptRedeemerOrFile scriptFlagPrefix =
  pScriptDataOrFile (scriptFlagPrefix ++ "-redeemer")
    "The script redeemer, in JSON syntax."
    "The script redeemer, in the given JSON file."


pScriptDatumOrFile :: String -> WitCtx witctx -> Parser (ScriptDatumOrFile witctx)
pScriptDatumOrFile scriptFlagPrefix witctx =
  case witctx of
    WitCtxTxIn  -> (ScriptDatumOrFileForTxIn <$>
                     pScriptDataOrFile
                       (scriptFlagPrefix ++ "-datum")
                       "The script datum, in JSON syntax."
                       "The script datum, in the given JSON file.") <|>
                    pInlineDatumPresent
    WitCtxMint  -> pure NoScriptDatumOrFileForMint
    WitCtxStake -> pure NoScriptDatumOrFileForStake
 where
  pInlineDatumPresent :: Parser (ScriptDatumOrFile WitCtxTxIn)
  pInlineDatumPresent  =
    flag' InlineDatumPresentAtTxIn $ mconcat
      [ long (scriptFlagPrefix ++ "-inline-datum-present")
      , Opt.help "Inline datum present at transaction input."
      ]

pScriptDataOrFile :: String -> String -> String -> Parser ScriptDataOrFile
pScriptDataOrFile dataFlagPrefix helpTextForValue helpTextForFile =
  asum
    [ pScriptDataCborFile
    , pScriptDataFile
    , pScriptDataValue
    ]
  where
    pScriptDataCborFile = fmap ScriptDataCborFile . Opt.strOption $ mconcat
      [ Opt.long (dataFlagPrefix ++ "-cbor-file")
      , Opt.metavar "CBOR FILE"
      , Opt.help $ mconcat
        [ helpTextForFile
        , " The file must follow the special JSON schema for script data."
        ]
      ]

    pScriptDataFile = fmap ScriptDataJsonFile . Opt.strOption $ mconcat
      [ Opt.long (dataFlagPrefix ++ "-file")
      , Opt.metavar "JSON FILE"
      , Opt.help $ mconcat
        [ helpTextForFile ++ " The file must follow the special "
        , "JSON schema for script data."
        ]
      ]

    pScriptDataValue = fmap ScriptDataValue . Opt.option readerScriptData $ mconcat
      [ Opt.long (dataFlagPrefix ++ "-value")
      , Opt.metavar "JSON VALUE"
      , Opt.help $ mconcat
        [ helpTextForValue
        , " There is no schema: (almost) any JSON value is supported, including "
        , "top-level strings and numbers."
        ]
      ]

    readerScriptData :: ReadM HashableScriptData
    readerScriptData = do
      v <- Opt.str
      case Aeson.eitherDecode v of
        Left e -> fail $ "readerScriptData: " <> e
        Right sDataValue ->
          case scriptDataJsonToHashable ScriptDataJsonNoSchema sDataValue of
            Left err -> fail (displayError err)
            Right sd -> return sd

pDeprecatedProtocolParamsFile :: Parser (Deprecated ProtocolParamsFile)
pDeprecatedProtocolParamsFile =
  fmap (Deprecated . ProtocolParamsFile) $ Opt.strOption $ mconcat
    [ Opt.long "protocol-params-file"
    , Opt.metavar "FILE"
    , Opt.help $ mconcat
        [ "Filepath of the JSON-encoded protocol parameters file. "
        , "DEPRECATED The option is ignored and protocol parameters are "
        , "retrieved from the node."
        ]
    , Opt.completer (Opt.bashCompleter "file")
    ]

pProtocolParamsFile :: Parser ProtocolParamsFile
pProtocolParamsFile =
  fmap ProtocolParamsFile $ Opt.strOption $ mconcat
    [ Opt.long "protocol-params-file"
    , Opt.metavar "FILE"
    , Opt.help "Filepath of the JSON-encoded protocol parameters file"
    , Opt.completer (Opt.bashCompleter "file")
    ]

pCalculatePlutusScriptCost :: Parser TxBuildOutputOptions
pCalculatePlutusScriptCost =
  OutputScriptCostOnly <$> Opt.strOption
   ( Opt.long "calculate-plutus-script-cost" <>
     Opt.metavar "FILE" <>
     Opt.help "(File () Out) filepath of the script cost information." <>
     Opt.completer (Opt.bashCompleter "file")
   )

pCertificateFile
  :: BalanceTxExecUnits
  -> Parser (CertificateFile, Maybe (ScriptWitnessFiles WitCtxStake))
pCertificateFile balanceExecUnits =
  (,)
    <$> ( fmap CertificateFile $ asum
            [ Opt.strOption $ mconcat
                [ Opt.long "certificate-file"
                , Opt.metavar "CERTIFICATEFILE"
                , Opt.help helpText
                , Opt.completer (Opt.bashCompleter "file")
                ]
            , Opt.strOption (Opt.long "certificate" <> Opt.internal)
            ]
        )
    <*> optional (pCertifyingScriptOrReferenceScriptWit balanceExecUnits)
 where
  pCertifyingScriptOrReferenceScriptWit
    :: BalanceTxExecUnits -> Parser (ScriptWitnessFiles WitCtxStake)
  pCertifyingScriptOrReferenceScriptWit bExecUnits =
    pScriptWitnessFiles
     WitCtxStake
     balanceExecUnits
     "certificate" Nothing
     "the use of the certificate." <|>
    pPlutusStakeReferenceScriptWitnessFiles "certificate-" bExecUnits

  helpText = mconcat
    [ "Filepath of the certificate. This encompasses all "
    , "types of certificates (stake pool certificates, "
    , "stake key certificates etc). Optionally specify a script witness."
    ]

pTxMetadataJsonSchema :: Parser TxMetadataJsonSchema
pTxMetadataJsonSchema =
  asum
    [ Opt.flag' ()
        (  Opt.long "json-metadata-no-schema"
        <> Opt.help "Use the \"no schema\" conversion from JSON to tx metadata."
        )
        $> TxMetadataJsonNoSchema
    , Opt.flag' ()
        (  Opt.long "json-metadata-detailed-schema"
        <> Opt.help "Use the \"detailed schema\" conversion from JSON to tx metadata."
        )
        $> TxMetadataJsonDetailedSchema
    , -- Default to the no-schema conversion.
      pure TxMetadataJsonNoSchema
    ]

pMetadataFile :: Parser MetadataFile
pMetadataFile =
  asum
    [ fmap MetadataFileJSON
        $ asum
            [ Opt.strOption $ mconcat
                [ Opt.long "metadata-json-file"
                , Opt.metavar "FILE"
                , Opt.help "Filepath of the metadata file, in JSON format."
                , Opt.completer (Opt.bashCompleter "file")
                ]
            , Opt.strOption $ mconcat
                [ Opt.long "metadata-file" -- backward compat name
                , Opt.internal
                ]
            ]
    , fmap MetadataFileCBOR $ Opt.strOption $ mconcat
        [ Opt.long "metadata-cbor-file"
        , Opt.metavar "FILE"
        , Opt.help "Filepath of the metadata, in raw CBOR format."
        , Opt.completer (Opt.bashCompleter "file")
        ]
    ]

pWithdrawal
  :: BalanceTxExecUnits
  -> Parser (StakeAddress,
            Lovelace,
            Maybe (ScriptWitnessFiles WitCtxStake))
pWithdrawal balance =
    (\(stakeAddr,lovelace) maybeScriptFp -> (stakeAddr, lovelace, maybeScriptFp))
      <$> Opt.option (readerFromParsecParser parseWithdrawal)
            (  Opt.long "withdrawal"
            <> Opt.metavar "WITHDRAWAL"
            <> Opt.help helpText
            )
      <*> optional pWithdrawalScriptOrReferenceScriptWit
 where
  pWithdrawalScriptOrReferenceScriptWit :: Parser (ScriptWitnessFiles WitCtxStake)
  pWithdrawalScriptOrReferenceScriptWit =
   pScriptWitnessFiles
     WitCtxStake
     balance
     "withdrawal" Nothing
     "the withdrawal of rewards." <|>
   pPlutusStakeReferenceScriptWitnessFiles "withdrawal-" balance

  helpText = mconcat
    [ "The reward withdrawal as StakeAddress+Lovelace where "
    , "StakeAddress is the Bech32-encoded stake address "
    , "followed by the amount in Lovelace. Optionally specify "
    , "a script witness."
    ]

  parseWithdrawal :: Parsec.Parser (StakeAddress, Lovelace)
  parseWithdrawal =
    (,) <$> parseStakeAddress <* Parsec.char '+' <*> parseLovelace

pPlutusStakeReferenceScriptWitnessFiles
  :: String
  -> BalanceTxExecUnits -- ^ Use the @execution-units@ flag.
  -> Parser (ScriptWitnessFiles WitCtxStake)
pPlutusStakeReferenceScriptWitnessFiles prefix autoBalanceExecUnits =
  PlutusReferenceScriptWitnessFiles
    <$> pReferenceTxIn prefix "plutus"
    <*> pPlutusScriptLanguage prefix
    <*> pure NoScriptDatumOrFileForStake
    <*> pScriptRedeemerOrFile (prefix ++ "reference-tx-in")
    <*> (case autoBalanceExecUnits of
          AutoBalance -> pure (ExecutionUnits 0 0)
          ManualBalance -> pExecutionUnits $ prefix ++ "reference-tx-in")
    <*> pure Nothing

pPlutusScriptLanguage :: String -> Parser AnyScriptLanguage
pPlutusScriptLanguage prefix =
  Opt.flag' (AnyScriptLanguage $ PlutusScriptLanguage PlutusScriptV2)
    (  Opt.long (prefix ++ "plutus-script-v2")
    <> Opt.help "Specify a plutus script v2 reference script."
    )

pUpdateProposalFile :: Parser UpdateProposalFile
pUpdateProposalFile =
  fmap UpdateProposalFile
    $ asum
        [ Opt.strOption $ mconcat
          [ Opt.long "update-proposal-file"
          , Opt.metavar "FILE"
          , Opt.help "Filepath of the update proposal."
          , Opt.completer (Opt.bashCompleter "file")
          ]
        , Opt.strOption $ mconcat
          [ Opt.long "update-proposal"
          , Opt.internal
          ]
        ]

pRequiredSigner :: Parser RequiredSigner
pRequiredSigner =
  asum
    [ RequiredSignerSkeyFile <$> sKeyFile
    , RequiredSignerHash <$> sPayKeyHash
    ]
 where
    sKeyFile :: Parser (SigningKeyFile In)
    sKeyFile = fmap File $ Opt.strOption $ mconcat
      [ Opt.long "required-signer"
      , Opt.metavar "FILE"
      , Opt.help $ mconcat
        [ "Input filepath of the signing key (zero or more) whose "
        , "signature is required."
        ]
      , Opt.completer (Opt.bashCompleter "file")
      ]
    sPayKeyHash :: Parser (Hash PaymentKey)
    sPayKeyHash =
      Opt.option (readerFromParsecParser $ parseHash (AsHash AsPaymentKey)) $ mconcat
        [ Opt.long "required-signer-hash"
        , Opt.metavar "HASH"
        , Opt.help $ mconcat
          [ "Hash of the verification key (zero or more) whose "
          , "signature is required."
          ]
        ]

pWitnessSigningData :: Parser WitnessSigningData
pWitnessSigningData =
  KeyWitnessSigningData
    <$> ( fmap File $ Opt.strOption $ mconcat
          [ Opt.long "signing-key-file"
          , Opt.metavar "FILE"
          , Opt.help "Input filepath of the signing key (one or more)."
          , Opt.completer (Opt.bashCompleter "file")
          ]
        )
    <*> optional pByronAddress

pTxSubmitFile :: Parser FilePath
pTxSubmitFile =
  Opt.strOption $ mconcat
    [ Opt.long "tx-file"
    , Opt.metavar "FILE"
    , Opt.help "Filepath of the transaction you intend to submit."
    , Opt.completer (Opt.bashCompleter "file")
    ]

pTxIn :: BalanceTxExecUnits
      -> Parser (TxIn, Maybe (ScriptWitnessFiles WitCtxTxIn))
pTxIn balance =
  (,)
    <$> pTxIn'
    <*> optional pWitness
  where
    pTxIn' =
      Opt.option (readerFromParsecParser parseTxIn) $ mconcat
        [ Opt.long "tx-in"
        , Opt.metavar "TX-IN"
        , Opt.help "TxId#TxIx"
        ]
    pWitness =
      asum
        [ pPlutusReferenceScriptWitness balance
        , pSimpleReferenceSpendingScriptWitness
        , pEmbeddedPlutusScriptWitness
        ]

    pSimpleReferenceSpendingScriptWitness :: Parser (ScriptWitnessFiles WitCtxTxIn)
    pSimpleReferenceSpendingScriptWitness =
      createSimpleReferenceScriptWitnessFiles
        <$> pReferenceTxIn "simple-script-" "simple"
      where
        createSimpleReferenceScriptWitnessFiles
          :: TxIn
          -> ScriptWitnessFiles WitCtxTxIn
        createSimpleReferenceScriptWitnessFiles refTxIn  =
          let simpleLang = AnyScriptLanguage SimpleScriptLanguage
          in SimpleReferenceScriptWitnessFiles refTxIn simpleLang Nothing

    pPlutusReferenceScriptWitness :: BalanceTxExecUnits -> Parser (ScriptWitnessFiles WitCtxTxIn)
    pPlutusReferenceScriptWitness autoBalanceExecUnits =
      createPlutusReferenceScriptWitnessFiles
        <$> pReferenceTxIn "spending-" "plutus"
        <*> pPlutusScriptLanguage "spending-"
        <*> pScriptDatumOrFile "spending-reference-tx-in" WitCtxTxIn
        <*> pScriptRedeemerOrFile "spending-reference-tx-in"
        <*> (case autoBalanceExecUnits of
                AutoBalance -> pure (ExecutionUnits 0 0)
                ManualBalance -> pExecutionUnits "spending-reference-tx-in")
      where
        createPlutusReferenceScriptWitnessFiles
          :: TxIn
          -> AnyScriptLanguage
          -> ScriptDatumOrFile WitCtxTxIn
          -> ScriptRedeemerOrFile
          -> ExecutionUnits
          -> ScriptWitnessFiles WitCtxTxIn
        createPlutusReferenceScriptWitnessFiles refIn sLang sDatum sRedeemer execUnits =
          PlutusReferenceScriptWitnessFiles refIn sLang sDatum sRedeemer execUnits Nothing

    pEmbeddedPlutusScriptWitness :: Parser (ScriptWitnessFiles WitCtxTxIn)
    pEmbeddedPlutusScriptWitness =
      pScriptWitnessFiles
        WitCtxTxIn
        balance
        "tx-in" (Just "txin")
        "the spending of the transaction input."

pTxInCollateral :: Parser TxIn
pTxInCollateral =
  Opt.option (readerFromParsecParser parseTxIn) $ mconcat
    [ Opt.long "tx-in-collateral"
    , Opt.metavar "TX-IN"
    , Opt.help "TxId#TxIx"
    ]

pReturnCollateral :: Parser TxOutAnyEra
pReturnCollateral =
  Opt.option (readerFromParsecParser parseTxOutAnyEra)
          ( mconcat
            [ Opt.long "tx-out-return-collateral"
            , Opt.metavar "ADDRESS VALUE"
            -- TODO alonzo: Update the help text to describe the new syntax as well.
            , Opt.help ( "The transaction output as ADDRESS VALUE where ADDRESS is " <>
                        "the Bech32-encoded address followed by the value in " <>
                        "Lovelace. In the situation where your collateral txin " <>
                        "over collateralizes the transaction, you can optionally " <>
                        "specify a tx out of your choosing to return the excess Lovelace."
            )
            ]
          )
    <*> pure TxOutDatumByNone -- TODO: Babbage era - we should be able to return these
    <*> pure ReferenceScriptAnyEraNone -- TODO: Babbage era - we should be able to return these

pTotalCollateral :: Parser Lovelace
pTotalCollateral =
  Opt.option (Lovelace <$> readerFromParsecParser decimal) $ mconcat
  [ Opt.long "tx-total-collateral"
  , Opt.metavar "INTEGER"
  , Opt.help $ mconcat
    [ "The total amount of collateral that will be collected "
    , "as fees in the event of a Plutus script failure. Must be used "
    , "in conjuction with \"--tx-out-return-collateral\"."
    ]
  ]

pWitnessOverride :: Parser Word
pWitnessOverride = Opt.option Opt.auto $ mconcat
  [ Opt.long "witness-override"
  , Opt.metavar "WORD"
  , Opt.help "Specify and override the number of witnesses the transaction requires."
  ]


pTxOut :: Parser TxOutAnyEra
pTxOut =
        Opt.option (readerFromParsecParser parseTxOutAnyEra)
          (  Opt.long "tx-out"
          <> Opt.metavar "ADDRESS VALUE"
          -- TODO alonzo: Update the help text to describe the new syntax as well.
          <> Opt.help "The transaction output as ADDRESS VALUE where ADDRESS is \
                      \the Bech32-encoded address followed by the value in \
                      \the multi-asset syntax (including simply Lovelace)."
          )
    <*> pTxOutDatum
    <*> pRefScriptFp


pTxOutDatum :: Parser TxOutDatumAnyEra
pTxOutDatum =
      pTxOutDatumByHashOnly
  <|> pTxOutDatumByHashOf
  <|> pTxOutDatumByValue
  <|> pTxOutInlineDatumByValue
  <|> pure TxOutDatumByNone
  where
    pTxOutDatumByHashOnly =
      fmap TxOutDatumByHashOnly
      $ Opt.option (readerFromParsecParser $ parseHash (AsHash AsScriptData))
      $ mconcat
        [ Opt.long "tx-out-datum-hash"
        , Opt.metavar "HASH"
        , Opt.help $ mconcat
          [ "The script datum hash for this tx output, as "
          , "the raw datum hash (in hex)."
          ]
        ]

    pTxOutDatumByHashOf = TxOutDatumByHashOf <$>
        pScriptDataOrFile
          "tx-out-datum-hash"
          ( mconcat
            [ "The script datum hash for this tx output, by hashing the "
            , "script datum given here in JSON syntax."
            ]
          )
          ( mconcat
            [ "The script datum hash for this tx output, by hashing the "
            , "script datum in the given JSON file."
            ]
          )

    pTxOutDatumByValue =
      TxOutDatumByValue <$>
        pScriptDataOrFile
          "tx-out-datum-embed"
          ( mconcat
            [ "The script datum to embed in the tx for this output, "
            , "given here in JSON syntax."
            ]
          )
          ( mconcat
            [ "The script datum to embed in the tx for this output, "
            , "in the given JSON file."
            ]
          )

    pTxOutInlineDatumByValue =
      TxOutInlineDatumByValue <$>
        pScriptDataOrFile
          "tx-out-inline-datum"
          ( mconcat
            [ "The script datum to embed in the tx output as an inline datum, "
            , "given here in JSON syntax."
            ]
          )
          ( mconcat
            [ "The script datum to embed in the tx output as an inline datum, "
            , "in the given JSON file."
            ]
          )

pRefScriptFp :: Parser ReferenceScriptAnyEra
pRefScriptFp =
  ReferenceScriptAnyEra <$> Opt.strOption
    (  Opt.long "tx-out-reference-script-file"
    <> Opt.metavar "FILE"
    <> Opt.help "Reference script input file."
    <> Opt.completer (Opt.bashCompleter "file")
    ) <|> pure ReferenceScriptAnyEraNone

pMintMultiAsset
  :: BalanceTxExecUnits
  -> Parser (Value, [ScriptWitnessFiles WitCtxMint])
pMintMultiAsset balanceExecUnits =
  (,) <$> Opt.option
            (readerFromParsecParser parseValue)
              (  Opt.long "mint"
              <> Opt.metavar "VALUE"
              <> Opt.help helpText
              )
      <*> some (pMintingScriptOrReferenceScriptWit balanceExecUnits <|>
                pSimpleReferenceMintingScriptWitness <|>
                pPlutusMintReferenceScriptWitnessFiles balanceExecUnits
               )
 where
  pMintingScriptOrReferenceScriptWit
    :: BalanceTxExecUnits -> Parser (ScriptWitnessFiles WitCtxMint)
  pMintingScriptOrReferenceScriptWit bExecUnits =
   pScriptWitnessFiles
     WitCtxMint
     bExecUnits
     "mint" (Just "minting")
     "the minting of assets for a particular policy Id."

  pSimpleReferenceMintingScriptWitness :: Parser (ScriptWitnessFiles WitCtxMint)
  pSimpleReferenceMintingScriptWitness =
    createSimpleMintingReferenceScriptWitnessFiles
      <$> pReferenceTxIn "simple-minting-script-" "simple"
      <*> pPolicyId
   where
    createSimpleMintingReferenceScriptWitnessFiles
      :: TxIn
      -> PolicyId
      -> ScriptWitnessFiles WitCtxMint
    createSimpleMintingReferenceScriptWitnessFiles refTxIn pid =
      let simpleLang = AnyScriptLanguage SimpleScriptLanguage
      in SimpleReferenceScriptWitnessFiles refTxIn simpleLang (Just pid)

  pPlutusMintReferenceScriptWitnessFiles
    :: BalanceTxExecUnits ->  Parser (ScriptWitnessFiles WitCtxMint)
  pPlutusMintReferenceScriptWitnessFiles autoBalanceExecUnits =
   PlutusReferenceScriptWitnessFiles
     <$> pReferenceTxIn "mint-" "plutus"
     <*> pPlutusScriptLanguage "mint-"
     <*> pure NoScriptDatumOrFileForMint
     <*> pScriptRedeemerOrFile "mint-reference-tx-in"
     <*> (case autoBalanceExecUnits of
           AutoBalance -> pure (ExecutionUnits 0 0)
           ManualBalance -> pExecutionUnits "mint-reference-tx-in")
     <*> (Just <$> pPolicyId)

  helpText = mconcat
    [ "Mint multi-asset value(s) with the multi-asset cli syntax. "
    , "You must specify a script witness."
    ]

pPolicyId :: Parser PolicyId
pPolicyId =
  Opt.option (readerFromParsecParser policyId) $ mconcat
    [ Opt.long "policy-id"
    , Opt.metavar "HASH"
    , Opt.help "Policy id of minting script."
    ]


pInvalidBefore :: Parser SlotNo
pInvalidBefore = fmap SlotNo $ asum
  [ Opt.option (bounded "SLOT") $ mconcat
    [ Opt.long "invalid-before"
    , Opt.metavar "SLOT"
    , Opt.help "Time that transaction is valid from (in slots)."
    ]
  , Opt.option (bounded "SLOT") $ mconcat
    [ Opt.long "lower-bound"
    , Opt.metavar "SLOT"
    , Opt.help $ mconcat
      [ "Time that transaction is valid from (in slots) "
      , "(deprecated; use --invalid-before instead)."
      ]
    , Opt.internal
    ]
  ]

pInvalidHereafter :: Parser SlotNo
pInvalidHereafter =
  fmap SlotNo $ asum
  [ Opt.option (bounded "SLOT") $ mconcat
    [ Opt.long "invalid-hereafter"
    , Opt.metavar "SLOT"
    , Opt.help "Time that transaction is valid until (in slots)."
    ]
  , Opt.option (bounded "SLOT") $ mconcat
    [ Opt.long "upper-bound"
    , Opt.metavar "SLOT"
    , Opt.help $ mconcat
      [ "Time that transaction is valid until (in slots) "
      , "(deprecated; use --invalid-hereafter instead)."
      ]
    , Opt.internal
    ]
  , Opt.option (bounded "SLOT") $ mconcat
    [ Opt.long "ttl"
    , Opt.metavar "SLOT"
    , Opt.help "Time to live (in slots) (deprecated; use --invalid-hereafter instead)."
    , Opt.internal
    ]
  ]

pTxFee :: Parser Lovelace
pTxFee =
  fmap (Lovelace . (fromIntegral :: Natural -> Integer)) $ Opt.option Opt.auto $ mconcat
    [ Opt.long "fee"
    , Opt.metavar "LOVELACE"
    , Opt.help "The fee amount in Lovelace."
    ]

pWitnessFile :: Parser WitnessFile
pWitnessFile =
  fmap WitnessFile $ Opt.strOption $ mconcat
    [ Opt.long "witness-file"
    , Opt.metavar "FILE"
    , Opt.help "Filepath of the witness"
    , Opt.completer (Opt.bashCompleter "file")
    ]

pTxBodyFileIn :: Parser (TxBodyFile In)
pTxBodyFileIn =
  fmap File $ Opt.strOption $ mconcat
    [ Opt.long "tx-body-file"
    , Opt.metavar "FILE"
    , Opt.help "Input filepath of the JSON TxBody."
    , Opt.completer (Opt.bashCompleter "file")
    ]

pTxBodyFileOut :: Parser (TxBodyFile Out)
pTxBodyFileOut =
  fmap File $ asum
    [ Opt.strOption $ mconcat
      [ Opt.long "out-file"
      , Opt.metavar "FILE"
      , Opt.help "Output filepath of the JSON TxBody."
      , Opt.completer (Opt.bashCompleter "file")
      ]
    , Opt.strOption $ mconcat
      [ Opt.long "tx-body-file"
      , Opt.internal
      ]
    ]

pTxFileIn :: Parser (TxFile In)
pTxFileIn =
  fmap File $ Opt.strOption $ mconcat
    [ Opt.long "tx-file"
    , Opt.metavar "FILE"
    , Opt.help "Input filepath of the JSON Tx."
    , Opt.completer (Opt.bashCompleter "file")
    ]

pTxFileOut :: Parser (TxFile Out)
pTxFileOut =
  fmap File $ asum
    [ Opt.strOption $ mconcat
      [ Opt.long "out-file"
      , Opt.metavar "FILE"
      , Opt.help "Output filepath of the JSON Tx."
      , Opt.completer (Opt.bashCompleter "file")
      ]
    , Opt.strOption $ mconcat
      [ Opt.long "tx-file"
      , Opt.internal
      ]
    ]

pInputTxOrTxBodyFile :: Parser InputTxBodyOrTxFile
pInputTxOrTxBodyFile =
  asum
    [ InputTxBodyFile <$> pTxBodyFileIn
    , InputTxFile <$> pTxFileIn
    ]

pTxInCount :: Parser TxInCount
pTxInCount =
  fmap TxInCount $ Opt.option Opt.auto $ mconcat
    [ Opt.long "tx-in-count"
    , Opt.metavar "NATURAL"
    , Opt.help "The number of transaction inputs."
    ]

pTxOutCount :: Parser TxOutCount
pTxOutCount =
  fmap TxOutCount $ Opt.option Opt.auto $ mconcat
    [ Opt.long "tx-out-count"
    , Opt.metavar "NATURAL"
    , Opt.help "The number of transaction outputs."
    ]

pTxShelleyWitnessCount :: Parser TxShelleyWitnessCount
pTxShelleyWitnessCount =
  fmap TxShelleyWitnessCount $ Opt.option Opt.auto $ mconcat
    [ Opt.long "witness-count"
    , Opt.metavar "NATURAL"
    , Opt.help "The number of Shelley key witnesses."
    ]

pTxByronWitnessCount :: Parser TxByronWitnessCount
pTxByronWitnessCount =
  fmap TxByronWitnessCount $ Opt.option Opt.auto $ mconcat
    [ Opt.long "byron-witness-count"
    , Opt.metavar "NATURAL"
    , Opt.help "The number of Byron key witnesses (default is 0)."
    , Opt.value 0
    ]
