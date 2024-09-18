{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.CLI.EraBased.Transaction.Option
  ( pTransactionCmds
  )
where

import Cardano.Api hiding (QueryInShelleyBasedEra (..))
import Cardano.Api.Experimental qualified as Exp

import Cardano.CLI.Environment (EnvCli (..))
import Cardano.CLI.EraBased.Common.Option
import Cardano.CLI.EraBased.Transaction.Command
import Cardano.CLI.Option.Flag
import Cardano.CLI.Parser
import Cardano.CLI.Read
import Cardano.CLI.Type.Common

import Control.Monad
import Data.Foldable
import Data.Function ((&))
import Data.Functor
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Universe (Some)
import Options.Applicative hiding (help, str)
import Options.Applicative qualified as Opt
import Options.Applicative.Help qualified as H
import Prettyprinter (line)

pTransactionCmds
  :: Exp.IsEra era
  => EnvCli
  -> Maybe (Parser (TransactionCmds era))
pTransactionCmds envCli =
  subInfoParser
    "transaction"
    ( Opt.progDesc $
        mconcat
          [ "Transaction commands."
          ]
    )
    [ Just $
        Opt.hsubparser $
          commandWithMetavar "build-raw" $
            Opt.info pTransactionBuildRaw $
              Opt.progDescDoc $
                Just $
                  mconcat
                    [ pretty @String "Build a transaction (low-level, inconvenient)"
                    , line
                    , line
                    , H.annotate (H.color H.Yellow) $
                        mconcat
                          [ "Please note "
                          , H.annotate H.underlined "the order"
                          , " of some cmd options is crucial. If used incorrectly may produce "
                          , "undesired tx body. See nested [] notation above for details."
                          ]
                    ]
    , pTransactionBuildCmd envCli
    , pTransactionBuildEstimateCmd envCli
    , Just $
        Opt.hsubparser $
          commandWithMetavar "sign" $
            Opt.info (pTransactionSign envCli) $
              Opt.progDesc "Sign a transaction"
    , Just $
        Opt.hsubparser $
          commandWithMetavar "witness" $
            Opt.info (pTransactionCreateWitness envCli) $
              Opt.progDesc "Create a transaction witness"
    , Just $
        Opt.hsubparser $
          commandWithMetavar "assemble" $
            Opt.info pTransactionAssembleTxBodyWit $
              Opt.progDesc "Assemble a tx body and witness(es) to form a transaction"
    , Just pSignWitnessBackwardCompatible
    , Just $
        Opt.hsubparser $
          commandWithMetavar "submit" $
            Opt.info (pTransactionSubmit envCli) $
              Opt.progDesc $
                mconcat
                  [ "Submit a transaction to the local node whose Unix domain socket "
                  , "is obtained from the CARDANO_NODE_SOCKET_PATH environment variable."
                  ]
    , Just $
        Opt.hsubparser $
          commandWithMetavar "policyid" $
            Opt.info pTransactionPolicyId $
              Opt.progDesc "Calculate the PolicyId from the monetary policy script."
    , Just $
        Opt.hsubparser $
          commandWithMetavar "calculate-min-fee" $
            Opt.info pTransactionCalculateMinFee $
              Opt.progDesc "Calculate the minimum fee for a transaction."
    , Just $
        Opt.hsubparser $
          commandWithMetavar "calculate-min-required-utxo" $
            Opt.info pTransactionCalculateMinReqUTxO $
              Opt.progDesc "Calculate the minimum required UTxO for a transaction output."
    , Just $
        Opt.hsubparser $
          commandWithMetavar "calculate-plutus-script-cost" $
            Opt.info (pTransactionCalculatePlutusScriptCost envCli) $
              Opt.progDesc "Calculate the costs of the Plutus scripts of a given transaction."
    , Just pCalculateMinRequiredUtxoBackwardCompatible
    , Just $
        Opt.hsubparser $
          commandWithMetavar "hash-script-data" $
            Opt.info pTxHashScriptData $
              Opt.progDesc "Calculate the hash of script data."
    , Just $
        Opt.hsubparser $
          commandWithMetavar "txid" $
            Opt.info pTransactionId $
              Opt.progDesc "Print a transaction identifier."
    ]

-- Backwards compatible parsers
calcMinValueInfo :: Exp.IsEra era => ParserInfo (TransactionCmds era)
calcMinValueInfo =
  Opt.info (pTransactionCalculateMinReqUTxO <**> Opt.helper) $
    Opt.progDesc "DEPRECATED: Use 'calculate-min-required-utxo' instead."

pCalculateMinRequiredUtxoBackwardCompatible
  :: forall era. Exp.IsEra era => Parser (TransactionCmds era)
pCalculateMinRequiredUtxoBackwardCompatible =
  Opt.subparser @(TransactionCmds era) $
    Opt.command "calculate-min-value" calcMinValueInfo <> Opt.internal

assembleInfo :: ParserInfo (TransactionCmds era)
assembleInfo =
  Opt.info (pTransactionAssembleTxBodyWit <**> Opt.helper) $
    Opt.progDesc "Assemble a tx body and witness(es) to form a transaction"

pSignWitnessBackwardCompatible :: Parser (TransactionCmds era)
pSignWitnessBackwardCompatible =
  Opt.subparser $
    Opt.command "sign-witness" assembleInfo <> Opt.internal

pScriptValidity :: Parser ScriptValidity
pScriptValidity =
  asum
    [ Opt.flag' ScriptValid $
        mconcat
          [ Opt.long "script-valid"
          , Opt.help "Assertion that the script is valid. (default)"
          ]
    , Opt.flag' ScriptInvalid $
        mconcat
          [ Opt.long "script-invalid"
          , Opt.help $
              mconcat
                [ "Assertion that the script is invalid.  "
                , "If a transaction is submitted with such a script, "
                , "the script will fail and the collateral will be taken."
                ]
          ]
    ]

pTransactionBuildCmd
  :: forall era. Exp.IsEra era => EnvCli -> Maybe (Parser (TransactionCmds era))
pTransactionBuildCmd envCli = do
  era' <- forEraMaybeEon (convert $ Exp.useEra @era)
  pure $
    Opt.hsubparser $
      commandWithMetavar "build" $
        Opt.info (pCmd era') $
          Opt.progDescDoc $
            Just $
              mconcat
                [ pretty @String "Build a balanced transaction (automatically calculates fees)"
                , line
                , line
                , H.annotate (H.color H.Yellow) $
                    mconcat
                      [ "Please note "
                      , H.annotate H.underlined "the order"
                      , " of some cmd options is crucial. If used incorrectly may produce "
                      , "undesired tx body. See nested [] notation above for details."
                      ]
                ]
 where
  pCmd era' = do
    fmap TransactionBuildCmd $
      TransactionBuildCmdArgs era'
        <$> ( LocalNodeConnectInfo
                <$> pConsensusModeParams
                <*> pNetworkId envCli
                <*> pSocketPath envCli
            )
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
        <*> (fmap join . optional $ pMintMultiAsset @era AutoBalance)
        <*> optional pInvalidBefore
        <*> pInvalidHereafter era'
        <*> many (pCertificateFile AutoBalance)
        <*> many (pWithdrawal AutoBalance)
        <*> pTxMetadataJsonSchema
        <*> many
          ( pScriptFor
              "auxiliary-script-file"
              Nothing
              "Filepath of auxiliary script(s)"
          )
        <*> many pMetadataFile
        <*> pFeatured era' (optional pUpdateProposalFile)
        <*> pVoteFiles AutoBalance
        <*> pProposalFiles AutoBalance
        <*> pTreasuryDonation
        <*> pIsCborOutCanonical
        <*> pTxBuildOutputOptions

-- | Estimate the transaction fees without access to a live node.
pTransactionBuildEstimateCmd
  :: forall era
   . Exp.IsEra era
  => EnvCli -> Maybe (Parser (TransactionCmds era))
pTransactionBuildEstimateCmd _envCli = do
  pure $
    Opt.hsubparser $
      commandWithMetavar "build-estimate" $
        Opt.info pCmd $
          Opt.progDescDoc $
            Just $
              mconcat
                [ pretty @String
                    "Build a balanced transaction without access to a live node (automatically estimates fees)"
                , line
                , line
                , H.annotate (H.color H.Yellow) $
                    mconcat
                      [ "Please note "
                      , H.annotate H.underlined "the order"
                      , " of some cmd options is crucial. If used incorrectly may produce "
                      , "undesired tx body. See nested [] notation above for details."
                      ]
                ]
 where
  pCmd :: Parser (TransactionCmds era)
  pCmd = do
    fmap TransactionBuildEstimateCmd $
      TransactionBuildEstimateCmdArgs Exp.useEra
        <$> optional pScriptValidity
        <*> pNumberOfShelleyKeyWitnesses
        <*> optional pNumberOfByronKeyWitnesses
        <*> pProtocolParamsFile
        <*> pTotalUTxOValue
        <*> some (pTxIn ManualBalance)
        <*> many pReadOnlyReferenceTxIn
        <*> many pRequiredSigner
        <*> many pTxInCollateral
        <*> optional pReturnCollateral
        <*> many pTxOut
        <*> pChangeAddress
        <*> (fmap join . optional $ pMintMultiAsset @era ManualBalance)
        <*> optional pInvalidBefore
        <*> pInvalidHereafter Exp.useEra
        <*> many (pCertificateFile ManualBalance)
        <*> many (pWithdrawal ManualBalance)
        <*> optional pTotalCollateral
        <*> optional pReferenceScriptSize
        <*> pTxMetadataJsonSchema
        <*> many
          ( pScriptFor
              "auxiliary-script-file"
              Nothing
              "Filepath of auxiliary script(s)"
          )
        <*> many pMetadataFile
        <*> pVoteFiles ManualBalance
        <*> pProposalFiles ManualBalance
        <*> pCurrentTreasuryValueAndDonation
        <*> pIsCborOutCanonical
        <*> pTxBodyFileOut

pChangeAddress :: Parser TxOutChangeAddress
pChangeAddress =
  fmap TxOutChangeAddress $
    Opt.option (readerFromParsecParser parseAddressAny) $
      mconcat
        [ Opt.long "change-address"
        , Opt.metavar "ADDRESS"
        , Opt.help "Address where ADA in excess of the tx fee will go to."
        ]

pTransactionBuildRaw :: forall era. Exp.IsEra era => Parser (TransactionCmds era)
pTransactionBuildRaw =
  fmap TransactionBuildRawCmd $
    TransactionBuildRawCmdArgs Exp.useEra
      <$> optional pScriptValidity
      <*> some (pTxIn ManualBalance)
      <*> many pReadOnlyReferenceTxIn
      <*> many pTxInCollateral
      <*> optional pReturnCollateral
      <*> optional pTotalCollateral
      <*> many pRequiredSigner
      <*> many pTxOut
      <*> (fmap join . optional $ pMintMultiAsset @era ManualBalance)
      <*> optional pInvalidBefore
      <*> pInvalidHereafter Exp.useEra
      <*> pTxFee
      <*> many (pCertificateFile ManualBalance)
      <*> many (pWithdrawal ManualBalance)
      <*> pTxMetadataJsonSchema
      <*> many (pScriptFor "auxiliary-script-file" Nothing "Filepath of auxiliary script(s)")
      <*> many pMetadataFile
      <*> optional pProtocolParamsFile
      <*> pFeatured Exp.useEra (optional pUpdateProposalFile)
      <*> pVoteFiles ManualBalance
      <*> pProposalFiles ManualBalance
      <*> pCurrentTreasuryValueAndDonation
      <*> pIsCborOutCanonical
      <*> pTxBodyFileOut

pTransactionSign :: EnvCli -> Parser (TransactionCmds era)
pTransactionSign envCli =
  fmap TransactionSignCmd $
    TransactionSignCmdArgs
      <$> pInputTxOrTxBodyFile
      <*> many pWitnessSigningData
      <*> optional (pNetworkId envCli)
      <*> pIsCborOutCanonical
      <*> pTxFileOut

pTransactionCreateWitness :: EnvCli -> Parser (TransactionCmds era)
pTransactionCreateWitness envCli =
  fmap TransactionWitnessCmd $
    TransactionWitnessCmdArgs
      <$> pTxBodyFileIn
      <*> pWitnessSigningData
      <*> optional (pNetworkId envCli)
      <*> pOutputFile

pTransactionAssembleTxBodyWit :: Parser (TransactionCmds era)
pTransactionAssembleTxBodyWit =
  fmap TransactionSignWitnessCmd $
    TransactionSignWitnessCmdArgs
      <$> pTxBodyFileIn
      <*> many pWitnessFile
      <*> pIsCborOutCanonical
      <*> pOutputFile

pTransactionSubmit :: EnvCli -> Parser (TransactionCmds era)
pTransactionSubmit envCli =
  fmap TransactionSubmitCmd $
    TransactionSubmitCmdArgs
      <$> ( LocalNodeConnectInfo
              <$> pConsensusModeParams
              <*> pNetworkId envCli
              <*> pSocketPath envCli
          )
      <*> pTxSubmitFile

pTransactionPolicyId :: Parser (TransactionCmds era)
pTransactionPolicyId =
  fmap TransactionPolicyIdCmd $
    TransactionPolicyIdCmdArgs
      <$> pScript

pTransactionCalculateMinFee :: Parser (TransactionCmds era)
pTransactionCalculateMinFee =
  fmap TransactionCalculateMinFeeCmd $
    TransactionCalculateMinFeeCmdArgs
      <$> pTxBodyFileIn
      <*> pProtocolParamsFile
      <*> pTxShelleyWitnessCount
      <*> pTxByronWitnessCount
      <*> pReferenceScriptSize
      <*> pFormatFlags
        "calculate-min-fee query output"
        [ flagFormatJson & setDefault
        , flagFormatText
        , flagFormatYaml
        ]
      <*> optional pOutputFile
      -- Deprecated options:
      <* optional pNetworkIdDeprecated
      <* optional pTxInCountDeprecated
      <* optional pTxOutCountDeprecated

pTransactionCalculateMinReqUTxO :: Exp.IsEra era => Parser (TransactionCmds era)
pTransactionCalculateMinReqUTxO =
  fmap TransactionCalculateMinValueCmd $
    TransactionCalculateMinValueCmdArgs Exp.useEra
      <$> pProtocolParamsFile
      <*> pTxOutShelleyBased

pTransactionCalculatePlutusScriptCost
  :: EnvCli -> Parser (TransactionCmds era)
pTransactionCalculatePlutusScriptCost envCli =
  ( Opt.hsubparser
      . commandWithMetavar "online"
      . Opt.info (pTransactionCalculatePlutusScriptCostParams (pNodeConnectionInfo envCli))
      $ Opt.progDesc
        "Connect to a running node to get context info and calculate the costs of the Plutus scripts of a given transaction."
  )
    <|> ( Opt.hsubparser
            . commandWithMetavar "offline"
            . Opt.info (pTransactionCalculatePlutusScriptCostParams pLocalContext)
            $ Opt.progDesc
              "Manually provide get context info and calculate the costs of the Plutus scripts of a given transaction."
        )
 where
  pTransactionCalculatePlutusScriptCostParams nodeContext =
    TransactionCalculatePlutusScriptCostCmd
      <$> ( TransactionCalculatePlutusScriptCostCmdArgs
              <$> nodeContext
              <*> pTxInputFile
              <*> optional pOutputFile
          )

  pTxInputFile :: Parser FilePath
  pTxInputFile = parseFilePath "tx-file" "Filepath of the transaction whose Plutus scripts to calculate the cost."

pLocalContext :: Parser (NodeContextInfoSource era)
pLocalContext =
  ProvidedTransactionContextInfo
    <$> ( TransactionContext
            <$> pSystemStart
            <*> pMustExtendEraHistorySafeZone
            <*> pEraHistoryFile
            <*> pUtxoFile
            <*> pProtocolParamsFile
        )

pMustExtendEraHistorySafeZone :: Parser MustExtendSafeZone
pMustExtendEraHistorySafeZone =
  Opt.flag'
    MustExtendSafeZone
    ( mconcat
        [ Opt.long "unsafe-extend-safe-zone"
        , Opt.help $
            mconcat
              [ "Allow overriding the validity of the era history past the safe zone. The "
              , "safe zone is a period of time during which we are sure there won't be any "
              , "era transition (hard fork), and we are confident that the slot duration "
              , "will not change, thus the conversion from slot numbers to POSIX times "
              , "using the era history will be correct. "
              , "This safe zone is conservative. Even if we are past the safe zone, if "
              , "there hasn't been any era transition (hard fork) since we obtained it, we can "
              , "continue safely using the era history. "
              , "This flag essentially disables the safe zone check. This allows the user to "
              , "use the era history past the safe zone, at the user's discretion."
              ]
        ]
    )
    <|> pure DoNotExtendSafeZone

pSystemStart :: Parser SystemStartOrGenesisFileSource
pSystemStart =
  asum
    [ SystemStartLiteral <$> (systemStartUTC <|> systemStartPOSIX)
    , SystemStartFromGenesisFile . GenesisFile
        <$> parseFilePath
          "genesis-file"
          "Path to the Byron genesis file from which to get the start time."
    ]

systemStartPOSIX :: Parser SystemStart
systemStartPOSIX =
  SystemStart . posixSecondsToUTCTime . fromInteger
    <$> ( Opt.option integralReader $
            mconcat
              [ Opt.long "start-time-posix"
              , Opt.metavar "POSIX_TIME"
              , Opt.help
                  "The genesis start time as POSIX seconds."
              ]
        )

systemStartUTC :: Parser SystemStart
systemStartUTC =
  SystemStart . convertTime
    <$> ( Opt.strOption $
            mconcat
              [ Opt.long "start-time-utc"
              , Opt.metavar "UTC_TIME"
              , Opt.help
                  "The genesis start time in YYYY-MM-DDThh:mm:ssZ format."
              ]
        )

pEraHistoryFile :: Parser (File EraHistory In)
pEraHistoryFile =
  File
    <$> parseFilePath
      "era-history-file"
      ( mconcat
          [ "Filepath of the era history file as produced by the 'query era-history' command. "
          , "The era history contains information about when era transitions happened and can "
          , "be used together with the start time to convert slot numbers to POSIX times."
          ]
      )

pUtxoFile :: Parser (File (Some UTxO) In)
pUtxoFile =
  File
    <$> ( parseFilePath "utxo-file" $
            mconcat
              [ "Filepath to a JSON-encoded UTxO file as produced by the 'query utxo' "
              , "command. Only UTxOs referenced by the transaction are needed, not the "
              , "whole UTxO, but unnecessary info will be ignored."
              ]
        )

pNodeConnectionInfo :: EnvCli -> Parser (NodeContextInfoSource era)
pNodeConnectionInfo envCli =
  NodeConnectionInfo
    <$> ( LocalNodeConnectInfo
            <$> pConsensusModeParams
            <*> pNetworkId envCli
            <*> pSocketPath envCli
        )

pTxHashScriptData :: Parser (TransactionCmds era)
pTxHashScriptData =
  fmap TransactionHashScriptDataCmd $
    TransactionHashScriptDataCmdArgs
      <$> pScriptDataOrFile
        "script-data"
        "The script data."
        "The script data file."

pTransactionId :: Parser (TransactionCmds era)
pTransactionId =
  fmap TransactionTxIdCmd $
    TransactionTxIdCmdArgs
      <$> pInputTxOrTxBodyFile
      <*> pFormatFlags
        "output"
        [ flagFormatJson & setDefault
        , flagFormatText
        , flagFormatYaml
        ]

pIsCborOutCanonical :: Parser TxCborFormat
pIsCborOutCanonical =
  ( Opt.switch $
      mconcat
        [ Opt.long "out-canonical-cbor"
        , Opt.help
            "Produce transaction in canonical CBOR according to RFC7049. Only this part of CIP-21 is implemented."
        ]
  )
    <&> \case
      True -> TxCborCanonical
      False -> TxCborNotCanonical
