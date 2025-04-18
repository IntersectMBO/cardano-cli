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
import Cardano.CLI.Type.Common

import Control.Monad
import Data.Foldable
import Data.Function ((&))
import Data.Functor
import Options.Applicative hiding (help, str)
import Options.Applicative qualified as Opt
import Options.Applicative.Help qualified as H
import Prettyprinter (line)

{- HLINT ignore "Use <$>" -}
{- HLINT ignore "Move brackets to avoid $" -}

pTransactionCmds
  :: ()
  => ShelleyBasedEra era
  -> EnvCli
  -> Maybe (Parser (TransactionCmds era))
pTransactionCmds era' envCli =
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
            Opt.info (pTransactionBuildRaw era') $
              Opt.progDescDoc $
                Just $
                  mconcat
                    [ pretty @String "Build a transaction (low-level, inconvenient)"
                    , line
                    , line
                    , H.yellow $
                        mconcat
                          [ "Please note "
                          , H.underline "the order"
                          , " of some cmd options is crucial. If used incorrectly may produce "
                          , "undesired tx body. See nested [] notation above for details."
                          ]
                    ]
    , pTransactionBuildCmd era' envCli
    , forShelleyBasedEraInEon era' Nothing (`pTransactionBuildEstimateCmd` envCli)
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
            Opt.info (pTransactionCalculateMinReqUTxO era') $
              Opt.progDesc "Calculate the minimum required UTxO for a transaction output."
    , Just $
        Opt.hsubparser $
          commandWithMetavar "calculate-plutus-script-cost" $
            Opt.info (pTransactionCalculatePlutusScriptCost envCli) $
              Opt.progDesc "Calculate the costs of the Plutus scripts of a given transaction."
    , Just $ pCalculateMinRequiredUtxoBackwardCompatible era'
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
calcMinValueInfo :: ShelleyBasedEra era -> ParserInfo (TransactionCmds era)
calcMinValueInfo era' =
  Opt.info (pTransactionCalculateMinReqUTxO era') $
    Opt.progDesc "DEPRECATED: Use 'calculate-min-required-utxo' instead."

pCalculateMinRequiredUtxoBackwardCompatible :: ShelleyBasedEra era -> Parser (TransactionCmds era)
pCalculateMinRequiredUtxoBackwardCompatible era' =
  Opt.subparser $
    Opt.command "calculate-min-value" (calcMinValueInfo era') <> Opt.internal

assembleInfo :: ParserInfo (TransactionCmds era)
assembleInfo =
  Opt.info pTransactionAssembleTxBodyWit $
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
  :: ShelleyBasedEra era -> EnvCli -> Maybe (Parser (TransactionCmds era))
pTransactionBuildCmd sbe envCli = do
  era' <- forEraMaybeEon (toCardanoEra sbe)
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
                , H.yellow $
                    mconcat
                      [ "Please note "
                      , H.underline "the order"
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
        <*> some (pTxIn sbe AutoBalance)
        <*> many pReadOnlyReferenceTxIn
        <*> many pRequiredSigner
        <*> many pTxInCollateral
        <*> optional pReturnCollateral
        <*> optional pTotalCollateral
        <*> many pTxOut
        <*> pChangeAddress
        <*> (fmap join . optional $ pMintMultiAsset sbe AutoBalance)
        <*> optional pInvalidBefore
        <*> pInvalidHereafter sbe
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
        <*> pVoteFiles sbe AutoBalance
        <*> pProposalFiles sbe AutoBalance
        <*> pTreasuryDonation sbe
        <*> pIsCborOutCanonical
        <*> pTxBuildOutputOptions

-- | Estimate the transaction fees without access to a live node.
pTransactionBuildEstimateCmd
  :: forall era. MaryEraOnwards era -> EnvCli -> Maybe (Parser (TransactionCmds era))
pTransactionBuildEstimateCmd eon' _envCli = do
  era' <- forEraMaybeEon (toCardanoEra eon')
  pure $
    Opt.hsubparser $
      commandWithMetavar "build-estimate" $
        Opt.info (pCmd era') $
          Opt.progDescDoc $
            Just $
              mconcat
                [ pretty @String
                    "Build a balanced transaction without access to a live node (automatically estimates fees)"
                , line
                , line
                , H.yellow $
                    mconcat
                      [ "Please note "
                      , H.underline "the order"
                      , " of some cmd options is crucial. If used incorrectly may produce "
                      , "undesired tx body. See nested [] notation above for details."
                      ]
                ]
 where
  pCmd :: Exp.Era era -> Parser (TransactionCmds era)
  pCmd era' = do
    let sbe = convert era'
    fmap TransactionBuildEstimateCmd $
      TransactionBuildEstimateCmdArgs era'
        <$> optional pScriptValidity
        <*> pNumberOfShelleyKeyWitnesses
        <*> optional pNumberOfByronKeyWitnesses
        <*> pProtocolParamsFile
        <*> pTotalUTxOValue
        <*> some (pTxIn sbe ManualBalance)
        <*> many pReadOnlyReferenceTxIn
        <*> many pRequiredSigner
        <*> many pTxInCollateral
        <*> optional pReturnCollateral
        <*> many pTxOut
        <*> pChangeAddress
        <*> (fmap join . optional $ pMintMultiAsset sbe ManualBalance)
        <*> optional pInvalidBefore
        <*> pInvalidHereafter sbe
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
        <*> pFeatured (toCardanoEra sbe) (optional pUpdateProposalFile)
        <*> pVoteFiles sbe ManualBalance
        <*> pProposalFiles sbe ManualBalance
        <*> pCurrentTreasuryValueAndDonation sbe
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

pTransactionBuildRaw :: ShelleyBasedEra era -> Parser (TransactionCmds era)
pTransactionBuildRaw era' =
  fmap TransactionBuildRawCmd $
    TransactionBuildRawCmdArgs era'
      <$> optional pScriptValidity
      <*> some (pTxIn era' ManualBalance)
      <*> many pReadOnlyReferenceTxIn
      <*> many pTxInCollateral
      <*> optional pReturnCollateral
      <*> optional pTotalCollateral
      <*> many pRequiredSigner
      <*> many pTxOut
      <*> (fmap join . optional $ pMintMultiAsset era' ManualBalance)
      <*> optional pInvalidBefore
      <*> pInvalidHereafter era'
      <*> pTxFee
      <*> many (pCertificateFile ManualBalance)
      <*> many (pWithdrawal ManualBalance)
      <*> pTxMetadataJsonSchema
      <*> many (pScriptFor "auxiliary-script-file" Nothing "Filepath of auxiliary script(s)")
      <*> many pMetadataFile
      <*> optional pProtocolParamsFile
      <*> pFeatured era' (optional pUpdateProposalFile)
      <*> pVoteFiles era' ManualBalance
      <*> pProposalFiles era' ManualBalance
      <*> pCurrentTreasuryValueAndDonation era'
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
        ]
      <*> optional pOutputFile
      -- Deprecated options:
      <* optional pNetworkIdDeprecated
      <* optional pTxInCountDeprecated
      <* optional pTxOutCountDeprecated

pTransactionCalculateMinReqUTxO :: ShelleyBasedEra era -> Parser (TransactionCmds era)
pTransactionCalculateMinReqUTxO era' =
  fmap TransactionCalculateMinValueCmd $
    TransactionCalculateMinValueCmdArgs era'
      <$> pProtocolParamsFile
      <*> pTxOutShelleyBased

pTransactionCalculatePlutusScriptCost :: EnvCli -> Parser (TransactionCmds era)
pTransactionCalculatePlutusScriptCost envCli =
  fmap TransactionCalculatePlutusScriptCostCmd $
    TransactionCalculatePlutusScriptCostCmdArgs
      <$> ( LocalNodeConnectInfo
              <$> pConsensusModeParams
              <*> pNetworkId envCli
              <*> pSocketPath envCli
          )
      <*> pTxInputFile
      <*> optional pOutputFile
 where
  pTxInputFile :: Parser FilePath
  pTxInputFile = parseFilePath "tx-file" "Filepath of the transaction whose Plutus scripts to calculate the cost."

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
