{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.CLI.EraBased.Options.Transaction
  ( pTransactionCmds
  ) where

import           Cardano.Api hiding (QueryInShelleyBasedEra (..))

import           Cardano.CLI.Environment (EnvCli (..))
import           Cardano.CLI.EraBased.Commands.Transaction
import           Cardano.CLI.EraBased.Options.Common
import           Cardano.CLI.Types.Common

import           Data.Foldable
import           Options.Applicative hiding (help, str)
import qualified Options.Applicative as Opt
import qualified Options.Applicative.Help as H
import           Prettyprinter (line, pretty)

{- HLINT ignore "Use <$>" -}
{- HLINT ignore "Move brackets to avoid $" -}

pTransactionCmds :: ()
  => CardanoEra era
  -> EnvCli
  -> Maybe (Parser (TransactionCmds era))
pTransactionCmds era envCli =
  subInfoParser "transaction"
    ( Opt.progDesc
        $ mconcat
          [ "Transaction commands."
          ]
    )
    [ Just
        $ subParser "build-raw"
        $ Opt.info (pTransactionBuildRaw era)
        $ Opt.progDescDoc
        $ Just $ mconcat
            [ pretty @String "Build a transaction (low-level, inconvenient)"
            , line
            , line
            , H.yellow $ mconcat
                [ "Please note the order of some cmd options is crucial. If used incorrectly may produce "
                , "undesired tx body. See nested [] notation above for details."
                ]
            ]
    , pTransactionBuildCmd era envCli
    , Just
        $ subParser "sign"
        $ Opt.info (pTransactionSign envCli)
        $ Opt.progDesc "Sign a transaction"
    , Just
        $ subParser "witness"
        $ Opt.info (pTransactionCreateWitness envCli)
        $ Opt.progDesc "Create a transaction witness"
    , Just
        $ subParser "assemble"
        $ Opt.info pTransactionAssembleTxBodyWit
        $ Opt.progDesc "Assemble a tx body and witness(es) to form a transaction"
    , Just pSignWitnessBackwardCompatible
    , Just
        $ subParser "submit"
        $ Opt.info (pTransactionSubmit envCli)
        $ Opt.progDesc
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
        $ Opt.info (pTransactionCalculateMinFee envCli)
        $ Opt.progDesc "Calculate the minimum fee for a transaction."
    , caseByronOrShelleyBasedEra
        (const Nothing)
        (\sbe -> Just $ subParser "calculate-min-required-utxo"
                      $ Opt.info (pTransactionCalculateMinReqUTxO sbe)
                      $ Opt.progDesc "Calculate the minimum required UTxO for a transaction output."
        )
        era
    , caseByronOrShelleyBasedEra
        (const Nothing)
        (Just . pCalculateMinRequiredUtxoBackwardCompatible)
        era
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

-- Backwards compatible parsers
calcMinValueInfo :: ShelleyBasedEra era -> ParserInfo (TransactionCmds era)
calcMinValueInfo era =
  Opt.info (pTransactionCalculateMinReqUTxO era)
    $ Opt.progDesc "DEPRECATED: Use 'calculate-min-required-utxo' instead."

pCalculateMinRequiredUtxoBackwardCompatible :: ShelleyBasedEra era -> Parser (TransactionCmds era)
pCalculateMinRequiredUtxoBackwardCompatible era =
  Opt.subparser
    $ Opt.command "calculate-min-value" (calcMinValueInfo era) <> Opt.internal

assembleInfo :: ParserInfo (TransactionCmds era)
assembleInfo =
  Opt.info pTransactionAssembleTxBodyWit
    $ Opt.progDesc "Assemble a tx body and witness(es) to form a transaction"

pSignWitnessBackwardCompatible :: Parser (TransactionCmds era)
pSignWitnessBackwardCompatible =
  Opt.subparser
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

pTransactionBuildCmd :: CardanoEra era -> EnvCli -> Maybe (Parser (TransactionCmds era))
pTransactionBuildCmd era envCli = do
  w <- forEraMaybeEon era
  pure
    $ subParser "build"
    $ Opt.info (pCmd w)
    $ Opt.progDescDoc
    $ Just $ mconcat
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
  where
    pCmd :: ShelleyBasedEra era ->  Parser (TransactionCmds era)
    pCmd sbe =
      fmap TransactionBuildCmd $
        TransactionBuildCmdArgs sbe
          <$> pSocketPath envCli
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
          <*> pInvalidHereafter (shelleyBasedToCardanoEra sbe)
          <*> many (pCertificateFile AutoBalance)
          <*> many (pWithdrawal AutoBalance)
          <*> pTxMetadataJsonSchema
          <*> many (pScriptFor
                      "auxiliary-script-file"
                      Nothing
                      "Filepath of auxiliary script(s)")
          <*> many pMetadataFile
          <*> pFeatured (shelleyBasedToCardanoEra sbe) (optional pUpdateProposalFile)
          <*> many (pFileInDirection "vote-file" "Filepath of the vote.")
          <*> many (pFileInDirection "proposal-file" "Filepath of the proposal.")
          <*> (OutputTxBodyOnly <$> pTxBodyFileOut <|> pCalculatePlutusScriptCost)

pChangeAddress :: Parser TxOutChangeAddress
pChangeAddress =
  fmap TxOutChangeAddress $ Opt.option (readerFromParsecParser parseAddressAny) $ mconcat
    [ Opt.long "change-address"
    , Opt.metavar "ADDRESS"
    , Opt.help "Address where ADA in excess of the tx fee will go to."
    ]

pTransactionBuildRaw :: CardanoEra era -> Parser (TransactionCmds era)
pTransactionBuildRaw era =
  fmap TransactionBuildRawCmd $
    TransactionBuildRawCmdArgs era
      <$> optional pScriptValidity
      <*> some (pTxIn ManualBalance)
      <*> many pReadOnlyReferenceTxIn
      <*> many pTxInCollateral
      <*> optional pReturnCollateral
      <*> optional pTotalCollateral
      <*> many pRequiredSigner
      <*> many pTxOut
      <*> optional (pMintMultiAsset ManualBalance)
      <*> optional pInvalidBefore
      <*> pInvalidHereafter era
      <*> optional pTxFee
      <*> many (pCertificateFile ManualBalance )
      <*> many (pWithdrawal ManualBalance)
      <*> pTxMetadataJsonSchema
      <*> many (pScriptFor "auxiliary-script-file" Nothing "Filepath of auxiliary script(s)")
      <*> many pMetadataFile
      <*> optional pProtocolParamsFile
      <*> pFeatured era (optional pUpdateProposalFile)
      <*> many (pFileInDirection "vote-file" "Filepath of the vote.")
      <*> many (pFileInDirection "proposal-file" "Filepath of the proposal.")
      <*> pTxBodyFileOut

pTransactionSign  :: EnvCli -> Parser (TransactionCmds era)
pTransactionSign envCli =
  fmap TransactionSignCmd $
    TransactionSignCmdArgs
      <$> pInputTxOrTxBodyFile
      <*> many pWitnessSigningData
      <*> optional (pNetworkId envCli)
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
      <*> pOutputFile

pTransactionSubmit :: EnvCli -> Parser (TransactionCmds era)
pTransactionSubmit envCli =
  fmap TransactionSubmitCmd $
    TransactionSubmitCmdArgs
      <$> pSocketPath envCli
      <*> pConsensusModeParams
      <*> pNetworkId envCli
      <*> pTxSubmitFile

pTransactionPolicyId :: Parser (TransactionCmds era)
pTransactionPolicyId =
  fmap TransactionPolicyIdCmd $
    TransactionPolicyIdCmdArgs
      <$> pScript

pTransactionCalculateMinFee :: EnvCli -> Parser (TransactionCmds era)
pTransactionCalculateMinFee envCli  =
  fmap TransactionCalculateMinFeeCmd $
    TransactionCalculateMinFeeCmdArgs
      <$> pTxBodyFileIn
      <*> pNetworkId envCli
      <*> pProtocolParamsFile
      <*> pTxInCount
      <*> pTxOutCount
      <*> pTxShelleyWitnessCount
      <*> pTxByronWitnessCount

pTransactionCalculateMinReqUTxO :: ShelleyBasedEra era -> Parser (TransactionCmds era)
pTransactionCalculateMinReqUTxO era =
  fmap TransactionCalculateMinValueCmd $
    TransactionCalculateMinValueCmdArgs era
      <$> pProtocolParamsFile
      <*> pTxOutShelleyBased

pTxHashScriptData :: Parser (TransactionCmds era)
pTxHashScriptData =
  fmap TransactionHashScriptDataCmd $
    TransactionHashScriptDataCmdArgs
      <$> pScriptDataOrFile
            "script-data"
            "The script data, in JSON syntax."
            "The script data, in the given JSON file."

pTransactionId  :: Parser (TransactionCmds era)
pTransactionId =
  fmap TransactionTxIdCmd $
    TransactionTxIdCmdArgs
      <$> pInputTxOrTxBodyFile

pTransactionView :: Parser (TransactionCmds era)
pTransactionView =
  fmap TransactionViewCmd $
    TransactionViewCmdArgs
      <$> pTxViewOutputFormat
      <*> pMaybeOutputFile
      <*> pInputTxOrTxBodyFile
